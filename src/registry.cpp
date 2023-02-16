#include "registry.hpp"

#include <algorithm>
#include <fstream>
#include <iostream>
#include <regex>

const std::string whitespace = "\r\n\t\f\v";

std::string tolower(std::string s)
{
    for (auto &c : s)
        c = std::tolower(c);
    return s;
}

int Registry::lex()
{
    // Loop through records
    for (auto &rec : this->records)
    {
        // rec.print();
        this->lex_record(rec);
    }

    return 0;
}

void Registry::lex_record(const Record &rec)
{
    std::shared_ptr<Module> mod;

    // If keyword is typedef, usefrom, or param, find or add module
    if ((rec.keyword.compare("typedef") == 0) || (rec.keyword.compare("usefrom") == 0) ||
        (rec.keyword.compare("param") == 0))
    {
        // Get lowercase module name
        std::string name_lower = tolower(rec.module_name);

        // Find module in map or add it to map
        auto it = this->modules.find(name_lower);
        if (it == this->modules.end())
        {
            mod = std::make_shared<Module>(rec.module_name, rec.module_nickname);
            this->modules[name_lower] = mod;
        }
        else
        {
            mod = it->second;
        }

        // If keyword is usefrom, set flag
        mod->use_from = (rec.keyword.compare("usefrom") == 0) || !rec.is_root;
    }
    else if (rec.keyword.compare("dimspec") == 0)
    {
        // Check if dimspec has already been defined
        auto name = rec.module_name;
        auto name_lower = tolower(rec.module_name);
        auto it = this->dim_specs.find(name_lower);
        if (it != this->dim_specs.end())
        {
            std::cerr << "Registry warning: dimspec (" << name << ") already defined"
                      << std::endl;
        }

        // Parse dimension spec
        auto dim_spec = std::make_shared<DimSpec>(rec.struct_name, name);

        // Add dim to registry
        this->dim_specs[name_lower] = dim_spec;
    }
    else
    {
        std::cerr << "Registry warning: unknown keyword " << rec.keyword << " in "
                  << rec.file_name << std::endl;
        return;
    }

    // If parameter
    if (rec.keyword.compare("param") == 0)
    {
        // Find parameter type in registry, display message if not found
        auto param_type = this->find_type(rec.field_type, this->data_types);
        if (param_type == nullptr)
        {
            std::cerr << "Registry warning: type " << rec.field_type
                      << " used before defined for " << rec.field_name << std::endl;
        }

        // Add parameter to module
        mod->params.push_back(Parameter(rec, param_type));
    }
    // Struct
    else
    {
        // Copy struct name and get lowercase name
        auto struct_name = rec.struct_name;
        auto struct_name_lower = tolower(struct_name);

        // Get interface iterator from struct name
        bool is_interface_type =
            this->interface_names.find(struct_name_lower) != this->interface_names.end();

        // If interface name was found for struct, prepend module nickname
        if (is_interface_type)
        {
            struct_name = mod->nickname + "_" + struct_name;
            struct_name_lower = tolower(struct_name);
        }

        // Get structure type from module
        auto struct_type = this->find_type(struct_name, mod->data_types);

        // If struct type not found and use_from set, get type from registry
        if (struct_type == nullptr && mod->use_from)
        {
            struct_type = this->find_type(struct_name, this->data_types);
        }

        // If structure type not found, create and add to module or registry
        if (struct_type == nullptr)
        {
            // Create struct type
            struct_type = std::make_shared<DataType>(rec.struct_name, struct_name,
                                                     TypeClass::Derived);
            struct_type->use_from = mod->use_from;
            struct_type->module = mod;

            // Add type to struct or registry depending on use_from
            if (mod->use_from)
            {
                this->data_types[struct_name_lower] = struct_type;
            }
            else
            {
                mod->data_types[struct_name_lower] = struct_type;
            }
        }

        // Get field type from module or registry
        auto field_type = this->find_type(rec.field_type, mod->data_types);
        if (field_type == nullptr)
            field_type = this->find_type(rec.field_type, this->data_types);
        if (field_type == nullptr)
        {
            std::cerr << "Registry warning: type " << rec.field_type
                      << " used before defined for " << rec.field_name << std::endl;
        }

        // Create field and add to struct
        struct_type->fields.push_back(Field(rec, field_type));
    }
}

int Registry::parse(const std::string &file_name, const bool is_root)
{
    std::ifstream inp_file;

    // If this is the root file, open given file name
    if (is_root)
    {
        std::cerr << "input file: " << file_name << std::endl;
        inp_file.open(file_name);
        if (!inp_file)
        {
            std::cerr << "Registry program cannot open " << file_name << " for reading. ";
            std::cerr << "Ending." << std::endl;
            return -1;
        }
    }
    // Otherwise, find and open include file
    else
    {
        // If this include file has been parsed, return
        if (this->include_files.find(file_name) != this->include_files.end())
            return 0;

        // Loop through directories and try to open file, break on success
        for (auto &dir : this->include_dirs)
        {
            inp_file.open(dir + "/" + file_name);
            if (inp_file)
                break;
        }

        // If file not opened successfully, return error
        if (!inp_file)
        {
            std::cerr << "Registry warning: cannot open " << file_name << ". Ignoring."
                      << std::endl;
            return 0;
        }

        // Display message about opening file
        std::cerr << "opening " << file_name << std::endl;

        // Add file to list of includes
        this->include_files.insert(file_name);
    }

    // Loop through lines in file to read records
    Record rec(file_name, is_root);
    std::string line;
    while (std::getline(inp_file, line))
    {
        // Remove whitespace from line beginning/end and reduce whitespace
        // inside
        line = std::regex_replace(line, std::regex("^\\s+"), "");
        line = std::regex_replace(line, std::regex("\\s+$"), "");
        line = std::regex_replace(line, std::regex("\\s+"), " ");

        // Skip empty lines and comment lines
        if (line.length() == 0 || line[0] == '#')
            continue;

        // If line starts with include or usefrom and remainder of line has no
        // spaces
        if (((line.compare(0, 8, "include ") == 0) ||
             (line.compare(0, 8, "usefrom ") == 0)) &&
            !std::regex_search(line.substr(8), std::regex("\\s+")))
        {
            // Assume remainder of line is a file name
            auto file_name = line.substr(8);

            // Parse file and abort on error
            if (this->parse(file_name, false) != 0)
            {
                std::cerr << "error parsing" << file_name << std::endl;
                return -1;
            }

            continue;
        }

        // Parse line into record
        rec.parse(line);

        // Store record
        this->records.push_back(rec);
    }

    return 0;
}

std::shared_ptr<DataType> Registry::find_type(
    std::string const &type_name,
    std::map<std::string, std::shared_ptr<DataType>> &data_types)
{
    // Convert type name to lowercase
    std::string type_name_lower = tolower(type_name);

    // Pointer to type
    std::shared_ptr<DataType> data_type;

    // Search for type in registry, return if found
    auto it = data_types.find(type_name_lower);
    if (it != data_types.end())
    {
        return it->second;
    }

    // If type starts with character (string type), build type and return it
    if (type_name_lower.compare(0, 9, "character") == 0)
    {
        // Build type
        data_type = std::make_shared<DataType>(type_name, type_name);
        data_type->string_len = type_name.substr(10, type_name.size() - 11);

        // Add type to registry
        this->data_types[type_name_lower] = data_type;
        return data_type;
    }

    return data_type;
}

int Registry::gen_module_files(std::string const &out_dir, const bool gen_c_code)
{
    for (auto &it : this->modules)
    {
        auto &mod = it.second;

        // If module's use from flag is set, continue
        if (mod->use_from)
        {
            continue;
        }

        auto file_name = mod->name + "_Types.f90";
        auto file_path = out_dir + "/" + file_name;
        std::cerr << "generating " << file_name << std::endl;

        std::ofstream out(file_path);
        if (!out)
        {
            return -1;
        }

        out << "!STARTOFREGISTRYGENERATEDFILE '" << file_name << "'\n";
        out << "!\n";
        out << "! WARNING This file is generated automatically by the FAST registry.\n";
        out << "! Do not edit.  Your changes to this file will be lost.\n";
        out << "!\n";

        // gen_module(fp, p, prog_ver);

        out << "!ENDOFREGISTRYGENERATEDFILE\n";

        // generate .h files for C/C++:
        if (gen_c_code)
        {
            auto file_name = mod->name + "_Types.h";
            auto file_path = out_dir + "/" + file_name;
            std::cerr << "generating " << file_name << std::endl;

            std::ofstream out(file_path);
            if (!out)
            {
                return -1;
            }

            out << "//STARTOFREGISTRYGENERATEDFILE '" << file_name << "'\n";
            out << "//\n";
            out << "// WARNING This file is generated automatically by the FAST "
                   "registry.\n";
            out << "// Do not edit.  Your changes to this file will be lost.\n";
            out << "//\n";

            out << "\n#ifndef _" << mod->name << "_TYPES_H\n";
            out << "#define _" << mod->name << "_TYPES_H\n\n";
            out << "\n#ifdef _WIN32 //define something for Windows (32-bit)\n";
            out << "#  include \"stdbool.h\"\n";
            out << "#  define CALL __declspec( dllexport )\n";
            out << "#elif _WIN64 //define something for Windows (64-bit)\n";
            out << "#  include \"stdbool.h\"\n";
            out << "#  define CALL __declspec( dllexport ) \n";
            out << "#else\n";
            out << "#  include <stdbool.h>\n";
            out << "#  define CALL \n";
            out << "#endif\n\n\n";

            // gen_c_module(fph, p);

            out << "\n#endif // _" << mod->name << "_TYPES_H\n\n\n";
            out << "//ENDOFREGISTRYGENERATEDFILE\n";
        }
    }
}