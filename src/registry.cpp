
#include <algorithm>
#include <fstream>
#include <iostream>
#include <regex>

#include "registry.hpp"

const std::string whitespace = "\r\n\t\f\v";

std::string tolower(std::string s)
{
    for (auto &c : s)
        c = std::tolower(c);
    return s;
}

void Registry::lex_record(const Record &rec)
{
    std::shared_ptr<Module> mod;

    // If keyword is typedef, usefrom, or param, find or add module
    if ((rec.type == Record::Type::Param) || (rec.type == Record::Type::Field))
    {
        // Get lowercase module name
        std::string name_lower = tolower(rec.module_name);

        // Find module in map or add it to map
        auto it = this->modules.find(name_lower);
        if (it == this->modules.end())
        {
            mod = std::make_shared<Module>(rec.module_name, rec.module_nickname,
                                           rec.is_root);
            this->modules[name_lower] = mod;
        }
        else
        {
            mod = it->second;
        }
    }

    // Parameter
    if (rec.type == Record::Type::Param)
    {
        // Find parameter type in registry, display message if not found
        auto param_type = this->find_type(rec.param.type, this->data_types);
        if (param_type == nullptr)
        {
            std::cerr << "Registry warning: type " << rec.param.type
                      << " used before defined for " << rec.param.name << std::endl;
        }

        // Add parameter to module
        mod->params.push_back(Parameter(rec.param, param_type));
        return;
    }

    // Field record
    if (rec.type == Record::Type::Field)
    {
        // Get derived data type name
        auto ddt_name = rec.field.derived_type;
        auto ddt_name_short = ddt_name;

        // Remove module prefix from name
        std::string prefix = tolower(mod->nickname) + "_";
        if (tolower(ddt_name_short).compare(0, prefix.size(), prefix) == 0)
            ddt_name_short = ddt_name_short.substr(prefix.size());

        // If interface name was found for derived data type, prepend module nickname
        auto it = this->interface_names.find(tolower(ddt_name_short));
        auto is_interface_type = it != this->interface_names.end();
        if (is_interface_type)
        {
            ddt_name = mod->nickname + "_" + ddt_name_short;
        }

        // Get structure type from module
        auto ddt = this->find_type(tolower(ddt_name), mod->data_types);

        // If struct type not found and module is not root, get type from registry
        if (ddt == nullptr && !mod->is_root)
            ddt = this->find_type(tolower(ddt_name), this->data_types);

        // If structure type not found, create and add to module or registry
        if (ddt == nullptr)
        {
            // Get short name from interface if this is an interface type
            if (is_interface_type)
                ddt_name_short = it->second.name_short;

            // Create struct type
            ddt = std::make_shared<DataType>(rec.field.derived_type, ddt_name,
                                             DataType::Type::Derived, mod);

            ddt->is_interface = is_interface_type;
            ddt->name_base = ddt_name_short;

            // Add type to registry if not root, else add to module
            if (!rec.is_root)
            {
                this->data_types[tolower(ddt_name)] = ddt;
            }
            else
            {
                mod->data_types[tolower(ddt_name)] = ddt;
                mod->data_type_order.push_back(tolower(ddt_name));
            }
        }

        // Get field type from module or registry
        auto field_type = this->find_type(rec.field.type, mod->data_types);
        if (field_type == nullptr)
            field_type = this->find_type(rec.field.type, this->data_types);
        if (field_type == nullptr)
        {
            std::cerr << "Registry warning: type " << rec.field.type
                      << " used before defined for " << rec.field.name << std::endl;
        }

        // Create field
        Field field(rec.field, field_type);

        // Collect field props into struct
        ddt->max_dims = std::max(field.dims.size(), ddt->max_dims);
        ddt->has_pointer |= field.has_pointer;

        // Create field and add to struct
        ddt->fields.push_back(field);
        return;
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
    Record rec(is_root);
    std::string line;
    for (size_t line_num = 1; std::getline(inp_file, line); ++line_num)
    {
        // Parse line into record
        rec.parse(line, file_name, line_num);

        // Skip empty lines and comment lines
        if (rec.type == Record::Type::Empty || rec.type == Record::Type::Comment)
        {
            continue;
        }

        // If include line, parse file and abort on error
        if (rec.type == Record::Type::Include)
        {
            if (this->parse(rec.include.file_name, false) != 0)
            {
                std::cerr << "error parsing" << rec.include.file_name << std::endl;
                return -1;
            }

            continue;
        }

        // Lex record
        this->lex_record(rec);
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

int Registry::gen_module_files(std::string const &out_dir)
{
    std::shared_ptr<Module> mod;
    for (auto &it : this->modules)
    {
        mod = it.second;
        if (mod->is_root)
            break;
    }

    auto file_name = mod->name + "_Types.f90";
    auto file_path = out_dir + "/" + file_name;
    std::cerr << "generating " << file_name << std::endl;

    std::ofstream out(file_path);
    if (!out)
    {
        return -1;
    }

    this->gen_fortran_module(out, *mod);

    // generate .h files for C/C++:
    if (this->gen_c_code)
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

        // this->gen_c_module(fph, p);

        out << "\n#endif // _" << mod->name << "_TYPES_H\n\n\n";
        out << "//ENDOFREGISTRYGENERATEDFILE\n";
    }

    return 0;
}