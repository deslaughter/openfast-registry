#include <fstream>
#include <iomanip>
#include <memory>
#include <regex>

#include "registry.hpp"

const int MAX_FIELDS = 10;

int Registry::parse(const std::string &file_name, const int recurse_level)
{
    std::ifstream inp_file;
    std::vector<std::string> fields_prev;

    // If this is the root file, open given file name
    if (recurse_level == 0)
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
        {
            return 0;
        }

        // Loop through directories and try to open file, break on success
        for (auto &dir : this->include_dirs)
        {
            inp_file.open(dir + "/" + file_name);
            if (inp_file)
            {
                break;
            }
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

    // Loop through lines in file and parse
    std::string line;
    for (size_t line_num = 1; std::getline(inp_file, line); ++line_num)
    {
        // Parse line into record
        if (this->parse_line(line, fields_prev, recurse_level) != 0)
        {
            std::cerr << "Error reading " << file_name << ":" << line_num << "\n";
            break;
        }
    }

    // If this file is included by the root file, save module
    if (recurse_level == 1)
    {
        auto slash_index = fields_prev[1].find("/");
        bool has_slash = slash_index != std::string::npos;
        auto module_name = has_slash ? fields_prev[1].substr(0, slash_index) : fields_prev[1];
        this->use_modules.push_back(module_name);
    }

    return 0;
}

const std::regex quote_split_re("\"");
const std::regex space_split_re("\\s+");
const std::regex strip_start("^\\s+");
const std::regex strip_end("\\s+$");
const std::sregex_token_iterator re_end;

int Registry::parse_line(const std::string &line, std::vector<std::string> &fields_prev,
                         const int recurse_level)
{
    std::istringstream iss(line);
    std::string s;
    std::vector<std::string> fields;

    // Read fields from line while respecting quotes
    while (iss >> std::quoted(s))
    {
        // If # found in unquoted field, break iteration
        if (s.find("#") != std::string::npos && s.find(" ") == std::string::npos)
            break;

        fields.push_back(s);
    }

    // Skip empty line
    if (fields.size() == 0 || fields[0][0] == '#')
    {
        return 0;
    }

    //--------------------------------------------------------------------------
    // Include Line
    //--------------------------------------------------------------------------

    if (fields.size() == 2 &&
        (fields[0].compare("include") == 0 || fields[0].compare("usefrom") == 0))
    {
        auto file_name = fields[1];
        if (this->parse(file_name, recurse_level + 1) != 0)
        {
            std::cerr << "error parsing" << file_name << std::endl;
            return -1;
        }
        return 0;
    }

    //--------------------------------------------------------------------------
    // Populate Fields
    //--------------------------------------------------------------------------

    // Resize and fill remaining fields
    fields.resize(MAX_FIELDS, "-");

    // Propagate field values from previous fields if requested
    for (int i = 0; i < MAX_FIELDS; i++)
        if (fields[i].compare("^") == 0)
            fields[i] = fields_prev[i];

    // Update previous fields to current values
    fields_prev = fields;

    //--------------------------------------------------------------------------
    // Get Module
    //--------------------------------------------------------------------------

    // Shared pointer to module
    std::shared_ptr<Module> mod;

    // Is this the root module
    auto is_root = recurse_level == 0;

    // Parse module name and nickname from field
    auto slash_index = fields[1].find("/");
    bool has_slash = slash_index != std::string::npos;
    auto module_name = has_slash ? fields[1].substr(0, slash_index) : fields[1];
    auto module_nickname = has_slash ? fields[1].substr(slash_index + 1) : fields[1];

    // Find module in map or add it to map
    auto it = this->modules.find(module_name);
    if (it == this->modules.end())
    {
        mod = std::make_shared<Module>(module_name, module_nickname, is_root);
        this->modules[module_name] = mod;
    }
    else
    {
        mod = it->second;
    }

    //--------------------------------------------------------------------------
    // Parameter Line
    //--------------------------------------------------------------------------

    if (fields[0].compare("param") == 0)
    {
        auto name = fields[4];
        auto type = fields[3];
        auto value = fields[6];
        auto desc = fields[8];
        auto units = fields[9];

        // Find parameter type in registry, display message if not found
        auto param_type = this->find_data_type(type);
        if (param_type == nullptr)
        {
            std::cerr << "Registry warning: type " << type << " used before defined for " << name
                      << std::endl;
        }

        // Add parameter to module
        mod->params.push_back(Parameter(name, param_type, value, desc, units));
        return 0;
    }

    //--------------------------------------------------------------------------
    // Derived Type Line
    //--------------------------------------------------------------------------

    if ((fields[0].compare("typedef") == 0) || (fields[0].compare("usefrom") == 0))
    {
        auto ddt_name_base = fields[2];
        auto field_type_name = fields[3];
        auto name = fields[4];
        auto dims = fields[5];
        auto init_value = fields[6];
        auto ctrl = fields[7];
        auto desc = fields[8];
        auto units = fields[9];

        // Get derived data type name
        auto ddt_name = ddt_name_base;
        auto ddt_name_short = ddt_name_base;

        // Remove module prefix from name
        std::string prefix = tolower(mod->nickname) + "_";
        if (tolower(ddt_name_short).compare(0, prefix.size(), prefix) == 0)
        {
            ddt_name_short = ddt_name_short.substr(prefix.size());
        }

        // If interface name was found for derived data type, prepend module nickname
        auto it = this->interface_names.find(ddt_name_short);
        auto is_interface_type = it != this->interface_names.end();
        if (is_interface_type)
        {
            ddt_name = mod->nickname + "_" + ddt_name_short;
        }

        // Get data type from module
        auto ddt_dt = this->find_data_type(ddt_name, mod);

        // If struct type not found and module is not root, get from registry
        if (ddt_dt == nullptr && !mod->is_root)
            ddt_dt = this->find_data_type(ddt_name);

        // If derived data type not found, create and add to module or registry
        if (ddt_dt == nullptr)
        {
            // Get short name from interface if this is an interface type
            if (is_interface_type)
                ddt_name_short = it->second->name_short;

            // Create derived data type
            ddt_dt = std::make_shared<DataType>(mod, ddt_name_base, ddt_name_short, ddt_name);

            // Add interface to type if found
            if (is_interface_type)
                ddt_dt->derived.interface = it->second;

            // Add type module if this is root; otherwise, add to registry
            if (is_root)
            {
                mod->data_types[ddt_name] = ddt_dt;
                mod->data_type_order.push_back(ddt_name);
            }
            else
            {
                this->data_types[ddt_name] = ddt_dt;
            }
        }

        // Get field data type from module or registry
        auto field_dt = this->find_data_type(field_type_name, mod);
        if (field_dt == nullptr)
        {
            field_dt = this->find_data_type(field_type_name);
        }
        if (field_dt == nullptr)
        {
            std::cerr << "Error: type " << field_type_name << " used before defined for " << name
                      << std::endl;
            return 1;
        }

        // Create field
        Field field(name, field_dt, dims, ctrl, init_value, desc, units);

        // If C code will be generated, then field is a pointer if it's allocatable,
        // not a derived type, and the field name doesn't start with "writeoutput"
        if (this->gen_c_code && field.is_allocatable)
            field.is_pointer |= (tolower(field.name.substr(0, 11)).compare("writeoutput") != 0) &&
                                (field.data_type->tag != DataType::Tag::Derived);

        // If field is a mesh derived type (MeshType or MeshMapType)
        // or a derived type that contains a mesh,
        // set flag in derived data type
        if ((field.data_type->tag == DataType::Tag::Derived) &&
            field.data_type->derived.contains_mesh)
            ddt_dt->derived.contains_mesh = true;

        // Accumulate max rank of fields in derived data type
        ddt_dt->derived.max_rank = std::max(ddt_dt->derived.max_rank, field.rank);

        // Add field to derived data type
        ddt_dt->derived.fields.push_back(field);
        return 0;
    }

    // Line is invalid
    std::cerr << "Error: invalid line: '" << line << "'\n";
    return 1;
}
