#ifndef REGISTRY_HPP
#define REGISTRY_HPP

#include <iostream>
#include <map>
#include <memory>
#include <regex>
#include <set>
#include <sstream>
#include <string>
#include <vector>

std::string tolower(std::string s);

// case-independent (ci) string less_than: returns true if s1 < s2
struct ci_less : std::binary_function<std::string, std::string, bool>
{
    // case-independent (ci) compare_less binary function
    struct nocase_compare : public binary_function<unsigned char, unsigned char, bool>
    {
        bool operator()(const unsigned char &c1, const unsigned char &c2) const
        {
            return tolower(c1) < tolower(c2);
        }
    };

    bool operator()(const std::string &s1, const std::string &s2) const
    {
        return lexicographical_compare(s1.begin(), s1.end(), // source range
                                       s2.begin(), s2.end(), // dest range
                                       nocase_compare());    // comparison
    }
};

enum class Period
{
    None,
    TwoPi,
};

struct Module;
struct Field;

struct DataType
{
    enum class Type
    {
        Simple,
        Derived,
    };

    struct Simple
    {
        std::string name;
        std::string maps_to_fortran;
        std::string maps_to_c;
        std::string string_len;
    };

    struct Derived
    {
        std::string name;
        std::string name_short;
        std::string name_prefixed;
        std::shared_ptr<Module> module;
        std::vector<Field> fields;
        bool contains_pointer = false;
        bool is_interface = false;
        size_t max_dims = 0;
    };

    Type type;

    Simple simple;
    Derived derived;

    // Constructor for simple type
    DataType(const std::string &name, const std::string &maps_to_fortran, const std::string &maps_to_c) : type(Type::Simple)
    {
        this->simple.name = name;
        this->simple.maps_to_fortran = maps_to_fortran;
        this->simple.maps_to_c = maps_to_c;
    }

    // Constructor for derived type
    DataType(std::shared_ptr<Module> mod, const std::string &name, const std::string &name_short = "",
             const std::string &name_prefixed = "", const bool is_interface = false, const bool contains_pointer = false)
        : type(Type::Derived)
    {
        this->derived.name = name;
        this->derived.module = mod;
        this->derived.name_short = name_short.empty() ? name : name_short;
        this->derived.name_prefixed = name_prefixed.empty() ? name : name_prefixed;
        this->derived.is_interface = is_interface;
        this->derived.contains_pointer = contains_pointer;
    }
};

struct DimSpec
{
    size_t i;
    bool is_deferred = false;
    bool is_pointer = false;
    int lower_bound = 1;
    int upper_bound = -1;

    DimSpec(std::string spec)
    {
        // Get indices of first colon and asterisk
        auto i = spec.find(":");
        auto j = spec.find("*");

        // If colon was found
        if (i != std::string::npos)
        {
            // If colon is the only character, this is a deferred dimension
            this->is_deferred = spec.size() == 1;

            // If colon isn't first, then parse the lower bound, otherwise 1
            this->lower_bound = i > 0 ? std::stoi(spec.substr(0, i)) : 1;

            // Parse the upper bound
            this->upper_bound = this->is_deferred ? -1 : std::stoi(spec.substr(i + 1));
        }
        // If asterisk was found
        else if (j != std::string::npos)
        {
            this->is_deferred = true;
            this->is_pointer = true;
        }
        else
        {
            this->lower_bound = 1;
            this->upper_bound = std::stoi(spec);
        }
    }
};

struct Field
{
    std::string name;
    std::shared_ptr<DataType> type;
    std::vector<DimSpec> dims;
    std::string init_value = "";
    std::string desc = "-";
    std::string units = "-";
    Period gen_periodic = Period::None;
    int num_dims = 0;
    int boundary_array = 0;
    int sub_grid = 0;
    bool is_pointer = false;

    Field(const std::string &name, std::shared_ptr<DataType> const &type, const std::string &dims, const std::string &ctrl,
          const std::string &init_value, const std::string &desc, const std::string &units)
    {
        this->name = name;
        this->type = type;

        if (init_value.compare("-") != 0)
            this->init_value = init_value;

        if (ctrl.compare("2pi") != 0)
            this->gen_periodic = Period::TwoPi;

        if (desc.compare("-") != 0)
            this->desc = desc;

        if (units.compare("-") != 0)
            this->units = units;

        if (dims.compare("-") != 0)
        {
            // Parse dims, throw exception on error
            if (this->parse_dims(dims) != 0)
                throw std::invalid_argument("invalid dimensions: " + dims);

            // Add dimension number
            for (size_t i = 0; i < this->dims.size(); ++i)
                this->dims[i].i = i + 1;

            // Field is a pointer if any dim is a pointer
            this->is_pointer |= std::any_of(this->dims.begin(), this->dims.end(), [](const DimSpec &ds) { return ds.is_pointer; });
        }
    }

    int parse_dims(std::string dim_field)
    {
        // If no dimensions specified
        if (dim_field.size() == 0)
            return 0;

        // Remove leading and trailing braces
        if (dim_field[0] == '{')
            dim_field = dim_field.substr(1);
        if (dim_field.back() == '}')
            dim_field.pop_back();

        // If dim field is only digits, parse number
        if (std::all_of(dim_field.begin(), dim_field.end(), ::isdigit))
        {
            this->dims.push_back(DimSpec(dim_field));
            return 0;
        }

        // If all dims are colons or asterisks, no braces
        if (std::all_of(dim_field.begin(), dim_field.end(), [](char c) { return c == '*'; }) ||
            std::all_of(dim_field.begin(), dim_field.end(), [](char c) { return c == ':'; }))
        {
            for (auto &dim : dim_field)
                this->dims.push_back(DimSpec(std::string(1, dim)));
            return 0;
        }

        // Split by braces
        std::regex split("\\}\\{");
        std::sregex_token_iterator iter(dim_field.begin(), dim_field.end(), split, -1);
        std::sregex_token_iterator re_end;
        for (; iter != re_end; ++iter)
        {
            this->dims.push_back(DimSpec(*iter));
        }

        return 0;
    }
};

struct Parameter
{
    std::string name;
    std::shared_ptr<DataType> type;
    std::string value = "";
    std::string desc = "-";
    std::string units = "-";

    Parameter(const std::string &name, std::shared_ptr<DataType> &type, const std::string &value, const std::string &desc,
              const std::string &units)
    {
        this->name = name;
        this->type = type;
        if (value.compare("-") != 0)
            this->value = value;
        if (desc.compare("-") != 0)
            this->desc = desc;
        if (desc.compare("-") != 0)
            this->desc = desc;
    }
};

struct Module
{
    std::string name;
    std::string nickname;
    std::vector<Parameter> params;
    std::map<std::string, std::shared_ptr<DataType>, ci_less> data_types;
    std::vector<std::string> data_type_order;
    bool is_root = false;

    Module(std::string name, std::string nickname, bool is_root) : name(name), nickname(nickname), is_root(is_root)
    {
    }
};

struct InterfaceData
{
    std::string name;
    std::string name_short;
    bool only_floats;

    InterfaceData(std::string name, std::string name_short, bool only_floats)
        : name(name), name_short(name_short), only_floats(only_floats)
    {
    }
};

struct Registry
{
    std::vector<std::string> include_dirs = {"."};
    std::set<std::string> include_files;
    std::map<std::string, InterfaceData, ci_less> interface_names;
    std::map<std::string, std::shared_ptr<Module>, ci_less> modules;
    std::map<std::string, std::shared_ptr<DataType>, ci_less> data_types;
    bool gen_c_code = false;
    bool no_extrap_interp = false;

    Registry()
    {
        // Simple types
        auto IntKi = std::make_shared<DataType>("IntKi", "INTEGER(IntKi)", "int");
        auto SiKi = std::make_shared<DataType>("SiKi", "REAL(SiKi)", "float");
        auto R4Ki = std::make_shared<DataType>("R4Ki", "REAL(R4Ki)", "float");
        auto ReKi = std::make_shared<DataType>("ReKi", "REAL(ReKi)", "float");
        auto R8Ki = std::make_shared<DataType>("R8Ki", "REAL(R8Ki)", "float");
        auto DbKi = std::make_shared<DataType>("DbKi", "REAL(DbKi)", "double");
        auto logical = std::make_shared<DataType>("Logical", "LOGICAL", "bool");

        // Derived types
        auto mesh = std::make_shared<DataType>(nullptr, "MeshType", "MeshType", "MeshType", false, true);
        auto dll = std::make_shared<DataType>(nullptr, "DLL_Type");

        // Map of data types
        this->data_types = std::map<std::string, std::shared_ptr<DataType>, ci_less>{
            {"integer", IntKi}, {"intki", IntKi},     {"b4ki", IntKi},
            {"real", ReKi},     {"reki", ReKi},       {"siki", SiKi},
            {"r4ki", R4Ki},     {"r8ki", R8Ki},       {"doubleprecision", DbKi},
            {"dbki", DbKi},     {"logical", logical}, {"meshtype", mesh},
            {"dll_type", dll},
        };

        this->interface_names = std::map<std::string, InterfaceData, ci_less>{
            {"InitInputType", InterfaceData("InitInputType", "InitInput", false)},
            {"InitOutputType", InterfaceData("InitOutputType", "InitOutput", false)},
            {"InputType", InterfaceData("InputType", "Input", true)},
            {"OutputType", InterfaceData("OutputType", "Output", true)},
            {"ContinuousStateType", InterfaceData("ContinuousStateType", "ContState", true)},
            {"DiscreteStateType", InterfaceData("DiscreteStateType", "DiscState", true)},
            {"ConstraintStateType", InterfaceData("ConstraintStateType", "ConstrState", true)},
            {"OtherStateType", InterfaceData("OtherStateType", "OtherState", false)},
            {"MiscVarType", InterfaceData("MiscVarType", "Misc", false)},
            {"ParameterType", InterfaceData("ParameterType", "Param", false)},
            {"PartialOutputPInputType", InterfaceData("PartialOutputPInputType", "dYdu", true)},
            {"PartialContStatePInputType", InterfaceData("PartialContStatePInputType", "dXdu", true)},
            {"PartialDiscStatePInputType", InterfaceData("PartialDiscStatePInputType", "dXddu", true)},
            {"PartialConstrStatePInputType", InterfaceData("PartialConstrStatePInputType", "dZdu", true)},
        };
    }

    // Parsing
    int parse(const std::string &file_name, const bool is_root);
    int parse_line(std::string line, std::vector<std::string> &fields_prev, bool is_root);
    std::shared_ptr<DataType> find_type(const std::string &type_name, std::shared_ptr<Module> mod = nullptr)
    {
        // Pointer to type
        std::shared_ptr<DataType> data_type;

        // Get map of data types to search
        // If module was provided, search it; otherwise, search registry
        auto &data_types = mod == nullptr ? this->data_types : mod->data_types;

        // Search for type in registry, return if found
        auto it = data_types.find(type_name);
        if (it != data_types.end())
        {
            return it->second;
        }

        // If type starts with character (string type), build type and return it
        if (tolower(type_name).compare(0, 9, "character") == 0)
        {
            // Get string length
            auto string_len = type_name.substr(10, type_name.size() - 11);

            // C type
            auto c_type = "char[" + string_len + "]";

            // Build type
            data_type = std::make_shared<DataType>(type_name, type_name, c_type);
            data_type->simple.string_len = string_len;

            // Add type to registry
            this->data_types[type_name] = data_type;
            return data_type;
        }

        return nullptr;
    }

    // Output
    int gen_module_files(std::string const &out_dir);
    int gen_fortran_module(const Module &mod, const std::string &out_dir);
    int gen_c_module(const Module &mod, const std::string &out_dir);
    void gen_copy(std::ostream &w, const Module &mod, const DataType::Derived &ddt);
    void gen_destroy(std::ostream &w, const Module &mod, const DataType::Derived &ddt);
    void gen_pack(std::ostream &w, const Module &mod, const DataType::Derived &ddt);
    void gen_unpack(std::ostream &w, const Module &mod, const DataType::Derived &ddt);
};

#endif
