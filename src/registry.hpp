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

#include "record.hpp"

std::string tolower(std::string s);

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

    Type type;

    // Simple
    std::string name;
    std::string maps_to;
    std::string string_len;
    std::shared_ptr<Module> module;
    std::vector<Field> fields;

    // Derived
    std::string name_base;
    size_t max_dims = 0;
    bool has_pointer = false;
    bool is_interface = false;

    DataType(std::string name, std::string maps_to, Type type_class = Type::Simple,
             std::shared_ptr<Module> module = nullptr, std::string name_short = "")
        : name(name), name_base(name_short), maps_to(maps_to), type(type_class),
          module(module)
    {
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
    bool has_pointer = false;

    Field(Record::Field const &rec, std::shared_ptr<DataType> const &type)
    {
        this->name = rec.name;
        this->type = type;

        this->has_pointer |= type->has_pointer;

        if (rec.init_value.compare("-") != 0)
            this->init_value = rec.init_value;

        if (rec.ctrl.compare("2pi") != 0)
            this->gen_periodic = Period::TwoPi;

        if (rec.desc.compare("-") != 0)
            this->desc = rec.desc;

        if (rec.units.compare("-") != 0)
            this->units = rec.units;

        if (rec.dims.compare("-") != 0)
        {
            // Parse dims, throw exception on error
            if (this->parse_dims(rec.dims) != 0)
                throw std::invalid_argument("invalid dimensions: " + rec.dims);

            // Add dimension number
            for (size_t i = 0; i < this->dims.size(); ++i)
                this->dims[i].i = i + 1;

            // Field is a pointer if any dim is a pointer
            this->has_pointer |=
                std::any_of(this->dims.begin(), this->dims.end(),
                            [](const DimSpec &ds) { return ds.is_pointer; });
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
        if (std::all_of(dim_field.begin(), dim_field.end(),
                        [](char c) { return c == '*'; }) ||
            std::all_of(dim_field.begin(), dim_field.end(),
                        [](char c) { return c == ':'; }))
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

    Parameter(const Record::Param &rec, std::shared_ptr<DataType> &type)
    {
        this->name = rec.name;
        this->type = type;
        if (rec.value.compare("-") != 0)
            this->value = rec.value;
        if (rec.desc.compare("-") != 0)
            this->desc = rec.desc;
        if (rec.desc.compare("-") != 0)
            this->desc = rec.desc;
    }
};

struct Module
{
    std::string name;
    std::string nickname;
    std::vector<Parameter> params;
    std::map<std::string, std::shared_ptr<DataType>> data_types;
    std::vector<std::string> data_type_order;
    bool is_root = false;

    Module(std::string name, std::string nickname, bool is_root)
        : name(name), nickname(nickname), is_root(is_root)
    {
    }
};

struct InterfaceData
{
    std::string name;
    std::string name_short;
};

struct Registry
{
    std::vector<std::string> include_dirs = {"."};
    std::set<std::string> include_files;
    std::map<std::string, InterfaceData> interface_names;
    std::map<std::string, std::shared_ptr<Module>> modules;
    std::map<std::string, std::shared_ptr<DataType>> data_types;
    bool gen_c_code = false;

    Registry()
    {
        auto IntKi = std::make_shared<DataType>("IntKi", "INTEGER(IntKi)");
        auto ReKi = std::make_shared<DataType>("ReKi", "REAL(ReKi)");
        auto DbKi = std::make_shared<DataType>("DbKi", "REAL(DbKi)");
        auto mesh =
            std::make_shared<DataType>("MeshType", "MeshType", DataType::Type::Derived);
        mesh->has_pointer = true;

        this->data_types = std::initializer_list<
            std::map<std::string, std::shared_ptr<DataType>>::value_type>{
            {"integer", IntKi},
            {"intki", IntKi},
            {"b4ki", IntKi},
            {"real", ReKi},
            {"reki", ReKi},
            {"siki", std::make_shared<DataType>("SiKi", "REAL(SiKi)")},
            {"r4ki", std::make_shared<DataType>("R4Ki", "REAL(R4Ki)")},
            {"r8ki", std::make_shared<DataType>("R8Ki", "REAL(R8Ki)")},
            {"doubleprecision", DbKi},
            {"dbki", DbKi},
            {"logical", std::make_shared<DataType>("Logical", "LOGICAL")},
            {"meshtype", mesh},
            {"dll_type",
             std::make_shared<DataType>("DLL_Type", "DLL_Type", DataType::Type::Derived)},
        };

        this->interface_names =
            std::initializer_list<std::map<std::string, InterfaceData>::value_type>{
                {"initinputtype", InterfaceData{"InitInputType", "InitInput"}},
                {"initoutputtype", InterfaceData{"InitOutputType", "InitOutput"}},
                {"inputtype", InterfaceData{"InputType", "Input"}},
                {"outputtype", InterfaceData{"OutputType", "Output"}},
                {"continuousstatetype",
                 InterfaceData{"ContinuousStateType", "ContState"}},
                {"discretestatetype", InterfaceData{"DiscreteStateType", "DiscState"}},
                {"constraintstatetype",
                 InterfaceData{"ConstraintStateType", "ConstrState"}},
                {"otherstatetype", InterfaceData{"OtherStateType", "OtherState"}},
                {"miscvartype", InterfaceData{"MiscVarType", "Misc"}},
                {"parametertype", InterfaceData{"ParameterType", "Param"}},
                {"partialoutputpinputtype",
                 InterfaceData{"PartialOutputPInputType", "dYdu"}},
                {"partialcontstatepinputtype",
                 InterfaceData{"PartialConstStatePInputType", "dXdu"}},
                {"partialdiscstatepinputtype",
                 InterfaceData{"PartialDiscStatePInputType", "dXddu"}},
                {"partialconstrstatepinputtype",
                 InterfaceData{"PartialConstrStatePInputType", "dZdu"}},
            };
    }

    int parse(const std::string &file_name, const bool is_root);
    void lex_record(const Record &rec);
    std::shared_ptr<DataType> find_type(
        const std::string &type_name,
        std::map<std::string, std::shared_ptr<DataType>> &data_types);
    int gen_module_files(std::string const &out_dir);
    void gen_fortran_module(std::ostream &out, const Module &mod);
    void gen_copy(std::ostream &out, const Module &mod, const DataType &ddt);
};

#endif
