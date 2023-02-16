#ifndef REGISTRY_HPP
#define REGISTRY_HPP

#include <map>
#include <memory>
#include <regex>
#include <set>
#include <sstream>
#include <string>
#include <vector>

enum class TypeClass
{
    Simple,
    Derived,
};

enum class Period
{
    None,
    TwoPi,
};

struct Record
{
    std::string keyword;
    std::string module_name;
    std::string module_nickname;
    std::string struct_name;
    std::string field_type;
    std::string field_name;
    std::string dims;
    std::string init_value;
    std::string ctrl;
    std::string desc;
    std::string units;
    std::string file_name;
    bool is_root = false;

    Record(const std::string &file_name, const bool is_root)
    {
        this->file_name = file_name;
        this->is_root = is_root;
    }

    void print();
    void parse(std::string const &line);
};

struct Module;
struct Field;

struct DataType
{
    std::string name;
    std::string maps_to;
    std::string string_len;
    TypeClass type_class;
    bool is_interface_type = false;
    bool use_from = false;
    std::shared_ptr<Module> module;
    std::vector<Field> fields;

    DataType(std::string name, std::string maps_to,
             TypeClass type_class = TypeClass::Simple,
             std::shared_ptr<Module> module = nullptr, bool use_from = false)
    {
        this->name = name;
        this->maps_to = maps_to;
        this->type_class = type_class;
        this->use_from = use_from;
        this->module = module;
    }
};

struct DimSpec
{
    std::string param_name;
    bool deferred = false;
    bool is_pointer = false;
    int lower_bound = 1;
    int upper_bound = -1;

    DimSpec(std::string spec, std::string name = "")
    {
        this->param_name = name;

        // If spec starts with constant, remove it
        if (spec.compare(0, 9, "constant=") == 0)
        {
            spec = spec.substr(9);
        }

        // Get indices of first colon and asterisk
        auto i = spec.find(":");
        auto j = spec.find("*");

        // If colon was found
        if (i != std::string::npos)
        {
            // If colon is the only character, this is a deferred dimension
            this->deferred = spec.size() == 1;

            // If colon isn't first, then parse the lower bound, otherwise 1
            this->lower_bound = i > 0 ? std::stoi(spec.substr(0, i)) : 1;

            // Parse the upper bound
            this->upper_bound = this->deferred ? -1 : std::stoi(spec.substr(i + 1));
        }
        // If asterisk was found
        else if (j != std::string::npos)
        {
            this->deferred = true;
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
    std::vector<std::shared_ptr<DimSpec>> dims;
    std::string init_value = "";
    std::string desc = "-";
    std::string units = "-";
    Period gen_periodic = Period::None;
    int num_dims = 0;
    int boundary_array = 0;
    int sub_grid = 0;

    Field(Record const &rec, std::shared_ptr<DataType> const &type)
    {
        this->name = rec.struct_name;
        this->type = type;

        if (rec.init_value.compare("-") != 0)
            this->init_value = rec.init_value;

        if (rec.ctrl.compare("2pi") != 0)
            this->gen_periodic = Period::TwoPi;

        if (rec.desc.compare("-") != 0)
            this->desc = rec.desc;

        if (rec.desc.compare("-") != 0)
            this->desc = rec.desc;

        if (rec.dims.compare("-") != 0)
        {
            auto dims = rec.dims;

            if (std::all_of(dims.begin(), dims.end(), ::isdigit))
            {
                this->dims.push_back(std::make_shared<DimSpec>(dims));
            }
            else
            {
                // Get dims without braces
                dims = std::regex_replace(rec.dims, std::regex("[{|}]"), "");

                // If all dims are colons or all dims are asterisks
                if ((std::count(dims.begin(), dims.end(), ':') == dims.size()) ||
                    (std::count(dims.begin(), dims.end(), '*') == dims.size()))
                {
                    for (auto &dim : dims)
                    {
                        this->dims.push_back(
                            std::make_shared<DimSpec>(std::string(1, dim)));
                    }
                }
                else
                {
                    // Replace braces with space
                    dims = std::regex_replace(rec.dims, std::regex("\\}\\{"), " ");
                    dims = std::regex_replace(dims, std::regex("[\\{|\\}]"), "");
                    std::string dim;
                    std::stringstream split_dims(dims);
                    while (std::getline(split_dims, dim, ' '))
                    {
                        this->dims.push_back(std::make_shared<DimSpec>(dim));
                    }
                }
            }
        }
    }
};

struct Parameter
{
    std::string name;
    std::shared_ptr<DataType> type;
    std::string value = "";
    std::string desc = "-";
    std::string units = "-";
    bool is_root = false;

    Parameter(const Record &rec, std::shared_ptr<DataType> &type)
    {
        this->name = rec.field_name;
        this->type = type;
        if (rec.init_value.compare("-") != 0)
            this->value = rec.init_value;
        if (rec.desc.compare("-") != 0)
            this->desc = rec.desc;
        if (rec.desc.compare("-") != 0)
            this->desc = rec.desc;
        this->is_root = rec.is_root;
    }
};

struct Module
{
    std::string name;
    std::string nickname;
    int use_from;
    std::vector<Parameter> params;
    std::map<std::string, std::shared_ptr<DataType>> data_types;

    Module(std::string name, std::string nickname)
    {
        this->name = name;
        this->nickname = nickname;
        this->use_from = 0;
    }

    void gen_fortran_module(std::ofstream &out);
};

struct Registry
{
    bool output_c_code = false;
    bool no_extrap = false;
    bool show_nodes = false;
    std::string module_name;
    std::string module_nickname;
    std::vector<std::string> include_dirs = {"."};
    std::set<std::string> include_files;
    std::map<std::string, std::pair<std::string, std::string>> interface_names;
    std::vector<std::string> symbols;
    std::vector<Record> records;
    std::map<std::string, std::shared_ptr<DimSpec>> dim_specs;
    std::map<std::string, std::shared_ptr<Module>> modules;
    std::map<std::string, std::shared_ptr<DataType>> data_types;

    Registry()
    {
        auto IntKi = std::make_shared<DataType>("IntKi", "INTEGER(IntKi)");
        auto ReKi = std::make_shared<DataType>("ReKi", "REAL(ReKi)");
        auto DbKi = std::make_shared<DataType>("DbKi", "REAL(DbKi)");

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
            {"meshtype",
             std::make_shared<DataType>("MeshType", "MeshType", TypeClass::Derived)},
            {"dll_type",
             std::make_shared<DataType>("DLL_Type", "DLL_Type", TypeClass::Derived)},
        };

        this->interface_names = std::initializer_list<
            std::map<std::string, std::pair<std::string, std::string>>::value_type>{
            {"initinputtype", std::make_pair("InitInputType", "InitInput")},
            {"initoutputtype", std::make_pair("InitOutputType", "InitOutput")},
            {"inputtype", std::make_pair("InputType", "Input")},
            {"outputtype", std::make_pair("OutputType", "Output")},
            {"continuousstatetype", std::make_pair("ContinuousStateType", "ContState")},
            {"discretestatetype", std::make_pair("DiscreteStateType", "DiscState")},
            {"constraintstatetype", std::make_pair("ConstraintStateType", "ConstrState")},
            {"otherstatetype", std::make_pair("OtherStateType", "OtherState")},
            {"miscvartype", std::make_pair("MiscVarType", "Misc")},
            {"parametertype", std::make_pair("ParameterType", "Param")},
            {"partialoutputpinputtype",
             std::make_pair("PartialOutputPInputType", "dYdu")},
            {"partialcontstatepinputtype",
             std::make_pair("PartialConstStatePInputType", "dXdu")},
            {"partialdiscstatepinputtype",
             std::make_pair("PartialDiscStatePInputType", "dXddu")},
            {"partialconstrstatepinputtype",
             std::make_pair("PartialConstrStatePInputType", "dZdu")},
        };
    }

    int parse(const std::string &file_name, const bool is_root);
    int lex();
    void lex_record(const Record &rec);
    std::shared_ptr<DataType> find_type(
        const std::string &type_name,
        std::map<std::string, std::shared_ptr<DataType>> &data_types);
    int gen_module_files(std::string const &out_dir, const bool gen_c_code);
};

#endif
