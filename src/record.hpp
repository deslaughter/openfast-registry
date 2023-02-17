#ifndef RECORD_HPP

#include <string>

const int MAX_FIELDS = 10;

struct Record
{
    enum class Type
    {
        Empty,
        Comment,
        Include,
        Param,
        Field,
    };

    struct Include
    {
        std::string file_name;
    };

    struct Param
    {
        std::string name;
        std::string type;
        std::string value;
        std::string desc;
        std::string units;
    };

    struct Field
    {
        std::string derived_type;
        std::string type;
        std::string name;
        std::string dims;
        std::string init_value;
        std::string ctrl;
        std::string desc;
        std::string units;
    };

    Type type;
    Include include;
    Param param;
    Field field;
    std::string module_name;
    std::string module_nickname;
    std::vector<std::string> fields_prev;
    bool is_root;

    Record(const bool is_root) : is_root(is_root)
    {
        this->fields_prev.resize(MAX_FIELDS, "");
    }
    void print();
    int parse(std::string line, const std::string &file_name, const size_t line_num);
};

#define RECORD_HPP
#endif