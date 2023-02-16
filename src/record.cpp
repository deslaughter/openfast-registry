#include <algorithm>
#include <iostream>
#include <map>
#include <sstream>
#include <vector>

#include "registry.hpp"

void Record::print()
{
    std::cout << keyword << "|" << module_name << "|" << module_nickname << "|";
    std::cout << struct_name << "|" << field_type << "|";
    std::cout << field_name << "|" << dims << "|";
    std::cout << init_value << "|" << ctrl << "|";
    std::cout << desc << "|" << units << "|" << file_name << std::endl;
}

void Record::parse(std::string const &line)
{
    const int MAX_FIELDS = 10;
    std::vector<std::string> fields;
    std::stringstream split_line(line);
    std::string field;
    int counter = 0;
    for (int i = 0; std::getline(split_line, field, '\"'); ++i)
    {
        if (i % 2 != 0)
        {
            if (!field.empty())
                fields.push_back(field);
        }
        else
        {
            std::stringstream split_field(field);
            while (std::getline(split_field, field, ' '))
            {
                if (!field.empty())
                    fields.push_back(field);
            }
        }
    }

    // Resize and fill remaining fields
    fields.resize(MAX_FIELDS, "-");

    // Populate record with field value if not ditto character
    if (fields[0].compare("^") != 0)
        this->keyword = fields[0];
    if (fields[1].compare("^") != 0)
    {
        auto slash_index = fields[1].find("/");
        if (slash_index != std::string::npos)
        {
            this->module_name = fields[1].substr(0, slash_index);
            this->module_nickname = fields[1].substr(slash_index + 1);
        }
        else
        {
            this->module_name = fields[1];
            this->module_nickname = fields[1];
        }
    }
    if (fields[2].compare("^") != 0)
        this->struct_name = fields[2];
    if (fields[3].compare("^") != 0)
        this->field_type = fields[3];
    if (fields[4].compare("^") != 0)
        this->field_name = fields[4];
    if (fields[5].compare("^") != 0)
        this->dims = fields[5];
    if (fields[6].compare("^") != 0)
        this->init_value = fields[6];
    if (fields[7].compare("^") != 0)
        this->ctrl = fields[7];
    if (fields[8].compare("^") != 0)
        this->desc = fields[8];
    if (fields[9].compare("^") != 0)
        this->units = fields[9];
}