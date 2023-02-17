#include <algorithm>
#include <iostream>
#include <map>
#include <regex>
#include <sstream>
#include <vector>

#include "record.hpp"

void Record::print()
{
    switch (type)
    {
    case Record::Type::Include:
        std::cout << "include: " << include.file_name << std::endl;
        break;
    case Record::Type::Param:
        std::cout << "param:   " << module_name << "|" << module_nickname << "|"
                  << param.name << "|" << param.value << "|" << param.desc << "|"
                  << param.units << std::endl;
        break;
    case Record::Type::Field:
        std::cout << "field:   " << module_name << "|" << module_nickname << "|"
                  << field.derived_type << "|" << field.type << "|" << field.name << "|"
                  << field.dims << "|" << field.init_value << "|" << field.ctrl << "|"
                  << field.desc << "|" << field.units << "|" << std::endl;
        break;
    default:
        break;
    }
}

const std::regex quote_split_re("\"");
const std::regex space_split_re("\\s+");
const std::regex strip_start("^\\s+");
const std::regex strip_end("\\s+$");
const std::sregex_token_iterator re_end;

int Record::parse(std::string line, const std::string &file_name, const size_t line_num)
{
    // Remove leading and trailing space from line
    line = std::regex_replace(line, strip_start, "");
    line = std::regex_replace(line, strip_end, "");

    // Comment or empty line
    if (line.size() == 0)
    {
        this->type = Record::Type::Empty;
        return 0;
    }
    else if (line[0] == '#')
    {
        this->type = Record::Type::Comment;
        return 0;
    }

    // Split line into fields by quotes
    std::vector<std::string> fields;
    int counter = 0;
    std::sregex_token_iterator iter(line.begin(), line.end(), quote_split_re, -1);
    for (; iter != re_end; ++iter)
    {
        ++counter;
        // If quoted field, add to fields
        if (counter % 2 == 0)
        {
            // if (!std::string(*iter).empty())
            fields.push_back(*iter);
        }
        // Otherwise, split by space
        else
        {
            std::string field = *iter;
            std::sregex_token_iterator iter(field.begin(), field.end(), space_split_re,
                                            -1);
            for (; iter != re_end; ++iter)
            {
                if (!std::string(*iter).empty())
                    fields.push_back(*iter);
            }
        }
    }

    // Include line
    if (fields.size() == 2 &&
        (fields[0].compare("include") == 0 || fields[0].compare("usefrom") == 0))
    {
        this->type = Record::Type::Include;
        this->include.file_name = fields[1];
        return 0;
    }

    // Resize and fill remaining fields
    fields.resize(MAX_FIELDS, "-");

    // Propagate field values from previous fields if requested
    for (int i = 0; i < MAX_FIELDS; i++)
        if (fields[i].compare("^") == 0)
            fields[i] = fields_prev[i];

    // Update previous fields to current values
    fields_prev = fields;

    // Parse module name and nickname from field
    auto slash_index = fields[1].find("/");
    bool has_slash = slash_index != std::string::npos;
    this->module_name = has_slash ? fields[1].substr(0, slash_index) : fields[1];
    this->module_nickname = has_slash ? fields[1].substr(slash_index + 1) : fields[1];

    // If parameter line
    if (fields[0].compare("param") == 0)
    {
        this->type = Record::Type::Param;
        this->param.name = fields[4];
        this->param.type = fields[3];
        this->param.value = fields[6];
        this->param.desc = fields[8];
        this->param.units = fields[9];
        return 0;
    }

    // If struct field line
    if ((fields[0].compare("typedef") == 0) || (fields[0].compare("usefrom") == 0))
    {
        this->type = Record::Type::Field;
        this->field.derived_type = fields[2];
        this->field.type = fields[3];
        this->field.name = fields[4];
        this->field.dims = fields[5];
        this->field.init_value = fields[6];
        this->field.ctrl = fields[7];
        this->field.desc = fields[8];
        this->field.units = fields[9];
        return 0;
    }

    std::cerr << "(" << file_name << ":" << line_num
              << ") warning: ignoring invalid line: '" << line << "'\n";
    return 1;
}