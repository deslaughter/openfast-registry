
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

int Registry::gen_module_files(std::string const &out_dir)
{
    // Find root module
    std::shared_ptr<Module> mod;
    for (auto &it : this->modules)
    {
        if (it.second->is_root)
        {
            mod = it.second;
            break;
        }
    }

    // If module not found, return error
    if (mod == nullptr)
    {
        std::cerr << "unable to find root module\n";
        return -1;
    }

    // If error generating fortran module, return error
    if (this->gen_fortran_module(*mod, out_dir) != 0)
    {
        std::cerr << "error generating fortran module\n";
        return -1;
    }

    // If C code generation requested, output .h files for C/C++
    if (this->gen_c_code)
    {
        if (this->gen_c_module(*mod, out_dir) != 0)
        {
            std::cerr << "error generating C module\n";
            return -1;
        }
    }

    return 0;
}