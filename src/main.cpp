#include <algorithm>
#include <fstream>
#include <iostream>
#include <regex>

#include "registry.hpp"
#include "templates.hpp"

int output_template(std::string &module_name, std::string &module_nickname,
                    bool overwrite, bool is_template);

const std::string thisprog_ver = "FAST Registry";

int main(int argc, char *argv[])
{
    std::cerr << std::endl;
    std::cerr << "----- " << thisprog_ver << " --------------" << std::endl;
    std::cerr << "----------------------------------------------------------"
              << std::endl;

    // Read command line arguments
    std::vector<std::string> arguments;
    for (int i = 0; i < argc; ++i)
    {
        arguments.push_back(argv[i]);
    }

    std::string out_dir = "."; // if no OutDir is listed, use current directory
    std::string inp_file_name;
    std::string module_name, module_nickname;
    bool output_force_template = false;
    bool keep_temp_files = false;
    bool output_c_code = false;

    // Create registry object
    Registry reg;

    for (auto it = arguments.begin(); it != arguments.end(); ++it)
    {
        auto arg = *it;

        if (arg.find("-D") == 0)
        {
            reg.symbols.push_back(arg.substr(2));
        }
        else if (arg.find("/D=") == 0)
        {
            reg.symbols.push_back(arg.substr(3));
        }
        else if ((arg.compare("-force") == 0) || (arg.compare("/force") == 0))
        {
            output_force_template = true;
        }
        else if ((arg.compare("-O")) == 0 || (arg.compare("/O")) == 0)
        {
            std::advance(it, 1);
            if (it != arguments.end())
            {
                out_dir = *it;
            }
        }
        else if ((arg.compare("-I")) == 0 || (arg.compare("/I")) == 0)
        {
            std::advance(it, 1);
            if (it != arguments.end())
            {
                reg.include_dirs.push_back(*it);
            }
        }
        else if ((arg.compare("-ccode")) == 0 || (arg.compare("/ccode")) == 0)
        {
            output_c_code = true;
        }
        else if ((arg.compare("-noextrap")) == 0 || (arg.compare("/noextrap")) == 0)
        {
            reg.no_extrap = true;
        }
        else if ((arg.compare("-shownodes")) == 0 || (arg.compare("/shownodes")) == 0)
        {
            reg.show_nodes = true;
        }
        else if ((arg.compare("-template")) == 0 || (arg.compare("-registry")) == 0 ||
                 (arg.compare("/template")) == 0 || (arg.compare("/registry")) == 0)
        {
            std::advance(it, 1);
            if (it != arguments.end())
            {
                module_name = *it;
            }
            else
            {
                std::cerr << usage_template;
                return EXIT_FAILURE;
            }
            std::advance(it, 1);
            if (it != arguments.end())
            {
                module_nickname = *it;
            }
            else
            {
                std::cerr << usage_template;
                return EXIT_FAILURE;
            }

            if (arg.substr(1).compare("template") == 0)
            {
                return output_template(module_name, module_nickname,
                                       output_force_template, true);
            }
            else
            {
                return output_template(module_name, module_nickname,
                                       output_force_template, false);
            }
        }

        else if ((arg.compare("-keep") == 0) || (arg.compare("/keep") == 0))
        {
            keep_temp_files = 1;
        }
        else if ((arg.compare("-h") == 0) || (arg.compare("/h") == 0))
        {
            std::cerr << usage_template;
            return EXIT_SUCCESS;
        }
        else
        {
            inp_file_name = arg;
        }
    }

    // If input file name was not specified, exit with error
    if (inp_file_name.empty())
    {
        std::cerr << usage_template;
        return EXIT_FAILURE;
    }

    // Parse the registry file
    if (reg.parse(inp_file_name, true) != 0)
    {
        std::cerr << "Error parsing " << inp_file_name << ". Ending." << std::endl;
        return EXIT_FAILURE;
    }

    reg.lex();

    reg.gen_module_files(out_dir, output_c_code);

    return EXIT_SUCCESS;
}

int output_template(std::string &module_name, std::string &module_nickname,
                    bool overwrite, bool is_template)
{
    // Create file name depending on if template or registry
    std::string fname = module_name + (is_template ? ".f90" : "_Registry.txt");

    // If overwrite not requested and file exists, return error
    if (!overwrite)
    {
        std::ifstream infile(fname);
        if (infile.good())
        {
            std::cerr << "Registry exiting. Attempt to overwrite file (" << fname;
            std::cerr
                << ") . Move out of the way or specify -force before -template option. "
                << std::endl;
            return EXIT_FAILURE;
        }
    }

    // Open output file, return on error
    std::ofstream outfile(fname);
    if (!outfile.is_open())
    {
        std::cerr << "Registry exiting. Failure opening " << fname << std::endl;
        return EXIT_FAILURE;
    }

    // Select file contents
    auto contents = (is_template ? module_template : registry_template).substr(1);

    // Populate module name and module nickname
    contents = std::regex_replace(contents, std::regex("ModuleName"), module_name);
    contents = std::regex_replace(contents, std::regex("ModName"), module_nickname);

    // Output contents to file
    outfile << contents;

    return EXIT_SUCCESS;
}
