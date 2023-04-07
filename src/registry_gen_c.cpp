#include "registry.hpp"
#include "templates.hpp"

#include <fstream>
#include <iostream>

int Registry::gen_c_module(const Module &mod, const std::string &out_dir)
{
    auto file_name = mod.name + "_Types.h";
    auto file_path = out_dir + "/" + file_name;
    std::cerr << "generating " << file_name << std::endl;

    std::ofstream out(file_path);
    if (!out)
    {
        std::cerr << "error creating module file: '" << file_path << "'\n";
        return -1;
    }

    out << "//STARTOFREGISTRYGENERATEDFILE '" << file_name << "'\n";
    out << "//\n";
    out << "// WARNING This file is generated automatically by the FAST "
           "registry.\n";
    out << "// Do not edit.  Your changes to this file will be lost.\n";
    out << "//\n";

    out << "\n#ifndef _" << mod.name << "_TYPES_H\n";
    out << "#define _" << mod.name << "_TYPES_H\n\n";
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

    out << "\n#endif // _" << mod.name << "_TYPES_H\n\n\n";
    out << "//ENDOFREGISTRYGENERATEDFILE\n";
    return 0;
}