#include <iostream>

#include "registry.hpp"
#include "templates.hpp"

std::string remove_nickname(const std::string nickname, std::string src)
{
    std::string prefix = tolower(nickname) + "_";

    if (tolower(src).compare(0, prefix.size(), prefix) == 0)
    {
        return src.substr(prefix.size());
    }

    return src;
}

std::string dimstr(size_t d)
{
    switch (d)
    {
    case 0:
        return "";
    case 1:
        return "(i1)";
    case 2:
        return "(i1,i2)";
    case 3:
        return "(i1,i2,i3)";
    case 4:
        return "(i1,i2,i3,i4)";
    case 5:
        return "(i1,i2,i3,i4,i5)";
    }
    return " REGISTRY ERROR TOO MANY DIMS ";
}

std::string dimstr_c(size_t d)
{
    switch (d)
    {
    case 0:
        return "";
    case 1:
        return "[i1]";
    case 2:
        return "[i2][i1]";
    case 3:
        return "[i3][i2][i1]";
    case 4:
        return "[i4][i3][i2][i1]";
    case 5:
        return "[i5][i4][i3][i2][i1]";
    }
    return " REGISTRY ERROR TOO MANY DIMS ";
}

void Registry::gen_fortran_module(std::ostream &out, const Module &mod)
{
    // Write preamble
    out << std::regex_replace(FAST_preamble.substr(1), std::regex("ModuleName"),
                              mod.name);

    // Output USE statements for non-root modules
    for (auto const &it : this->modules)
    {
        auto &inc_mod = *it.second;
        if (!inc_mod.is_root && tolower(inc_mod.name).compare("nwtc_library") != 0)
        {
            out << "USE " << inc_mod.name << "_Types\n";
        }
    }

    // if this is the NWTC Library, we're not going to print "USE NWTC_Library"
    if (tolower(mod.name).compare("nwtc_library") == 0)
    {
        out << "USE SysSubs\n";
    }
    else
    {
        out << "USE NWTC_Library\n";
    }

    out << "IMPLICIT NONE\n";

    // output parameters
    for (auto &param : mod.params)
    {
        out << "    " << param.type->maps_to << ", PUBLIC, PARAMETER ";
        out << " :: " << param.name;

        if (!param.value.empty())
        {
            out << " = " << param.value;
        }

        if (param.desc.compare("-") != 0 || param.units.compare("-") != 0)
        {
            out << "      ! " << param.desc << " [" << param.units << "]";
        }
        out << "\n";
    }

    // Generate each derived data type
    for (auto &dt_name : mod.data_type_order)
    {
        // Get data type
        auto it = mod.data_types.find(dt_name);
        auto data_type = it->second;

        // if (*q->maps_to)
        //     remove_nickname(mod.nickname, tolower(q->maps_to), nonick);

        out << "! =========  " << data_type->maps_to << (gen_c_code ? "_C" : "")
            << "  =======\n";

        // 2 passes for C  code, 1st pass generates bound ddt
        for (int ipass = (gen_c_code) ? 0 : 1; ipass < 2; ipass++)
        {
            if (data_type->type != DataType::Type::Derived)
                continue;

            bool is_C = ipass == 0;

            out << "  TYPE, " << (is_C ? "BIND(C)" : "PUBLIC")
                << " :: " << data_type->maps_to << (is_C ? "_C" : "") << "\n";

            if (gen_c_code)
            {
                if (is_C)
                {
                    out << "   TYPE(C_PTR) :: object = C_NULL_PTR\n";
                }
                else
                {
                    out << "    TYPE( " << data_type->maps_to << "_C ) :: C_obj\n";
                }
            }

            // Loop through fields
            for (auto &field : data_type->fields)
            {
                // check max number of dimensions
                // check if this type contains any pointers/meshes or types
                // that have pointers/meshes
                // if (field.num_dims > q->max_num_dims)
                //     q->max_num_dims = field.num_dims;

                // if (field.num_dims > mod.module_ddt_list->max_num_dims)
                //     mod.module_ddt_list->max_num_dims = field.num_dims;

                if (is_C)
                {
                    // if (field.num_dims == 0 && field.type->type_type != DERIVED)
                    // {
                    //     out<< "    %s :: %s \n",
                    //                 c_types_binding(field.type->maps_to), field.name);
                    // }
                    // else if (field.num_dims > 0 && field.type->type_type != DERIVED)
                    // {
                    //         if (field.dims[0]->deferred)
                    //         {
                    //             out<< "    TYPE(C_ptr) :: %s = C_NULL_PTR
                    //                 \n ",
                    //                         field.name);
                    //             out<< "    INTEGER(C_int) :: %s_Len = 0 \n",
                    //                     field.name);
                    //         }
                    //         else
                    //         {
                    //             if (strcmp(C_type(field.type->maps_to), "char"))
                    //             {
                    //                 out<< "    TYPE(C_PTR) :: %s(", field.name);
                    //                 for (i = 0; i < field.num_dims; i++)
                    //                 {
                    //                     out<< "%d", field.dims[i]->coord_end);
                    //                     if (i < field.num_dims - 1)
                    //                         out<< ",");
                    //                 }
                    //                 out<< ")\n");
                    //             }
                    //         }
                    // }
                }
                else
                {
                    if (field.type->type == DataType::Type::Derived)
                    {
                        out << "    TYPE(" << field.type->maps_to << ") ";

                        // checkContainsMesh(r);
                        // if (field.containsPtr)
                        //     q->containsPtr = 1;

                        // tmp[0] = '\0';
                        // if (*data_type->maps_to)
                        //     remove_nickname(mod.nickname, tolower(data_type->maps_to),
                        //                     tmp);
                        // if (must_have_real_or_double(tmp))
                        //     checkOnlyReals(data_type->maps_to, r);
                    }
                    else
                    {
                        // tmp[0] = '\0';
                        // if (*data_type->maps_to)
                        //     remove_nickname(mod.nickname, tolower(data_type->maps_to),
                        //                     tmp);
                        // if (must_have_real_or_double(tmp))
                        // {
                        //     if (strncmp(field.type->maps_to, "REAL", 4))
                        //     {
                        //         fprintf(stderr,
                        //                 "Registry warning: %s contains a "
                        //                 "field (%s) whose type is not real "
                        //                 "or double: %s\n",
                        //                 data_type->maps_to, field.name,
                        //                 field.type->maps_to);
                        //     }
                        // }

                        if (gen_c_code && field.has_pointer)
                        {
                            // out << "    %s ", c_types_binding(field.type->maps_to);
                        }
                        else
                        {
                            out << "    " << field.type->maps_to << " ";
                        }
                    }

                    if (!field.dims.empty())
                    {
                        // if one dim is deferred they all
                        if (field.dims[0].is_deferred)
                        {
                            out << ", DIMENSION(";
                            bool first = true;
                            for (auto &dim : field.dims)
                            {
                                out << (first ? "" : ",") << ":";
                                first = false;
                            }

                            out << "), "
                                << (field.has_pointer ? "POINTER " : "ALLOCATABLE ");
                        }
                        else
                        {
                            out << ", DIMENSION(";
                            bool first = true;
                            for (auto &dim : field.dims)
                            {
                                out << (first ? "" : ",") << dim.lower_bound << ":"
                                    << dim.upper_bound;
                                ;
                                first = false;
                            }
                            out << ") ";
                        }
                    }

                    out << " :: " << field.name << " ";

                    if (field.has_pointer)
                    {
                        out << "=> NULL() ";
                    }
                    else if (field.num_dims == 0 && !field.init_value.empty())
                    {
                        out << "= " << field.init_value << " ";
                    }

                    if (field.desc.compare("-") != 0 || field.units.compare("-") != 0)
                    {
                        out << "     !< " << field.desc << " [" << field.units << "]";
                    }

                    out << "\n";
                }
            }

            out << "  END TYPE " << data_type->maps_to << (is_C ? "_C" : "") << "\n";
        }

        out << "! =======================\n";
    }

    // if (gen_c_code)
    // {
    //     for (q = mod.module_ddt_list; q; q = q->next)
    //     {

    //         if (q->usefrom == 0)
    //         {

    //             char *ddtname, *ddtnamelong, nonick[NAMELEN];
    //             ddtname = q->name;

    //             remove_nickname(mod.nickname, ddtname, nonick);

    //             if (is_a_fast_interface_type(nonick))
    //             {
    //                 ddtnamelong = nonick;
    //                 ddtname = fast_interface_type_shortname(nonick);
    //             }
    //             else
    //             {
    //                 ddtnamelong = ddtname;
    //             }
    //         }
    //     }
    // } // gen_c_code

    out << "CONTAINS\n";
    // Generate each derived data type
    for (auto &dt_name : mod.data_type_order)
    {
        // Get data type
        auto it = mod.data_types.find(dt_name);
        auto &ddt = *it->second;

        this->gen_copy(out, mod, ddt);
        // gen_destroy(fp, mod, ddt_name, ddt_name_long);
        // gen_pack(fp, mod, ddt_name, ddt_name_long);
        // gen_unpack(fp, mod, ddt_name, ddt_name_long);

        // if (gen_c_code)
        // {
        //     gen_copy_c2f(fp, mod, ddt_name, ddt_name_long);
        //     gen_copy_f2c(fp, mod, ddt_name, ddt_name_long);
        // }
    }

    // if (strcmp(tolower(mod.name), "airfoilinfo") == 0)
    // { // make interpolation routines for AirfoilInfo module
    //     gen_ExtrapInterp(fp, ModName, "Output", "OutputType", "ReKi", 1);
    //     gen_ExtrapInterp(fp, ModName, "UA_BL_Type", "UA_BL_Type", "ReKi", 1);
    // }
    // else if (!sw_noextrap)
    // {
    //     if (strcmp(tolower(mod.name), "dbemt") == 0)
    //     { // make interpolation routines for element-level DBEMT module
    //         gen_ExtrapInterp(fp, ModName, "ElementInputType", "ElementInputType",
    //         "DbKi",
    //                          1);
    //     }
    //     //        else if (strcmp(tolower(mod.name), "bemt") == 0) {
    //     //            gen_ExtrapInterp(fp, ModName, "SkewWake_InputType",
    //     //            "SkewWake_InputType", "DbKi",1);
    //     //        }
    //     //        else if (strcmp(tolower(mod.name), "aerodyn") == 0) {
    //     //            gen_ExtrapInterp(fp, ModName, "RotInputType", "RotInputType",
    //     //            "DbKi",0); // don't append "AD_" to the type name!
    //     //        }

    //     gen_ExtrapInterp(fp, ModName, "Input", "InputType", "DbKi", 1);
    //     gen_ExtrapInterp(fp, ModName, "Output", "OutputType", "DbKi", 1);
    // }

    out << "END MODULE " << mod.name << "_Types\n";
    out << "!ENDOFREGISTRYGENERATEDFILE\n";
}

void Registry::gen_copy(std::ostream &out, const Module &mod, const DataType &ddt)
{
    out << " SUBROUTINE " << mod.nickname << "_Copy" << ddt.name_base << "( Src"
        << ddt.name_base << "Data, Dst" << ddt.name_base
        << "Data, CtrlCode, ErrStat, ErrMsg )\n";
    out << "   TYPE(" << ddt.maps_to << "), INTENT("
        << (ddt.has_pointer == 1 ? "INOUT" : "IN") << ") :: Src" << ddt.name_base
        << "Data\n";
    out << "   TYPE(" << ddt.maps_to << "), INTENT(INOUT) :: Dst" << ddt.name_base
        << "Data\n";
    out << "   INTEGER(IntKi),  INTENT(IN   ) :: CtrlCode\n";
    out << "   INTEGER(IntKi),  INTENT(  OUT) :: ErrStat\n";
    out << "   CHARACTER(*),    INTENT(  OUT) :: ErrMsg\n";
    out << "! Local \n";
    out << "   INTEGER(IntKi)                 :: i,j,k\n";
    for (int d = 1; d <= ddt.max_dims; d++)
        out << "   INTEGER(IntKi)                 :: i" << d << ", i" << d << "_l, i" << d
            << "_u  !  bounds (upper/lower) for an array dimension " << d << "\n";
    out << "   INTEGER(IntKi)                 :: ErrStat2\n";
    out << "   CHARACTER(ErrMsgLen)           :: ErrMsg2\n";
    out << "   CHARACTER(*), PARAMETER        :: RoutineName = '" << mod.nickname
        << "_Copy" << ddt.name_base << "'\n";
    out << "! \n";
    out << "   ErrStat = ErrID_None\n";
    out << "   ErrMsg  = \"\"\n";

    // Loop through fields
    for (auto &field : ddt.fields)
    {
        if (field.type == nullptr)
            continue;

        std::string src = "Src" + ddt.name_base + "Data%" + field.name;
        std::string dst = "Dst" + ddt.name_base + "Data%" + field.name;
        std::string alloc_assoc = field.has_pointer ? "ASSOCIATED" : "ALLOCATED";

        // check if this is an allocatable array:
        if (field.dims.size() > 0 && field.dims[0].is_deferred)
        {

            out << "IF (" << alloc_assoc << "(" << src << ")) THEN\n";

            std::string tmp;

            for (int d = 1; d <= field.dims.size(); d++)
            {
                out << "  i" << d << "_l = LBOUND(" << src << "," << d << ")\n";
                out << "  i" << d << "_u = UBOUND(" << src << "," << d << ")\n";
                tmp += ",i" + std::to_string(d) + "_l:i" + std::to_string(d) + "_u";
            }

            out << "  IF (.NOT. " << alloc_assoc << "(" << dst << ")) THEN \n";
            out << "    ALLOCATE(" << dst << "(" << tmp.substr(1) << "),STAT=ErrStat2)\n";
            out << "    IF (ErrStat2 /= 0) THEN \n";
            out << "      CALL SetErrStat(ErrID_Fatal, 'Error allocating Dst"
                << ddt.name_base << "Data%" << field.name
                << ".', ErrStat, ErrMsg,RoutineName)\n";
            out << "      RETURN\n";
            out << "    END IF\n";

            // bjj: this needs to be updated if we've got multiple dimension
            // arrays
            if (gen_c_code && ddt.has_pointer)
            {
                out << "    Dst%sData%%c_obj%%%s_Len = SIZE(Dst%sData%%%s)\n",
                    ddt.name_base, field.name, ddt.name_base, field.name;
                out << "    IF (Dst%sData%%c_obj%%%s_Len > 0) &\n", ddt.name_base,
                    field.name;

                out << "          Dst%sData%%c_obj%%%s = C_LOC( Dst%sData%%%s(",
                    ddt.name_base, field.name, ddt.name_base, field.name;
                for (int d = 1; d <= field.dims.size(); d++)
                {
                    out << (d > 1 ? "," : "") << " i" << d << "_l";
                }
                out << " ) )\n";
            }

            out << "  END IF\n"; // end dest allocated/associated
        }

        // includes mesh and dll_type
        if (field.type->type == DataType::Type::Derived)
        {
            for (int d = field.dims.size(); d >= 1; d--)
            {
                out << "    DO i" << d << " = LBOUND(" << src << "," << d << "), UBOUND("
                    << src << "," << d << ")\n";
            }

            if (tolower(field.type->name).compare("meshtype") == 0)
            {
                out << "      CALL MeshCopy( " << src << dimstr(field.dims.size()) << ", "
                    << dst << dimstr(field.dims.size())
                    << ", CtrlCode, ErrStat2, ErrMsg2 )\n";
                out << "         CALL SetErrStat(ErrStat2, ErrMsg2, "
                       "ErrStat, ErrMsg, RoutineName)\n";
                out << "         IF (ErrStat>=AbortErrLev) RETURN\n";
            }
            else if (tolower(field.type->name).compare("dll_type") == 0)
            {
                out << "      " << dst << " = " << src << "\n";
            }
            else
            {
                auto name_short = field.type->name_base;
                if (!field.type->is_interface)
                    name_short = tolower(name_short);
                out << "      CALL " << field.type->module->nickname << "_Copy"
                    << name_short << "( " << src << dimstr(field.dims.size()) << ", "
                    << dst << dimstr(field.dims.size())
                    << ", CtrlCode, ErrStat2, ErrMsg2 )\n";
                out << "         CALL SetErrStat(ErrStat2, ErrMsg2, "
                       "ErrStat, ErrMsg,RoutineName)\n";
                out << "         IF (ErrStat>=AbortErrLev) RETURN\n";
            }

            for (auto &d : field.dims)
            {
                out << "    ENDDO\n";
            }
        }
        else
        {
            out << "    " << dst << " = " << src << "\n";
            // if (sw_ccode && !field.is_pointer)
            // {
            //     if (field.dims.size() == 0) // scalar of any type OR a character
            //         array
            //         {
            //             out << "    Dst%sData%%C_obj%%%s = "
            //                    "Src%sData%%C_obj%%%s\n",
            //                 ddt.name_short, field.name, ddt.name_short, field.name;
            //         }
            // }
        }

        // close IF (check on allocatable array)
        if (field.dims.size() > 0 && field.dims[0].is_deferred)
            out << "ENDIF\n";
    }

    out << " END SUBROUTINE " << mod.nickname << "_Copy" << ddt.name_base << "\n\n";
}