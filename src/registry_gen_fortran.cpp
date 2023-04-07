#include <fstream>
#include <iostream>

#include "registry.hpp"
#include "templates.hpp"

const int MAXRECURSE = 9;

void gen_ExtrapInterp(std::ostream &w, const Module &mod, std::string typnm, std::string typnmlong, std::string xtypnm,
                      const bool useModPrefix);

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

int Registry::gen_fortran_module(const Module &mod, const std::string &out_dir)
{
    // Create file name and path
    auto file_name = mod.name + "_Types.f90";
    auto file_path = out_dir + "/" + file_name;
    std::cerr << "generating " << file_name << std::endl;

    std::ofstream w(file_path);
    if (!w)
    {
        std::cerr << "error creating module file: '" << file_path << "'\n";
        return -1;
    }

    // Write preamble
    w << std::regex_replace(FAST_preamble.substr(1), std::regex("ModuleName"), mod.name);

    // Output USE statements for non-root modules
    for (auto const &it : this->modules)
    {
        auto &inc_mod = *it.second;
        if (!inc_mod.is_root && tolower(inc_mod.name).compare("nwtc_library") != 0)
        {
            w << "USE " << inc_mod.name << "_Types\n";
        }
    }

    // if this is the NWTC Library, we're not going to print "USE NWTC_Library"
    if (tolower(mod.name).compare("nwtc_library") == 0)
    {
        w << "USE SysSubs\n";
    }
    else
    {
        w << "USE NWTC_Library\n";
    }

    w << "IMPLICIT NONE\n";

    // output parameters
    for (auto &param : mod.params)
    {
        w << "    " << param.type->simple.maps_to_fortran << ", PUBLIC, PARAMETER ";
        w << " :: " << param.name;

        if (!param.value.empty())
        {
            w << " = " << param.value;
        }

        if (param.desc.compare("-") != 0 || param.units.compare("-") != 0)
        {
            w << "      ! " << param.desc << " [" << param.units << "]";
        }
        w << "\n";
    }

    // Generate each derived data type
    for (auto &dt_name : mod.data_type_order)
    {
        // Get data type
        auto it = mod.data_types.find(dt_name);
        auto &data_type = *it->second;

        // If not a derived daa type, continue
        if (data_type.type != DataType::Type::Derived)
            continue;

        // Get derived data type
        auto &ddt = data_type.derived;

        w << "! =========  " << ddt.name_prefixed << (gen_c_code ? "_C" : "") << "  =======\n";

        // 2 passes for C  code, 1st pass generates bound ddt
        for (int ipass = (gen_c_code) ? 0 : 1; ipass < 2; ipass++)
        {
            bool is_C = ipass == 0;

            w << "  TYPE, " << (is_C ? "BIND(C)" : "PUBLIC") << " :: " << ddt.name_prefixed << (is_C ? "_C" : "") << "\n";

            if (gen_c_code)
            {
                if (is_C)
                {
                    w << "   TYPE(C_PTR) :: object = C_NULL_PTR\n";
                }
                else
                {
                    w << "    TYPE( " << ddt.name_prefixed << "_C ) :: C_obj\n";
                }
            }

            // Loop through fields
            for (auto &field : ddt.fields)
            {
                // check max number of dimensions
                // check if this type contains any pointers/meshes or types
                // that have pointers/meshes
                // if (field.num_dims > ddt.max_dimsmax_num_dims)
                //     ddt.max_dimsmax_num_dims = field.num_dims;

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
                        w << "    TYPE(" << field.type->derived.name_prefixed << ") ";

                        // tmp[0] = '\0';
                        // if (*ddt->maps_to)
                        //     remove_nickname(mod.nickname, tolower(ddt->maps_to),
                        //                     tmp);
                        // if (must_have_real_or_double(tmp))
                        //     checkOnlyReals(ddt->maps_to, r);
                    }
                    else
                    {
                        // tmp[0] = '\0';
                        // if (*ddt->maps_to)
                        //     remove_nickname(mod.nickname, tolower(ddt->maps_to),
                        //                     tmp);
                        // if (must_have_real_or_double(tmp))
                        // {
                        //     if (strncmp(field.type->maps_to, "REAL", 4))
                        //     {
                        //         fprintf(stderr,
                        //                 "Registry warning: %s contains a "
                        //                 "field (%s) whose type is not real "
                        //                 "or double: %s\n",
                        //                 ddt->maps_to, field.name,
                        //                 field.type->maps_to);
                        //     }
                        // }

                        if (gen_c_code && field.is_pointer)
                        {
                            // out << "    %s ", c_types_binding(field.type->maps_to);
                        }
                        else
                        {
                            w << "    " << field.type->simple.maps_to_fortran << " ";
                        }
                    }

                    if (!field.dims.empty())
                    {
                        // if one dim is deferred they all
                        if (field.dims[0].is_deferred)
                        {
                            w << ", DIMENSION(";
                            bool first = true;
                            for (auto &dim : field.dims)
                            {
                                w << (first ? "" : ",") << ":";
                                first = false;
                            }

                            w << "), " << (field.is_pointer ? "POINTER " : "ALLOCATABLE ");
                        }
                        else
                        {
                            w << ", DIMENSION(";
                            bool first = true;
                            for (auto &dim : field.dims)
                            {
                                w << (first ? "" : ",") << dim.lower_bound << ":" << dim.upper_bound;
                                ;
                                first = false;
                            }
                            w << ") ";
                        }
                    }

                    w << " :: " << field.name << " ";

                    if (field.is_pointer)
                    {
                        w << "=> NULL() ";
                    }
                    else if (field.num_dims == 0 && !field.init_value.empty())
                    {
                        w << "= " << field.init_value << " ";
                    }

                    if (field.desc.compare("-") != 0 || field.units.compare("-") != 0)
                    {
                        w << "     !< " << field.desc << " [" << field.units << "]";
                    }

                    w << "\n";
                }
            }

            w << "  END TYPE " << data_type.derived.name_prefixed << (is_C ? "_C" : "") << "\n";
        }

        w << "! =======================\n";
    }

    w << "CONTAINS\n";

    // Loop through derived data types
    for (auto &dt_name : mod.data_type_order)
    {
        // Get data type
        auto it = mod.data_types.find(dt_name);
        auto &data_type = *it->second;

        // Generate functions
        this->gen_copy(w, mod, data_type.derived);
        this->gen_destroy(w, mod, data_type.derived);
        this->gen_pack(w, mod, data_type.derived);
        this->gen_unpack(w, mod, data_type.derived);

        if (gen_c_code)
        {
            // gen_copy_c2f(fp, mod, ddt_name, ddt_name_long);
            // gen_copy_f2c(fp, mod, ddt_name, ddt_name_long);
        }
    }

    // make interpolation routines for AirfoilInfo module
    if (tolower(mod.name).compare("airfoilinfo") == 0)
    {
        gen_ExtrapInterp(w, mod, "Output", "OutputType", "ReKi", 1);
        gen_ExtrapInterp(w, mod, "UA_BL_Type", "UA_BL_Type", "ReKi", 1);
    }
    else if (!this->no_extrap_interp)
    {
        // make interpolation routines for element-level DBEMT module
        if (tolower(mod.name).compare("dbemt") == 0)
        {
            gen_ExtrapInterp(w, mod, "ElementInputType", "ElementInputType", "DbKi", 1);
        }

        gen_ExtrapInterp(w, mod, "Input", "InputType", "DbKi", 1);
        gen_ExtrapInterp(w, mod, "Output", "OutputType", "DbKi", 1);
    }

    w << "END MODULE " << mod.name << "_Types\n";
    w << "!ENDOFREGISTRYGENERATEDFILE\n";
    return 0;
}

void Registry::gen_copy(std::ostream &w, const Module &mod, const DataType::Derived &ddt)
{
    w << " SUBROUTINE " << mod.nickname << "_Copy" << ddt.name_short << "( Src" << ddt.name_short << "Data, Dst" << ddt.name_short
      << "Data, CtrlCode, ErrStat, ErrMsg )\n";
    w << "   TYPE(" << ddt.name_prefixed << "), INTENT(" << (ddt.contains_pointer == 1 ? "INOUT" : "IN") << ") :: Src"
      << ddt.name_short << "Data\n";
    w << "   TYPE(" << ddt.name_prefixed << "), INTENT(INOUT) :: Dst" << ddt.name_short << "Data\n";
    w << "   INTEGER(IntKi),  INTENT(IN   ) :: CtrlCode\n";
    w << "   INTEGER(IntKi),  INTENT(  OUT) :: ErrStat\n";
    w << "   CHARACTER(*),    INTENT(  OUT) :: ErrMsg\n";
    w << "! Local \n";
    w << "   INTEGER(IntKi)                 :: i,j,k\n";
    for (int d = 1; d <= ddt.max_dims; d++)
        w << "   INTEGER(IntKi)                 :: i" << d << ", i" << d << "_l, i" << d
          << "_u  !  bounds (upper/lower) for an array dimension " << d << "\n";
    w << "   INTEGER(IntKi)                 :: ErrStat2\n";
    w << "   CHARACTER(ErrMsgLen)           :: ErrMsg2\n";
    w << "   CHARACTER(*), PARAMETER        :: RoutineName = '" << mod.nickname << "_Copy" << ddt.name_short << "'\n";
    w << "! \n";
    w << "   ErrStat = ErrID_None\n";
    w << "   ErrMsg  = \"\"\n";

    // Loop through fields
    for (auto &field : ddt.fields)
    {
        if (field.type == nullptr)
            continue;

        std::string src = "Src" + ddt.name_short + "Data%" + field.name;
        std::string dst = "Dst" + ddt.name_short + "Data%" + field.name;
        std::string alloc_assoc = field.is_pointer ? "ASSOCIATED" : "ALLOCATED";

        // check if this is an allocatable array:
        if (field.dims.size() > 0 && field.dims[0].is_deferred)
        {

            w << "IF (" << alloc_assoc << "(" << src << ")) THEN\n";

            std::string tmp;

            for (int d = 1; d <= field.dims.size(); d++)
            {
                w << "  i" << d << "_l = LBOUND(" << src << "," << d << ")\n";
                w << "  i" << d << "_u = UBOUND(" << src << "," << d << ")\n";
                tmp += ",i" + std::to_string(d) + "_l:i" + std::to_string(d) + "_u";
            }

            w << "  IF (.NOT. " << alloc_assoc << "(" << dst << ")) THEN \n";
            w << "    ALLOCATE(" << dst << "(" << tmp.substr(1) << "),STAT=ErrStat2)\n";
            w << "    IF (ErrStat2 /= 0) THEN \n";
            w << "      CALL SetErrStat(ErrID_Fatal, 'Error allocating Dst" << ddt.name_short << "Data%" << field.name
              << ".', ErrStat, ErrMsg,RoutineName)\n";
            w << "      RETURN\n";
            w << "    END IF\n";

            // bjj: this needs to be updated if we've got multidimensional arrays
            if (gen_c_code && ddt.contains_pointer)
            {
                w << "    Dst" << ddt.name_short << "Data%c_obj%" << field.name << "_Len = SIZE(Dst" << ddt.name_short << "Data%"
                  << field.name << ")\n";
                w << "    IF (Dst" << ddt.name_short << "Data%c_obj%" << field.name << "_Len > 0) &\n";

                w << "          Dst" << ddt.name_short << "Data%c_obj%" << field.name << " = C_LOC( Dst" << ddt.name_short
                  << "Data%" << field.name << "(";
                for (int d = 1; d <= field.dims.size(); d++)
                {
                    w << (d > 1 ? "," : "") << " i" << d << "_l";
                }
                w << " ) )\n";
            }

            w << "  END IF\n";
        }

        // includes mesh and dll_type
        if (field.type->type == DataType::Type::Derived)
        {
            auto &ddt = field.type->derived;

            for (int d = field.dims.size(); d >= 1; d--)
            {
                w << "    DO i" << d << " = LBOUND(" << src << "," << d << "), UBOUND(" << src << "," << d << ")\n";
            }

            if (ddt.name_short.compare("MeshType") == 0)
            {
                w << "      CALL MeshCopy( " << src << dimstr(field.dims.size()) << ", " << dst << dimstr(field.dims.size())
                  << ", CtrlCode, ErrStat2, ErrMsg2 )\n";
                w << "         CALL SetErrStat(ErrStat2, ErrMsg2, "
                     "ErrStat, ErrMsg, RoutineName)\n";
                w << "         IF (ErrStat>=AbortErrLev) RETURN\n";
            }
            else if (ddt.name_short.compare("DLL_Type") == 0)
            {
                w << "      " << dst << " = " << src << "\n";
            }
            else
            {
                auto name_short = ddt.name_short;
                if (!ddt.is_interface)
                    name_short = tolower(name_short);
                w << "      CALL " << ddt.module->nickname << "_Copy" << name_short << "( " << src << dimstr(field.dims.size())
                  << ", " << dst << dimstr(field.dims.size()) << ", CtrlCode, ErrStat2, ErrMsg2 )\n";
                w << "         CALL SetErrStat(ErrStat2, ErrMsg2, "
                     "ErrStat, ErrMsg,RoutineName)\n";
                w << "         IF (ErrStat>=AbortErrLev) RETURN\n";
            }

            for (auto &d : field.dims)
            {
                w << "    ENDDO\n";
            }
        }
        else
        {
            w << "    " << dst << " = " << src << "\n";
            // if (gen_c_code && !field.is_pointer)
            // {
            //     if (field.dims.size() == 0) // scalar of any type OR a character
            //         array
            //         {
            //             out << "    Dst%sData%%C_obj%%%s = "
            //                    "Src%sData%%C_obj%%%s\n",
            //                 ddt.name_short, field.name, ddt.name_short,
            //                 field.name;
            //         }
            // }
        }

        // close IF (check on allocatable array)
        if (field.dims.size() > 0 && field.dims[0].is_deferred)
            w << "ENDIF\n";
    }

    w << " END SUBROUTINE " << mod.nickname << "_Copy" << ddt.name_short << "\n\n";
}

void Registry::gen_destroy(std::ostream &out, const Module &mod, const DataType::Derived &ddt)
{
    auto ddt_data = ddt.name_short + "Data";
    auto routine_name = mod.nickname + "_Destroy" + ddt.name_short;

    out << " SUBROUTINE " << routine_name << "( " << ddt_data << ", ErrStat, ErrMsg, DEALLOCATEpointers )\n";
    out << "  TYPE(" << ddt.name_prefixed << "), INTENT(INOUT) :: " << ddt_data << "\n";
    out << "  INTEGER(IntKi),  INTENT(  OUT) :: ErrStat\n";
    out << "  CHARACTER(*),    INTENT(  OUT) :: ErrMsg\n";
    out << "  LOGICAL,OPTIONAL,INTENT(IN   ) :: DEALLOCATEpointers\n";
    out << "  \n";
    out << "  INTEGER(IntKi)                 :: i, i1, i2, i3, i4, i5 \n";
    out << "  LOGICAL                        :: DEALLOCATEpointers_local\n";
    out << "  INTEGER(IntKi)                 :: ErrStat2\n";
    out << "  CHARACTER(ErrMsgLen)           :: ErrMsg2\n";
    out << "  CHARACTER(*),    PARAMETER :: RoutineName = '" << routine_name << "'\n\n";
    out << "  ErrStat = ErrID_None\n";
    out << "  ErrMsg  = \"\"\n\n";
    out << "  IF (PRESENT(DEALLOCATEpointers)) THEN\n";
    out << "     DEALLOCATEpointers_local = DEALLOCATEpointers\n";
    out << "  ELSE\n";
    out << "     DEALLOCATEpointers_local = .true.\n";
    out << "  END IF\n";
    out << "  \n";

    for (auto &field : ddt.fields)
    {
        if (field.type == nullptr)
        {
            std::cerr << "Registry warning generating " << mod.nickname << "_Destroy" << ddt.name_short << ": " << field.name
                      << " has no type.\n";
            continue;
        }

        // Combine data name and field name
        auto ddt_field = ddt_data + "%" + field.name;

        // If field is an array with deferred dimensions
        if (field.dims.size() > 0 && field.dims[0].is_deferred)
        {
            out << "IF (" << (field.is_pointer ? "ASSOCIATED" : "ALLOCATED") << "(" << ddt_field << ")) THEN\n";
        }

        // If field is a derived data type
        if (field.type->type == DataType::Type::Derived)
        {
            for (int d = field.dims.size(); d >= 1; d--)
            {
                out << "DO i" << d << " = LBOUND(" << ddt_field << "," << d << "), UBOUND(" << ddt_field << "," << d << ")\n";
            }

            auto ddt_field_dims = ddt_field + dimstr(field.dims.size());

            if (field.type->derived.name.compare("MeshType") == 0)
            {
                out << "  CALL MeshDestroy( " << ddt_field_dims << ", ErrStat2, ErrMsg2 )\n";
                out << "     CALL SetErrStat(ErrStat2, ErrMsg2, ErrStat, ErrMsg, RoutineName)\n";
            }
            else if (field.type->derived.name.compare("DLL_Type") == 0)
            {
                out << "  CALL FreeDynamicLib( " << ddt_field_dims << ", ErrStat2, ErrMsg2 )\n";
                out << "     CALL SetErrStat(ErrStat2, ErrMsg2, ErrStat, ErrMsg, RoutineName)\n";
            }
            else
            {
                auto name_short =
                    field.type->derived.is_interface ? field.type->derived.name_short : tolower(field.type->derived.name_short);
                out << "  CALL " << field.type->derived.module->nickname << "_Destroy" << name_short << "( " << ddt_field_dims
                    << ", ErrStat2, ErrMsg2, DEALLOCATEpointers_local )\n";
                out << "     CALL SetErrStat(ErrStat2, ErrMsg2, ErrStat, ErrMsg, RoutineName)\n";
            }

            for (int d = field.dims.size(); d >= 1; d--)
            {
                out << "ENDDO\n";
            }
        }

        if (field.dims.size() > 0 && field.dims[0].is_deferred)
        {
            if (field.is_pointer)
                out << " IF (DEALLOCATEpointers_local) &\n";

            out << "  DEALLOCATE(" << ddt_field << ")\n";

            if (field.is_pointer)
            {
                out << "  " << ddt_field << " => NULL()\n";
                if (gen_c_code)
                {
                    auto ddt_field_c = ddt_data + "%C_obj%" + field.name;
                    out << "  " << ddt_field_c << " = C_NULL_PTR\n";
                    out << "  " << ddt_field_c << "_Len = 0\n";
                }
            }
            out << "ENDIF\n";
        }
    }

    out << " END SUBROUTINE " << mod.nickname << "_Destroy" << ddt.name_short << "\n\n";
}

void Registry::gen_pack(std::ostream &out, const Module &mod, const DataType::Derived &ddt)
{
    auto ddt_data = ddt.name_short + "Data";
    auto routine_name = mod.nickname + "_Pack" + ddt.name_short;

    out << " SUBROUTINE " << routine_name << "( ReKiBuf, DbKiBuf, IntKiBuf, Indata, ErrStat, ErrMsg, SizeOnly )\n";
    out << "  REAL(ReKi),       ALLOCATABLE, INTENT(  OUT) :: ReKiBuf(:)\n";
    out << "  REAL(DbKi),       ALLOCATABLE, INTENT(  OUT) :: DbKiBuf(:)\n";
    out << "  INTEGER(IntKi),   ALLOCATABLE, INTENT(  OUT) :: IntKiBuf(:)\n";
    out << "  TYPE(" << ddt.name_prefixed << "),  INTENT(IN) :: InData\n";
    out << "  INTEGER(IntKi),   INTENT(  OUT) :: ErrStat\n";
    out << "  CHARACTER(*),     INTENT(  OUT) :: ErrMsg\n";
    out << "  LOGICAL,OPTIONAL, INTENT(IN   ) :: SizeOnly\n";
    out << "    ! Local variables\n";
    out << "  INTEGER(IntKi)                 :: Re_BufSz\n";
    out << "  INTEGER(IntKi)                 :: Re_Xferred\n";
    out << "  INTEGER(IntKi)                 :: Db_BufSz\n";
    out << "  INTEGER(IntKi)                 :: Db_Xferred\n";
    out << "  INTEGER(IntKi)                 :: Int_BufSz\n";
    out << "  INTEGER(IntKi)                 :: Int_Xferred\n";
    out << "  INTEGER(IntKi)                 :: i,i1,i2,i3,i4,i5\n";
    out << "  LOGICAL                        :: OnlySize ! if present and true, do not pack, just allocate buffers\n";
    out << "  INTEGER(IntKi)                 :: ErrStat2\n";
    out << "  CHARACTER(ErrMsgLen)           :: ErrMsg2\n";
    out << "  CHARACTER(*), PARAMETER        :: RoutineName = '" << routine_name << "'\n";

    out << " ! buffers to store subtypes, if any\n";
    out << "  REAL(ReKi),      ALLOCATABLE   :: Re_Buf(:)\n";
    out << "  REAL(DbKi),      ALLOCATABLE   :: Db_Buf(:)\n";
    out << "  INTEGER(IntKi),  ALLOCATABLE   :: Int_Buf(:)\n\n";

    out << "  OnlySize = .FALSE.\n";
    out << "  IF ( PRESENT(SizeOnly) ) THEN\n";
    out << "    OnlySize = SizeOnly\n";
    out << "  ENDIF\n";
    out << "    !\n";

    out << "  ErrStat = ErrID_None\n";
    out << "  ErrMsg  = \"\"\n";
    out << "  Re_BufSz  = 0\n";
    out << "  Db_BufSz  = 0\n";
    out << "  Int_BufSz  = 0\n";

    bool frst = true;

    for (auto &field : ddt.fields)
    {
        if (field.type == nullptr)
        {
            std::cerr << "Registry warning generating " << routine_name << ": " << field.name << " has no type.\n";
            return;
        }

        auto assoc_alloc = field.is_pointer ? "ASSOCIATED" : "ALLOCATED";
        auto field_dims = field.name + dimstr(field.dims.size());

        if (field.dims.size() > 0 && field.dims[0].is_deferred)
        {
            out << "  Int_BufSz   = Int_BufSz   + 1     ! " << field.name << " allocated yes/no\n";
            out << "  IF ( " << assoc_alloc << "(InData%" << field.name << ") ) THEN\n";
            out << "    Int_BufSz   = Int_BufSz   + 2*" << field.dims.size() << "  ! " << field.name
                << " upper/lower bounds for each dimension\n";
        }

        //  call individual routines to pack data from subtypes:
        if (field.type->type == DataType::Type::Derived)
        {
            if (frst)
            {
                out << "   ! Allocate buffers for subtypes, if any (we'll get sizes from these) \n";
                frst = false;
            }

            for (int d = field.dims.size(); d >= 1; d--)
            {
                out << "    DO i" << d << " = LBOUND(InData%" << field.name << "," << d << "), UBOUND(InData%" << field.name << ","
                    << d << ")\n";
            }
            out << "      Int_BufSz   = Int_BufSz + 3  ! " << field.name << ": size of buffers for each call to pack subtype\n";

            if (field.type->derived.name.compare("MeshType") == 0)
            {
                out << "      CALL MeshPack( InData%" << field_dims << ", Re_Buf, Db_Buf, Int_Buf, ErrStat2, ErrMsg2, .TRUE. ) ! "
                    << field.name << " \n";
            }
            else if (field.type->derived.name.compare("DLL_Type") == 0)
            {
                out << "      CALL DLLTypePack( InData%" << field_dims
                    << ", Re_Buf, Db_Buf, Int_Buf, ErrStat2, ErrMsg2, .TRUE. ) ! " << field.name << " \n";
            }
            else if (field.type->type == DataType::Type::Derived)
            {
                auto name_short =
                    field.type->derived.is_interface ? field.type->derived.name_short : tolower(field.type->derived.name_short);
                out << "      CALL " << field.type->derived.module->nickname << "_Pack" << name_short
                    << "( Re_Buf, Db_Buf, Int_Buf, InData%" << field_dims << ", ErrStat2, ErrMsg2, .TRUE. ) ! " << field.name
                    << " \n";
            }

            out << "        CALL SetErrStat(ErrStat2, ErrMsg2, ErrStat, ErrMsg, RoutineName)\n";
            out << "        IF (ErrStat >= AbortErrLev) RETURN\n\n";

            out << "      IF(ALLOCATED(Re_Buf)) THEN ! " << field.name << "\n";
            out << "         Re_BufSz  = Re_BufSz  + SIZE( Re_Buf  )\n";
            out << "         DEALLOCATE(Re_Buf)\n";
            out << "      END IF\n";

            out << "      IF(ALLOCATED(Db_Buf)) THEN ! " << field.name << "\n";
            out << "         Db_BufSz  = Db_BufSz  + SIZE( Db_Buf  )\n";
            out << "         DEALLOCATE(Db_Buf)\n";
            out << "      END IF\n";

            out << "      IF(ALLOCATED(Int_Buf)) THEN ! " << field.name << "\n";
            out << "         Int_BufSz = Int_BufSz + SIZE( Int_Buf )\n";
            out << "         DEALLOCATE(Int_Buf)\n";
            out << "      END IF\n";

            for (int d = field.dims.size(); d >= 1; d--)
            {
                out << "    END DO\n";
            }
        }
        // intrinsic data types
        else
        {

            // do all dimensions of arrays (no need for loop over i%d)

            std::string size = field.dims.size() > 0 ? "SIZE(InData%" + field.name + ")" : "1";

            if (field.type->simple.maps_to_fortran.compare("REAL(ReKi)") == 0 ||
                field.type->simple.maps_to_fortran.compare("REAL(SiKi)") == 0)
            {
                out << "      Re_BufSz   = Re_BufSz   + " << size << "  ! " << field.name << "\n";
            }
            else if (field.type->simple.maps_to_fortran.compare("REAL(DbKi)") == 0 ||
                     field.type->simple.maps_to_fortran.compare("REAL(R8Ki)") == 0)
            {
                out << "      Db_BufSz   = Db_BufSz   + " << size << "  ! " << field.name << "\n";
            }
            else if (field.type->simple.maps_to_fortran.compare("INTEGER(IntKi)") == 0 ||
                     field.type->simple.maps_to_fortran.compare("LOGICAL") == 0)
            {
                out << "      Int_BufSz  = Int_BufSz  + " << size << "  ! " << field.name << "\n";
            }
            else
            {
                out << "      Int_BufSz  = Int_BufSz  + " << size << "*LEN(InData%" << field.name << ")  ! " << field.name << "\n";
            }
        }

        if (field.dims.size() > 0 && field.dims[0].is_deferred)
            out << "  END IF\n";
    }

    // Allocate buffers
    out << "  IF ( Re_BufSz  .GT. 0 ) THEN \n";
    out << "     ALLOCATE( ReKiBuf(  Re_BufSz  ), STAT=ErrStat2 )\n";
    out << "     IF (ErrStat2 /= 0) THEN \n";
    out << "       CALL SetErrStat(ErrID_Fatal, 'Error allocating ReKiBuf.', ErrStat, ErrMsg,RoutineName)\n";
    out << "       RETURN\n";
    out << "     END IF\n";
    out << "  END IF\n";

    out << "  IF ( Db_BufSz  .GT. 0 ) THEN \n";
    out << "     ALLOCATE( DbKiBuf(  Db_BufSz  ), STAT=ErrStat2 )\n";
    out << "     IF (ErrStat2 /= 0) THEN \n";
    out << "       CALL SetErrStat(ErrID_Fatal, 'Error allocating DbKiBuf.', ErrStat, ErrMsg,RoutineName)\n";
    out << "       RETURN\n";
    out << "     END IF\n";
    out << "  END IF\n";

    out << "  IF ( Int_BufSz  .GT. 0 ) THEN \n";
    out << "     ALLOCATE( IntKiBuf(  Int_BufSz  ), STAT=ErrStat2 )\n";
    out << "     IF (ErrStat2 /= 0) THEN \n";
    out << "       CALL SetErrStat(ErrID_Fatal, 'Error allocating IntKiBuf.', ErrStat, ErrMsg,RoutineName)\n";
    out << "       RETURN\n";
    out << "     END IF\n";
    out << "  END IF\n";
    out << "  IF(OnlySize) RETURN ! return early if only trying to allocate buffers (not pack them)\n\n";

    if (gen_c_code)
    {
        out << "  IF (C_ASSOCIATED(InData%%C_obj%%object)) ";
        out << "CALL SetErrStat(ErrID_Severe,'C_obj%%object cannot be packed.',ErrStat,ErrMsg,RoutineName)\n\n";
    }

    out << "  Re_Xferred  = 1\n";
    out << "  Db_Xferred  = 1\n";
    out << "  Int_Xferred = 1\n\n";

    std::string mainIndent = "";

    // Pack data
    for (auto &field : ddt.fields)
    {
        auto assoc_alloc = field.is_pointer ? "ASSOCIATED" : "ALLOCATED";
        auto field_dims = field.name + dimstr(field.dims.size());

        if (!field.dims.empty() && field.dims.front().is_deferred)
        {
            // store whether the data type is allocated and the bounds of each dimension
            out << "  IF ( .NOT. " << assoc_alloc << "(InData%" << field.name << ") ) THEN\n";
            out << "    IntKiBuf( Int_Xferred ) = 0\n"; // not allocated
            out << "    Int_Xferred = Int_Xferred + 1\n";
            out << "  ELSE\n";
            out << "    IntKiBuf( Int_Xferred ) = 1\n"; // allocated
            out << "    Int_Xferred = Int_Xferred + 1\n";
            for (int d = 1; d <= field.dims.size(); d++)
            {
                out << "    IntKiBuf( Int_Xferred    ) = LBOUND(InData%" << field.name << "," << d << ")\n";
                out << "    IntKiBuf( Int_Xferred + 1) = UBOUND(InData%" << field.name << "," << d << ")\n";
                out << "    Int_Xferred = Int_Xferred + 2\n";
            }
            out << "\n";
            mainIndent = "  ";
        }
        else
        {
            mainIndent = "";
        }

        //  call individual routines to pack data from subtypes:
        if (field.type->type == DataType::Type::Derived)
        {
            if (frst == 1)
            {
                out << "   ! Allocate buffers for subtypes, if any (we'll get sizes from these) \n";
                frst = false;
            }

            for (int d = field.dims.size(); d >= 1; d--)
            {
                out << "    DO i" << d << " = LBOUND(InData%" << field.name << "," << d << "), UBOUND(InData%" << field.name << ","
                    << d << ")\n";
            }

            if (field.type->derived.name.compare("MeshType") == 0)
            {
                out << "      CALL MeshPack( InData%" << field_dims << ", Re_Buf, Db_Buf, Int_Buf, ErrStat2, ErrMsg2, OnlySize ) ! "
                    << field.name << " \n";
            }
            else if (field.type->derived.name.compare("DLL_Type") == 0)
            {
                out << "      CALL DLLTypePack( InData%" << field_dims
                    << ", Re_Buf, Db_Buf, Int_Buf, ErrStat2, ErrMsg2, OnlySize ) ! " << field.name << " \n";
            }
            else
            {
                auto name_short =
                    field.type->derived.is_interface ? field.type->derived.name_short : tolower(field.type->derived.name_short);
                out << "      CALL " << field.type->derived.module->nickname << "_Pack" << name_short
                    << "( Re_Buf, Db_Buf, Int_Buf, InData%" << field_dims << ", ErrStat2, ErrMsg2, OnlySize ) ! " << field.name
                    << " \n";
            }

            out << "        CALL SetErrStat(ErrStat2, ErrMsg2, ErrStat, ErrMsg, RoutineName)\n";
            out << "        IF (ErrStat >= AbortErrLev) RETURN\n\n";

            out << "      IF(ALLOCATED(Re_Buf)) THEN\n";
            out << "        IntKiBuf( Int_Xferred ) = SIZE(Re_Buf); Int_Xferred = Int_Xferred + 1\n";
            out << "        IF (SIZE(Re_Buf) > 0) ReKiBuf( Re_Xferred:Re_Xferred+SIZE(Re_Buf)-1 ) = Re_Buf\n";
            out << "        Re_Xferred = Re_Xferred + SIZE(Re_Buf)\n";
            out << "        DEALLOCATE(Re_Buf)\n";
            out << "      ELSE\n";
            out << "        IntKiBuf( Int_Xferred ) = 0; Int_Xferred = Int_Xferred + 1\n";
            out << "      ENDIF\n";

            out << "      IF(ALLOCATED(Db_Buf)) THEN\n";
            out << "        IntKiBuf( Int_Xferred ) = SIZE(Db_Buf); Int_Xferred = Int_Xferred + 1\n";
            out << "        IF (SIZE(Db_Buf) > 0) DbKiBuf( Db_Xferred:Db_Xferred+SIZE(Db_Buf)-1 ) = Db_Buf\n";
            out << "        Db_Xferred = Db_Xferred + SIZE(Db_Buf)\n";
            out << "        DEALLOCATE(Db_Buf)\n";
            out << "      ELSE\n";
            out << "        IntKiBuf( Int_Xferred ) = 0; Int_Xferred = Int_Xferred + 1\n";
            out << "      ENDIF\n";

            out << "      IF(ALLOCATED(Int_Buf)) THEN\n";
            out << "        IntKiBuf( Int_Xferred ) = SIZE(Int_Buf); Int_Xferred = Int_Xferred + 1\n";
            out << "        IF (SIZE(Int_Buf) > 0) IntKiBuf( Int_Xferred:Int_Xferred+SIZE(Int_Buf)-1 ) = Int_Buf\n";
            out << "        Int_Xferred = Int_Xferred + SIZE(Int_Buf)\n";
            out << "        DEALLOCATE(Int_Buf)\n";
            out << "      ELSE\n";
            out << "        IntKiBuf( Int_Xferred ) = 0; Int_Xferred = Int_Xferred + 1\n";
            out << "      ENDIF\n";

            for (int d = field.dims.size(); d >= 1; d--)
            {
                out << "    END DO\n";
            }
        }
        else
        {
            // intrinsic data types
            // do all dimensions of arrays (no need for loop over i%d)
            auto indent = "  " + mainIndent;

            for (int d = field.dims.size(); d >= 1; d--)
            {
                out << indent << "  DO i" << d << " = LBOUND(InData%" << field.name << "," << d << "), UBOUND(InData%" << field.name
                    << "," << d << ")\n";
                indent += "  ";
            }

            if (field.type->simple.maps_to_fortran.compare("REAL(ReKi)") == 0 ||
                field.type->simple.maps_to_fortran.compare("REAL(SiKi)") == 0)
            {
                out << indent << "  ReKiBuf(Re_Xferred) = InData%" << field_dims << "\n";
                out << indent << "  Re_Xferred = Re_Xferred + 1\n";
            }
            else if (field.type->simple.maps_to_fortran.compare("REAL(DbKi)") == 0 ||
                     field.type->simple.maps_to_fortran.compare("REAL(R8Ki)") == 0)
            {
                out << indent << "  DbKiBuf(Db_Xferred) = InData%" << field_dims << "\n";
                out << indent << "  Db_Xferred = Db_Xferred + 1\n";
            }
            else if (field.type->simple.maps_to_fortran.compare("INTEGER(IntKi)") == 0)
            {
                out << indent << "  IntKiBuf(Int_Xferred) = InData%" << field_dims << "\n";
                out << indent << "  Int_Xferred = Int_Xferred + 1\n";
            }
            else if (field.type->simple.maps_to_fortran.compare("LOGICAL") == 0)
            {
                out << indent << "  IntKiBuf(Int_Xferred) = TRANSFER(InData%" << field_dims << ", IntKiBuf(1))\n";
                out << indent << "  Int_Xferred = Int_Xferred + 1\n";
            }
            else
            {
                out << indent << "  DO I = 1, LEN(InData%" << field.name << ")\n";
                out << indent << "    IntKiBuf(Int_Xferred) = ICHAR(InData%" << field_dims << "(I:I), IntKi)\n";
                out << indent << "    Int_Xferred = Int_Xferred + 1\n";
                out << indent << "  END DO ! I\n";
            }

            for (int d = field.dims.size(); d >= 1; d--)
            {
                indent = "  ";
                indent += mainIndent;
                for (int i = 1; i < d; i++)
                {
                    indent += "  ";
                }
                out << indent << "  END DO\n";
            }
        }

        if (!field.dims.empty() && field.dims.front().is_deferred)
        {
            out << "  END IF\n";
        }
    }

    out << " END SUBROUTINE " << routine_name << "\n\n";
}

void Registry::gen_unpack(std::ostream &out, const Module &mod, const DataType::Derived &ddt)
{
    auto routine_name = mod.nickname + "_UnPack" + ddt.name_short;

    out << " SUBROUTINE " << routine_name << "( ReKiBuf, DbKiBuf, IntKiBuf, Outdata, ErrStat, ErrMsg )\n";
    out << "  REAL(ReKi),      ALLOCATABLE, INTENT(IN   ) :: ReKiBuf(:)\n";
    out << "  REAL(DbKi),      ALLOCATABLE, INTENT(IN   ) :: DbKiBuf(:)\n";
    out << "  INTEGER(IntKi),  ALLOCATABLE, INTENT(IN   ) :: IntKiBuf(:)\n";
    out << "  TYPE(" << ddt.name_prefixed << "), INTENT(INOUT) :: OutData\n";
    out << "  INTEGER(IntKi),  INTENT(  OUT) :: ErrStat\n";
    out << "  CHARACTER(*),    INTENT(  OUT) :: ErrMsg\n";
    out << "    ! Local variables\n";
    out << "  INTEGER(IntKi)                 :: Buf_size\n";
    out << "  INTEGER(IntKi)                 :: Re_Xferred\n";
    out << "  INTEGER(IntKi)                 :: Db_Xferred\n";
    out << "  INTEGER(IntKi)                 :: Int_Xferred\n";
    out << "  INTEGER(IntKi)                 :: i\n";
    for (int d = 1; d <= ddt.max_dims; d++)
    {
        out << "  INTEGER(IntKi)                 :: i" << d << ", i" << d << "_l, i" << d
            << "_u  !  bounds (upper/lower) for an array dimension " << d << "\n";
    }
    out << "  INTEGER(IntKi)                 :: ErrStat2\n";
    out << "  CHARACTER(ErrMsgLen)           :: ErrMsg2\n";
    out << "  CHARACTER(*), PARAMETER        :: RoutineName = '" << routine_name << "'\n";

    out << " ! buffers to store meshes, if any\n";
    out << "  REAL(ReKi),      ALLOCATABLE   :: Re_Buf(:)\n";
    out << "  REAL(DbKi),      ALLOCATABLE   :: Db_Buf(:)\n";
    out << "  INTEGER(IntKi),  ALLOCATABLE   :: Int_Buf(:)\n";
    out << "    !\n";
    out << "  ErrStat = ErrID_None\n";
    out << "  ErrMsg  = \"\"\n";
    out << "  Re_Xferred  = 1\n";
    out << "  Db_Xferred  = 1\n";
    out << "  Int_Xferred  = 1\n";

    // BJJ: TODO:  if there are C types, we're going to have to associate with C data structures....

    std::string tmp;

    // Unpack data
    for (auto &field : ddt.fields)
    {
        tmp = "";
        std::string mainIndent;
        bool is_deferred = !field.dims.empty() && field.dims.front().is_deferred;
        auto assoc_alloc = field.is_pointer ? "ASSOCIATED" : "ALLOCATED";
        auto field_dims = field.name + dimstr(field.dims.size());

        if (is_deferred)
        {
            // determine if the array was allocated when packed:
            out << "  IF ( IntKiBuf( Int_Xferred ) == 0 ) THEN  ! " << field.name << " not allocated\n";
            out << "    Int_Xferred = Int_Xferred + 1\n";
            out << "  ELSE\n";
            out << "    Int_Xferred = Int_Xferred + 1\n";

            for (int d = 1; d <= field.dims.size(); d++)
            {
                out << "    i" << d << "_l = IntKiBuf( Int_Xferred    )\n";
                out << "    i" << d << "_u = IntKiBuf( Int_Xferred + 1)\n";
                out << "    Int_Xferred = Int_Xferred + 2\n";
                tmp += (d > 1 ? ",i" : "i") + std::to_string(d) + "_l:i" + std::to_string(d) + "_u";
            }

            out << "    IF (" << assoc_alloc << "(OutData%" << field.name << ")) DEALLOCATE(OutData%" << field.name
                << ")\n"; // BJJ: need NULLIFY(), too?
            out << "    ALLOCATE(OutData%" << field.name << "(" << tmp << "),STAT=ErrStat2)\n";
            out << "    IF (ErrStat2 /= 0) THEN \n";
            out << "       CALL SetErrStat(ErrID_Fatal, 'Error allocating OutData%" << field.name
                << ".', ErrStat, ErrMsg,RoutineName)\n";
            out << "       RETURN\n";
            out << "    END IF\n";

            // bjj: this needs to be updated if we've got multiple dimension arrays
            if (gen_c_code && field.is_pointer)
            {
                out << "    OutData%c_obj%" << field.name << "_Len = SIZE(OutData%" << field.name << ")\n";
                out << "    IF (OutData%c_obj%" << field.name << "_Len > 0) &\n";

                out << "       OutData%c_obj%" << field.name << " = C_LOC( OutData%" << field.name << "(";
                for (int d = 1; d <= field.dims.size(); d++)
                {
                    out << " i" << d << "_l";
                    if (d < field.dims.size())
                    {
                        out << ",";
                    }
                }
                out << " ) )\n";
            }
            mainIndent = "  ";
        }
        else
        {
            for (int d = 1; d <= field.dims.size(); d++)
            {
                out << "    i" << d << "_l = LBOUND(OutData%" << field.name << "," << d << ")\n";
                out << "    i" << d << "_u = UBOUND(OutData%" << field.name << "," << d << ")\n";
                tmp += (d > 1 ? ",i" : "i") + std::to_string(d) + "_l:i" + std::to_string(d) + "_u";
            }
            mainIndent = "";
        }

        //  call individual routines to pack data from subtypes:
        if (field.type->type == DataType::Type::Derived)
        {
            for (int d = field.dims.size(); d >= 1; d--)
            {
                out << "    DO i" << d << " = LBOUND(OutData%" << field.name << "," << d << "), UBOUND(OutData%" << field.name
                    << "," << d << ")\n";
            }

            // initialize buffers to send to subtype-unpack routines:
            // reals:
            out << "      Buf_size=IntKiBuf( Int_Xferred )\n";
            out << "      Int_Xferred = Int_Xferred + 1\n";
            out << "      IF(Buf_size > 0) THEN\n";
            out << "        ALLOCATE(Re_Buf(Buf_size),STAT=ErrStat2)\n";
            out << "        IF (ErrStat2 /= 0) THEN \n";
            out << "           CALL SetErrStat(ErrID_Fatal, 'Error allocating Re_Buf.', ErrStat, ErrMsg,RoutineName)\n";
            out << "           RETURN\n";
            out << "        END IF\n";

            out << "        Re_Buf = ReKiBuf( Re_Xferred:Re_Xferred+Buf_size-1 )\n";
            out << "        Re_Xferred = Re_Xferred + Buf_size\n";
            out << "      END IF\n";

            // doubles:
            out << "      Buf_size=IntKiBuf( Int_Xferred )\n";
            out << "      Int_Xferred = Int_Xferred + 1\n";
            out << "      IF(Buf_size > 0) THEN\n";
            out << "        ALLOCATE(Db_Buf(Buf_size),STAT=ErrStat2)\n";
            out << "        IF (ErrStat2 /= 0) THEN \n";
            out << "           CALL SetErrStat(ErrID_Fatal, 'Error allocating Db_Buf.', ErrStat, ErrMsg,RoutineName)\n";
            out << "           RETURN\n";
            out << "        END IF\n";

            out << "        Db_Buf = DbKiBuf( Db_Xferred:Db_Xferred+Buf_size-1 )\n";
            out << "        Db_Xferred = Db_Xferred + Buf_size\n";
            out << "      END IF\n";

            // integers:
            out << "      Buf_size=IntKiBuf( Int_Xferred )\n";
            out << "      Int_Xferred = Int_Xferred + 1\n";
            out << "      IF(Buf_size > 0) THEN\n";
            out << "        ALLOCATE(Int_Buf(Buf_size),STAT=ErrStat2)\n";
            out << "        IF (ErrStat2 /= 0) THEN \n";
            out << "           CALL SetErrStat(ErrID_Fatal, 'Error allocating Int_Buf.', ErrStat, ErrMsg,RoutineName)\n";
            out << "           RETURN\n";
            out << "        END IF\n";

            out << "        Int_Buf = IntKiBuf( Int_Xferred:Int_Xferred+Buf_size-1 )\n";
            out << "        Int_Xferred = Int_Xferred + Buf_size\n";
            out << "      END IF\n";

            if (field.type->derived.name.compare("MeshType") == 0)
            {
                out << "      CALL MeshUnpack( OutData%" << field_dims << ", Re_Buf, Db_Buf, Int_Buf, ErrStat2, ErrMsg2 ) ! "
                    << field.name << " \n";
            }
            else if (field.type->derived.name.compare("DLL_Type") == 0)
            {
                out << "      CALL DLLTypeUnpack( OutData%" << field_dims << ", Re_Buf, Db_Buf, Int_Buf, ErrStat2, ErrMsg2 ) ! "
                    << field.name << " \n";
            }
            else
            {
                auto name_short =
                    field.type->derived.is_interface ? field.type->derived.name_short : tolower(field.type->derived.name_short);
                out << "      CALL " << field.type->derived.module->nickname << "_Unpack" << name_short
                    << "( Re_Buf, Db_Buf, Int_Buf, OutData%" << field_dims << ", ErrStat2, ErrMsg2 ) ! " << field.name << " \n";
            }
            out << "        CALL SetErrStat(ErrStat2, ErrMsg2, ErrStat, ErrMsg, RoutineName)\n";
            out << "        IF (ErrStat >= AbortErrLev) RETURN\n\n";

            out << "      IF(ALLOCATED(Re_Buf )) DEALLOCATE(Re_Buf )\n";
            out << "      IF(ALLOCATED(Db_Buf )) DEALLOCATE(Db_Buf )\n";
            out << "      IF(ALLOCATED(Int_Buf)) DEALLOCATE(Int_Buf)\n";

            for (int d = field.dims.size(); d >= 1; d--)
            {
                out << "    END DO\n";
            }
        }
        else
        {
            auto indent = "  " + mainIndent;
            for (int d = field.dims.size(); d >= 1; d--)
            {
                out << indent << "  DO i" << d << " = LBOUND(OutData%" << field.name << "," << d << "), UBOUND(OutData%"
                    << field.name << "," << d << ")\n";
                indent += "  "; // create an indent
            }

            if (field.type->simple.maps_to_fortran.compare("REAL(ReKi)") == 0 ||
                field.type->simple.maps_to_fortran.compare("REAL(SiKi)") == 0)
            {
                if (gen_c_code && field.is_pointer)
                {
                    out << indent << "  OutData%" << field_dims << " = REAL(ReKiBuf(Re_Xferred), C_FLOAT)\n";
                }
                else if (field.type->simple.maps_to_fortran.compare("REAL(SiKi)") == 0)
                {
                    out << indent << "  OutData%" << field_dims << " = REAL(ReKiBuf(Re_Xferred), SiKi)\n";
                }
                else
                {
                    out << indent << "  OutData%" << field_dims << " = ReKiBuf(Re_Xferred)\n";
                }
                out << indent << "  Re_Xferred = Re_Xferred + 1\n";
            }
            else if (field.type->simple.maps_to_fortran.compare("REAL(DbKi)") == 0 ||
                     field.type->simple.maps_to_fortran.compare("REAL(R8Ki)") == 0)
            {
                if (gen_c_code && field.is_pointer)
                {
                    out << indent << "  OutData%" << field_dims << " = REAL(DbKiBuf(Db_Xferred), C_DOUBLE)\n";
                }
                else if (field.type->simple.maps_to_fortran.compare("REAL(R8Ki)") == 0)
                {
                    out << indent << "  OutData%" << field_dims << " = REAL(DbKiBuf(Db_Xferred), R8Ki)\n";
                }
                else
                {
                    out << indent << "  OutData%" << field_dims << " = DbKiBuf(Db_Xferred)\n";
                }
                out << indent << "  Db_Xferred = Db_Xferred + 1\n";
            }
            else if (field.type->simple.maps_to_fortran.compare("INTEGER(IntKi)") == 0)
            {
                out << indent << "  OutData%" << field_dims << " = IntKiBuf(Int_Xferred)\n";
                out << indent << "  Int_Xferred = Int_Xferred + 1\n";
            }
            else if (field.type->simple.maps_to_fortran.compare("LOGICAL") == 0)
            {
                out << indent << "  OutData%" << field_dims << " = TRANSFER(IntKiBuf(Int_Xferred), OutData%" << field_dims << ")\n";
                out << indent << "  Int_Xferred = Int_Xferred + 1\n";
            }

            else /*if (field.type->simple.maps_to_fortran("CHARACTER")) */
            {

                out << indent << "  DO I = 1, LEN(OutData%" << field.name << ")\n";
                out << indent << "    OutData%" << field_dims << "(I:I) = CHAR(IntKiBuf(Int_Xferred))\n";
                out << indent << "    Int_Xferred = Int_Xferred + 1\n";
                out << indent << "  END DO ! I\n";
            }

            for (int d = field.dims.size(); d >= 1; d--)
            {
                indent = "  " + mainIndent;
                for (int i = 1; i < d; i++)
                {
                    indent += "  ";
                }
                out << indent << "  END DO\n";
            }

            // need to move scalars and strings to the %c_obj% type, too!
            // compare with copy routine

            if (gen_c_code && !field.is_pointer && field.dims.size() == 0)
            {
                if (field.type->simple.maps_to_fortran.compare("REAL(ReKi)") == 0 ||
                    field.type->simple.maps_to_fortran.compare("REAL(SiKi)") == 0 ||
                    field.type->simple.maps_to_fortran.compare("REAL(DbKi)") == 0 ||
                    field.type->simple.maps_to_fortran.compare("REAL(R8Ki)") == 0 ||
                    field.type->simple.maps_to_fortran.compare("INTEGER(IntKi)") == 0 ||
                    field.type->simple.maps_to_fortran.compare("LOGICAL") == 0)
                {
                    out << "      OutData%C_obj%" << field.name << " = OutData%" << field.name << "\n";
                }
                else
                { // characters need to be copied differently
                    out << "      OutData%C_obj%" << field.name << " = TRANSFER(OutData%" << field.name << ", OutData%C_obj%"
                        << field.name << " )\n";
                }
            }
        }

        if (is_deferred)
        {
            out << "  END IF\n";
        }
    }

    out << " END SUBROUTINE " << routine_name << "\n\n";
    return; //(0) ;
}

// void gen_extint_order(std::ostream &w, const Module &mod, std::string typnm, std::string uy, const int order, const Field &field,
//                       std::string deref, int recurselevel)
// {

//     std::string derefrecurse, indent, tmp;

//     if (recurselevel > MAXRECURSE)
//     {
//         fprintf(stderr, "REGISTRY ERROR: too many levels of array subtypes\n");
//         exit(9);
//     }

//     if (field.type == nullptr)
//         return;

//     auto assoc_alloc = field.is_pointer ? "ASSOCIATED" : "ALLOCATED";

//     // check if this is an allocatable array:
//     if (field.dims.size() > 0 && field.dims.front().is_deferred)
//     {
//         w << "IF (" << assoc_alloc << "(" << uy << "_out" << deref << "%" << field.name << ") .AND. " << assoc_alloc << "(" << uy
//           << "1" << deref << "%" << field.name << ")) THEN\n";
//     }

//     if (field.type->type == DataType::Type::Derived)
//     {
//         auto &ddt = field.type->derived;

//         if (field.type->derived.module->name.compare(mod.name) == 0)
//         {
//             for (auto &r1 : ddt.fields)
//             {
//                 derefrecurse = deref + "%" + field.name;

//                 for (int j = field.dims.size(); j > 0; j--)
//                 {
//                     w << "  DO i%d%d = LBOUND(%s_out%s,%d),UBOUND(%s_out%s,%d)\n", recurselevel, j, uy, derefrecurse, j, uy,
//                         derefrecurse, j;
//                 }

//                 if (field.dims.size() > 0)
//                 {
//                     derefrecurse += "(";
//                     for (int j = 1; j <= field.dims.size(); j++)
//                     {
//                         derefrecurse += "i" + std::to_string(recurselevel) + std::to_string(j);
//                         if (j < field.dims.size())
//                             derefrecurse += ",";
//                     }
//                     derefrecurse += ")";
//                 }

//                 gen_extint_order(w, mod, typnm, uy, order, r1, derefrecurse, recurselevel + 1);
//                 for (int j = field.dims.size(); j > 0; j--)
//                 {
//                     w << "  ENDDO\n";
//                 }
//             }
//         }
//         else
//         {
//             for (int j = field.dims.size(); j > 0; j--)
//             {
//                 w << "  DO i%d = LBOUND(%s_out%s%%%s,%d),UBOUND(%s_out%s%%%s,%d)\n", j, uy, deref, field.name, j, uy, deref,
//                     field.name, j;
//             }

//             if (!strcmp(field.type->mapsto, "MeshType"))
//             {
//                 if (order == 0)
//                 {
//                     w << "      CALL MeshCopy(%s1%s%%%s%s, %s_out%s%%%s%s, MESH_UPDATECOPY, ErrStat2, ErrMsg2 )\n", uy, deref,
//                         field.name, dimstr(field.dims.size()), uy, deref, field.name, dimstr(field.dims.size());
//                 }
//                 else if (order == 1)
//                 {
//                     w << "      CALL MeshExtrapInterp1(%s1%s%%%s%s, %s2%s%%%s%s, tin, %s_out%s%%%s%s, tin_out, ErrStat2, ErrMsg2
//                     )\n",
//                         uy, deref, field.name, dimstr(field.dims.size()), uy, deref, field.name, dimstr(field.dims.size()), uy,
//                         deref, field.name, dimstr(field.dims.size());
//                 }
//                 else if (order == 2)
//                 {
//                     w << "      CALL MeshExtrapInterp2(%s1%s%%%s%s, %s2%s%%%s%s, %s3%s%%%s%s, tin, %s_out%s%%%s%s, tin_out,
//                     ErrStat2, ErrMsg2 )\n",
//                         uy, deref, field.name, dimstr(field.dims.size()), uy, deref, field.name, dimstr(field.dims.size()), uy,
//                         deref, field.name, dimstr(field.dims.size()), uy, deref, field.name, dimstr(field.dims.size());
//                 }
//             }
//             else
//             {
//                 char nonick2[NAMELEN];
//                 remove_nickname(field.type->derived.module->nickname, field.type->name, nonick2);

//                 if (order == 0)
//                 {
//                     w<< "      CALL %s_Copy%s(%s1%s%%%s%s, %s_out%s%%%s%s, MESH_UPDATECOPY, ErrStat2, ErrMsg2 )\n",
//                             field.type->derived.module->nickname, fast_interface_type_shortname(nonick2), uy, deref, field.name,
//                             dimstr(field.dims.size()), uy, deref, field.name, dimstr(field.dims.size()));
//                 }
//                 else if (order == 1)
//                 {
//                     w<<"      CALL %s_%s_ExtrapInterp1( %s1%s%%%s%s, %s2%s%%%s%s, tin, %s_out%s%%%s%s, tin_out, ErrStat2, ErrMsg2
//                     )\n",
//                         field.type->derived.module->nickname, fast_interface_type_shortname(nonick2), uy, deref, field.name,
//                         dimstr(field.dims.size()), uy, deref, field.name, dimstr(field.dims.size()), uy, deref, field.name,
//                         dimstr(field.dims.size()));
//                 }
//                 else if (order == 2)
//                 {
//                     w << "      CALL %s_%s_ExtrapInterp2( %s1%s%%%s%s, %s2%s%%%s%s, %s3%s%%%s%s, tin, %s_out%s%%%s%s, tin_out,
//                     ErrStat2, ErrMsg2 )\n",
//                         field.type->derived.module->nickname, fast_interface_type_shortname(nonick2), uy, deref, field.name,
//                         dimstr(field.dims.size()), uy, deref, field.name, dimstr(field.dims.size()), uy, deref, field.name,
//                         dimstr(field.dims.size()), uy, deref, field.name, dimstr(field.dims.size());
//                 }
//             }

//             w << "        CALL SetErrStat(ErrStat2, ErrMsg2, ErrStat, ErrMsg,RoutineName)\n";

//             for (int j = field.dims.size(); j >= 1; j--)
//             {
//                 w << "   ENDDO\n";
//             }
//         }
//     }
//     else if (!strcmp(field.type->mapsto, "REAL(ReKi)") || !strcmp(field.type->mapsto, "REAL(SiKi)") ||
//              !strcmp(field.type->mapsto, "REAL(R8Ki)") || !strcmp(field.type->mapsto, "REAL(DbKi)"))
//     {
//         if (order == 0)
//         {
//             // bjj: this should probably have some "IF ALLOCATED" statements around it, but we're just calling
//             //  the copy routine
//             fprintf(fp, "  %s_out%s%%%s = %s1%s%%%s\n", uy, deref, field.name, uy, deref, field.name);
//         }
//         else
//             strcpy(indent, "");
//         for (j = field.dims.size(); j > 0; j--)
//         {
//             fprintf(fp, "%s  DO i%d = LBOUND(%s_out%s%%%s,%d),UBOUND(%s_out%s%%%s,%d)\n", indent, j, uy, deref, field.name, j,
//             uy,
//                     deref, field.name, j);
//             strcat(indent, "  "); // create an indent
//         }

//         if (order == 1)
//         {
//             if (field.gen_periodic)
//             {
//                 fprintf(fp, "%s  CALL Angles_ExtrapInterp( %s1%s%%%s%s, %s2%s%%%s%s, tin, %s_out%s%%%s%s, tin_out )\n", indent,
//                 uy,
//                         deref, field.name, dimstr(field.dims.size()), uy, deref, field.name, dimstr(field.dims.size()), uy,
//                         deref, field.name, dimstr(field.dims.size()));
//             }
//             else
//             {
//                 fprintf(fp, "%s  b = -(%s1%s%%%s%s - %s2%s%%%s%s)\n", indent, uy, deref, field.name, dimstr(field.dims.size()),
//                 uy,
//                         deref, field.name, dimstr(field.dims.size()));
//                 fprintf(fp, "%s  %s_out%s%%%s%s = %s1%s%%%s%s + b * ScaleFactor\n", indent, uy, deref, field.name,
//                         dimstr(field.dims.size()), uy, deref, field.name, dimstr(field.dims.size()));
//             };
//         }
//         if (order == 2)
//         {
//             if (field.gen_periodic)
//             {
//                 fprintf(fp, "%s  CALL Angles_ExtrapInterp( %s1%s%%%s%s, %s2%s%%%s%s, %s3%s%%%s%s, tin, %s_out%s%%%s%s, tin_out
//                 )\n",
//                         indent, uy, deref, field.name, dimstr(field.dims.size()), uy, deref, field.name,
//                         dimstr(field.dims.size()), uy, deref, field.name, dimstr(field.dims.size()), uy, deref, field.name,
//                         dimstr(field.dims.size()));
//             }
//             else
//             {
//                 fprintf(fp, "%s  b = (t(3)**2*(%s1%s%%%s%s - %s2%s%%%s%s) + t(2)**2*(-%s1%s%%%s%s + %s3%s%%%s%s))*
//                 scaleFactor\n",
//                         indent, uy, deref, field.name, dimstr(field.dims.size()), uy, deref, field.name,
//                         dimstr(field.dims.size()), uy, deref, field.name, dimstr(field.dims.size()), uy, deref, field.name,
//                         dimstr(field.dims.size()));
//                 fprintf(fp, "%s  c = ( (t(2)-t(3))*%s1%s%%%s%s + t(3)*%s2%s%%%s%s - t(2)*%s3%s%%%s%s ) * scaleFactor\n", indent,
//                 uy,
//                         deref, field.name, dimstr(field.dims.size()), uy, deref, field.name, dimstr(field.dims.size()), uy,
//                         deref, field.name, dimstr(field.dims.size()));
//                 fprintf(fp, "%s  %s_out%s%%%s%s = %s1%s%%%s%s + b  + c * t_out\n", indent, uy, deref, field.name,
//                         dimstr(field.dims.size()), uy, deref, field.name, dimstr(field.dims.size()));
//             }
//         }
//         for (j = field.dims.size(); j >= 1; j--)
//         {
//             strcpy(indent, "");
//             for (i = 1; i < j; i++)
//             {
//                 strcat(indent, "  ");
//             }
//             fprintf(fp, "%s  END DO\n", indent);
//         }
//     }
//     // check if this is an allocatable array:
//     if (field.dims.size() > 0 && field.dims.front().is_deferred)
//     {
//         fprintf(fp, "END IF ! check if allocated\n");
//     }
// }

// void calc_extint_order(std::ostream &w, const Module &mod, const Field &field, int recurselevel, int &max_ndims, int
// &max_nrecurs,
//                        int &max_alloc_ndims)
// {
//     // bjj: make sure this is consistent with logic of gen_extint_order

//     if (field.type != NULL)
//     {

//         if (field.type->type == DataType::Type::Derived)
//         {
//             if ((q = get_entry(make_lower_temp(field.type->name), mod.module_ddt_list)) != NULL)
//             {
//                 for (r1 = ddt.fields; r1; r1 = r1->next)
//                 {
//                     if (field.dims.size() > 0)
//                     {
//                         if (recurselevel > max_nrecurs)
//                             max_nrecurs = recurselevel;
//                         if (field.dims.size() > max_ndims)
//                             max_ndims = field.dims.size();
//                     }
//                     calc_extint_order(fp, mod, r1, recurselevel + 1, max_ndims, max_nrecurs, max_alloc_ndims);
//                 }
//             }
//             else if (!strcmp(field.type->mapsto, "MeshType"))
//             {
//                 if (field.dims.size() > 0)
//                 {
//                     if (field.dims.size() > max_ndims)
//                         max_ndims = field.dims.size();
//                 }
//             }
//             else
//             {
//                 if (field.dims.size() >= 1)
//                 {
//                     if (field.dims.size() > max_ndims)
//                         max_ndims = field.dims.size();
//                 }
//             }
//         }
//         else if (!strcmp(field.type->mapsto, "REAL(ReKi)") || !strcmp(field.type->mapsto, "REAL(SiKi)") ||
//                  !strcmp(field.type->mapsto, "REAL(R8Ki)") || !strcmp(field.type->mapsto, "REAL(DbKi)"))
//         {
//             if (/*order > 0 &&*/ field.dims.size() > max_alloc_ndims)
//                 max_alloc_ndims = field.dims.size();

//             if (field.dims.size() > max_ndims)
//                 max_ndims = field.dims.size();
//         }
//     }

//     if (recurselevel > MAXRECURSE)
//     {
//         fprintf(stderr, "REGISTRY ERROR: too many levels of array subtypes\n");
//         exit(9);
//     }
// }

void gen_ExtrapInterp1(std::ostream &w, const Module &mod, std::string &typnm, std::string &typnmlong, std::string &xtypnm,
                       std::string &uy, std::string &modPrefix, const int max_ndims, const int max_nrecurs,
                       const int max_alloc_ndims, const DataType::Derived &ddt)
{
    w << "\n";
    w << " SUBROUTINE " << mod.nickname << "_" << typnm << "_ExtrapInterp1(" << uy << "1, " << uy << "2, tin, " << uy
      << "_out, tin_out, ErrStat, ErrMsg )\n";
    w << "!\n";
    w << "! This subroutine calculates a extrapolated (or interpolated) " << typnm << " " << uy
      << "_out at time t_out, from previous/future time\n";
    w << "! values of " << uy << " (which has values associated with times in t).  Order of the interpolation is 1.\n";
    w << "!\n";
    w << "!  f(t) = a + b * t, or\n";
    w << "!\n";
    w << "!  where a and b are determined as the solution to\n";
    w << "!  f(t1) = " << uy << "1, f(t2) = " << uy << "2\n";
    w << "!\n";
    w << "!" << std::string(130, '.') << "\n";
    w << "\n";

    w << " TYPE(" << modPrefix << typnmlong << "), INTENT(" << (ddt.contains_pointer == 1 ? "INOUT" : "IN") << ")  :: " << uy
      << "1    ! " << typnm << " at t1 > t2\n";
    w << " TYPE(" << modPrefix << typnmlong << "), INTENT(" << (ddt.contains_pointer == 1 ? "INOUT" : "IN") << ")  :: " << uy
      << "2    ! " << typnm << " at t2 \n";
    w << " REAL(" << xtypnm << "),         INTENT(IN   )          :: tin(2)   ! Times associated with the " << typnm << "s\n";
    w << " TYPE(" << modPrefix << typnmlong << "), INTENT(INOUT)  :: " << uy << "_out ! " << typnm << " at tin_out\n";
    w << " REAL(" << xtypnm << "),         INTENT(IN   )          :: tin_out  ! time to be extrap/interp'd to\n";
    w << " INTEGER(IntKi),     INTENT(  OUT)          :: ErrStat  ! Error status of the operation\n";
    w << " CHARACTER(*),       INTENT(  OUT)          :: ErrMsg   ! Error message if ErrStat /= ErrID_None\n";
    w << "   ! local variables\n";
    w << " REAL(" << xtypnm << ")                                 :: t(2)     ! Times associated with the " << typnm << "s\n";
    w << " REAL(" << xtypnm << ")                                 :: t_out    ! Time to which to be extrap/interpd\n";
    w << " CHARACTER(*),                    PARAMETER :: RoutineName = '" << mod.nickname << "_" << typnm << "_ExtrapInterp1'\n";

    w << " REAL(DbKi)                                 :: b        ! temporary for extrapolation/interpolation\n";
    w << " REAL(DbKi)                                 :: ScaleFactor ! temporary for extrapolation/interpolation\n";
    w << " INTEGER(IntKi)                             :: ErrStat2 ! local errors\n";
    w << " CHARACTER(ErrMsgLen)                       :: ErrMsg2  ! local errors\n";
    for (int j = 1; j <= max_ndims; j++)
    {
        for (int i = 0; i <= max_nrecurs; i++)
        {
            w << " INTEGER                                    :: i" << i << j << "    ! dim" << j << " level " << i
              << " counter variable for arrays of ddts\n";
        }
    }
    for (int j = 1; j <= max_ndims; j++)
    {
        w << " INTEGER                                    :: i" << j << "    ! dim" << j << " counter variable for arrays\n";
    }
    w << "    ! Initialize ErrStat\n";
    w << " ErrStat = ErrID_None\n";
    w << " ErrMsg  = \"\"\n";
    w << "    ! we'll subtract a constant from the times to resolve some \n";
    w << "    ! numerical issues when t gets large (and to simplify the equations)\n";
    w << " t = tin - tin(1)\n";
    w << " t_out = tin_out - tin(1)\n";
    w << "\n";

    w << "   IF ( EqualRealNos( t(1), t(2) ) ) THEN\n";
    w << "     CALL SetErrStat(ErrID_Fatal, 't(1) must not equal t(2) to avoid a division-by-zero error.', ErrStat, ErrMsg,RoutineName)\n";
    w << "     RETURN\n";
    w << "   END IF\n\n";

    w << "   ScaleFactor = t_out / t(2)\n";

    for (const auto &field : ddt.fields)
    {
        // recursive
        // gen_extint_order(w, mod, typnm, uy, 1, field, "", 0);
    }

    w << " END SUBROUTINE " << mod.nickname << "_" << typnm << "_ExtrapInterp1\n";
    w << "\n";
}

void gen_ExtrapInterp2(std::ostream &w, const Module &mod, std::string &typnm, std::string &typnmlong, std::string &xtypnm,
                       std::string &uy, std::string &modPrefix, const int max_ndims, const int max_nrecurs,
                       const int max_alloc_ndims, const DataType::Derived &ddt)
{
    w << "\n";
    w << " SUBROUTINE " << mod.nickname << "_" << typnm << "_ExtrapInterp2(" << uy << "1, " << uy << "2, " << uy << "3, tin, " << uy
      << "_out, tin_out, ErrStat, ErrMsg )\n";
    w << "!\n";
    w << "! This subroutine calculates a extrapolated (or interpolated) " << typnm << " " << uy
      << "_out at time t_out, from previous/future time\n";
    w << "! values of " << uy << " (which has values associated with times in t).  Order of the interpolation is 2.\n";
    w << "!\n";
    w << "!  expressions below based on either\n";
    w << "!\n";
    w << "!  f(t) = a + b * t + c * t**2\n";
    w << "!\n";
    w << "!  where a, b and c are determined as the solution to\n";
    w << "!  f(t1) = " << uy << "1, f(t2) = " << uy << "2, f(t3) = " << uy << "3\n";
    w << "!\n";
    w << "!" << std::string(130, '.') << "\n";
    w << "\n";

    w << " TYPE(" << modPrefix << typnmlong << "), INTENT(" << (ddt.contains_pointer == 1 ? "INOUT" : "IN") << ")  :: " << uy
      << "1      ! " << typnm << " at t1 > t2 > t3\n";
    w << " TYPE(" << modPrefix << typnmlong << "), INTENT(" << (ddt.contains_pointer == 1 ? "INOUT" : "IN") << ")  :: " << uy
      << "2      ! " << typnm << " at t2 > t3\n";
    w << " TYPE(" << modPrefix << typnmlong << "), INTENT(" << (ddt.contains_pointer == 1 ? "INOUT" : "IN") << ")  :: " << uy
      << "3      ! " << typnm << " at t3\n";
    w << " REAL(" << xtypnm << "),                 INTENT(IN   )  :: tin(3)    ! Times associated with the " << typnm << "s\n";
    w << " TYPE(" << modPrefix << typnmlong << "), INTENT(INOUT)  :: " << uy << "_out     ! " << typnm << " at tin_out\n";
    w << " REAL(" << xtypnm << "),                 INTENT(IN   )  :: tin_out   ! time to be extrap/interp'd to\n";

    w << " INTEGER(IntKi),             INTENT(  OUT)  :: ErrStat   ! Error status of the operation\n";
    w << " CHARACTER(*),               INTENT(  OUT)  :: ErrMsg    ! Error message if ErrStat /= ErrID_None\n";
    w << "   ! local variables\n";
    w << " REAL(" << xtypnm << ")                                 :: t(3)      ! Times associated with the " << typnm << "s\n";
    w << " REAL(" << xtypnm << ")                                 :: t_out     ! Time to which to be extrap/interpd\n";
    w << " INTEGER(IntKi)                             :: order     ! order of polynomial fit (max 2)\n";

    w << " REAL(DbKi)                                 :: b        ! temporary for extrapolation/interpolation\n";
    w << " REAL(DbKi)                                 :: c        ! temporary for extrapolation/interpolation\n";
    w << " REAL(DbKi)                                 :: ScaleFactor ! temporary for extrapolation/interpolation\n";
    w << " INTEGER(IntKi)                             :: ErrStat2 ! local errors\n";
    w << " CHARACTER(ErrMsgLen)                       :: ErrMsg2  ! local errors\n";
    w << " CHARACTER(*),            PARAMETER         :: RoutineName = '" << mod.nickname << "_" << typnm << "_ExtrapInterp2'\n";
    for (int j = 1; j <= max_ndims; j++)
    {
        for (int i = 0; i <= max_nrecurs; i++)
        {
            w << " INTEGER                                    :: i" << i << j << "    ! dim" << j << " level " << i
              << " counter variable for arrays of ddts\n";
        }
    }
    for (int j = 1; j <= max_ndims; j++)
    {
        w << " INTEGER                                    :: i" << j << "    ! dim" << j << " counter variable for arrays\n";
    }
    w << "    ! Initialize ErrStat\n";
    w << " ErrStat = ErrID_None\n";
    w << " ErrMsg  = \"\"\n";
    w << "    ! we'll subtract a constant from the times to resolve some \n";
    w << "    ! numerical issues when t gets large (and to simplify the equations)\n";
    w << " t = tin - tin(1)\n";
    w << " t_out = tin_out - tin(1)\n";
    w << "\n";

    w << "   IF ( EqualRealNos( t(1), t(2) ) ) THEN\n";
    w << "     CALL SetErrStat(ErrID_Fatal, 't(1) must not equal t(2) to avoid a division-by-zero error.', ErrStat, ErrMsg,RoutineName)\n";
    w << "     RETURN\n";
    w << "   ELSE IF ( EqualRealNos( t(2), t(3) ) ) THEN\n";
    w << "     CALL SetErrStat(ErrID_Fatal, 't(2) must not equal t(3) to avoid a division-by-zero error.', ErrStat, ErrMsg,RoutineName)\n";
    w << "     RETURN\n";
    w << "   ELSE IF ( EqualRealNos( t(1), t(3) ) ) THEN\n";
    w << "     CALL SetErrStat(ErrID_Fatal, 't(1) must not equal t(3) to avoid a division-by-zero error.', ErrStat, ErrMsg,RoutineName)\n";
    w << "     RETURN\n";
    w << "   END IF\n\n";

    w << "   ScaleFactor = t_out / (t(2) * t(3) * (t(2) - t(3)))\n";

    // recursive
    for (const auto &field : ddt.fields)
    {
        // gen_extint_order(w, mod, typnm, uy, 2, field, "", 0);
    }

    w << " END SUBROUTINE " << mod.nickname << "_" << typnm << "_ExtrapInterp2\n";
    w << "\n";
}

void gen_ExtrapInterp(std::ostream &w, const Module &mod, std::string typnm, std::string typnmlong, std::string xtypnm,
                      const bool useModPrefix)
{
    int max_ndims, max_nrecurs, max_alloc_ndims;

    std::string uy = tolower(typnm).compare("output") == 0 ? "y" : "u";

    std::string modPrefix = useModPrefix ? mod.nickname + "_" : "";

    for (const auto &ddt_name : mod.data_type_order)
    {
        const auto &ddt = mod.data_types.find(ddt_name)->second->derived;

        if (tolower(ddt.name).compare(tolower(typnmlong)) != 0)
            continue;

        w << "\n";
        w << " SUBROUTINE " << mod.nickname << "_" << typnm << "_ExtrapInterp(" << uy << ", t, " << uy
          << "_out, t_out, ErrStat, ErrMsg )\n";
        w << "!\n";
        w << "! This subroutine calculates a extrapolated (or interpolated) " << typnm << " " << uy
          << "_out at time t_out, from previous/future time\n";
        w << "! values of " << uy
          << " (which has values associated with times in t).  Order of the interpolation is given by the size of " << uy << "\n";
        w << "!\n";
        w << "!  expressions below based on either\n";
        w << "!\n";
        w << "!  f(t) = a\n";
        w << "!  f(t) = a + b * t, or\n";
        w << "!  f(t) = a + b * t + c * t**2\n";
        w << "!\n";
        w << "!  where a, b and c are determined as the solution to\n";
        w << "!  f(t1) = " << uy << "1, f(t2) = " << uy << "2, f(t3) = " << uy << "3  (as appropriate)\n";
        w << "!\n";
        w << "!" << std::string(130, '.') << "\n";
        w << "\n";

        w << " TYPE(" << modPrefix << typnmlong << "), INTENT(" << (ddt.contains_pointer == 1 ? "INOUT" : "IN") << ")  :: " << uy
          << "(:) ! " << typnm << " at t1 > t2 > t3\n";
        w << " REAL(" << xtypnm << "),                 INTENT(IN   )  :: t(:)           ! Times associated with the " << typnm
          << "s\n";
        // jm Modified from INTENT(  OUT) to INTENT(INOUT) to prevent ALLOCATABLE array arguments in the DDT
        // jm from being maliciously deallocated through the call.See Sec. 5.1.2.7 of bonehead Fortran 2003 standard
        w << " TYPE(" << modPrefix << typnmlong << "), INTENT(INOUT)  :: " << uy << "_out ! " << typnm << " at tin_out\n";
        w << " REAL(" << xtypnm << "),                 INTENT(IN   )  :: t_out           ! time to be extrap/interp'd to\n";
        w << " INTEGER(IntKi),             INTENT(  OUT)  :: ErrStat         ! Error status of the operation\n";
        w << " CHARACTER(*),               INTENT(  OUT)  :: ErrMsg          ! Error message if ErrStat /= ErrID_None\n";
        w << "   ! local variables\n";
        w << " INTEGER(IntKi)                             :: order           ! order of polynomial fit (max 2)\n";
        w << " INTEGER(IntKi)                             :: ErrStat2        ! local errors\n";
        w << " CHARACTER(ErrMsgLen)                       :: ErrMsg2         ! local errors\n";
        w << " CHARACTER(*),    PARAMETER                 :: RoutineName = '" << mod.nickname << "_" << typnm << "_ExtrapInterp'\n";
        w << "    ! Initialize ErrStat\n";
        w << " ErrStat = ErrID_None\n";
        w << " ErrMsg  = \"\"\n";
        w << " if ( size(t) .ne. size(" << uy << ")) then\n";
        w << "    CALL SetErrStat(ErrID_Fatal,'size(t) must equal size(" << uy << ")',ErrStat,ErrMsg,RoutineName)\n";
        w << "    RETURN\n";
        w << " endif\n";

        w << " order = SIZE(" << uy << ") - 1\n";

        w << " IF ( order .eq. 0 ) THEN\n";
        w << "   CALL " << mod.nickname << "_Copy" << typnm << "(" << uy << "(1), " << uy
          << "_out, MESH_UPDATECOPY, ErrStat2, ErrMsg2 )\n";
        w << "     CALL SetErrStat(ErrStat2, ErrMsg2, ErrStat, ErrMsg,RoutineName)\n";
        w << " ELSE IF ( order .eq. 1 ) THEN\n";
        w << "   CALL " << mod.nickname << "_" << typnm << "_ExtrapInterp1(" << uy << "(1), " << uy << "(2), t, " << uy
          << "_out, t_out, ErrStat2, ErrMsg2 )\n";
        w << "     CALL SetErrStat(ErrStat2, ErrMsg2, ErrStat, ErrMsg,RoutineName)\n";
        w << " ELSE IF ( order .eq. 2 ) THEN\n";
        w << "   CALL " << mod.nickname << "_" << typnm << "_ExtrapInterp2(" << uy << "(1), " << uy << "(2), " << uy << "(3), t, "
          << uy << "_out, t_out, ErrStat2, ErrMsg2 )\n";
        w << "     CALL SetErrStat(ErrStat2, ErrMsg2, ErrStat, ErrMsg,RoutineName)\n";
        w << " ELSE \n";
        w << "   CALL SetErrStat(ErrID_Fatal,'size(" << uy
          << ") must be less than 4 (order must be less than 3).',ErrStat,ErrMsg,RoutineName)\n";
        w << "   RETURN\n";
        w << " ENDIF \n";

        w << " END SUBROUTINE " << mod.nickname << "_" << typnm << "_ExtrapInterp\n";
        w << "\n";

        max_ndims = 0;   // mod.module_ddt_list->max_ndims; //bjj: this is max for module, not for typnmlong
        max_nrecurs = 0; // MAXRECURSE;
        max_alloc_ndims = 0;

        // recursive
        for (const auto &field : ddt.fields)
        {
            // calc_extint_order(w, mod, field, 0, max_ndims, max_nrecurs, max_alloc_ndims);
        }

        gen_ExtrapInterp1(w, mod, typnm, typnmlong, xtypnm, uy, modPrefix, max_ndims, max_nrecurs, max_alloc_ndims, ddt);
        gen_ExtrapInterp2(w, mod, typnm, typnmlong, xtypnm, uy, modPrefix, max_ndims, max_nrecurs, max_alloc_ndims, ddt);
    }
}
