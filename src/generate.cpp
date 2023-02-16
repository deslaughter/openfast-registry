#include "registry.hpp"
#include "templates.hpp"

void Module::gen_fortran_module(std::ofstream &out)
{
    out << FAST_preamble;

    for (p = ModNames; p; p = p->next)
    {
        // Add use declarations for Modules that are included as "usefrom"
        if (p->usefrom == 1)
        {
            if (strcmp(make_lower_temp(p->name), "nwtc_library"))
            {
                fprintf(fp, "USE %s_Types\n", p->name);
            }
        }
    }

    if (sw_ccode)
    {
        // Generate a container object for the Fortran code to carry around a pointer
        // to the CPP object(s)
        // fprintf(fp,"USE %s_C_Types\n",ModName->nickname) ;
        fprintf(
            fp,
            "!USE, INTRINSIC :: ISO_C_Binding\n"); // this is inherited from
                                                   // NWTC_Library.f90, and older
                                                   // versions of gfortran complain
                                                   // about ambiguous data when we use
                                                   // this (it thinks it's declared
                                                   // twice; see
                                                   // http://gcc.gnu.org/ml/fortran/2013-04/msg00166.html
                                                   // )
    }

    // if this is the NWTC Library, we're not going to print "USE NWTC_Library"
    if (strcmp(make_lower_temp(ModName->name), "nwtc_library") == 0)
    {
        out << "USE SysSubs\n";
    }
    else
    {
        out << "USE NWTC_Library\n";
    }

    out << "IMPLICIT NONE\n";

    // generate parameters
    for (q = ModName->params; q; q = q->next)
    {
        out << "    " << << ", PUBLIC, PARAMETER ", q->type->mapsto;
        if (q->ndims > 0)
        {
            if (q->dims[0]->deferred)
            {
                fprintf(stderr,
                        "Registry warning: parameter %s can not have deferred type\n",
                        q->name);
                fprintf(fp, "), ALLOCATABLE ");
            }
            else
            {
                fprintf(fp, ", DIMENSION(");
                for (i = 0; i < q->ndims; i++)
                {
                    fprintf(fp, "%d:%d", q->dims[i]->coord_start, q->dims[i]->coord_end);
                    if (i < q->ndims - 1)
                        fprintf(fp, ",");
                }
                fprintf(fp, ") ");
            }
        }
        if (strlen(q->inival) > 0)
        {
            if (q->ndims > 0)
            {
                fprintf(fp, " :: %s = (/%s/)", q->name, q->inival);
            }
            else
            {
                fprintf(fp, " :: %s = %s ", q->name, q->inival);
            }
        }
        else
        {
            fprintf(fp, " :: %s", q->name);
        }
        if (strcmp(q->descrip, "-") ||
            strcmp(q->units, "-")) /* that is, if not equal "-" */
        {
            fprintf(fp, "     ! %s [%s]", q->descrip, q->units);
        }
        fprintf(fp, "\n");
    }

    // generate each derived data type
    for (q = ModName->module_ddt_list; q; q = q->next)
    {
        if (*q->mapsto)
            remove_nickname(ModName->nickname, make_lower_temp(q->mapsto), nonick);
        fprintf(fp, "! =========  %s%s  =======\n", q->mapsto, (sw_ccode) ? "_C" : "");
        for (ipass = (sw_ccode) ? 0 : 1; ipass < 2; ipass++)
        { // 2 passes for C code, 1st pass generates bound ddt
            if (q->usefrom == 0)
            {
                fprintf(fp, "  TYPE, %s :: %s%s\n", (ipass == 0) ? "BIND(C)" : "PUBLIC",
                        q->mapsto, (ipass == 0) ? "_C" : "");
                if (sw_ccode)
                {
                    if (ipass == 0)
                    {
                        //            q->containsPtr = 1;
                        // if (!strcmp(make_lower_temp(nonick), "otherstatetype") ||
                        // !strcmp(make_lower_temp(nonick), "initinputtype")){
                        fprintf(fp, "   TYPE(C_PTR) :: object = C_NULL_PTR\n");
                        //}
                    }
                    else
                    {
                        fprintf(fp, "    TYPE( %s_C ) :: C_obj\n", q->mapsto);
                    }
                }
                for (r = q->fields; r; r = r->next)
                {
                    if (r->type != NULL)
                    {
                        // check max number of dimmensions
                        // check if this type contains any pointers/meshes or types
                        // that have pointers/meshes
                        if (r->ndims > q->max_ndims)
                            q->max_ndims = r->ndims;
                        if (r->ndims > ModName->module_ddt_list->max_ndims)
                            ModName->module_ddt_list->max_ndims = r->ndims;
                        if (ipass == 0)
                        {
                            // r->containsPtr = 1;
                            // q->containsPtr = 1;
                            if (r->ndims == 0 && r->type->type_type != DERIVED)
                            {
                                fprintf(fp, "    %s :: %s \n",
                                        c_types_binding(r->type->mapsto), r->name);
                            }
                            else if (r->ndims > 0 && r->type->type_type != DERIVED)
                            {
                                if (r->dims[0]->deferred)
                                {
                                    fprintf(fp, "    TYPE(C_ptr) :: %s = C_NULL_PTR \n",
                                            r->name);
                                    fprintf(fp, "    INTEGER(C_int) :: %s_Len = 0 \n",
                                            r->name);
                                }
                                else
                                {
                                    if (strcmp(C_type(r->type->mapsto), "char"))
                                    {
                                        fprintf(fp, "    TYPE(C_PTR) :: %s(", r->name);
                                        for (i = 0; i < r->ndims; i++)
                                        {
                                            fprintf(fp, "%d", r->dims[i]->coord_end);
                                            if (i < r->ndims - 1)
                                                fprintf(fp, ",");
                                        }
                                        fprintf(fp, ")\n");
                                    }
                                }
                            }
                        }
                        else
                        { // ipass /= 0
                            if (r->type->type_type == DERIVED)
                            {
                                fprintf(fp, "    TYPE(%s) ", r->type->mapsto);

                                checkContainsMesh(r);
                                if (r->containsPtr)
                                    q->containsPtr = 1;

                                // bjj: we need to make sure these types map to reals,
                                // too
                                tmp[0] = '\0';
                                if (*q->mapsto)
                                    remove_nickname(ModName->nickname,
                                                    make_lower_temp(q->mapsto), tmp);
                                if (must_have_real_or_double(tmp))
                                    checkOnlyReals(q->mapsto, r);
                            }
                            else
                            {
                                tmp[0] = '\0';
                                if (*q->mapsto)
                                    remove_nickname(ModName->nickname,
                                                    make_lower_temp(q->mapsto), tmp);
                                if (must_have_real_or_double(tmp))
                                {
                                    if (strncmp(r->type->mapsto, "REAL", 4))
                                    {
                                        fprintf(stderr,
                                                "Registry warning: %s contains a "
                                                "field (%s) whose type is not real "
                                                "or double: %s\n",
                                                q->mapsto, r->name, r->type->mapsto);
                                    }
                                }
                                if (sw_ccode && is_pointer(r))
                                {
                                    fprintf(fp, "    %s ",
                                            c_types_binding(r->type->mapsto));
                                }
                                else
                                {
                                    fprintf(fp, "    %s ", r->type->mapsto);
                                }
                            }

                            if (r->ndims > 0)
                            {
                                if (r->dims[0]
                                        ->deferred) // if one dim is deferred they all
                                                    // have to be; see check in type.c
                                {
                                    fprintf(fp, ", DIMENSION(");
                                    for (i = 0; i < r->ndims; i++)
                                    {
                                        fprintf(fp, ":");
                                        if (i < r->ndims - 1)
                                            fprintf(fp, ",");
                                    }
                                    if (is_pointer(r))
                                    {
                                        fprintf(fp, "), POINTER ");
                                    }
                                    else
                                    {
                                        fprintf(fp, "), ALLOCATABLE ");
                                    }
                                }
                                else
                                {
                                    fprintf(fp, ", DIMENSION(");
                                    for (i = 0; i < r->ndims; i++)
                                    {
                                        if (r->dims[i]->dim_param == 0)
                                        {
                                            fprintf(fp, "%d:%d", r->dims[i]->coord_start,
                                                    r->dims[i]->coord_end);
                                        }
                                        else
                                        {
                                            // fprintf(stderr, "start, %s, %s, %s\n",
                                            // dimspec, dim_entry->name,
                                            // dim_entry->module);
                                            // if (r->module != NULL) { node_t
                                            // *param_dim =
                                            // get_entry(r->dims[i]->dim_param_name,
                                            // r->module->params); }

                                            fprintf(fp, "%s", r->dims[i]->dim_param_name);
                                        }
                                        if (i < r->ndims - 1)
                                            fprintf(fp, ",");
                                    }
                                    fprintf(fp, ") ");
                                }
                            }

                            if (is_pointer(r))
                            {
                                fprintf(fp, " :: %s => NULL() ", r->name);
                            }
                            else if (r->ndims == 0 && strlen(r->inival) > 0)
                            {
                                fprintf(fp, " :: %s = %s ", r->name, r->inival);
                            }
                            else
                            {
                                fprintf(fp, " :: %s ", r->name);
                            }

                            if (strcmp(r->descrip, "-") ||
                                strcmp(r->units, "-")) /* that is, if not equal "-" */
                            {
                                fprintf(fp, "     !< %s [%s]", r->descrip, r->units);
                            }
                            fprintf(fp, "\n");
                        } // ipass /= 0
                    }
                }
                fprintf(fp, "  END TYPE %s%s\n", q->mapsto, (ipass == 0) ? "_C" : "");
                // fprintf(stderr, "module %d type %d\n",
                // ModName->module_ddt_list->max_ndims, q->max_ndims);
            }
        }
        fprintf(fp, "! =======================\n");
    }

    if (sw_ccode)
    {
        for (q = ModName->module_ddt_list; q; q = q->next)
        {

            if (q->usefrom == 0)
            {

                char *ddtname, *ddtnamelong, nonick[NAMELEN];
                ddtname = q->name;

                remove_nickname(ModName->nickname, ddtname, nonick);

                if (is_a_fast_interface_type(nonick))
                {
                    ddtnamelong = nonick;
                    ddtname = fast_interface_type_shortname(nonick);
                }
                else
                {
                    ddtnamelong = ddtname;
                }
            }
        }
    } // sw_ccode

    fprintf(fp, "CONTAINS\n");
    for (q = ModName->module_ddt_list; q; q = q->next)
    {
        if (q->usefrom == 0)
        {

            char *ddtname, *ddtnamelong, nonick[NAMELEN];
            // ddtname = q->name ;
            ddtname = q->mapsto;

            remove_nickname(ModName->nickname, ddtname, nonick);

            // fprintf(stderr,">> %s %s %s \n",ModName->name, ddtname, nonick) ;

            if (is_a_fast_interface_type(nonick))
            {
                ddtnamelong = nonick;
                ddtname = fast_interface_type_shortname(nonick);
            }
            else
            {
                ddtnamelong = ddtname;
            }

            gen_copy(fp, ModName, ddtname, ddtnamelong, q);
            gen_destroy(fp, ModName, ddtname, ddtnamelong);
            gen_pack(fp, ModName, ddtname, ddtnamelong);
            gen_unpack(fp, ModName, ddtname, ddtnamelong);
            if (sw_ccode)
            {
                gen_copy_c2f(fp, ModName, ddtname, ddtnamelong);
                gen_copy_f2c(fp, ModName, ddtname, ddtnamelong);
            }
        }
    }
    // bjj: removed gen_modname_pack and gen_modname_unpack because i don't see them
    // being used any differently than the other pack/unpack routines 02/22/2014
    //    gen_modname_pack( fp, ModName ) ;
    //    gen_modname_unpack( fp, ModName ) ;
    //    gen_rk4( fp, ModName ) ;

    if (strcmp(make_lower_temp(ModName->name), "airfoilinfo") == 0)
    { // make interpolation routines for AirfoilInfo module
        gen_ExtrapInterp(fp, ModName, "Output", "OutputType", "ReKi", 1);
        gen_ExtrapInterp(fp, ModName, "UA_BL_Type", "UA_BL_Type", "ReKi", 1);
    }
    else if (!sw_noextrap)
    {
        if (strcmp(make_lower_temp(ModName->name), "dbemt") == 0)
        { // make interpolation routines for element-level DBEMT module
            gen_ExtrapInterp(fp, ModName, "ElementInputType", "ElementInputType", "DbKi",
                             1);
        }
        //        else if (strcmp(make_lower_temp(ModName->name), "bemt") == 0) {
        //            gen_ExtrapInterp(fp, ModName, "SkewWake_InputType",
        //            "SkewWake_InputType", "DbKi",1);
        //        }
        //        else if (strcmp(make_lower_temp(ModName->name), "aerodyn") == 0) {
        //            gen_ExtrapInterp(fp, ModName, "RotInputType", "RotInputType",
        //            "DbKi",0); // don't append "AD_" to the type name!
        //        }

        gen_ExtrapInterp(fp, ModName, "Input", "InputType", "DbKi", 1);
        gen_ExtrapInterp(fp, ModName, "Output", "OutputType", "DbKi", 1);
    }

    fprintf(fp, "END MODULE %s_Types\n", ModName->name);
}