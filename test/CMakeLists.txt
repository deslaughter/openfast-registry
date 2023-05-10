
set(TEST_REF_DIR ${CMAKE_CURRENT_SOURCE_DIR}/reference)
set(TEST_INP_DIR ${CMAKE_CURRENT_SOURCE_DIR}/input)
set(TEST_OUT_DIR ${CMAKE_CURRENT_BINARY_DIR}/output)

add_custom_target(build-test-dir ALL
    COMMAND ${CMAKE_COMMAND} -E make_directory ${TEST_OUT_DIR})

set(CMAKE_Fortran_MODULE_DIRECTORY ${CMAKE_BINARY_DIR}/ftnmods)

function(add_reg_test reg_file)
  string(REPLACE ".txt" "" test_name ${reg_file})
  string(REPLACE "_Registry" "" test_name ${test_name})
  add_test(NAME ${test_name} COMMAND openfast_registry_cpp -O ${TEST_OUT_DIR} -I ${TEST_INP_DIR}
            ${TEST_INP_DIR}/${reg_file} ${ARGN})
endfunction()

add_reg_test(AeroAcoustics_registry.txt -noextrap)
add_reg_test(aerodyn_driver_registry.txt -noextrap)
add_reg_test(aerodyn_inflow_registry.txt -noextrap)
add_reg_test(aerodyn_registry.txt)
add_reg_test(airfoilinfo_registry.txt -noextrap)
add_reg_test(awae_registry.txt -noextrap)
add_reg_test(bemt_registry.txt)
add_reg_test(conv_radiation.txt)
add_reg_test(current.txt)
add_reg_test(dbemt_registry.txt)
add_reg_test(elastodyn_registry.txt)
add_reg_test(extptfm_mckf_registry.txt)
add_reg_test(fast_farm_registry.txt -noextrap)
add_reg_test(fast_registry.txt -noextrap)
add_reg_test(fastwrapper_registry.txt -noextrap)
add_reg_test(feam_registry.txt)
add_reg_test(fvw_registry.txt)
add_reg_test(hydrodyn.txt)
add_reg_test(icefloe_fastregistry.inp)
add_reg_test(ifw_flowfield.txt -noextrap)
add_reg_test(inflowwind_io.txt -noextrap)
add_reg_test(inflowwind.txt)
add_reg_test(lidar.txt)
add_reg_test(map_fortran_registry.txt -noextrap)
add_reg_test(map_registry.txt -ccode)
add_reg_test(moordyn_registry.txt)
add_reg_test(morison.txt)
add_reg_test(openfoam_registry.txt -ccode)
add_reg_test(orcaflexinterface.txt)
add_reg_test(registry_beamdyn.txt)
add_reg_test(registry_icedyn.txt)
add_reg_test(registry_nwtc_library_typedef_nomesh.txt)
add_reg_test(registry-ad14.txt)
add_reg_test(registry-dwm.txt)
add_reg_test(Registry_NWTC_Library_typedef_nomesh.txt -noextrap)
add_reg_test(sc_dataex_registry.txt -ccode -noextrap)
add_reg_test(servodyn_registry.txt)
add_reg_test(ss_excitation.txt)
add_reg_test(ss_radiation.txt)
add_reg_test(strucctrl_registry.txt)
add_reg_test(subdyn_registry.txt)
add_reg_test(supercontroller_registry.txt -ccode)
add_reg_test(unsteadyaero_registry.txt)
add_reg_test(wakedynamics_registry.txt -noextrap)
add_reg_test(wamit.txt)
add_reg_test(wamit2.txt)
add_reg_test(waves.txt)
add_reg_test(waves2.txt)

# add_library(mods
#   NWTC_Library.f90
#   SysSubs.f90
#   ${TEST_OUT_DIR}/AeroAcoustics_Types.f90
#   ${TEST_OUT_DIR}/AeroDyn_Driver_Types.f90
#   ${TEST_OUT_DIR}/AeroDyn_Inflow_Types.f90
#   ${TEST_OUT_DIR}/AeroDyn_Types.f90
#   ${TEST_OUT_DIR}/AeroDyn14_Types.f90
#   ${TEST_OUT_DIR}/AirfoilInfo_Types.f90
#   ${TEST_OUT_DIR}/AWAE_Types.f90
#   ${TEST_OUT_DIR}/BeamDyn_Types.f90
#   ${TEST_OUT_DIR}/BEMT_Types.f90
#   ${TEST_OUT_DIR}/Conv_Radiation_Types.f90
#   ${TEST_OUT_DIR}/Current_Types.f90
#   ${TEST_OUT_DIR}/DBEMT_Types.f90
#   ${TEST_OUT_DIR}/DWM_Types.f90
#   ${TEST_OUT_DIR}/ElastoDyn_Types.f90
#   ${TEST_OUT_DIR}/ExtPtfm_MCKF_Types.f90
#   ${TEST_OUT_DIR}/FAST_Farm_Types.f90
#   ${TEST_OUT_DIR}/FAST_Types.f90
#   ${TEST_OUT_DIR}/FASTWrapper_Types.f90
#   ${TEST_OUT_DIR}/FEAMooring_Types.f90
#   ${TEST_OUT_DIR}/FVW_Types.f90
#   ${TEST_OUT_DIR}/HydroDyn_Types.f90
#   ${TEST_OUT_DIR}/IceDyn_Types.f90
#   ${TEST_OUT_DIR}/IceFloe_Types.f90
#   ${TEST_OUT_DIR}/IfW_FlowField_Types.f90
#   ${TEST_OUT_DIR}/InflowWind_IO_Types.f90
#   ${TEST_OUT_DIR}/InflowWind_Types.f90
#   ${TEST_OUT_DIR}/Lidar_Types.f90
#   ${TEST_OUT_DIR}/MAP_Fortran_Types.f90
#   ${TEST_OUT_DIR}/MAP_Types.f90
#   ${TEST_OUT_DIR}/MoorDyn_Types.f90
#   ${TEST_OUT_DIR}/Morison_Types.f90
#   ${TEST_OUT_DIR}/NWTC_Library_Types.f90
#   ${TEST_OUT_DIR}/OpenFOAM_Types.f90
#   ${TEST_OUT_DIR}/OrcaFlexInterface_Types.f90
#   ${TEST_OUT_DIR}/SCDataEx_Types.f90
#   ${TEST_OUT_DIR}/ServoDyn_Types.f90
#   ${TEST_OUT_DIR}/SS_Excitation_Types.f90
#   ${TEST_OUT_DIR}/SS_Radiation_Types.f90
#   ${TEST_OUT_DIR}/StrucCtrl_Types.f90
#   ${TEST_OUT_DIR}/SubDyn_Types.f90
#   ${TEST_OUT_DIR}/SuperController_Types.f90
#   ${TEST_OUT_DIR}/UnsteadyAero_Types.f90
#   ${TEST_OUT_DIR}/WakeDynamics_Types.f90
#   ${TEST_OUT_DIR}/WAMIT_Types.f90
#   ${TEST_OUT_DIR}/WAMIT2_Types.f90
#   ${TEST_OUT_DIR}/Waves_Types.f90
#   ${TEST_OUT_DIR}/Waves2_Types.f90
# )
# set_target_properties(mods PROPERTIES LINKER_LANGUAGE Fortran)
# set(CMAKE_Fortran_FLAGS "${CMAKE_Fortran_FLAGS} -ffree-line-length-none")
