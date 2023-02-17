import os
import subprocess

OF_EXEC = '../build/openfast_registry_cpp'
OUT_DIR = 'out'

args = [
    ['AeroAcoustics_Registry.txt', "-noextrap"],
    ['AeroDyn_Driver_Registry.txt', "-noextrap"],
    ['AeroDyn_Inflow_Registry.txt', "-noextrap"],
    ['AeroDyn_Registry.txt'],
    ['AirfoilInfo_Registry.txt', "-noextrap"],
    ['AWAE_Registry.txt', "-noextrap"],
    ['BeamDyn_Registry.txt', '-O', "output"],
    ['BEMT_Registry.txt'],
    ['Conv_Radiation.txt'],
    ['Current.txt'],
    ['DBEMT_Registry.txt'],
    ['ElastoDyn_Registry.txt'],
    ['ExtPtfm_MCKF_Registry.txt'],
    ['FAST_Farm_Registry.txt', "-noextrap"],
    ['FAST_Registry.txt', "-noextrap"],
    ['FASTWrapper_Registry.txt', "-noextrap"],
    ['FEAM_Registry.txt'],
    ['FVW_Registry.txt'],
    ['HydroDyn.txt'],
    ['IfW_4Dext.txt', "-noextrap"],
    ['IfW_BladedFFWind.txt', "-noextrap"],
    ['IfW_FFWind_Base.txt', "-noextrap"],
    ['IfW_HAWCWind.txt', "-noextrap"],
    ['IfW_TSFFWind.txt', "-noextrap"],
    ['IfW_UniformWind.txt', "-noextrap"],
    ['IfW_UserWind.txt', "-noextrap"],
    ['InflowWind.txt'],
    ['IceFloe_FASTRegistry.inp'],
    ['Lidar.txt'],
    ['MAP_Fortran_Registry.txt', "-noextrap"],
    ['MAP_Registry.txt', "-ccode"],
    ['MoorDyn_Registry.txt'],
    ['Morison.txt'],
    ['New_Registry.txt'],
    ['OpenFOAM_Registry.txt', "-ccode"],
    ['OrcaFlexInterface.txt'],
    ['Registry_BeamDyn.txt'],
    ['Registry_IceDyn.txt'],
    ['Registry-AD1', '.txt',  'O' 'output'],
    ['Registry-DWM', 'txt -O', 'output'],
    ['SC_DataEx_Registry.txt', "-ccode", "-noextrap"],
    ['ServoDyn_Registry.txt'],
    ['SS_Excitation.txt'],
    ['SS_Radiation.txt'],
    ['StrucCtrl_Registry.txt'],
    ['SubDyn_Registry.txt'],
    ['SuperController_Registry.txt', "-ccode"],
    ['UnsteadyAero_Registry.txt'],
    ['WakeDynamics_Registry.txt', "-noextrap"],
    ['WAMIT.txt'],
    ['WAMIT2.txt'],
    ['Waves.txt'],
    ['Waves2.txt'],
]

os.makedirs(OUT_DIR, exist_ok=True)
for a in args:
    print("\n--> "+a[0])
    try:
        subprocess.check_call([OF_EXEC, '-O', OUT_DIR, '-I',
                               'input', 'input/'+a[0]]+a[1:])
    except:
        pass
