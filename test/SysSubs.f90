module SysSubs

   use, intrinsic :: iso_fortran_env, only: int32, real32, real64
   use, intrinsic :: iso_c_binding

   public

   integer, parameter :: R4Ki = real32
   integer, parameter :: SiKi = real32
   integer, parameter :: R8Ki = real64
   integer, parameter :: DbKi = real64
   integer, parameter :: ReKi = real64
   integer, parameter :: IntKi = int32

   integer, parameter :: ChanLen = 20
   integer, parameter :: LinChanLen = 200
   integer, parameter :: MaxFileInfoLineLen = 1024

   integer, parameter :: ErrMsgLen = 1024
   integer, parameter :: ErrID_None = 0
   integer, parameter :: ErrID_Severe = 3
   integer, parameter :: ErrID_Fatal = 4

   integer, parameter :: MESH_UPDATECOPY = 3   !< parameter for type of mesh copy: updates fields in existing mesh

   integer           :: AbortErrLev = ErrID_Fatal

   type MeshType
   end type

   type MeshMapType
   end type

   type DLL_Type
   end type

   interface EqualRealNos
      module procedure EqualRealNos4
      module procedure EqualRealNos8
   end interface

   interface Angles_ExtrapInterp
      module procedure Angles_ExtrapInterp1_R4
      module procedure Angles_ExtrapInterp1_R8
      module procedure Angles_ExtrapInterp1_R4R
      module procedure Angles_ExtrapInterp1_R8R
      module procedure Angles_ExtrapInterp2_R4
      module procedure Angles_ExtrapInterp2_R8
      module procedure Angles_ExtrapInterp2_R4R
      module procedure Angles_ExtrapInterp2_R8R
   end interface

contains

   function EqualRealNos4(ReNum1, ReNum2)
      real(R4Ki), intent(IN) :: ReNum1, ReNum2
      logical :: EqualRealNos4
      EqualRealNos4 = ReNum1 == ReNum2
   end function

   function EqualRealNos8(ReNum1, ReNum2)
      real(R8Ki), intent(IN) :: ReNum1, ReNum2
      logical :: EqualRealNos8
      EqualRealNos8 = ReNum1 == ReNum2
   end function

   subroutine Angles_ExtrapInterp1_R4(Angle1, Angle2, tin, Angle_out, tin_out)
      real(R4Ki), intent(IN)  :: Angle1, Angle2
      real(R8Ki), intent(IN)  :: tin(:), tin_out
      real(R4Ki), intent(INOUT)  :: Angle_out
   end subroutine

   subroutine Angles_ExtrapInterp1_R8(Angle1, Angle2, tin, Angle_out, tin_out)
      real(R8Ki), intent(IN)  :: Angle1, Angle2
      real(R8Ki), intent(IN)  :: tin(:), tin_out
      real(R8Ki), intent(INOUT)  :: Angle_out
   end subroutine

   subroutine Angles_ExtrapInterp1_R4R(Angle1, Angle2, tin, Angle_out, tin_out)
      real(R4Ki), intent(IN)  :: Angle1, Angle2
      real(R4Ki), intent(IN)  :: tin(:), tin_out
      real(R4Ki), intent(INOUT)  :: Angle_out
   end subroutine

   subroutine Angles_ExtrapInterp1_R8R(Angle1, Angle2, tin, Angle_out, tin_out)
      real(R8Ki), intent(IN)  :: Angle1, Angle2
      real(R4Ki), intent(IN)  :: tin(:), tin_out
      real(R8Ki), intent(INOUT)  :: Angle_out
   end subroutine

   subroutine Angles_ExtrapInterp2_R4(Angle1, Angle2, Angle3, tin, Angle_out, tin_out)
      real(R4Ki), intent(IN)  :: Angle1, Angle2, Angle3
      real(R8Ki), intent(IN)  :: tin(:), tin_out
      real(R4Ki), intent(INOUT)  :: Angle_out
   end subroutine

   subroutine Angles_ExtrapInterp2_R8(Angle1, Angle2, Angle3, tin, Angle_out, tin_out)
      real(R8Ki), intent(IN)  :: Angle1, Angle2, Angle3
      real(R8Ki), intent(IN)  :: tin(:), tin_out
      real(R8Ki), intent(INOUT)  :: Angle_out
   end subroutine

   subroutine Angles_ExtrapInterp2_R4R(Angle1, Angle2, Angle3, tin, Angle_out, tin_out)
      real(R4Ki), intent(IN)  :: Angle1, Angle2, Angle3
      real(R4Ki), intent(IN)  :: tin(:), tin_out
      real(R4Ki), intent(INOUT)  :: Angle_out
   end subroutine

   subroutine Angles_ExtrapInterp2_R8R(Angle1, Angle2, Angle3, tin, Angle_out, tin_out)
      real(R8Ki), intent(IN)  :: Angle1, Angle2, Angle3
      real(R4Ki), intent(IN)  :: tin(:), tin_out
      real(R8Ki), intent(INOUT)  :: Angle_out
   end subroutine
end module
