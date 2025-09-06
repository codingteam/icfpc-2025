module step_mod
    implicit none
    private
    type :: step_t
        integer(1) :: room_out
        integer(1) :: door_out
        integer(1) :: room_in
    end type step_t
    public :: step_t
end module step_mod
