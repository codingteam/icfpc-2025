module step_mod
    implicit none
    private
    type :: step_t
        integer(1) :: room_out = -1
        integer(1) :: door_out = -1
        integer(1) :: room_in = -1
    contains
        procedure :: show
    end type step_t
    public :: step_t
contains
    subroutine show(step)
        class(step_t), intent(in) :: step
        write(6, '(I3," ",I0," ",I3)') step%room_out, step%door_out, step%room_in
    end subroutine show
end module step_mod
