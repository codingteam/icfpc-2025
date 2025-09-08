module step_mod
    implicit none
    private

    !> @brief step in plan
    type :: step_t
        integer(1) :: room_out = -1 !< starting room
        integer(1) :: door_out = -1 !< starting door
        integer(1) :: room_in = -1  !< entered room
    contains
        procedure :: show
    end type step_t

    public :: step_t
contains

    !>
    !> @brief prints structure of step_t to stdout
    !>
    !> @param[in] step - step_t object
    !>
    !> @author foxtran
    !> @date   Sep 8, 2025
    !>
    subroutine show(step)
        class(step_t), intent(in) :: step
        write(6, '(I3," ",I0," ",I3)') step%room_out, step%door_out, step%room_in
    end subroutine show
end module step_mod
