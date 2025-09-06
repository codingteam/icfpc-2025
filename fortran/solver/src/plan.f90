module plan_mod
    use step_mod, only: step_t
    implicit none
    private
    type :: plan_t
        integer :: current = 1
        type(step_t), allocatable :: steps(:)
    contains
        procedure :: init
        procedure :: add_step
    end type plan_t
    public :: plan_t
contains
    subroutine init(plan, n_steps)
        class(plan_t), intent(inout) :: plan
        integer, intent(in) :: n_steps
        if (allocated(plan%steps)) deallocate(plan%steps)
        plan%current = 1
        allocate(plan%steps(n_steps))
    end subroutine init
    subroutine add_step(plan, room_out, door_out, room_in)
        class(plan_t), intent(inout) :: plan
        integer(1), intent(in) :: room_out, door_out, room_in
        plan%steps(plan%current)%room_out = room_out
        plan%steps(plan%current)%door_out = door_out
        plan%steps(plan%current)%room_in = room_in
        plan%current = plan%current + 1
    end subroutine add_step
end module plan_mod
