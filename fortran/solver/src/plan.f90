module plan_mod
    use step_mod, only: step_t
    implicit none
    private
    type :: plan_t
        integer :: current = 1
        type(step_t), allocatable :: steps(:)
    contains
        procedure :: init
        procedure :: inited
        procedure :: add_step
        procedure :: reset
        procedure :: plan_assignment
        generic :: assignment(=) => plan_assignment
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
    logical function inited(plan)
        class(plan_t), intent(in) :: plan
        inited = allocated(plan%steps)
    end function inited
    subroutine add_step(plan, room_out, door_out, room_in)
        class(plan_t), intent(inout) :: plan
        integer(1), intent(in) :: room_out, door_out, room_in
        plan%steps(plan%current)%room_out = room_out
        plan%steps(plan%current)%door_out = door_out
        plan%steps(plan%current)%room_in = room_in
        plan%current = plan%current + 1
    end subroutine add_step
    subroutine reset(plan)
        class(plan_t), intent(inout) :: plan
        plan%current = 1
    end subroutine reset
    subroutine plan_assignment(lhs, rhs)
        class(plan_t), intent(out) :: lhs
        class(plan_t), intent(in)  :: rhs
        integer :: i
        if (.not.allocated(rhs%steps)) return
        allocate(lhs%steps(size(rhs%steps)))
        do i = 1, size(rhs%steps)
            lhs%steps(i) = rhs%steps(i)
        end do
        lhs%current = rhs%current
    end subroutine plan_assignment
end module plan_mod
