module plan_mod
    use step_mod, only: step_t
    implicit none
    private

    !> @brief plan to explore the labyrinth
    ype :: plan_t
        integer :: current = 1                !< current step to add; only for internal usage
        type(step_t), allocatable :: steps(:) !< array of steps
    contains
        procedure :: init
        procedure :: inited
        procedure :: add_step
        procedure :: reset
        procedure :: generate
        procedure :: show
        procedure :: plan_assignment
        generic :: assignment(=) => plan_assignment
    end type plan_t

    public :: plan_t
contains

    !>
    !> @brief initialise plan_t
    !>
    !> @param[in,out] plan    - plan_t object
    !> @param[in]     n_steps - number of steps in plan, 6 * n_rooms or 9 * n_rooms, usually
    !>
    !> @author foxtran
    !> @date   Sep 8, 2025
    !>
    subroutine init(plan, n_steps)
        class(plan_t), intent(inout) :: plan
        integer, intent(in) :: n_steps
        if (allocated(plan%steps)) deallocate(plan%steps)
        plan%current = 1
        allocate(plan%steps(n_steps))
    end subroutine init

    !>
    !> @brief check if plan was initialised
    !>
    !> @param[in] plan - plan_t object
    !> @return         - was the plan_t object initialised
    !>
    !> @author foxtran
    !> @date   Sep 8, 2025
    !>
    logical function inited(plan)
        class(plan_t), intent(in) :: plan
        inited = allocated(plan%steps)
    end function inited

    !>
    !> @brief add step after exploration
    !>
    !> @param[in,out] plan     - plan_t object
    !> @param[in]     room_out - room from which guys exited
    !> @param[in]     door_out - door from which guys exited
    !> @param[in]     room_in  - room to which guys entered
    !>
    !> @author foxtran
    !> @date   Sep 8, 2025
    !>
    subroutine add_step(plan, room_out, door_out, room_in)
        class(plan_t), intent(inout) :: plan
        integer(1), intent(in) :: room_out, door_out, room_in
        plan%steps(plan%current)%room_out = room_out
        plan%steps(plan%current)%door_out = door_out
        plan%steps(plan%current)%room_in = room_in
        plan%current = plan%current + 1
    end subroutine add_step

    !>
    !> @brief resets initialisation of plan
    !>
    !> @param[in,out] plan     - plan_t object
    !>
    !> @author foxtran
    !> @date   Sep 8, 2025
    !>
    subroutine reset(plan)
        class(plan_t), intent(inout) :: plan
        plan%current = 1
    end subroutine reset

    !>
    !> @brief generates new plan to explore
    !>
    !> @details at the first step the most efficient way is to shuffle array containing the equal number of numbers from 0 to 5
    !>
    !> @param[in,out] plan - plan_t object
    !>
    !> @author foxtran
    !> @date   Sep 8, 2025
    !>
    subroutine generate(plan, kind)
        use random_mod, only: shuffle
        class(plan_t), intent(inout) :: plan
        integer, intent(in) :: kind
        integer(1), allocatable :: steps(:)
        integer(1), parameter :: filler(6) = (/ 0_1, 1_1, 2_1, 3_1, 4_1, 5_1 /)
        integer :: i
        if (kind /= 1) error stop "not implemented"
        allocate(steps(size(plan%steps)))
        do i = 1, size(steps), 6
            steps(i:i+5) = filler
        end do
        do i = 1, size(steps), 12
            if (i+11 > size(steps)) exit
            call shuffle(steps(i:i+11), 12)
        end do
        do i = 1, size(steps), 6
            call shuffle(steps(i:i+5), 6)
        end do
        do i = 1, size(steps)
            plan%steps(i)%door_out = steps(i)
        end do
    end subroutine generate

    !>
    !> @brief prints structure of plan_t to stdout
    !>
    !> @param[in] plan - plan_t object
    !>
    !> @author foxtran
    !> @date   Sep 8, 2025
    !>
    subroutine show(plan)
        class(plan_t), intent(in) :: plan
        integer :: i
        do i = 1, size(plan%steps)
            call plan%steps(i)%show()
        end do
    end subroutine show

    !>
    !> @brief allows assignment without compiler bugs
    !>
    !> @param[out] lhs - plan_t object for initialisation
    !> @param[in]  rhs - plan_t object with data
    !>
    !> @author foxtran
    !> @date   Sep 8, 2025
    !>
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
