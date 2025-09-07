module solver_mod
    use library_mod, only: library_t
    use task_mod, only: task_t
    implicit none
    private
    type :: solver_t
        logical(1) :: inited = .false.
        type(library_t) :: library
    contains
        procedure :: init
    end type solver_t
    public :: solver_t
contains
    subroutine init(solver, arg)
        use API_mod, only: select, explore
        use plan_mod, only: plan_t
        class(solver_t), intent(inout) :: solver
        character(len=*), intent(in) :: arg

        type(task_t) :: task
        type(plan_t), allocatable :: plans(:)
        integer :: i

        if (solver%inited) return
        solver%inited = .true._1

        call task%init(arg)
        call select(task)
        plans = generate_plans(task)
        call explore(task, plans)
        call solver%library%init(task%n_rooms, size(plans))
        do i = 1, size(plans)
            call solver%library%add_plan(plans(i))
        end do
        call solver%library%refine()
        call solver%library%show()
    contains
        function generate_plans(task) result(plans)
            type(task_t) :: task
            type(plan_t), allocatable :: plans(:)
            integer :: i
            if (task%kind == 1) then
                allocate(plans(1))
                do i = 1, size(plans)
                    call plans(i)%init(task%max_length)
                    call plans(i)%generate(task%kind)
                end do
            else
                error stop "not implemented"
            end if
        end function generate_plans
    end subroutine init
end module solver_mod
