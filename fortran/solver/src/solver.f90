module solver_mod
    use library_mod, only: library_t
    use task_mod, only: task_t
    implicit none
    private
    type :: solver_t
        logical(1) :: inited = .false.
        type(library_t) :: library
        type(task_t) :: task
    contains
        procedure :: init
        procedure :: solve
        procedure :: submit
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
        solver%task = task
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
    subroutine solve(solver)
        use guess_mod, only: guess_t
        class(solver_t), intent(inout) :: solver
        type(guess_t), allocatable :: guess(:)
        integer, parameter :: N_guess = 1024
        integer :: iter, guess_id, max_length
        allocate(guess(N_guess))
        do guess_id = 1, N_guess
            call guess(guess_id)%init(solver%library)
        end do
        do iter = 1, 6 * 9 * size(solver%library%rooms)
            max_length = 0
            do guess_id = 1, N_guess
                call guess(guess_id)%eval()
                call guess(guess_id)%next()
                max_length = max(max_length, guess(guess_id)%max_length)
            end do
            print '("iter: ",I4," ML:",I4)', iter, max_length
        end do
        call guess(1)%set_solution(solver%library)
    end subroutine solve
    subroutine submit(solver)
        use solution_mod, only: solution_t
        use API_mod, only: guess
        class(solver_t), intent(inout) :: solver
        type(solution_t) :: solution
        call solution%init(solver%library)
        call guess(solver%task, solution%to_json())
    end subroutine submit
end module solver_mod
