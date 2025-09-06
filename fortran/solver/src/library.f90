module library_mod
    use room_mod, only: room_t
    use plan_mod, only: plan_t
    implicit none
    private
    type :: library_t
        logical(1) :: inited = .false.
        type(room_t), allocatable :: rooms(:)
        type(plan_t), allocatable :: plans(:)
        integer :: current_plan = 1
    contains
        procedure :: init
        procedure :: add_plan
    end type library_t
    public :: library_t
contains
    subroutine init(library, n_rooms, n_plans)
        class(library_t), intent(inout) :: library
        integer, intent(in) :: n_rooms, n_plans
        integer :: i
        if (library%inited) return
        allocate(library%rooms(n_rooms))
        allocate(library%plans(n_plans))
        do i = 1, n_rooms
            call library%rooms(i)%init(n_rooms)
            library%rooms(i)%number = mod(i-1, 4)
        end do
        library%current_plan = 1
    end subroutine init
    subroutine add_plan(library, plan)
        class(library_t), intent(inout) :: library
        type(plan_t), intent(in) :: plan
        library%plans(library%current_plan) = plan
        library%current_plan = library%current_plan + 1
    end subroutine add_plan

end module library_mod
