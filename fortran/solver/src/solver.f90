module solver_mod
    use library_mod, only: library_t
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
    subroutine init(solver)
        class(solver_t), intent(inout) :: solver

        if (solver%inited) return
        solver%inited = .true._1

    end subroutine init
end module solver_mod
