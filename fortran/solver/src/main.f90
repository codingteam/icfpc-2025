program icfpc2025solver
    use solver_mod, only: solver_t
    implicit none
    character(len=255) :: arg_
    character(len=:), allocatable :: arg
    integer :: nargs
    nargs = command_argument_count()
    if (nargs /= 2) then
        call get_command_argument(0, arg_)
        arg = trim(arg_)
        error stop "Use: " // arg // " <file to solve>"
    end if
end program icfpc2025solver
