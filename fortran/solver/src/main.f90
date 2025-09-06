program icfpc2025solver
    use library_mod, only: library_t
    implicit none
    character(len=255) :: arg_
    character(len=:), allocatable :: arg
    integer :: nargs
    type(library_t) :: library
    nargs = command_argument_count()
    if (nargs /= 1) then
        call get_command_argument(0, arg_)
        arg = trim(arg_)
        error stop "Use: " // arg // " <file to solve>"
    end if
    call get_command_argument(1, arg_)
    if (len_trim(arg_) == 0) then
        error stop "Empty filename was set"
    end if
    arg = trim(arg_)
    call library%from_file(arg)
    call library%show()
end program icfpc2025solver
