program icfpc2025solver
    implicit none
    character(len=255) :: arg_
    character(len=:), allocatable :: arg
    integer :: nargs
    nargs = command_argument_count()
    if (nargs /= 1 .and. nargs /= 2) then
        call get_command_argument(0, arg_)
        arg = trim(arg_)
        write (6, *) "Use: " // arg // " file <file to solve>"
        write (6, *) "  or " // arg // " <taskname>"
        error stop 1
    end if
    call get_command_argument(1, arg_)
    if (len_trim(arg_) == 0) then
        error stop "Empty filename was set"
    end if
    arg = trim(arg_)
    if (arg == 'file') then
        call get_command_argument(2, arg_)
        if (len_trim(arg_) == 0) then
            error stop "Empty filename was set"
        end if
        arg = trim(arg_)
        block
            use library_mod, only: library_t
            type(library_t) :: library
            call library%from_file(arg)
            call library%show()
        end block
    else
        if (nargs /= 1) then
            write (6, *) "Use: " // arg // " file <file to solve>"
            write (6, *) "  or " // arg // " <taskname>"
            error stop 1
        end if
        block
            use solver_mod, only: solver_t
            type(solver_t) :: solver
            call solver%init(arg)
        end block
    end if
end program icfpc2025solver
