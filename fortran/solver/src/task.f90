module task_mod
    implicit none
    private
    type :: task_t
        character(len=:), allocatable :: name
        integer :: n_rooms = 0
        integer :: max_length = 0
        integer :: kind = 0
    contains
        procedure :: init
    end type task_t
    public :: task_t
contains
    subroutine init(task, name)
        class(task_t), intent(inout) :: task
        character(len=*) :: name
        task%name = trim(name)
        select case(task%name)
            case ("probatio")
                task%n_rooms = 3
                task%kind = 1
            case ("primus")
                task%n_rooms = 6
                task%kind = 1
            case ("secundus")
                task%n_rooms = 12
                task%kind = 1
            case ("tertius")
                task%n_rooms = 18
                task%kind = 1
            case ("quartus")
                task%n_rooms = 24
                task%kind = 1
            case ("quintus")
                task%n_rooms = 30
                task%kind = 1
            case ("aleph")
                task%n_rooms = 12
                task%kind = 2
            case ("beth")
                task%n_rooms = 24
                task%kind = 2
            case ("gimel")
                task%n_rooms = 36
                task%kind = 2
            case ("daleth")
                task%n_rooms = 48
                task%kind = 2
            case ("he")
                task%n_rooms = 60
                task%kind = 2
            case ("vau")
                task%n_rooms = 18
                task%kind = 2
            case ("zain")
                task%n_rooms = 36
                task%kind = 2
            case ("hhet")
                task%n_rooms = 54
                task%kind = 2
            case ("teth")
                task%n_rooms = 72
                task%kind = 2
            case ("iod")
                task%n_rooms = 90
                task%kind = 2
            case default
                error stop "Unknown task name '" // task%name // "'"
        end select
        if (task%kind == 1) then
            task%max_length = 9 * task%n_rooms
        else if (task%kind == 2) then
            task%max_length = 6 * task%n_rooms
        else
            error stop "Unknown task kind"
        end if
    end subroutine init
end module task_mod
