module task_mod
    implicit none
    private
    type :: task_t
        character(len=:), allocatable :: API_URL
        character(len=:), allocatable :: API_ID
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
        call setup_API(task)
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
            task%max_length = 18 * task%n_rooms
        else if (task%kind == 2) then
            task%max_length = 6 * task%n_rooms
        else
            error stop "Unknown task kind"
        end if
    contains
        subroutine setup_API(task)
            class(task_t), intent(inout) :: task
            integer :: envvarlen, stat
            call get_environment_variable("SERVER_URL", length = envvarlen, status = stat)
            if (stat > 0) then
                task%API_URL = "https://31pwr5t6ij.execute-api.eu-west-2.amazonaws.com"
            else
                allocate(character(len = envvarlen) :: task%API_URL)
                call get_environment_variable("SERVER_URL", value = task%API_URL)
            end if

            call get_environment_variable("ICFPC2025ID", length = envvarlen, status = stat)
            if (stat == 0) then
                allocate(character(len = envvarlen) :: task%API_ID)
                call get_environment_variable("ICFPC2025ID", value = task%API_ID)
            else
                error stop "Environment variable ICFPC2025ID is not set"
            end if
        end subroutine setup_API
    end subroutine init
end module task_mod
