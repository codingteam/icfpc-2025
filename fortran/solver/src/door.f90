module door_mod
    implicit none
    private

    !>
    !> @brief contains information about door in room
    !>
    type :: door_t
        logical(1) :: inited = .false.      !< was the door initialized
        integer(1) :: room                  !< real exit room
        logical(1), allocatable :: rooms(:) !< allowed exit rooms
    contains
        procedure :: init
        procedure :: show
        procedure :: door_t_assignment
        generic :: assignment(=) => door_t_assignment
    end type door_t

    public :: door_t
contains

    !>
    !> @brief initialize door
    !>
    !> @param[in] door - door_t object
    !> @param[in] n    - number of rooms in labyrinth
    !>
    !> @author foxtran
    !> @date   Sep 8, 2025
    !>
    subroutine init(door, n)
        class(door_t), intent(inout) :: door
        integer, intent(in) :: n
        if (door%inited) return
        door%inited = .true.
        door%room = -1
        allocate(door%rooms(n), source = .false._1)
    end subroutine init

    !>
    !> @brief prints structure of door to stdout
    !>
    !> @param[in] door - door_t object
    !>
    !> @author foxtran
    !> @date   Sep 8, 2025
    !>
    subroutine show(door)
        class(door_t), intent(in) :: door
        character(len=40) :: fmt
        write(fmt, '(A,I0,A)') '(I2,A,', size(door%rooms), '(L0," "))'
        write(6, fmt, advance = "no") door%room, ' | ', door%rooms
    end subroutine show

    !>
    !> @brief allows assignment without compiler bugs
    !>
    !> @param[out] lhs - door_t object for initialisation
    !> @param[in]  rhs - door_t object with data
    !>
    !> @author foxtran
    !> @date   Sep 8, 2025
    !>
    subroutine door_t_assignment(lhs, rhs)
        class(door_t), intent(out) :: lhs
        class(door_t), intent(in)  :: rhs
        lhs%inited = rhs%inited
        lhs%room = rhs%room
        allocate(lhs%rooms(size(rhs%rooms)))
        lhs%rooms = rhs%rooms
    end subroutine door_t_assignment
end module door_mod
