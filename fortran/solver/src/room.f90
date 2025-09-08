module room_mod
    use door_mod, only: door_t
    implicit none
    private

    !> @brief room of labyrinth
    type :: room_t
        logical(1) :: inited = .false. !< was initialised
        integer(1) :: number           !< 2 last bits of room id
        type(door_t) :: doors(0:5)     !< doors in the room
    contains
        procedure :: init
        procedure :: show
        procedure :: room_t_assignment
        generic :: assignment(=) => room_t_assignment
    end type room_t

    public :: room_t
contains

    !>
    !> @brief initialise room_t
    !>
    !> @param[in,out] room - room_t object
    !> @param[in]     n    - number of rooms in labyrinth
    !>
    !> @author foxtran
    !> @date   Sep 8, 2025
    !>
    subroutine init(room, n)
        class(room_t), intent(inout) :: room
        integer, intent(in) :: n
        integer :: i
        if (room%inited) return
        room%inited = .true._1
        do i = lbound(room%doors, 1), ubound(room%doors, 1)
            call room%doors(i)%init(n)
        end do
    end subroutine init

    !>
    !> @brief prints structure of room_t to stdout
    !>
    !> @param[in] room - room_t object
    !>
    !> @author foxtran
    !> @date   Sep 8, 2025
    !>
    subroutine show(room)
        class(room_t), intent(in) :: room
        integer :: i
        write(6, '(A,I0)') repeat(" ", 3), room%number
        do i = lbound(room%doors, 1), ubound(room%doors, 1)
            write(6, '(A,I0,A)', advance="no") "  door #", i, ': '
            call room%doors(i)%show()
            write(6, *) ''
        end do
    end subroutine show

    !>
    !> @brief allows assignment without compiler bugs
    !>
    !> @param[out] lhs - room_t object for initialisation
    !> @param[in]  rhs - room_t object with data
    !>
    !> @author foxtran
    !> @date   Sep 8, 2025
    !>
    subroutine room_t_assignment(lhs, rhs)
        class(room_t), intent(out) :: lhs
        class(room_t), intent(in)  :: rhs
        lhs%inited = rhs%inited
        lhs%number = rhs%number
        lhs%doors = rhs%doors
    end subroutine room_t_assignment
end module room_mod
