module room_mod
    use door_mod, only: door_t
    implicit none
    private
    type :: room_t
        logical(1) :: inited = .false.
        integer(1) :: number
        type(door_t) :: doors(0:5)
    contains
        procedure :: init
        procedure :: show
    end type room_t
    public :: room_t
contains
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
end module room_mod
