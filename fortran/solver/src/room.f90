module room_mod
    use door_mod, only: door_t
    implicit none
    private
    type :: room_t
        logical(1) :: inited = .false.
        integer(1) :: number
        type(door_t) :: doors(6)
    contains
        procedure :: init
    end type room_t
    public :: room_t
contains
    subroutine init(room, n)
        class(room_t), intent(inout) :: room
        integer, intent(in) :: n
        integer :: i
        if (room%inited) return
        room%inited = .true._1
        do i = 1, 6
            call room%doors(i)%init(n)
        end do
    end subroutine init
end module room_mod
