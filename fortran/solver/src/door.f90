module door_mod
    implicit none
    private
    type :: door_t
        logical(1) :: inited = .false.
        integer(1) :: room
        logical(1), allocatable :: rooms(:)
    contains
        procedure :: init
    end type door_t
    public :: door_t
contains
    subroutine init(door, n)
        class(door_t), intent(inout) :: door
        integer, intent(in) :: n
        if (door%inited) return
        door%inited = .true.
        door%room = -1
        allocate(door%rooms(n), source = .false._1)
    end subroutine init
end module door_mod
