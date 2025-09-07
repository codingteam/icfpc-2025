module door_mod
    implicit none
    private
    type :: door_t
        logical(1) :: inited = .false.
        integer(1) :: room
        logical(1), allocatable :: rooms(:)
    contains
        procedure :: init
        procedure :: show
        procedure :: door_t_assignment
        generic :: assignment(=) => door_t_assignment
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
    subroutine show(door)
        class(door_t), intent(in) :: door
        character(len=40) :: fmt
        write(fmt, '(A,I0,A)') '(I2,A,', size(door%rooms), '(L0," "))'
        write(6, fmt, advance = "no") door%room, ' | ', door%rooms
    end subroutine show
    subroutine door_t_assignment(lhs, rhs)
        class(door_t), intent(out) :: lhs
        class(door_t), intent(in)  :: rhs
        lhs%inited = rhs%inited
        lhs%room = rhs%room
        allocate(lhs%rooms(size(rhs%rooms)))
        lhs%rooms = rhs%rooms
    end subroutine door_t_assignment
end module door_mod
