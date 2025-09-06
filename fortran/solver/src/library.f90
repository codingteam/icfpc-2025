module library_mod
    use room_mod, only: room_t
    implicit none
    private
    type :: library_t
        logical(1) :: inited = .false.
        type(room_t), allocatable :: rooms(:)
    contains
        procedure :: init
    end type library_t
contains
    subroutine init(library, n)
        class(library_t), intent(inout) :: library
        integer, intent(in) :: n
        integer :: i
        if (library%inited) return
        allocate(library%rooms(n))
        do i = 1, n
            call library%rooms(i)%init(n)
        end do
    end subroutine init
end module library_mod
