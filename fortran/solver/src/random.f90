module random_mod
    implicit none
    private
    interface shuffle
        module procedure shuffle_i1, shuffle_i4
    end interface shuffle
    public :: shuffle
contains
    subroutine shuffle_i1(arr, n)
        integer(1), intent(inout) :: arr(n)
        integer, intent(in) :: n

        integer(1) :: tmp
        real(4) :: r_randval
        integer :: i_randval
        integer :: i, j

        do j = 1, 4
            do i = 1, n
                call random_number(r_randval)
                i_randval = floor(1 + r_randval * n)
                tmp = arr(i)
                arr(i) = arr(i_randval)
                arr(i_randval) = tmp
            end do
        end do

    end subroutine shuffle_i1
    subroutine shuffle_i4(arr, n)
        integer(4), intent(inout) :: arr(n)
        integer, intent(in) :: n

        integer(4) :: tmp
        real(4) :: r_randval
        integer :: i_randval
        integer :: i, j

        do j = 1, 4
            do i = 1, n
                call random_number(r_randval)
                i_randval = floor(1 + r_randval * n)
                tmp = arr(i)
                arr(i) = arr(i_randval)
                arr(i_randval) = tmp
            end do
        end do

    end subroutine shuffle_i4
end module random_mod
