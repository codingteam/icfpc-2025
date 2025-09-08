! MIT License
!
! Copyright (c) 2025 foxtran
!
! Permission is hereby granted, free of charge, to any person obtaining a copy
! of this software and associated documentation files (the "Software"), to deal
! in the Software without restriction, including without limitation the rights
! to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
! copies of the Software, and to permit persons to whom the Software is
! furnished to do so, subject to the following conditions:
!
! The above copyright notice and this permission notice shall be included in all
! copies or substantial portions of the Software.
!
! THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
! IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
! FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
! AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
! LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
! OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE
! SOFTWARE.

module random_mod
    implicit none
    private

    !>
    !> @brief shuffle array
    !>
    !> @details integer(1) and integer(4) arrays are supported
    !>
    interface shuffle
        module procedure shuffle_i1, shuffle_i4
    end interface shuffle

    public :: shuffle, rand_int
contains

    !>
    !> @brief shuffles array
    !>
    !> @param[in,out] arr  - array for shuffling
    !> @param[in]     n    - size of array
    !> @param[in]     mask - (optional) mask which elements should be kept on its places
    !>
    !> @author foxtran
    !> @date   Sep 8, 2025
    !>
    subroutine shuffle_i1(arr, n, mask)
        integer(1), intent(inout) :: arr(n)
        integer, intent(in) :: n
        logical, optional, intent(in) :: mask(n)

        integer(1) :: tmp
        real(4) :: r_randval
        integer :: i_randval
        integer :: i, j

        if (present(mask)) then
            do j = 1, 4
                do i = 1, n
                    if (mask(i)) cycle
                    call random_number(r_randval)
                    i_randval = floor(1 + r_randval * n)
                    if (mask(i_randval)) cycle
                    tmp = arr(i)
                    arr(i) = arr(i_randval)
                    arr(i_randval) = tmp
                end do
            end do
        else
            do j = 1, 4
                do i = 1, n
                    call random_number(r_randval)
                    i_randval = floor(1 + r_randval * n)
                    tmp = arr(i)
                    arr(i) = arr(i_randval)
                    arr(i_randval) = tmp
                end do
            end do
        end if
    end subroutine shuffle_i1

    !>
    !> @brief shuffles array
    !>
    !> @param[in,out] arr  - array for shuffling
    !> @param[in]     n    - size of array
    !> @param[in]     mask - (optional) mask which elements should be kept on its places
    !>
    !> @author foxtran
    !> @date   Sep 8, 2025
    !>
    subroutine shuffle_i4(arr, n, mask)
        integer(4), intent(inout) :: arr(n)
        integer, intent(in) :: n
        logical, optional, intent(in) :: mask(n)

        integer(4) :: tmp
        real(4) :: r_randval
        integer :: i_randval
        integer :: i, j

        if (present(mask)) then
            do j = 1, 4
                do i = 1, n
                    if (mask(i)) cycle
                    call random_number(r_randval)
                    i_randval = floor(1 + r_randval * n)
                    if (mask(i_randval)) cycle
                    tmp = arr(i)
                    arr(i) = arr(i_randval)
                    arr(i_randval) = tmp
                end do
            end do
        else
            do j = 1, 4
                do i = 1, n
                    call random_number(r_randval)
                    i_randval = floor(1 + r_randval * n)
                    tmp = arr(i)
                    arr(i) = arr(i_randval)
                    arr(i_randval) = tmp
                end do
            end do
        end if
    end subroutine shuffle_i4

    !>
    !> @brief generate random int in range from min to max
    !>
    !> @param[in] min - minimal integer value
    !> @param[in] max - maximal integer value
    !> @return        - value in range [min, max]
    !>
    !> @author foxtran
    !> @date   Sep 8, 2025
    !>
    integer function rand_int(min, max) result(val)
        integer, intent(in):: min, max
        real :: r
        call random_number(r)
        val = floor(min + r * (max - min + 1))
    end function rand_int
end module random_mod
