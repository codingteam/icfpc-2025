module guess_mod
    use library_mod, only: library_t
    implicit none
    private
    type :: simplified_library_t
        logical :: inited = .false.
        integer :: n_rooms
        integer :: max_length
        integer, allocatable :: guess(:,:)
        logical, allocatable :: mask(:,:)
        logical, allocatable :: final_mask(:,:)
        integer(1), allocatable :: room_type(:)
    contains
        procedure :: init => sl_init
        procedure :: execute_plan
        procedure :: get_mask
    end type simplified_library_t
contains
    subroutine sl_init(library, n_rooms, guess, mask)
        class(simplified_library_t), intent(inout) :: library
        integer, intent(in) :: n_rooms
        integer, intent(in) :: guess(:)
        logical, intent(in) :: mask(:)
        integer :: room_id, door_id, cnt

        if (library%inited) return
        library%inited = .true.
        library%n_rooms = n_rooms
        library%max_length = 9 * n_rooms

        allocate(library%guess(n_rooms, 0:5))
        allocate(library%mask(n_rooms, 0:5), source = .false.)
        allocate(library%final_mask(n_rooms, 0:5), source = .false.)
        allocate(library%room_type(n_rooms))

        cnt = 1
        do room_id = 1, n_rooms
            do door_id = 0, 5
                library%guess(room_id, door_id) = guess(cnt)
                library%mask(room_id, door_id) = mask(cnt)
                library%final_mask(room_id, door_id) = mask(cnt)
                cnt = cnt + 1
            end do
            library%room_type(room_id) = mod(room_id - 1, 4)
        end do
    end subroutine sl_init
    subroutine execute_plan(library, plan)
        use plan_mod, only: plan_t
        class(simplified_library_t), intent(inout) :: library
        type(plan_t), intent(in) :: plan
        logical, allocatable :: mask(:,:)
        integer :: step_id, length, current_room, next_room
        integer(1) :: next_door, next_room_type
        allocate(mask(library%n_rooms, 0:5), source = .false.)
        mask = library%mask
        current_room = 0
        do step_id = 1, size(plan%steps)
            next_door = plan%steps(step_id)%door_out
            next_room_type = plan%steps(step_id)%room_in
            next_room = library%guess(current_room, next_door)
            if (library%room_type(next_room) == next_room_type) then
                mask(current_room, next_door) = .true.
                current_room = next_room
            else
                library%max_length = min(step_id, library%max_length)
                exit
            end if
        end do
        library%final_mask = library%final_mask .and. mask
    end subroutine execute_plan
    function get_mask(library) result(mask)
        class(simplified_library_t), intent(in) :: library
        logical, allocatable :: mask(:)
        integer :: room_id, door_id, cnt
        cnt = 1
        do room_id = 1, library%n_rooms
            do door_id = 0, 5
                mask(cnt) = library%final_mask(room_id, door_id)
            end do
        end do
    end function get_mask
end module guess_mod
