module guess_mod
    use library_mod, only: library_t
    implicit none
    private

    !> @brief helper type for guesses; contains simplified view of library_t
    type :: simplified_library_t
        logical :: inited = .false.             !< was initialised
        integer :: n_rooms                      !< number of rooms in labyrinth
        integer, allocatable :: guess(:,:)      !< guessed labyrinth in form (room_out, door_out) -> (room_in)
        logical, allocatable :: mask(:,:)       !< paired to guessed labyrinth; contains info about 100% sure guesses
        logical, allocatable :: final_mask(:,:) !< like mask, but contains info after checking plans
        integer(1), allocatable :: room_type(:) !< types of rooms (in range from 0 to 3)
    contains
        procedure :: init => sl_init
        procedure :: execute_plan
        procedure :: get_mask
    end type simplified_library_t

    !> @brief contains guess of labyrinth
    type :: guess_t
        logical :: inited = .false.         !< was initialised
        integer :: max_length               !< length of proper guess
        type(library_t), pointer :: library !< pointer to global labyrinth info
        integer, allocatable :: guess(:)    !< guess of rooms' enters in 1D shape
        logical, allocatable :: mask(:)     !< paired to guess of rooms'; contains info about 100% sure guesses
    contains
        procedure :: init => guess_init
        procedure :: eval
        procedure :: next
        procedure :: set_solution
    end type guess_t

    public :: guess_t
contains

    !>
    !> @brief initialise simplified_library_t
    !>
    !> @param[in,out] library - simplified_library_t object
    !> @param[in]     n_rooms - number of rooms in labyrinth
    !> @param[in]     guess   - guess about labyrinth in 1D form
    !> @param[in]     mask    - info about correct guess about labyrinth in 1D form
    !>
    !> @author foxtran
    !> @date   Sep 8, 2025
    !>
    subroutine sl_init(library, n_rooms, guess, mask)
        class(simplified_library_t), intent(inout) :: library
        integer, intent(in) :: n_rooms
        integer, intent(in) :: guess(:)
        logical, intent(in) :: mask(:)
        integer :: room_id, door_id, cnt

        if (library%inited) return
        library%inited = .true.
        library%n_rooms = n_rooms

        allocate(library%guess(n_rooms, 0:5))
        allocate(library%mask(n_rooms, 0:5), source = .false.)
        allocate(library%final_mask(n_rooms, 0:5), source = .true.)
        allocate(library%room_type(n_rooms))

        cnt = 1
        do room_id = 1, n_rooms
            do door_id = 0, 5
                library%guess(room_id, door_id) = guess(cnt)
                library%mask(room_id, door_id) = mask(cnt)
                cnt = cnt + 1
            end do
            library%room_type(room_id) = mod(room_id - 1, 4)
        end do
    end subroutine sl_init

    !>
    !> @brief executes plan to validate guess
    !>
    !> @param[in,out] library - simplified_library_t object
    !> @param[in]     plan    - plan for check
    !>
    !> @author foxtran
    !> @date   Sep 8, 2025
    !>
    integer function execute_plan(library, plan) result(max_length)
        use plan_mod, only: plan_t
        class(simplified_library_t), intent(inout) :: library
        type(plan_t), intent(in) :: plan
        logical, allocatable :: mask(:,:)
        integer :: step_id, length, current_room, next_room
        integer(1) :: next_door, next_room_type
        allocate(mask(library%n_rooms, 0:5), source = .false.)
        mask = library%mask
        current_room = 1
        max_length = 0
        do step_id = 1, size(plan%steps)
            next_door = plan%steps(step_id)%door_out
            next_room_type = plan%steps(step_id)%room_in
            next_room = library%guess(current_room, next_door)
            if (library%room_type(next_room) == next_room_type) then
                mask(current_room, next_door) = .true.
                current_room = next_room
            else
                exit
            end if
            max_length = step_id
        end do
        library%final_mask = library%final_mask .and. mask
    end function execute_plan

    !>
    !> @brief return info about which guesses are correct
    !>
    !> @param[in] library - simplified_library_t object
    !> @return            - 1D mask of correct guesses
    !>
    !> @author foxtran
    !> @date   Sep 8, 2025
    !>
    function get_mask(library) result(mask)
        class(simplified_library_t), intent(in) :: library
        logical, allocatable :: mask(:)
        integer :: room_id, door_id, cnt
        allocate(mask(6*library%n_rooms), source = .false.)
        cnt = 1
        do room_id = 1, library%n_rooms
            do door_id = 0, 5
                mask(cnt) = library%final_mask(room_id, door_id)
                cnt = cnt + 1
            end do
        end do
    end function get_mask

    !>
    !> @brief initialise guess_t
    !>
    !> @param[in,out] guess   - guess_t object
    !> @param[in]     library - info about labyrinth
    !>
    !> @author foxtran
    !> @date   Sep 8, 2025
    !>
    subroutine guess_init(guess, library)
        class(guess_t), intent(inout) :: guess
        type(library_t), target, intent(in) :: library
        integer :: n_rooms, room_id, door_id, cnt, room_in

        if (guess%inited) return
        guess%inited = .true.

        guess%library => library
        n_rooms = size(guess%library%rooms)
        guess%max_length = size(guess%library%plans(1)%steps)
        allocate(guess%guess(6*n_rooms), source = -1)
        allocate(guess%mask(6*n_rooms), source = .false.)

        cnt = 1
        do room_id = 1, n_rooms
            do door_id = 0, 5
                room_in = guess%library%rooms(room_id)%doors(door_id)%room
                if (room_in /= -1) then
                    guess%guess(cnt) = room_in
                    guess%mask(cnt) = .true.
                end if
                cnt = cnt + 1
            end do
        end do

        call guess%next()
    end subroutine guess_init

    !>
    !> @brief check how guess is good
    !>
    !> @param[in,out] guess   - guess_t object
    !>
    !> @author foxtran
    !> @date   Sep 8, 2025
    !>
    subroutine eval(guess)
        class(guess_t), intent(inout) :: guess
        type(simplified_library_t) :: library
        integer :: plan_id, n_rooms, max_length
        n_rooms = size(guess%library%rooms)
        max_length = size(guess%library%plans(1)%steps)
        call library%init(n_rooms, guess%guess, guess%mask)
        do plan_id = 1, size(guess%library%plans)
            max_length = min(max_length, &
                library%execute_plan(guess%library%plans(plan_id)))
        end do
        guess%mask = library%get_mask()
        guess%max_length = max_length
    end subroutine eval

    !>
    !> @brief generate new guess based on info after eval
    !>
    !> @details tries to generate random labyrinth that as least has proper connections.
    !>            However, it does not respect rules about which connections are available in reality.
    !>            So, it sucks a lot.
    !>
    !> @param[in,out] guess   - guess_t object
    !>
    !> @author foxtran
    !> @date   Sep 8, 2025
    !>
    subroutine next(guess)
        use random_mod, only: shuffle, rand_int
        class(guess_t), intent(inout) :: guess
        integer, allocatable :: rooms(:)
        integer :: door_id, room_id, n_rooms, cnt
        integer :: idx, i1, i2
        integer :: room_in, curr_room
        logical :: found

        n_rooms = size(guess%library%rooms)
        allocate(rooms(n_rooms), source = 6)

        ! count, which connections are already know
        do cnt = 1, size(guess%guess)
            if (.not.guess%mask(cnt)) then
                guess%guess(cnt) = 0
            else
                room_in = guess%guess(cnt)
                rooms(room_in) = rooms(room_in) - 1
            end if
        end do

        ! main loop to generate labyrinth
        ! it implements the following algorithm:
        !   1 ) door by door, select random target room
        !   2 ) on targeted room, find already known connection
        !   3a) if connection exists: change sign of this connections
        !   3b) otherwise: assign first free door to originated room
        !   4)  shuffle exit rooms in each room to prevent non-random in 3b
        do cnt = 1, size(guess%guess)
            if (guess%guess(cnt) /= 0) cycle
            curr_room = (cnt - 1) / 6 + 1
            do
                room_id = rand_int(1, n_rooms)
                if (rooms(room_id) > 0) then
                    rooms(room_id) = rooms(room_id) - 1
                    exit
                end if
            end do
            guess%guess(cnt) = -room_id
            if (room_id == curr_room) cycle
            i1 = (room_id - 1) * 6 + 1
            i2 = i1 + 5
            found = .false.
            do idx = i1, i2
                if (guess%mask(idx) .and. guess%guess(idx) == curr_room) then
                    guess%guess(idx) = -guess%guess(idx)
                    found = .true.
                    exit
                end if
            end do
            if (.not.found) then
                do idx = i1, i2
                    if (guess%guess(idx) == 0) then
                        guess%guess(idx) = -curr_room
                        exit
                    end if
                end do
            end if
        end do

        guess%guess = abs(guess%guess)

        ! shuffle after bad generation, otherwise the generation is not random, actually
        do room_id = 1, n_rooms
            i1 = (room_id - 1) * 6 + 1
            i2 = i1 + 5
            call shuffle(guess%guess(i1:i2), 6, guess%mask(i1:i2))
        end do
    end subroutine next

    !>
    !> @brief apply guess to labyrinth
    !>
    !> @param[in] guess     - guess_t object
    !> @param[in] library   - solved labyrinth
    !>
    !> @author foxtran
    !> @date   Sep 8, 2025
    !>
    subroutine set_solution(guess, library)
        class(guess_t), intent(in) :: guess
        type(library_t), intent(inout) :: library
        integer :: room_id, door_id, n_rooms, cnt
        n_rooms = size(library%rooms)
        cnt = 1
        do room_id = 1, n_rooms
            do door_id = 0, 5
                library%rooms(room_id)%doors(door_id)%room = guess%guess(cnt)
                cnt = cnt + 1
            end do
        end do
    end subroutine set_solution
end module guess_mod
