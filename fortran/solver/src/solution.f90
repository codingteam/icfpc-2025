module solution_mod
    implicit none
    private

    !> @brief save connection (room, door) -> (room, door)
    type :: connection_t
        integer :: room_out = -1 !< exit room
        integer :: door_out = -1 !< exit door
        integer :: room_in = -1 !< enter room
        integer :: door_in = -1 !< enter door
    contains
        procedure :: to_json => connection_t_to_json
        procedure :: connection_t_assignment
        procedure :: connection_t_equal
        procedure :: connection_t_not_equal
        procedure :: gen_swap
        generic :: assignment(=) => connection_t_assignment
        generic :: operator(==) => connection_t_equal
        generic :: operator(/=) => connection_t_not_equal
    end type connection_t

    !> @brief final solution to submit
    type :: solution_t
        logical(1) :: inited = .false.                    !< was initialised
        type(connection_t), allocatable :: connections(:) !< list of all connections
        integer :: n_conns                                !< number of connections
    contains
        procedure :: init
        procedure :: to_json => solution_t_to_json
    end type solution_t

    public :: solution_t
contains

    !>
    !> @brief serialises connection_t to JSON
    !>
    !> @param[in] connection - connection_t object
    !> @return               - serialised JSON string
    !>
    !> @author foxtran
    !> @date   Sep 8, 2025
    !>
    function connection_t_to_json(connection) result(json)
        class(connection_t), intent(in) :: connection
        character(len=:), allocatable :: json
        character(len=120) :: json_tmp
        write(json_tmp, '(A,4(I0,A))') '{ "from": { "room": ', connection%room_out - 1, ', ' // &
                                                   '"door": ', connection%door_out, ' }, ' // &
                                           '"to": { "room": ', connection%room_in - 1, ', ' // &
                                                   '"door": ', connection%door_in, ' } }'
        json = trim(json_tmp)
    end function connection_t_to_json

    !>
    !> @brief generate connection with exchanged input-output
    !>
    !> @param[in] connection - connection_t object
    !> @return               - swapped in<->out room-door pair
    !>
    !> @author foxtran
    !> @date   Sep 8, 2025
    !>
    function gen_swap(connection) result(new_connection)
        class(connection_t), intent(in) :: connection
        type(connection_t) :: new_connection
        new_connection%room_out = connection%room_in
        new_connection%door_out = connection%door_in
        new_connection%room_in = connection%room_out
        new_connection%door_in = connection%door_out
    end function gen_swap

    !>
    !> @brief allows assignment without compiler bugs
    !>
    !> @param[out] lhs - connection_t object for initialisation
    !> @param[in]  rhs - connection_t object with data
    !>
    !> @author foxtran
    !> @date   Sep 8, 2025
    !>
    subroutine connection_t_assignment(lhs, rhs)
        class(connection_t), intent(out) :: lhs
        class(connection_t), intent(in)  :: rhs
        lhs%room_out = rhs%room_out
        lhs%door_out = rhs%door_out
        lhs%room_in = rhs%room_in
        lhs%door_in = rhs%door_in
    end subroutine connection_t_assignment

    !>
    !> @brief checks if two connections are equal
    !>
    !> @param[in] lhs - connection_t object
    !> @param[in] rhs - connection_t object
    !> @return        - .true. if connection are equal
    !>
    !> @author foxtran
    !> @date   Sep 8, 2025
    !>
    logical function connection_t_equal(lhs, rhs) result(equal)
        class(connection_t), intent(in) :: lhs
        class(connection_t), intent(in) :: rhs
        equal = &
          (lhs%room_out == rhs%room_out) .and. &
          (lhs%door_out == rhs%door_out) .and. &
          (lhs%room_in == rhs%room_in) .and. &
          (lhs%door_in == rhs%door_in)
    end function connection_t_equal

    !>
    !> @brief checks if two connections are not equal
    !>
    !> @param[in] lhs - connection_t object
    !> @param[in] rhs - connection_t object
    !> @return        - .true. if connection are not equal
    !>
    !> @author foxtran
    !> @date   Sep 8, 2025
    !>
    logical function connection_t_not_equal(lhs, rhs) result(equal)
        class(connection_t), intent(in) :: lhs
        class(connection_t), intent(in) :: rhs
        equal = .not. (lhs == rhs)
    end function connection_t_not_equal

    !>
    !> @brief serialises solution_t to JSON
    !>
    !> @param[in] solution - solution_t object
    !> @return             - serialised JSON string
    !>
    !> @author foxtran
    !> @date   Sep 8, 2025
    !>
    function solution_t_to_json(solution) result(json)
        class(solution_t), intent(in) :: solution
        character(len=:), allocatable :: json
        character(len=4) :: tmp
        integer :: i
        json = '"map": { "rooms": [ '
        do i = 1, size(solution%connections) / 6
            write(tmp, '(I0)') mod(i-1, 4)
            json = json // trim(tmp)
            if (i /= size(solution%connections) / 6) json = json // ', '
        end do
        json = json // ' ], "startingRoom": 0, "connections": [ '
        do i = 1, size(solution%connections)
            json = json // solution%connections(i)%to_json()
            if (i /= size(solution%connections)) json = json // ', '
        end do
        json = json // ' ] }'
    end function solution_t_to_json

    !>
    !> @brief initialise solution_t
    !>
    !> @details resolves (room_out, door_out) -> (room_in) to (room_out, door_out) <-> (room_in, door_in)
    !>
    !> @param[in,out] solution - solution_t object
    !> @param[in]     library  - labyrinth
    !>
    !> @author foxtran
    !> @date   Sep 8, 2025
    !>
    subroutine init(solution, library)
        use library_mod, only: library_t
        class(solution_t), intent(inout) :: solution
        type(library_t), intent(in) :: library
        type(library_t) :: libint
        integer :: room_id, door_id, conn_id
        integer :: n_rooms

        if (solution%inited) return
        solution%inited = .true.

        libint = library
        call libint%refine()
        n_rooms = size(libint%rooms)

        ! validate
        do room_id = 1, n_rooms
            do door_id = lbound(libint%rooms(room_id)%doors, 1), ubound(libint%rooms(room_id)%doors, 1)
                if (libint%rooms(room_id)%doors(door_id)%room < 0) then
                    call libint%show()
                    error stop "Solution is not full"
                    return
                end if
            end do
        end do

        allocate(solution%connections(6 * n_rooms))

        conn_id = 1
        do room_id = 1, n_rooms
            do door_id = lbound(libint%rooms(room_id)%doors, 1), ubound(libint%rooms(room_id)%doors, 1)
                if (libint%rooms(room_id)%doors(door_id)%room == -2) cycle

                block
                    type(connection_t) :: conn1, conn2
                    integer :: door_id_in
                    logical :: found

                    conn1%room_out = room_id
                    conn1%door_out = door_id
                    conn1%room_in = libint%rooms(room_id)%doors(door_id)%room

                    found = .false.
                    do door_id_in = lbound(libint%rooms(conn1%room_in)%doors, 1), ubound(libint%rooms(conn1%room_in)%doors, 1)
                        if (libint%rooms(conn1%room_in)%doors(door_id_in)%room == conn1%room_out) then
                            found = .true.
                            conn1%door_in = door_id_in
                            libint%rooms(conn1%room_in)%doors(door_id_in)%room = -2
                            libint%rooms(room_id)%doors(door_id)%room = -2
                            exit
                        end if
                    end do
                    if (.not.found) then
                        call libint%show()
                        print *,conn1%to_json()
                        error stop "Incomplete solution"
                    end if

                    conn2 = conn1%gen_swap()
                    solution%connections(conn_id) = conn1
                    conn_id = conn_id + 1
                    if (conn1 /= conn2) then
                        solution%connections(conn_id) = conn2
                        conn_id = conn_id + 1
                    end if
                end block
            end do
        end do
        solution%n_conns = conn_id
    end subroutine init
end module solution_mod
