module library_mod
    use room_mod, only: room_t
    use plan_mod, only: plan_t
    implicit none
    private

    !> @brief info about labyrinth
    type :: library_t
        logical(1) :: inited = .false.        !< was initialized
        type(room_t), allocatable :: rooms(:) !< rooms in labyrinth
        type(plan_t), allocatable :: plans(:) !< explored plans
        integer :: current_plan = 1           !< id of current plan; for internal usage
    contains
        procedure :: init
        procedure :: from_file
        procedure :: add_plan
        procedure :: refine
        procedure :: show
        procedure :: library_t_assignment
        generic :: assignment(=) => library_t_assignment
    end type library_t

    public :: library_t
contains

    !>
    !> @brief initialise library_t
    !>
    !> @param[in,out] library - library_t object
    !> @param[in]     n_rooms - number of rooms in labyrinth
    !> @param[in]     n_plans - number of plans applied to labyrinth
    !>
    !> @author foxtran
    !> @date   Sep 8, 2025
    !>
    subroutine init(library, n_rooms, n_plans)
        class(library_t), intent(inout) :: library
        integer, intent(in) :: n_rooms, n_plans
        integer :: i
        if (library%inited) return
        library%inited = .true.
        allocate(library%rooms(n_rooms))
        allocate(library%plans(n_plans))
        do i = 1, n_rooms
            call library%rooms(i)%init(n_rooms)
            library%rooms(i)%number = mod(i-1, 4)
        end do
        library%current_plan = 1
    end subroutine init

    !>
    !> @brief save extra plan
    !>
    !> @param[in,out] library - library_t object
    !> @param[in]     n_rooms - number of rooms in labyrinth
    !> @param[in]     n_plans - number of plans applied to labyrinth
    !>
    !> @author foxtran
    !> @date   Sep 8, 2025
    !>
    subroutine add_plan(library, plan)
        class(library_t), intent(inout) :: library
        type(plan_t), intent(in) :: plan
        library%plans(library%current_plan) = plan
        library%current_plan = library%current_plan + 1
    end subroutine add_plan

    !>
    !> @brief initialise library_t from file
    !>
    !> @note See examples/ about structure
    !>
    !> @param[in,out] library  - library_t object
    !> @param[in]     filename - file to load
    !>
    !> @author foxtran
    !> @date   Sep 8, 2025
    !>
    subroutine from_file(library, filename)
        class(library_t), intent(inout) :: library
        character(len=*), intent(in) :: filename

        integer :: lu, ios
        character(len=32) :: string

        integer :: n_rooms, n_plans, plans_length
        integer(1) :: room_out, door_out, room_in

        type(plan_t) :: plan

        if (library%inited) return
        library%inited = .true.

        open(newunit = lu, file = filename, status = "old", action = "read")
        read(lu, *) n_rooms, n_plans, plans_length

        call library%init(n_rooms, n_plans)

        do
            read(lu, '(A)', iostat=ios) string
            if (ios /= 0) then
                print *, "Badly formatted file"
                exit
            end if
            if (trim(string) == "sssss") then
                if (plan%inited()) call library%add_plan(plan)
                call plan%init(plans_length)
            else if (trim(string) == "xxxxx") then
                call library%add_plan(plan)
                exit
            else
                read(string, *) room_out, door_out, room_in
                call plan%add_step(room_out, door_out, room_in)
            end if
        end do

        close(lu)

        call library%refine()

    end subroutine from_file

    !>
    !> @brief do some simple refinements based on knowledge about labyrinth
    !>
    !> @param[in,out] library - library_t object
    !>
    !> @author foxtran
    !> @date   Sep 8, 2025
    !>
    subroutine refine(library)
        class(library_t), intent(inout) :: library
        integer :: plan_id, step_id, room_id, door_id, room_idx

        do plan_id = 1, size(library%plans)
            associate (room_out => library%plans(plan_id)%steps(1)%room_out, &
                       door_out => library%plans(plan_id)%steps(1)%door_out, &
                       room_in => library%plans(plan_id)%steps(1)%room_in, &
                       room => library%rooms(1))
                if (room%doors(door_out)%room == -1) &
                    room%doors(door_out)%rooms(room_in + 1::4) = .true._1
            end associate
            do step_id = 2, size(library%plans(plan_id)%steps)
                associate (room_out => library%plans(plan_id)%steps(step_id)%room_out, &
                           door_out => library%plans(plan_id)%steps(step_id)%door_out, &
                           room_in => library%plans(plan_id)%steps(step_id)%room_in)
                    do room_id = room_out + 1, size(library%rooms), 4
                        associate (room => library%rooms(room_id))
                            if (room%doors(door_out)%room == -1) &
                                room%doors(door_out)%rooms(room_in + 1::4) = .true._1
                        end associate
                    end do
                end associate
            end do
        end do
        do room_id = 1, size(library%rooms)
            associate (room => library%rooms(room_id))
                do door_id = lbound(room%doors, 1), ubound(room%doors, 1)
                    associate (door => room%doors(door_id))
                        if (count(door%rooms .eqv. .true._1) == 1) then
                            do room_idx = 1, size(door%rooms)
                                if (door%rooms(room_idx)) then
                                    door%room = room_idx
                                    exit
                                end if
                            end do
                        else if (count(door%rooms .eqv. .true._1) == 0) then
                            door%rooms = .true.
                        end if
                    end associate
                end do
            end associate
        end do
    end subroutine refine

    !>
    !> @brief prints structure of library to stdout
    !>
    !> @param[in] library - library_t object
    !>
    !> @author foxtran
    !> @date   Sep 8, 2025
    !>
    subroutine show(library)
        class(library_t), intent(in) :: library
        integer :: i
        do i = 1, size(library%rooms)
            write(6, '(A,I0,A)', advance = "no") 'room #', i, ':'
            call library%rooms(i)%show()
        end do
    end subroutine show

    !>
    !> @brief allows assignment without compiler bugs
    !>
    !> @param[out] lhs - library_t object for initialisation
    !> @param[in]  rhs - library_t object with data
    !>
    !> @author foxtran
    !> @date   Sep 8, 2025
    !>
    subroutine library_t_assignment(lhs, rhs)
        class(library_t), intent(out) :: lhs
        class(library_t), intent(in)  :: rhs
        integer :: i
        if (.not.rhs%inited) return
        lhs%inited = .true.
        allocate(lhs%rooms(size(rhs%rooms)))
        allocate(lhs%plans(size(rhs%plans)))
        do i = 1, size(lhs%rooms)
            lhs%rooms(i) = rhs%rooms(i)
        end do
        do i = 1, size(lhs%plans)
            lhs%plans(i) = rhs%plans(i)
        end do
        lhs%current_plan = rhs%current_plan
    end subroutine library_t_assignment

end module library_mod
