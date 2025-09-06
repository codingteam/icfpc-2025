module library_mod
    use room_mod, only: room_t
    use plan_mod, only: plan_t
    implicit none
    private
    type :: library_t
        logical(1) :: inited = .false.
        type(room_t), allocatable :: rooms(:)
        type(plan_t), allocatable :: plans(:)
        integer :: current_plan = 1
    contains
        procedure :: init
        procedure :: from_file
        procedure :: add_plan
        procedure :: refine
        procedure :: show
    end type library_t
    public :: library_t
contains
    subroutine init(library, n_rooms, n_plans)
        class(library_t), intent(inout) :: library
        integer, intent(in) :: n_rooms, n_plans
        integer :: i
        if (library%inited) return
        allocate(library%rooms(n_rooms))
        allocate(library%plans(n_plans))
        do i = 1, n_rooms
            call library%rooms(i)%init(n_rooms)
            library%rooms(i)%number = mod(i-1, 4)
        end do
        library%current_plan = 1
    end subroutine init
    subroutine add_plan(library, plan)
        class(library_t), intent(inout) :: library
        type(plan_t), intent(in) :: plan
        library%plans(library%current_plan) = plan
        library%current_plan = library%current_plan + 1
    end subroutine add_plan
    subroutine from_file(library, filename)
        class(library_t), intent(inout) :: library
        character(len=*), intent(in) :: filename

        integer :: lu, ios
        character(len=32) :: string

        integer :: n_rooms, n_plans, plans_length
        integer(1) :: room_out, door_out, room_in

        type(plan_t) :: plan

        if (library%inited) return

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
    subroutine refine(library)
        class(library_t), intent(inout) :: library
        integer :: plan_id, step_id, room_id

        do plan_id = 1, size(library%plans)
            do step_id = 1, size(library%plans(plan_id)%steps)
                associate (room_out => library%plans(plan_id)%steps(step_id)%room_out, &
                           door_out => library%plans(plan_id)%steps(step_id)%door_out, &
                           room_in => library%plans(plan_id)%steps(step_id)%room_in)
                    do room_id = room_out + 1, size(library%rooms), 4
                        associate (room => library%rooms(room_id))
                            room%doors(door_out)%rooms(room_in + 1::4) = .true._1
                        end associate
                    end do
                end associate
            end do
        end do
    end subroutine refine
    subroutine show(library)
        class(library_t), intent(in) :: library
        integer :: i
        do i = 1, size(library%rooms)
            write(6, '(A,I0,A)', advance = "no") 'room #', i, ':'
            call library%rooms(i)%show()
        end do
    end subroutine show

end module library_mod
