module solver_mod
    use library_mod, only: library_t
    use plan_mod, only: plan_t
    implicit none
    private
    type :: solver_t
        logical(1) :: inited = .false.
        type(library_t) :: library
    contains
        procedure :: init
    end type solver_t
    public :: solver_t
contains
    subroutine init(solver, filename)
        class(solver_t), intent(inout) :: solver
        character(len=*), intent(in) :: filename

        integer :: lu, ios
        character(len=32) :: string

        integer :: n_rooms, n_plans, plans_length
        integer(1) :: room_out, door_out, room_in

        type(plan_t) :: plan

        if (solver%inited) return
        solver%inited = .true._1

        open(newunit = lu, file = filename, status = "old", action = "read")

        read(lu, *) n_rooms, n_plans, plans_length

        call solver%library%init(n_rooms, n_plans)

        do
            read(lu, '(A)', iostat=ios) string
            if (ios /= 0) then
                print *, "Badly formatted file"
                exit
            end if
            if (trim(string) == "sssss") then
                if (plan%inited()) call solver%library%add_plan(plan)
                call plan%init(plans_length)
            else if (trim(string) == "xxxxx") then
                call solver%library%add_plan(plan)
                exit
            else
                read(string, *) room_out, door_out, room_in
                call plan%add_step(room_out, door_out, room_in)
            end if
        end do

        close(lu)

    end subroutine init
end module solver_mod
