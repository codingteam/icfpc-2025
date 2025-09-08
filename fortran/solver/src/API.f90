module API_mod
    use task_mod, only: task_t
    use plan_mod, only: plan_t
    implicit none
    private

    public :: select, explore, guess
contains

    !>
    !> @brief choose new task on server
    !>
    !> @param[in] task - selected task
    !>
    !> @author foxtran
    !> @date   Sep 8, 2025
    !>
    subroutine select(task)
        type(task_t), intent(in) :: task

        character(len=:), allocatable :: command, header, request, data

        integer :: lu

        open(newunit = lu, file = "select", status = "unknown")
        close(lu, status = "delete")

        header = ' --header "Content-Type: application/json" '
        request = ' --request POST '
        data = ' --data ''{ "id": "' // task%API_ID // '", "problemName": "' // task%name // '" }'' '

        command = 'curl -s ' // header // request // data // task%API_URL // '/select > select'
        call execute_command_line(command, wait=.true.)

        open(newunit = lu, file = "select", status = "old")
        block
            integer :: idx
            data = repeat(" ", 1024)
            read(lu, '(A)') data
            idx = index(data, ':')
            data = data(idx+2:)
            idx = index(data, '"')
            data = data(:idx-1)
            write(6, '("[INFO] selected task: ",A)') data
        end block
        close(lu, status = "delete")
    end subroutine select

    !>
    !> @brief explore task on server
    !>
    !> @note plans does not implement method to_json, so its serialiser is inside
    !>
    !> @param[in] task  - selected task
    !> @param[in] plans - routes to walk
    !>
    !> @author foxtran
    !> @date   Sep 8, 2025
    !>
    subroutine explore(task, plans)
        type(task_t), intent(in) :: task
        type(plan_t), intent(inout) :: plans(:)

        character(len=:), allocatable :: command, header, request, data, data_plans
        character(len=1) :: tmp

        integer :: lu, i, j

        open(newunit = lu, file = "explore", status = "unknown")
        close(lu, status = "delete")

        header = ' --header "Content-Type: application/json" '
        request = ' --request POST '

        data_plans = '['
        do i = 1, size(plans)
            data_plans = data_plans // '"'
            do j = 1, size(plans(i)%steps)
                write(tmp, '(I0)') plans(i)%steps(j)%door_out
                data_plans = data_plans // tmp
            end do
            data_plans = data_plans // '"'
            if (i /= size(plans)) data_plans = data_plans // ', '
        end do
        data_plans = data_plans // ']'

        data = ' --data ''{ "id": "' // task%API_ID // '", "plans": ' // data_plans // ' }'' '

        command = 'curl -s ' // header // request // data // task%API_URL // '/explore > explore'
        call execute_command_line(command, wait=.true.)

        open(newunit = lu, file = "explore", status = "old")
        block
            integer :: idx
            integer(1), allocatable :: rooms(:)
            data_plans = repeat(" ", 1024*1024)
            read(lu, '(A)') data_plans
            idx = index(data_plans, '[')
            data_plans = data_plans(idx+1:)
            idx = index(data_plans, ']', back=.true.)
            data_plans = data_plans(:idx)
            do i = 1, size(plans)
                data = data_plans(index(data_plans, '['):index(data_plans, ']'))
                data = data(2:len(data)-1)
                do j = 1, len(data)
                    if (data(i:i) == ",") data(i:i) = " "
                end do

                allocate(rooms(size(plans(i)%steps) + 1))

                call plans(i)%reset()
                read(data, *) rooms
                do j = 1, size(rooms) - 1
                    call plans(i)%add_step(rooms(j), plans(i)%steps(j)%door_out, rooms(j+1))
                end do

                deallocate(rooms)

                data_plans = data_plans(2:)
                idx = index(data_plans, '[')
                if (idx > 0) data_plans = data_plans(idx:)
            end do

        end block
        close(lu, status = "delete")
    end subroutine explore

    !>
    !> @brief submit solution to server
    !>
    !> @param[in] task     - selected task
    !> @param[in] solution - solution
    !>
    !> @author foxtran
    !> @date   Sep 8, 2025
    !>
    subroutine guess(task, solution)
        type(task_t), intent(in) :: task
        character(len=*), intent(in) :: solution

        character(len=:), allocatable :: command, header, request, data

        integer :: lu

        open(newunit = lu, file = "guess", status = "unknown")
        close(lu, status = "delete")

        header = ' --header "Content-Type: application/json" '
        request = ' --request POST '
        data = ' --data ''{ "id": "' // task%API_ID // '", ' // solution // ' }'' '

        command = 'curl -s ' // header // request // data // task%API_URL // '/guess > guess'
        call execute_command_line(command, wait=.true.)

        open(newunit = lu, file = "guess", status = "old")
        block
            integer :: idx
            data = repeat(" ", 1024)
            read(lu, '(A)') data
            print '(A,A)', 'Resolution: ', trim(data)
        end block
        close(lu, status = "delete")
    end subroutine guess
end module API_mod
