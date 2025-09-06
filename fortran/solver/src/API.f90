module API_mod
    use task_mod, only: task_t
    implicit none
    private
    public :: select
contains
    subroutine select(task)
        type(task_t), intent(in) :: task

        character(len=:), allocatable :: command, header, request, data

        integer :: lu

        open(newunit = lu, file = "select", status = "unknown")
        close(lu, status = "delete")

        header = ' --header "Content-Type: application/json" '
        request = ' --request POST '
        data = ' --data ''{ "id": "' // task%API_ID // '", "problemName": "probatio" }'' '

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
end module API_mod
