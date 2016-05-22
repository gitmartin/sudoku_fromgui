!!! sudoku solver, by Martin C.
!also check sudoku python script for input stuff

! Sudoku program. Inputs, a file with a line like
! 3..5.6.7...8.. where . are blanks. Uses python script for input.
! can get files from sudoku-solutions.com
program sud
    implicit none
    integer :: row,col,i
    integer :: m(9,9) = 0
    call read_sud(m, 9)
    print*, 'Original: '
    call print_sud(m,9)
    print*,
    call brute_force_solve(m,1)
end program sud

recursive subroutine brute_force_solve(m,rl)
    implicit none
    integer :: m(9,9), n(9,9)
    integer :: row, col, i,rl,count_zeros !rl = recursion level
    logical :: backtrack, error
    print*,'recursion level ', rl
    backtrack = .false.
    do row = 1,9
        do col = 1,9
            if (m(row,col)==0) then
                do i = 1,9
                    if (.not. error(m,row,col,i)) then
                        n = m ! make a copy in Fortran
                        n(row, col) = i
                        call brute_force_solve(n,rl+1) ! recursion
                    end if
                end do
                backtrack = .true.
                exit ! exit inner do loop
            end if
        end do
        if (backtrack) exit ! exit outer loop
    end do

    call count_z(m, count_zeros)
    if (count_zeros == 0) then
        print*,
        print*,'Found a solution!'
        call print_sud(m,9)
        stop
    end if

end subroutine brute_force_solve

! count the number of zeros (blanks) in the sudoku. if == 0 we're done!
subroutine count_z(m, nz)
    integer :: m(9,9),nz,r,c
    nz = 0
    do r = 1,9
        do c = 1,9
            if (m(r,c)==0) nz = nz +1
        end do
    end do
end subroutine count_z

! check if there is an error in the sudoku, ie
! same number in col, row or box
! m is the sudoku, r row, c col, i candidate number in m(r,c)
function error(m,r,c,i)
    implicit none
    integer:: r,c
    integer :: m(9,9)
    integer :: i,j
    logical :: in_square, error
    error = .false.
    do j = 1,9
        if (m(r,j) == i)  error = .true.
        if (m(j,c) == i) error = .true.
    end do
    call check_in_square(m, r,c,i, in_square)
    if (in_square) error = .true.
end function error

!  1 2 3   9 square of 9
!  4 5 6
!  7 8 9
! is the number i already in the square?
subroutine check_in_square(m,r,c,i, in_square)
    implicit none
    integer:: square_number =0
    integer :: r,c,i
    integer :: m(9,9)
    logical :: in_square

    if (r<=3 .and. c<=3) square_number = 1
    if (r<=3 .and. c<=6 .and. c>=4) square_number = 2
    if (r<=3 .and. c<=9 .and. c>=7) square_number = 3
    if (r<=6 .and. r>=4 .and. c<=3) square_number = 4
    if (r<=6 .and. r>=4 .and. c<=6 .and. c>=4) square_number = 5
    if (r<=6 .and. r>=4 .and. c<=9 .and. c>=7) square_number = 6
    if (r<=9 .and. r>=7 .and. c<=3) square_number = 7
    if (r<=9 .and. r>=7 .and. c<=6 .and. c>=4) square_number = 8
    if (r<=9 .and. r>=7 .and. c<=9 .and. c>=7) square_number = 9

    in_square = .false.

    if (square_number ==0 ) print*, 'FATAL PROBLEM'

    if (square_number == 1) then
        if (m(1,1) ==i .or. m(1,2) ==i .or. m(1,3) ==i .or. m(2,1) ==i .or. m(2,2) ==i.or. m(2,3) ==i.or.&
            m(3,1) ==i .or. m(3,2) ==i .or. m(3,3) ==i) then
            in_square = .true.
        end if
    elseif (square_number == 2) then
        if (m(1,4) ==i .or. m(1,5) ==i .or. m(1,6) ==i .or. m(2,4) ==i .or. m(2,5) ==i.or. m(2,6) ==i.or.&
            m(3,4) ==i .or. m(3,5) ==i .or. m(3,6) ==i) then
            in_square = .true.
        end if
    elseif (square_number == 3) then
        if (m(1,7) ==i .or. m(1,8) ==i .or. m(1,9) ==i .or. m(2,7) ==i .or. m(2,8) ==i.or. m(2,9) ==i.or.&
            m(3,7) ==i .or. m(3,8) ==i .or. m(3,9) ==i) then
            in_square = .true.
        end if
    elseif (square_number == 4) then
        if (m(4,1) ==i .or. m(4,2) ==i .or. m(4,3) ==i .or. m(5,1) ==i .or. m(5,2) ==i.or. m(5,3) ==i.or.&
            m(6,1) ==i .or. m(6,2) ==i .or. m(6,3) ==i) then
            in_square = .true.
        end if
    elseif (square_number == 5) then
        if (m(4,4) ==i .or. m(4,5) ==i .or. m(4,6) ==i .or. m(5,4) ==i .or. m(5,5) ==i.or. m(5,6) ==i.or.&
            m(6,4) ==i .or. m(6,5) ==i .or. m(6,6) ==i) then
            in_square = .true.
        end if

    elseif (square_number == 6) then
        if (m(4,7) ==i .or. m(4,8) ==i .or. m(4,9) ==i .or. m(5,7) ==i .or. m(5,8) ==i.or. m(5,9) ==i.or.&
            m(6,7) ==i .or. m(6,8) ==i .or. m(6,9) ==i) then
            in_square = .true.
        end if
    elseif (square_number == 7) then
        if (m(7,1) ==i .or. m(7,2) ==i .or. m(7,3) ==i .or. m(8,1) ==i .or. m(8,2) ==i.or. m(8,3) ==i.or.&
            m(9,1) ==i .or. m(9,2) ==i .or. m(9,3) ==i) then
            in_square = .true.
        end if
    elseif (square_number == 8) then
        if (m(7,4) ==i .or. m(7,5) ==i .or. m(7,6) ==i .or. m(8,4) ==i .or. m(8,5) ==i.or. m(8,6) ==i.or.&
            m(9,4) ==i .or. m(9,5) ==i .or. m(9,6) ==i) then
            in_square = .true.
        end if

    elseif (square_number == 9) then
        if (m(7,7) ==i .or. m(7,8) ==i .or. m(7,9) ==i .or. m(8,7) ==i .or. m(8,8) ==i.or. m(8,9) ==i.or.&
            m(9,7) ==i .or. m(9,8) ==i .or. m(9,9) ==i) then
            in_square = .true.
        end if
    end if
end subroutine check_in_square

subroutine read_sud(m,n)
    implicit none
    integer::n,row,col
    integer :: m(n,n)
    integer :: i
    call system('./readsudoku.py sudoku24773.txt') !!!!!!!!!! input txt file
    open(10,file='/home/martin/Downloads/sud.txt')
    read(10,*) m
    m = transpose(m)
end subroutine read_sud

subroutine print_sud(m,n) ! n is size (square)
    implicit none
    integer::n,row,col
    integer::m(n,n)
    write(*,*)'______________________________'

    do row=1,3
        write(*,1)  m(row,1),m(row,2),m(row,3),'|', &
            m(row,4),m(row,5),m(row,6),'|', &
            m(row,7),m(row,8),m(row,9)
    end do
    write(*,*)'-------------------'
    do row=4,6
        write(*,1)  m(row,1),m(row,2),m(row,3),'|', &
            m(row,4),m(row,5),m(row,6),'|', &
            m(row,7),m(row,8),m(row,9)
    end do
    write(*,*)'-------------------'
    do row=7,9
        write(*,1)  m(row,1),m(row,2),m(row,3),'|', &
            m(row,4),m(row,5),m(row,6),'|', &
            m(row,7),m(row,8),m(row,9)
    end do

1   format(I2,I2,I2,A,I2,I2,I2,A,I2,I2,I2)
end subroutine print_sud
