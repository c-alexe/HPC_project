subroutine change_tag(track_tag, parent_module)
    character(len=13), intent(inout) :: track_tag
    character(len=10), intent(in) :: parent_module

    track_tag = trim(parent_module) // '(' //trim(track_tag) // ')'
end subroutine

subroutine change_intercept(old_c, m, module_x_position, module_y_position)
    real(kind=selected_real_kind(12)), intent(in) :: m, module_x_position, module_y_position
    real(kind=selected_real_kind(12)), intent(inout) :: old_c

    old_c = old_c + module_x_position*m - module_y_position
end subroutine

subroutine is_pair(m1, c1, q1, m2, c2, q2, r, answer)
    real(kind=selected_real_kind(12)), intent(in) :: m1, c1, m2, c2, r
    integer(kind=selected_int_kind(1)), intent(in) :: q1, q2
    real(kind=selected_real_kind(12)) :: intersection_x, intersection_y
    logical, intent(out) :: answer

    answer = .false.
    if (q1/=q2) then
        intersection_x=(c1-c2)/(m2-m1)
        intersection_y=(m2*c1-m1*c2)/(m2-m1)
        if (intersection_x**2 + intersection_y**2 < r**2) then
            answer = .true.
        end if
    end if
end subroutine

real(kind=selected_real_kind(12)) function invariant_mass(pt_1, m_1, pt_2, m_2)
    real(kind=selected_real_kind(12)), intent(in) :: pt_1, m_1, pt_2, m_2
    invariant_mass = sqrt(2*pt_1*pt_2*(cosh(log(tan(atan(abs(m_2)/2))/tan(atan(abs(m_1)/2))))-1))
end function invariant_mass

! Main program
program myproject
    implicit none

    type module_coordinates
        character(len=10) :: module_name
        real(kind=selected_real_kind(12)) :: x_coordinate, y_coordinate
    end type module_coordinates

    type tracks
        sequence
        character(len=13) :: track_name
        integer(kind=selected_int_kind(1)) :: q
        real(kind=selected_real_kind(12)) :: m, c, pt
    end type tracks
    
    integer, parameter :: k5 = selected_int_kind(5) 
    integer, parameter :: r64 = selected_real_kind(12)
    integer, parameter :: maximum_modules = 100
    integer, parameter :: MAXLINE = 256
    character(len=10) :: module_name, module_name_tbc, track_module_name
    character(len=10), dimension(maximum_modules) :: modules_to_be_changed, allowed_modules
    character(len=13) :: track_no, command 
    character(len=MAXLINE) :: filename_modules, filename_tracks
    integer :: iu_im, iu_l, iu_g, iu_p, iu_fm, ios, ios_read
    integer(kind=selected_int_kind(1)) :: track_charge
    integer(kind=k5) :: tracks_no, tracks_no_input, modules_tbc_no, modules_no, pairs_no, this_shift, i, j
    real(kind=r64) :: x, y, delta_x, delta_y
    real(kind=r64) :: track_m, track_c, track_pt, radius
    real(kind=r64), external :: invariant_mass
    type(module_coordinates), dimension(maximum_modules) :: module_shifts, modules_array
    type(tracks), dimension(:), allocatable :: tracks_array

    logical :: module_check = .false.
    logical :: exists, pair_status

    interface
        subroutine change_tag(track_tag, parent_module)
            character(len=13), intent(inout) :: track_tag
            character(len=10), intent(in) :: parent_module
        end subroutine
    end interface


    interface
        subroutine change_intercept(old_c, m, module_x_position, module_y_position)
            real(kind=selected_real_kind(12)), intent(in) :: m, module_x_position, module_y_position
            real(kind=selected_real_kind(12)), intent(inout) :: old_c
    end subroutine
    end interface

    interface
        subroutine is_pair(m1, c1, q1, m2, c2, q2, r, answer)
            real(kind=selected_real_kind(12)), intent(in) :: m1, c1, m2, c2, r
            integer(kind=selected_int_kind(1)), intent(in) :: q1, q2
            logical, intent(out) :: answer
        end subroutine is_pair
    end interface

    print *, "Filename modules:"
    read *, filename_modules
    inquire(file=filename_modules, exist=exists)
    if(.not.exists) then
        print *, 'Modules file does not exit'
        error stop 
    end if

    print *, "Filename tracks:"
    read *, filename_tracks
    inquire(file=filename_tracks, exist=exists)
    if(.not.exists) then
        print *, 'Tracks file does not exit'
        error stop 
    end if

    ! Find possible module names before inquiring user
    open(newunit=iu_im, file=filename_modules, status='old', iostat=ios, action='read')
    if (ios==0) then
        ios_read=0
        i=0
        do while(ios_read==0)
            read(iu_im, *, pad='no', iostat = ios_read, end=1) module_name, x, y
            i=i+1
            modules_array(i) = module_coordinates(module_name, x, y)
            allowed_modules(i) = module_name
1       end do
        modules_no = i
        close(iu_im)
    else 
        print *, 'ERROR while opening modules file' 
        stop
    end if 

    ! Ask for new positions of modules
    modules_tbc_no = 0
    do i=1, modules_no
2       print*, "Do you want to change a module's position?"
        read*, command

        if(command == 'no' .or. command == 'n' .or. command == 'No' .or. command == 'N') go to 5

        print *, 'Which module?'
        read *, module_name_tbc
        call is_changed(module_name_tbc, allowed_modules, modules_no, module_check)
        if (module_check.eqv..false.) then 
            print *, "Not a possible module name, please try again"
            go to 2
        end if

3       print *, 'Change in x?'
        read *, delta_x
        if (delta_x >0.2 .or. delta_x <-0.2) then
            print*, 'The shift is too large, -0.2<= shift <=0.2'
            go to 3
        end if
        
4       print *, 'Change in y?'
        read *, delta_y
        if (delta_y >0.2 .or. delta_y <-0.2) then
            print*, 'The shift is too large, -0.2<= shift <=0.2'
            go to 4
        end if

        module_shifts(i) = module_coordinates(module_name_tbc, delta_x, delta_y)
        modules_to_be_changed(i) = module_name_tbc  

        modules_tbc_no = modules_tbc_no + 1 
    end do 

5   print *, 'What is the radius?'
    read *, radius 
    if (radius > 0.5) then
        print *, 'Radius is too large, 0 < radius < 0.5'
        go to 5
    end if

    ! Modify initial modules
    open(newunit=iu_fm, file='final_modules.txt', status='new', iostat=ios)
    if (ios==0) then
        do i=1, modules_no
            call is_changed(allowed_modules(i), modules_to_be_changed, modules_tbc_no, module_check)
            if (module_check.eqv..false.) then 
                write(iu_fm, '(a,f10.4,f10.4)') modules_array(i)%module_name, modules_array(i)%x_coordinate, &
                modules_array(i)%y_coordinate
            else
                do j=1, modules_tbc_no
                    if (allowed_modules(i)==modules_to_be_changed(j)) then
                        this_shift=j
                    end if
                end do    
                write(iu_fm, '(a,f10.4,f10.4)') modules_array(i)%module_name, &
                modules_array(i)%x_coordinate + module_shifts(this_shift)%x_coordinate, &
                modules_array(i)%y_coordinate + module_shifts(this_shift)%y_coordinate

                modules_array(i) = module_coordinates(modules_array(i)%module_name, modules_array(i)%x_coordinate + &
                module_shifts(this_shift)%x_coordinate, modules_array(i)%y_coordinate + module_shifts(this_shift)%y_coordinate)
            end if
        end do
        close(iu_fm)
    else 
        print *, 'ERROR while creating output file "final_modules.txt", check if name is already in use'
        stop
    end if

    ! Read and process tracks information
    open(newunit=iu_l, file=filename_tracks, status='old', iostat=ios)
    if (ios==0) then
        open(newunit=iu_g, file='global_tracks.txt', iostat=ios, status='new')
        if (ios==0) then
            read(iu_l, *, iostat = ios_read) tracks_no_input
            if (ios_read/=0) then
                print *, "Cannot read total number of tracks"
                stop
            end if
            allocate(tracks_array(tracks_no_input))
            tracks_no=0 
            ios_read=0
            do while(ios_read==0)
                read(iu_l, *, iostat = ios_read, end=6) track_module_name, track_no, track_charge, track_m, track_c, track_pt
                tracks_no = tracks_no + 1
                call change_tag(track_no, track_module_name)
                do i=1, modules_no 
                    if (track_module_name==modules_array(i)%module_name) then
                        ! Transform local y intercept to global coordinates
                        call change_intercept(track_c, track_m, modules_array(i)%x_coordinate, modules_array(i)%y_coordinate)
                    end if
                end do
                write(iu_g, '(a,i3,f15.4,f15.4,f15.4,f15.4)') track_no, track_charge, track_m, track_c, track_pt
                tracks_array(tracks_no) = tracks(track_no, track_charge, track_m, track_c, track_pt)
6           end do
            if (tracks_no/=tracks_no_input) print*, "Warning: the given number of tracks does not correspond to input"
            close(iu_g)
        else 
            print *, 'ERROR while creating output file "global_tracks.txt", check if name is already in use file'
            stop
        end if
        close(iu_l)
    else 
        print *, 'ERROR while opening tracks file'
        stop
    end if 

    ! Look for pairs
    open(newunit=iu_p, file='pairs.txt', iostat=ios, status='new')
    if (ios==0) then
        write(iu_p, '(a13,a13,a20)') "Track 1      ", "Track 2      ","Invariant mass [GeV]"
        pairs_no=0
        do i=1, tracks_no
            do j=i+1, tracks_no 
                call is_pair(tracks_array(i)%m, tracks_array(i)%c, tracks_array(i)%q, &
                 tracks_array(j)%m, tracks_array(j)%c, tracks_array(j)%q, radius, pair_status)
                if (pair_status.eqv..true.) then
                    pairs_no = pairs_no + 1
                    write(iu_p, '(a,a)', advance='no') tracks_array(i)%track_name, tracks_array(j)%track_name 
                    write(iu_p, '(f15.4)') invariant_mass(tracks_array(i)%pt, tracks_array(i)%m, tracks_array(j)%pt, &
                    tracks_array(j)%m)
                end if
            end do
        end do
        write(*, '(a,i0)') "Pairs found: ", pairs_no
        close(iu_p)
    else 
        print *, 'ERROR while creating output file "pairs.txt", check if name is already in use'      
        stop  
    end if

    contains
        subroutine is_changed(current_module, module_array, array_size, answer)
            character(len=10), intent(in) :: current_module
            ! in presentation say it s internal for the array size
            character(len=10), dimension(maximum_modules), intent(in) :: module_array
            integer, intent(in) :: array_size
            logical, intent(out) :: answer
            integer :: k

            answer = .false. 
            do k=1, array_size
                if (current_module==module_array(k)) then
                    answer = .true.
                end if
            end do
        end subroutine is_changed
end program myproject
