program exer02
    implicit none
    real :: sin = 0, fat, x, y, z, w, S, last_S
    integer :: n, i

    x = 0.1
    y = 0.2
    z = 0.3
    w = 0.4

    ! x = 0.1
    n = 1
    do
     fat = 1
     do i = 1, 2*n - 1
      fat = fat*i
     end do

     S = (((-1)**(n+1)) * (x**(2*n-1))) / fat
     if (sin .ne. sin + S) then
      last_S = S
      sin = sin + S
      n = n + 1
     else 
      exit
     end if

    end do
    print*, x, abs(last_S)/sin

    ! x = 0.2

    sin = 0
    n = 1
    do
     fat = 1
     do i = 1, 2*n - 1
      fat = fat*i
     end do

     S = (((-1)**(n+1)) * (y**(2*n-1))) / fat
     if (sin .ne. sin + S) then
      last_S = S
      sin = sin + S
      n = n + 1
     else 
      exit
     end if

    end do

    print*, y, abs(last_S)/sin

    ! x = 0.3
    n = 1
    sin = 0

    do
     fat = 1
     do i = 1, 2*n - 1
      fat = fat*i
     end do

     S = (((-1)**(n+1)) * (z**(2*n-1))) / fat
     if (sin .ne. sin + S) then
      last_S = S
      sin = sin + S
      n = n + 1
     else 
      exit
     end if

    end do

    print*, z, abs(last_S)/sin

    ! x = 0.4

    sin = 0
    n = 1

    do
     fat = 1
     do i = 1, 2*n - 1
      fat = fat*i
     end do

     S = (((-1)**(n+1)) * (w**(2*n-1))) / fat
     if (sin .ne. sin + S) then
      last_S = S
      sin = sin + S
      n = n + 1
     else 
      exit
     end if

    end do

    print*, w, abs(last_S)/sin

end program