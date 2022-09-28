program exer02
    implicit none
    real :: fat
    real(4) :: sin_s = 0.e0, x1 = 0.1e0, y1 = 0.2e0, z1 = 0.3e0, w1 = 0.4e0, S1, last_S1
    real(8) :: sin_d = 0.d0, x2 = 0.1e0, y2 = 0.2e0, z2 = 0.3e0, w2 = 0.4e0, S2, last_S2
    integer :: n, i

    !PRECISÃO SIMPLES   

    ! x = 0.1
    n = 1
    do
     fat = 1
     do i = 1, 2*n - 1
      fat = fat*i
     end do

     S1 = (((-1)**(n+1)) * (x1**(2*n-1))) / fat
     if (sin_s .ne. sin_s + S1) then
      last_S1 = S1
      sin_s = sin_s + S1
      n = n + 1
     else 
      exit
     end if

    end do
    print*, x1, abs(last_S1)/sin_s

    ! x = 0.2

    sin_s = 0.e0
    n = 1
    do
     fat = 1
     do i = 1, 2*n - 1
      fat = fat*i
     end do

     S1 = (((-1)**(n+1)) * (y1**(2*n-1))) / fat
     if (sin_s .ne. sin_s + S1) then
      last_S1 = S1
      sin_s = sin_s + S1
      n = n + 1
     else 
      exit
     end if

    end do

    print*, y1, abs(last_S1)/sin_s

    ! x = 0.3
    n = 1
    sin_s = 0.e0

    do
     fat = 1
     do i = 1, 2*n - 1
      fat = fat*i
     end do

     S1 = (((-1)**(n+1)) * (z1**(2*n-1))) / fat
     if (sin_s .ne. sin_s + S1) then
      last_S1 = S1
      sin_s = sin_s + S1
      n = n + 1
     else 
      exit
     end if

    end do

    print*, z1, abs(last_S1)/sin_s

    ! x = 0.4

    sin_s = 0.e0
    n = 1

    do
     fat = 1
     do i = 1, 2*n - 1
      fat = fat*i
     end do

     S1 = (((-1)**(n+1)) * (w1**(2*n-1))) / fat
     if (sin_s .ne. sin_s + S1) then
      last_S1 = S1
      sin_s = sin_s + S1
      n = n + 1
     else 
      exit
     end if

    end do

    print*, w1, abs(last_S1)/sin_s

    ! ---------------------------------------------------------------------------------------------------

    !PRECISÃO DUPLA   

    ! x = 0.1
    n = 1
    do
     fat = 1
     do i = 1, 2*n - 1
      fat = fat*i
     end do

     S2 = (((-1)**(n+1)) * (x2**(2*n-1))) / fat
     if (sin_d .ne. sin_d + S2) then
      last_S2 = S2
      sin_d = sin_d + S2
      n = n + 1
     else 
      exit
     end if

    end do
    print*, x2, abs(last_S2)/sin_d

    ! x = 0.2

    sin_d = 0.d0
    n = 1
    do
     fat = 1
     do i = 1, 2*n - 1
      fat = fat*i
     end do

     S2 = (((-1)**(n+1)) * (y2**(2*n-1))) / fat
     if (sin_d .ne. sin_d + S2) then
      last_S2 = S2
      sin_d = sin_d + S2
      n = n + 1
     else 
      exit
     end if

    end do

    print*, y2, abs(last_S2)/sin_d

    ! x = 0.3
    n = 1
    sin_d = 0.d0

    do
     fat = 1
     do i = 1, 2*n - 1
      fat = fat*i
     end do

     S2 = (((-1)**(n+1)) * (z2**(2*n-1))) / fat
     if (sin_d .ne. sin_d + S2) then
      last_S2 = S2
      sin_d = sin_d + S2
      n = n + 1
     else 
      exit
     end if

    end do

    print*, z2, abs(last_S2)/sin_d

    ! x = 0.4

    sin_s = 0.e0
    n = 1

    do
     fat = 1
     do i = 1, 2*n - 1
      fat = fat*i
     end do
     
     
     S2 = (((-1)**(n+1)) * (w2**(2*n-1))) / fat
     if (sin_d .ne. sin_d + S2) then
      last_S2 = S2
      sin_d = sin_d + S2
      n = n + 1
     else 
      exit
     end if

    end do

    print*, w2, abs(last_S2)/sin_d

end program