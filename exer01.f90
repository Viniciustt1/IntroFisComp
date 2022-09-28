program exer01
implicit none
real(4) :: a = 1.0e0 !precisão simples
real(8) :: b = 1.0d0 !precisão dupla
real(16) :: c = 1.0_16 !precisão quadrupla
integer :: n, v, u

! PRECISÃO SIMPLES
print*, 'PRECISÃO SIMPLES'
do n = 1, 1000

 if (1 .ne. 1 + a) then
  a = a/2
  else
    exit
    end if
print*, a, a+1
end do

 ! PRECISÃO DUPLA
print*, 'PRECISÃO DUPLA'
do v = 1, 1000

 if (1 .ne. 1 + b) then
  b = b/2
  else
    exit
    end if
print*, b, b+1
end do

 ! PRECISÃO QUADRUPLA
print*, 'PRECISÃO QUADRUPLA'
do u = 1, 1000

 if (1 .ne. 1 + c) then
  c = c/2
  else
    exit
    end if
print*, c, c+1
end do

print*, n-1, a !bits, precisão

print*, v-1, b !bits, precisão

print*, u-1, c !bits, precisão

end program 
