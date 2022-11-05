program exerC1
    IMPLICIT NONE
    real(8), parameter :: g = 10.0d0, pi = 4*atan(1.0d0)
    integer :: i
    real(8) :: T_sim, Dt, w0, w1, theta0, theta1, E, m, l

    open(10, file = 'exerC1_out.dat', status = 'unknown')
    read(*,*) theta0
    read(*,*) l
    read(*,*) m
    read(*,*) Dt
    read(*,*) T_sim !tempos em segundo e velocidade em m/s
    
    w0 = 0.0d0
    theta0 = theta0*(pi/180.0d0) !convertendo theta para radianos
    E = (m*(w0**2)*(l**2))/2.0d0 + m*l*(1-cos(theta0))*g
    
    write(10,*) 0.0d0, theta0, E
    
    do i = 1, int(T_sim/Dt)
      w1 = w0 - ((g/l)*theta0)*Dt
      theta1 = theta0 + w0*Dt
      w0 = w1
      
      if (theta1 > pi) then
        theta1 = theta1 - (2.0d0*pi)
      else
        if (theta1 < -pi) then
          theta1 = theta1 + (2.0d0*pi)
        end if
      end if
      
      theta0 = theta1
      E = (m*(w1**2)*(l**2))/2.0d0 + m*l*(1-cos(theta1))*g

      write(10,*) i*Dt, theta1, E

    end do
    
    close(10)
end program