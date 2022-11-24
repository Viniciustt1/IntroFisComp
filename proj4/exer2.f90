program exer2
    implicit none

    real(8) :: lambda, dt, t_med 
    integer, dimension(:), allocatable :: N !matriz que armazena os estados dos átomos
    real(8), dimension(:), allocatable :: tempo_vida !matriz que conta o tempo de vida
    integer :: i, j, N_0, N_t
    real(8), parameter :: t = 10.0d0

    
    read(*,*) N_0
    read(*,*) dt
    read(*,*) lambda

    allocate(N(N_0), tempo_vida(N_0)) 

    do j = 1, N_0
        N(j) = 1
        tempo_vida(j) = 0.0d0 
    end do

    do i = 0, int(t/dt)
        N_t = 0
        do j = 1, N_0

            if (rand() < lambda*dt .and. i /= 0) then
                N(j) = 0
            end if

            N_t = N_t + N(j)

            if (N(j) == 1) then
                tempo_vida(j) = tempo_vida(j) + dt 
            end if

        end do

    end do

    close(1)

    do j = 1, N_0
        t_med = t_med + tempo_vida(j)
    end do

    t_med = t_med/N_0 !tempo de vida médio

    print*, t_med, (1/lambda) 

end program 