FUNCTION f(x)

real(8), intent(in) :: x
real(8) :: f 

f = exp(4*x)*cos(x/2)

END FUNCTION

program exerA
    IMPLICIT NONE
    real(8) :: f, x1, x2, x3, x4, f1, f2, f3
    real(8), parameter :: x = 0.5d0 
    integer :: n, i
    real(8), ALLOCATABLE, dimension(:) :: h,  ds_3, df_2, dt_2, dss_3, dss_5, dta_5

    f1 = (1.0d0/2)*exp(4*x)*((8.0d0)*cos(x/2)-sin(x/2)) !derivada primeira de f (cálculo direto)
    f2 = (1.0d0/4)*exp(4*x)*((63.0d0)*cos(x/2)-(16.0d0)*sin(x/2)) !derivada segunda de f (cálculo direto)
    f3 = (1.0d0/8)*exp(4*x)*((488.0d0)*cos(x/2)-(191.0d0)*sin(x/2)) !derivada terceira de f (cálculo direto)
    
    open(3, file = "tabA_in.dat", status = "old")
    read(3,*) n
    allocate(h(n), ds_3(n), df_2(n), dt_2(n), dss_3(n), dss_5(n), dta_5(n))
    read(3,*) (h(i), i = 1, n)
    close(3)

    
    do i = 1, n
      x1 = x + h(i) 
      x2 = x + 2.0d0*h(i)
      x3 = x - h(i)
      x4 = x - 2.0d0*h(i)
      
      !os valores abaixo fornecem o desvio entre os valores:

      ds_3(i) = abs(((f(x1) - f(x3))/2.0d0*h(i)) - f1) !derivada simétrica de 3 pontos
      df_2(i) = abs(((f(x1) - f(x))/h(i)) - f1) !derivada para frente de 2 pontos
      dt_2(i) = abs(((f(x) - f(x3))/h(i)) - f1) !derivada para trás de 2 pontos
      dss_3(i) = abs(((f(x1) - 2.0d0*f(x) + f(x3))/h(i)**2) - f2) !derivada segunda simétrica de 3 pontos
      dss_5(i) = abs(((-f(x2) + 16.0d0*f(x1) - 30.0d0*f(x) + 16.0d0*f(x3) - f(x4))/12.0d0*(h(i)**2)) - f2) !derivada segunda simétrica de 5 pontos
      dta_5(i) = abs(((f(x2) - 2.0d0*f(x1) + 2.0d0*f(x3) - f(x4))/2.0d0*(h(i)**3)) - f3) ! derivada terceir anti-simétrica de 5 pontos

    end do

    open(4, file = "tabA_out.dat", status = "replace")
    do i = 1, n
      write(4,*) h(i), ds_3(i), df_2(i), dt_2(i), dss_3(i), dss_5(i), dta_5(i)
    end do
    close(4)
end program