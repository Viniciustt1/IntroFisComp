contains   
module function
    implicit none
    function esc_prod(x, escalar, n) !função que cálcula o produto escalar de dois vetores

    real, dimension(n,1), intent(in) :: x, y
    real :: escalar 
    integer :: i, n 
    
    escalar = 0

    do i = 1, n 
     escalar = escalar + x(n,1)*y(n,i) !cálculo do produto escalar
    end do
 
    end function

    function prod(A, B) !função para a multiplicação de matrizes
    real, dimension(n,n), intent(in) :: A
    real, dimension(n,1) :: prod, B
    integer :: i, j
    
   
    do i = 1, n 
      do j = 1, n 
       prod(i,1) = 0
       prod(i,1) = prod(i,1) + A(i,j)*B(j,1)
      end do
    end do
  
    end function

end module
      

program exer05
    IMPLICIT NONE
    real :: lbd, e !(erro)
    integer, parameter :: dp = kind(0.d0)
    real(kind = dp), allocatable, dimension(:,:) :: M
    real, dimension(:,:), allocatable :: x, u
    integer :: i, j, n
    
    print*, "Entre com o erro: \e"
    read(*,*) e

    print*, "Entre com a dimensão da matriz: \n "
    read(*,*) n !lendo a dimensão da matriz
    allocate(M(n,n), x(n,1), u(n,1)) !alocando as matrizes
    
    print*, "Entre com a linha da matriz: \n"
    do i = 1, n
      read(*,*) (M(i,j), j = 1, n) !escrevendo as linhas da matriz
    end do
   
    do i = 1, n !gerando os vetores de teste x e u 
      x(i, 1) = i
      u(i, 1) = i+1
    end do

    do
      u = prod(M, x) !definindo u como o produto da matriz M com a matriz x

      if (abs(lbd - esc_prod(x, u, n))/esc_prod(x, x, n) > e) then
        lbd = esc_prod(x, u, n)/esc_prod(x, x, n)
        x = u
      else
        exit
      end if

    print*, lbd
    print*, x

end program



