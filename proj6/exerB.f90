PROGRAM exerB
  IMPLICIT NONE

 
  REAL(8) :: dt, t, r, d_j, d_tj, fat_mul     
  REAL(8) :: v_0, x_0, y_0, x, y, x_i, y_i                                             !Condições para o planeta Terra
  REAL(8) :: v0_j, x0_j, y0_j, x_j, y_j, xi_j, yi_j                                    !Condições para Júpiter
  REAL(8), PARAMETER :: GMs = 4*((4*ATAN(1.0d0))**2)                                   !Constante gravitacional associada ao Sol
  REAL(8), PARAMETER :: GMj = (GMs*(1.9d0*10e27))/(2.0d0*10e30)                        !Constante gravitacional associada à Júpiter
  REAL(8), PARAMETER :: GMt = (GMs*(6.0d0*10e24))/(2.0d0*10e30)                        !Constante gravitacional associada à Terra
  INTEGER :: i                                                                         !Variável de loop

  
  dt = 0.001d0
  t = 0.0d0

  !Introduzindo as condições iniciais para o planeta Terra, temos os seguintes dados:
  r = 1.0d0         !Valor de distância entre a Terra e o Sol
  v_0 = 2*(4*ATAN(1.0d0))   !Velocidade inicial, de acordo com a força centrípeta

  x_0 = r
  y_0 = 0.0d0

  x = r
  y = 0.0d0
  y = y_0 + (v_0*dt)  !Espaço percorrido de maneira uniforme

  !Para as condições iniciais de Júpiter, teremos os seguintes dados:
  d_j = 5.2d0                                 !Valor para a distância entre Júpiter e Sol
  v0_j = sqrt(4*((4*ATAN(1.0d0))**2) / d_j)   !Valor para a velocidade de órbita de Júpiter

  x0_j = d_j
  y0_j = 0.0d0

  x_j = d_j
  y_j = 0.0d0
  y_j = y0_j + (v0_j*dt)   !Espaço percorrido de maneira uniforme

  !Abrindo os arquivos para a aplicação dos dados coletados:
  OPEN(17, file = 'trajB_out.dat', status = 'unknown')
  !OPEN(5, file = 'jupiterB_out.dat')
  WRITE(*,*) 'Insira abaixo um valor positivo para o fator multiplicativo da massa de Júpiter:'
  READ(*,*) fat_mul

  !Iniciando o loop para a construção do movimento orbital, teremos:
  DO i = 1, 20000
    !Definindo as trajetórias e os traçados de órbita
    r = sqrt((x**2) + (y**2))
    d_j = sqrt((x_j**2) + (y_j**2))
    d_tj = sqrt(((x - x_j)**2) + ((y - y_j)**2))

    !Aplicando a ideia numérica de Verlet, teremos, para Júpiter:
    yi_j = 2*y_j - y0_j - (((dt**2)*GMs*y_j) / (d_j**3)) - (((dt**2)*GMt*(x_j - x)) / (d_tj**3))
    xi_j = 2*x_j - x0_j - (((dt**2)*GMs*x_j) / (d_j**3)) - (((dt**2)*GMt*(y_j - y)) / (d_tj**3))

    !Aplicando, para a Terra, a ideia de Verlet:
    y_i = 2*y - y_0 - (((dt**2)*GMs*y) / (r**3)) - (((dt**2)*fat_mul*GMj*(x - x_j)) / (d_tj**3))
    x_i = 2*x - x_0 - (((dt**2)*GMs*x) / (r**3)) - (((dt**2)*fat_mul*GMj*(y - y_j)) / (d_tj**3))

    y_0 = y
    x_0 = x
    x = x_i
    y = y_i

    y0_j = y_j
    x0_j = x_j
    x_j = xi_j
    y_j = yi_j

    t = t + dt

    !Aplicando os dados obtidos no arquivo aberto, teremos.
    WRITE(17,*) t, x_i, y_i
    !WRITE(5,*) xi_j, yi_j
  END DO

  WRITE(*,*) 'A partir do fator multiplicativo fornecido como parâmetro de entrada, somos capazes de gerar alterações ', &
  'no comportamento da órbita da Terra. Dessa forma, para valores abaixo de 100, tal variação é extremamente local ', &
  'sendo praticamente imperceptível. No entanto, para valores entre 100 e 900, notaremos uma deformação cada vez ', &
  'maior da trajetória do nosso planeta, até que, para valores entre 950 e 1000, esse seja retirado de sua órbita e ', &
  'jogado para além do Sistema Solar.'

END PROGRAM
