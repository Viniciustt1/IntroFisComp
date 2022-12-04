PROGRAM exerA
  IMPLICIT NONE

  REAL(8) :: x, y, dt, r, periodo, v_0, x_0, y_0, x_i, y_i, t    
  REAL(8), PARAMETER :: GMs = 4*((4*ATAN(1.0d0))**2)                              
  REAL(8), DIMENSION(9) :: d_sol, v_sol, t_sol                     
  INTEGER :: i, j                                                                 


  d_sol(1) = 0.39d0    !Mercúrio
  d_sol(2) = 0.72d0    !Vênus
  d_sol(3) = 1.0d0     !Terra
  d_sol(4) = 1.52d0    !Marte
  d_sol(5) = 5.20d0    !Júpiter
  d_sol(6) = 9.24d0    !Saturno
  d_sol(7) = 19.19d0   !Urano
  d_sol(8) = 30.06d0   !Netuno
  d_sol(9) = 39.53d0   !Plutão

  
  DO i = 1, 9
    !Assim, a velocidade centrípeta de cada corpo é dada por:
    v_sol(i) = sqrt((4*(4*ATAN(1.0d0))**2) / d_sol(i))
  END DO

  
  READ(*,*) r
  READ(*,*) v_0
  READ(*,*) dt

  !Configurando os parâmetros iniciais do problema, teremos a seguinte descrição:
  x_0 = r
  y_0 = 0.0d0
  x = r
  y = y_0 + (v_0*dt)
  t = 0.0d0

  
  OPEN(15, file = 'trajA1_out.dat', status = 'unknown') !Dados para um período completo de movimento
  OPEN(16, file = 'tabA1_out.dat', status = 'unknown')   !Dados para a verificação da Lei das Áreas

  WRITE(15,*) t, x_0, y_0

  
  i = 0
  DO WHILE (i == 0)
    !Descrição de uma órbita circular
    r = sqrt((x**2) + (y**2))
    !Modelo de Verlet aplicado para a determinação dos parâmetros:
    y_i = 2*y - y_0 - (dt**2)*GMs*y / (r**3)
    x_i = 2*x - x_0 - (dt**2)*GMs*x / (r**3)
    IF (y_i > 0 .AND. y < 0) THEN
      i = i + 1
    END IF

    !Reconfigurando as variáveis para os próximos cálculos, teremos:
    y_0 = y
    x_0 = x
    x = x_i
    y = y_i
    t = t + dt

    WRITE(15,*) t, x_i, y_i
  END DO

  periodo = t

  !Iniciando a construção das colunas da tabela de dados 'tab1_out.dat', teremos:
  WRITE(16,*) '(Planeta)', '        (v_0)', '                 (T^2 / R^3)'

  !Iniciando uma vez mais um loop que percorrerá os dados associados à cada planeta datado, teremos:
  DO j = 1, 9, 1
    i = 0
    x_0 = d_sol(j)
    y_0 = 0.0d0
    y = y_0 + v_sol(j)*dt
    x = d_sol(j)
    t = 0.0d0

    !Determinando uma vez mais os valores a partir do método de Verlet, teremos as seguintes descrições:
    DO WHILE (i == 0)
      r = sqrt((x**2) + (y**2))
      y_i = 2.0d0*y - y_0 - (dt**2)*GMs*y / (r**3)
      x_i = 2.0d0*x - x_0 - (dt**2)*GMs*x / (r**3)

      IF (y_i > 0 .AND. y < 0) THEN
        i = i + 1
      END IF

      y_0 = y
      x_0 = x
      x = x_i
      y = y_i
      t = t + dt
    END DO

    t_sol(j) = (t**2) / (d_sol(j)**3)
  END DO

    !Exibindo então os resultados na tabela 'tabA1_out.dat', teremos:
    WRITE(16,*) 'Mercúrio', v_sol(1), t_sol(1)
    WRITE(16,*) 'Vênus', v_sol(2), t_sol(2)
    WRITE(16,*) 'Terra', v_sol(3), t_sol(3)
    WRITE(16,*) 'Marte', v_sol(4), t_sol(4)
    WRITE(16,*) 'Júpiter', v_sol(5), t_sol(5)
    WRITE(16,*) 'Saturno', v_sol(6), t_sol(6)
    WRITE(16,*) 'Urano', v_sol(7), t_sol(7)
    WRITE(16,*) 'Netuno', v_sol(8), t_sol(8)
    WRITE(16,*) 'Plutão', v_sol(9), t_sol(9)

    !Explicitando a resposta para a pergunta presente no tópico 2.) da TAREFA A, temos:
    PRINT*, 'Considerando a pergunta realizada no tópico 2.) da TAREFA A, temos que a variável associada ao tempo ', &
    'deve ser escolhida de tal maneira que seja suficientemente pequena, uma vez que, para valores grandes, o termo ', &
    'quadrático apresentado nas equações tanto de Euler-Cromer quanto de Verlet dominarão a evolução do sistema ', &
    'ao longo das iterações. Sendo assim, valores a partir de 10e(-2) ou de 10e(-3) são os mais indicados'

    CLOSE(15)
    CLOSE(16)

    !Iniciando uma extensão do programa para coletar dados, teremos a seguinte configuração
    !Abrindo o arquivo das trajetórias para lê-lo logo em seguida:
    OPEN(15, file = 'trajA1_out.dat', status = 'unknown')
    !Abrindo um arquivo extra para podermos depositar os dados necessários para a construção do gráfico solicitado:
    OPEN(17, file = 'graf1.dat', status = 'unknown')
    OPEN(18, file = 'graf2.dat', status = 'unknown')
    OPEN(19, file = 'graf3.dat', status = 'unknown')
    OPEN(20, file = 'graf4.dat', status = 'unknown')

    !Definindo o período em quadrantes, teremos:
    dt = periodo / 4.0d0

    !Realizando um loop para calcular certas trajetórias nos intervalos específicos de cada quadrante, teremos:
    DO WHILE (dt < periodo)
      READ(15,*) dt, x, y
      !Aqui, basta variarmos os sinais para obtermos os dados de cada intervalo:

      !1º Quadrante: dt >= (periodo/4.0d0) .AND. dt < periodo
      !2º Quadrante: dt >= 2*(periodo/4.0d0) .AND. dt < periodo
      !3º Quadrante: dt >= 3*(periodo/4.0d0) .AND. dt < periodo
      !4º Quadrante: dt <= (periodo/4.0d0) .AND. dt < periodo

      IF (dt >= (periodo/4.0d0) .AND. dt < periodo) THEN
        WRITE(17,*) x, y
      END IF
      
      if (dt >= 2*(periodo/4.0d0) .AND. dt < periodo) THEN
        write(18,*) x, y
      end if

      if (dt >= 3*(periodo/4.0d0) .AND. dt < periodo) THEN
        write(19,*) x, y
      end if

      if (dt <= (periodo/4.0d0) .AND. dt < periodo) THEN
        write(20,*) x, y
      end if

    END DO

    close(15)
    close(17)
    close(18)
    close(19)
    close(20)

  END PROGRAM
