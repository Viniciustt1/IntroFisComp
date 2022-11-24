PROGRAM exer3
  IMPLICIT NONE
  INTEGER :: j, N_0
  REAL(8) :: lambda, dt, t, N

  !Abrindo os arquivos necessários para a leitura e para a colocação de resultados, teremos, respectivamente:
  OPEN(13, file = 'decai_in.dat', status = 'old')
  OPEN(14, file = 'decai_out', status = 'replace')

  !Lendo então os dados fornecidos no arquivo 'decai_in', temos as seguintes variáveis:
  READ(13,*) t 
  READ(13,*) N_0
  READ(13,*) dt
  READ(13,*) lambda

  WRITE(14,*) 0.0d0, N_0

  !Para as condições iniciais do problema, consideremos apenas:
  N = N_0

  !Iniciando então o loop para determinarmos o decaimento do sistema atômico, teremos:
  DO j = 1, int(t/dt)
    !De acordo com a relação fornecida, temos que a expressão considerada é dada por:
    N = N - (lambda*dt*N)
    WRITE(14,*) (dt*j), N
  END DO
  close(13)
  close(14)
END PROGRAM
