program graph_centrality
  implicit none
  integer, parameter :: nmax = 100
  integer :: n, i, j, s, t, stack(nmax), sp, pred(nmax, nmax), pred_count(nmax)
  integer :: adj(nmax, nmax), sigma(nmax), dist(nmax), queue(nmax), qp
  real :: betweenness(nmax), betweenness_norm(nmax), delta(nmax)
  real :: norm_factor
  logical :: visited(nmax)
  character(len=100) :: entrada, saida
  
  ! Abrindo o arquivo de entrada
  open(unit=5, file='betweenness.dat', status='old', action='read')
  read(5,*) n                        ! Número de vértices
  read(5,*) entrada                   ! Nome do arquivo da matriz de adjacência
  read(5,*) saida                      ! Nome do arquivo de saída
  close(5)
  
  ! Lendo a matriz de adjacência
  open(unit=10, file=entrada, status='old', action='read')
  do i = 1, n
     read(10,*) (adj(i, j), j = 1, n)
  end do
  close(10)

  ! Inicializando betweenness centrality
  betweenness = 0.0

  ! Algoritmo de Brandes para calcular a centralidade de intermediação
  do s = 1, n
     ! Inicializando estruturas
     sigma = 0
     dist = -1
     pred_count = 0
     visited = .false.
     sp = 0
     qp = 0
     queue = 0
     stack = 0
     delta = 0.0

     ! Inicializando fonte
     sigma(s) = 1
     dist(s) = 0
     qp = qp + 1
     queue(qp) = s

     ! BFS para encontrar todos os caminhos mais curtos
     do while (qp > 0)
        i = queue(1)
        do j = 1, qp-1
           queue(j) = queue(j+1)
        end do
        qp = qp - 1
        sp = sp + 1
        stack(sp) = i
        do j = 1, n
           if (adj(i, j) /= 0) then
              if (dist(j) < 0) then
                 qp = qp + 1
                 queue(qp) = j
                 dist(j) = dist(i) + 1
              end if
              if (dist(j) == dist(i) + 1) then
                 sigma(j) = sigma(j) + sigma(i)
                 pred(j, pred_count(j) + 1) = i
                 pred_count(j) = pred_count(j) + 1
              end if
           end if
        end do
     end do

     ! Acumulando dependências para a centralidade de intermediação
     do while (sp > 0)
        i = stack(sp)
        sp = sp - 1
        do j = 1, pred_count(i)
           t = pred(i, j)
           delta(t) = delta(t) + (real(sigma(t)) / real(sigma(i))) * (1.0 + delta(i))
        end do
        if (i /= s) then
           betweenness(i) = betweenness(i) + delta(i)
        end if
     end do
  end do

  ! Normalização correta da centralidade de intermediação
  norm_factor = real((n - 1) * (n - 2))
  if (norm_factor > 0.0) then
     do i = 1, n
        betweenness_norm(i) = betweenness(i) / norm_factor
     end do
  else
     betweenness_norm = 0.0
  end if

  ! Exibir resultados da centralidade de intermediação normalizada
  print *, "Centralidade de intermediação normalizada:" 
  do i = 1, n
     print *, "Nó", i, ":", betweenness_norm(i)
  end do


! Salvando resultados em arquivo de saída
  open(unit=20, file=saida, status='replace', action='write')
  write(20,'(A)') "Normalized Betweenness Centrality"
  write(20,'(A4, A15)') "Node", "Betweenness"
  do i = 1, n
     write(20,'(I4, F15.4)') i, betweenness_norm(i)
  end do
  close(20)

  print *, "Results saved in:", trim(saida)

end program graph_centrality
  
