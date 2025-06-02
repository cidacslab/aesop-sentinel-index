c     programa dendo2uQ
c     calcula o dendograma de uma rede a partir da eliminação sucessiva
c	dos nós com maior grau de betweeness
c	faz renumeracao dinamica dos nos, de modo que todos os nos de um cluster
c	sejam numerados contiguamente
c	constroi arquivo para tracar dendograma no origin
c	entrada dos dados via matriz de vizinhança
c	usa rotina rannyu para gerar numeros aleatorios
c	duas maneiras de gerar sementes independentes, para windows e linux
c	igual a dendo1, mas calcula a distância entre duas matrizes de vizinhancas
c	após a eliminação de uma conexão
c	calcula a modularidade Q definida por Newman e Girvan
c	criado em 27/10/2019, a partir de dendo2u(de 28/08/2018) - faz a gravacao em arquivos do resultado 
c	parcial apos a eliminacao de umnumero escolhido de ligacoes, permitindo reiniciar o processo caso 
c	o computador falhe por faltade eletricidade durante o processo. dendo2u (1. versão em 24/3/2017)  
c	resolve o problema da não conexão de um cluster que é quebrado pelo primeiro link eliminado 

c      use portlib 
      parameter(npm=5000,nml=2000000)
      integer icc(npm),iord(npm,4),ila(npm),inc(npm)
	integer am(npm,npm),bm(npm,npm),lar(npm),Em(npm,npm)
      real cm(npm,npm),yc(0:npm)
	real*8 rannyu
      integer ie(nml),ig(0:nml),iv(2,nml),iq(npm)
	character*8 saida
	character*26 saida1,saida2,saida3,saida4,saida5,saida6,saida7
	character*26 entrada2,entrada3,entrada4,Entrada1
	common /rnyucm/ mk1,mk2,mk3,mk4,km1,km2,km3,km4

c==================================================================
c	entrada2: numeração dos nós - no inicio usa a numeração identidade. caso o programa seja
c	reinicializado, os dados devem ser os contidos no arquivo saida5='d2'//saida//'5.dat'
c	entrada3: matriz de vizinhança da rede já com os links eliminados. no inicio, esta entrada
c	eh identica a entrada 4. caso o programa seja reinicializado, os dados devem ser os contidos
c	no arquivo saida7='d2'//saida//'7.dat'
c	entrada4: matriz de vizinhança da rede sem os links eliminados. no inicio, esta entrada
c	eh identica a entrada 3. caso o programa seja reinicializado, os dados devem ser os contidos
c	no arquivo saida3='d2'//saida//'3.dat'
c	Entrada1: matriz de vizinhança da rede original sempre, mesmo se o programa for reinicializado
c     nm: numero de nos na rede
c	npula: numero de linhas antes dos elementos da matriz
c	ngrava: numero de ligações eliminadas necessarias para se proceder a uma noma
c             gravacao de dados para reinicializar o programa

c	saida1: informa, no minimo, a sequencia das conexoes eliminadas 
c	        pode-se habilitar outras informaçoes sobre a evolucao do processo
c	        contudo, para redes grandes, o arquivo pode tornar-se muito grande
c	saida2: informa o cluster ao qual cada nó pertence, para cada valor de itera
c	        na ultima linha informa a numeracao inicial de cada nó
c	saida3: guarda a matriz de vizinhança com nós renumerados de acordo com a 
c	        sequencia de eliminaçoes de conexoes
c	saida4: dados para o dendograma. 
c     saida5: coluna unica, com a mesma informacao que na ultima linha de saida2
c	        usada para ilustrar dendograma feito com o origin
	
c     saida6: distancia entre duas matrizes de vizinhancas sucessivas em funcao de itera
c     saida7: guarda a matriz de vizinhança com links eliminados e nós renumerados de acordo com a 
c	        sequencia de eliminaçoes de conexoes
c==================================================================
c	reinicializacao

c	caso o programa seja interrompido por algum motivo, o processo pode ser reinicializado 
c	tomado os seguintes passos:

c	1) renomear o arquivo saida5='d2'//saida//'5.dat' para ser utilizado como novo entrada2 
c	2) apagar a primeira linha deste arquivo
c	3) renomear o arquivo saida7='d2'//saida//'7.dat' para ser utilizado como novo entrada3
c	4) alterar o arquivo de entrada edendo2t.dat, passando npula de 3 para 1  
c	5) renomear o arquivo saida3='d2'//saida//'3.dat' para ser utilizado como novo entrada4
c	6) alterar o arquivo de entrada edendo2t.dat, passando npula de 3 para 1  
c	7) opcional: mudar o nome dos arquivos de saida (variável "saida") em edendo2t.dat
c==================================================================

c	constantes para a rotina rnyucm e nao podem ser trocados
      mk1=0
      mk2=1
      mk3=3513
      mk4=821

c      sementes do aleatorio, devem ser trocados para cada run

      km1=3577 
      km2=5735
      km3=4689
      km4=9087

c	entrada de dados
c==================================================================
      open (unit=3,file='dendo2uQ.dat')

10    read (3,*,end=1000)nm,npula,iseed,ngrava
	if(nm.lt.0)stop

c	altera ou nao semente do aleatorio
c==================================================================

c	comandos para gerar semente aleatoria em linux
	if (iseed.eq.-1) then
      intime = time()
      km = intime/10000
      km = intime - km*10000
	else
	 km = iseed
	endif

c	comandos para gerar semente aleatoria em windows
c      call seed(iseed)

c      km = 10000*rand(0)

      km1 = km + km1
      km2 = (km + km2)*(km + km2)/km1
      km3 = (km + km3)*(km + km3)/km2
      km4 = (km + km4)*(km + km4)/km3

c	termina alteracao da semente do aleatorio
c	continua entrada de dados
c==================================================================
	read (3,500)Entrada1
	read (3,500)entrada2
	read (3,500)entrada3 
	read (3,500)entrada4
	read (3,505)saida
	saida1 = 'd2Q'//saida//'1.dat'
	saida2 = 'd2Q'//saida//'2.dat'
	saida3 = 'd2Q'//saida//'3.dat'
	saida4 = 'd2Q'//saida//'4.dat'
	saida5 = 'd2Q'//saida//'5.dat'
	saida6 = 'd2Q'//saida//'6.dat'
	saida7 = 'd2Q'//saida//'7.dat'

c	entrada da matriz am
c==================================================================

	open (unit=2,file=entrada2)
	open (unit=4,file=entrada3)
	open (unit=16,file='tempdendo1s.dat',form='unformatted')
	 
	do i = 1,npula
	 read(4,*)
	enddo
	   
	nlink = 0

	do i = 1,nm
	 read(4,fmt='(1000I2)')(lar(j),j=1,nm)
	 do j = 1,nm
	  am(i,j) = lar(j)
	  if (am(i,j).eq.1)nlink = nlink + 1
	 enddo
	enddo

	close(unit=4)

	nlink = nlink/2
	nlink1 = nlink
	write (*,*)'nlink = ',nlink
	if (nlink.gt.nml)stop
c==================================================================
c	entrada da matriz bm

	open (unit=4,file=entrada4)
	 
	do i = 1,npula
	 read(4,*)
	enddo
	   
	do i = 1,nm
	 read(4,510)(lar(j),j=1,nm)
	 do j = 1,nm
	  bm(i,j) = lar(j)
	 enddo
	enddo

	close(unit=4)
c==================================================================
c	entrada da matriz Em

	open (unit=4,file=Entrada1)
	 
	do i = 1,npula
	 read(4,*)
	enddo
	   
	do i = 1,nm
	 read(4,510)(lar(j),j=1,nm)
	 do j = 1,nm
	  Em(i,j) = lar(j)
	 enddo
	enddo

	close(unit=4)
c==================================================================
c	grava memoria do ordenamento inicial dos sitios

	do i = 1,nm
 	 read(2,*)iord(i,4)
	enddo

	open (unit=5,file=saida1)
	open (unit=6,file=saida2)
	open (unit=8,file=saida4)
	open (unit=9,file=saida5)
        open (unit=10,file=saida6)
!	write(5,*)
!	write(6,*)
!	write(8,*)
!	write(10,*)
!	     write(9,*)
	close (unit=5)
	close (unit=6)
	close (unit=8)
	close (unit=9)
	close (unit=10)
c	começa o grande loop para eliminação dos links com maior betweenness
c==================================================================

	id1 = 1
	itera = 0
	
      do while(id1.gt.0)

	 open (unit=5,file=saida1,access='append')
	 open (unit=6,file=saida2,access='append')
	 open (unit=8,file=saida4,access='append')
	 open (unit=10,file=saida6,access='append')
	 open (unit=9,file=saida5,access='append')
	 itera = itera + 1

	 if (itera.gt.nml)then
	  write (*,*) 'itera>nml'
	  stop
	 endif

c	determina o diametro
c==================================================================

       do i = 1,nm
	  do j = 1,nm
	   cm(i,j) = 0.
	  enddo
	 enddo

       do i = 1,nm
	  if (am(i,i).ne.0)write(*,*)itera,i,am(i,i)
	  if (am(i,i).ne.0)write(5,*)itera,i,am(i,i)
	 enddo

	 id1 = 0
       do i = 1,nm
	  do j = 1,nm
	   if (am(i,j).gt.id1) id1 = am(i,j)
	   if (am(i,j).eq.1)cm(i,j) = am(i,j)
	  enddo
	 enddo

c	identificação dos clusters antes da primeira eliminacao
c==================================================================
	 if (itera.eq.1)then
	  do i = 1,nm
	   icc(i) = 0
	   inc(i) = 0
	  enddo

	  id = 1	 					   
	  nc = 0

	  do i = 1,nm
	   if(nc.lt.nm) then
	    ic = 0
	    do j = 1,nm
	     if(icc(j).eq.0) then
	      if(am(i,j).ne.0.or.i.eq.j) then
	       icc(j) = id
	       nc = nc + 1
	       ic = 1
	      endif
	     endif
	    enddo
	    if (ic.eq.1) id = id + 1
	   endif
	  enddo

	  incm = 0
        do i = 1,nm
	   inc(icc(i)) = inc(icc(i)) + 1
	   incm = max(incm,icc(i))
	  enddo

	  nden = 0
	  do i = 1,incm
	   nden = nden + inc(i)*(inc(i)-1)/2
	  enddo

c	renumeracao dos sitios antes da primeira eliminacao
c==================================================================
 
	  do i = 1,nm
	   iord(i,1) = i
	   iord(i,2) = icc(i)
	  enddo

	  call orderset(iord,nm,4,2)

	  iwc = 0

	  do i = 1,nm
	   iord(i,3) = i
	   ila(i) = i
	  enddo

15	  iw = 0
	  iwc = iwc + 1
	   do i = 1,nm
	    if (iord(i,1).ne.iord(i,3)) then
	     i1 = iord(i,1)
	     i3 = iord(i,3)
	     j1 = i
	     j2 = ila(i1)
	     do j = 1,nm
	      if(j.ne.j2.and.j.ne.j1)then
	       iw = 1
	       mtemp = am(j1,j)
	       am(j1,j) = am(j2,j)
 	       am(j2,j) = mtemp
	       am(j,j1) = am(j1,j)
	       am(j,j2) = am(j2,j)
	       mtemp = bm(j1,j)
	       bm(j1,j) = bm(j2,j)
 	       bm(j2,j) = mtemp
	       bm(j,j1) = bm(j1,j)
	       bm(j,j2) = bm(j2,j)
	      endif
	     enddo
	     iord(ila(i1),3) = i3
	     iord(i,3) = i1
	     mtemp = ila(i1)
	     ila(i1) = ila(i3)
	     ila(i3) = mtemp

	    endif
	   enddo
	  if (iw.eq.1) goto 15
 	 
!!	  write(6,511)0*itera,(iord(k,2),k=1,nm) 	  
!!	  write(10,512)0*itera,dist,dist/max(1,nden)
!!	  write(16)0*itera,(iord(k,2),k=1,nm) 
!!           write(9,511)0*itera,(iord(k,4),k=1,nm) 	  
	 endif
c	comeca a calcular o grau de betweeness
c==================================================================

	 do id = 1,id1-1
	  i1 = id1 - id + 1 
	 
	  do i = 1,nm
         do j = i+1,nm
	    if(am(i,j).eq.i1) then
	     il = 0
	     do l = 1,nm
	      if (am(i,l).eq.1.and.am(j,l).eq.i1-1.or.
     .	   am(j,l).eq.1.and.am(i,l).eq.i1-1) il=il + 1
	     enddo
	     if (il.gt.0) then
	      do l = 1,nm
	       if (am(i,l).eq.1.and.am(j,l).eq.i1-1.or.
     .	    am(j,l).eq.1.and.am(i,l).eq.i1-1)then
	         cm(i,l) = cm(i,l) + (cm(i,j) + 1.)/il 
	         cm(j,l) = cm(j,l) + (cm(i,j) + 1.)/il 
		     cm(l,i) = cm(i,l)
	         cm(l,j) = cm(j,l)
		   endif
	      enddo
	     endif
	    endif
	   enddo
	  enddo

       enddo

c	corrige matriz de betweenness
c==================================================================

	 do i = 1,nm
	  do j = 1,nm
	   if (am(i,j).ne.1)cm(i,j)=0
	  enddo
	 enddo

c	procura o sitio	com o maior grau de betweeness
c=====================================================

	 xb = 0.
	 inb = 0
	 do i = 1,nm-1
	  do j = i+1,nm
	   if (am(i,j).eq.1) then
	    xb1 = cm(i,j)
	    if (xb1.gt.xb) then
	     xb = xb1
	     inb = 1
	     iv(1,inb) = i
	     iv(2,inb) = j
	    else if (xb1.eq.xb) then
	     inb = inb + 1
	     iv(1,inb) = i
	     iv(2,inb) = j
	    endif
	   endif
	  enddo
	 enddo

c	transfere a matriz am para cm
c=====================================================

	 do i = 1,nm
	  do j = 1,nm
	   cm(i,j) = am(i,j)
	  enddo
	 enddo

c	elimina um link
c=====================================================

	 if (inb.eq.1) then
	  am(iv(1,inb),iv(2,inb)) = 0
	  am(iv(2,inb),iv(1,inb)) = 0
	  i1 = iv(1,inb)
	  j1 = iv(2,inb)
	 else if (inb.gt.1) then
	  xt = rannyu()

c	comando usado apenas para testar programa
c	  xt = 1

	  il = min(int(xt*inb)+1,inb)
	  am(iv(1,il),iv(2,il)) = 0
	  am(iv(2,il),iv(1,il)) = 0
	  i1 = iv(1,il)
	  j1 = iv(2,il)
	 endif

	 ie(1) = i1
	 ie(2) = j1
	 iv(1,1) = i1
	 iv(2,1) = j1
	 nie = 2
	 nv = 1

c	calcula os efeitos da eliminacao do link (i1,j1)
c=====================================================

	 write(5,*)' itera',itera,' link elim', i1,j1,
     . iord(i1,4),iord(j1,4)

	 iel = 1

	 do while (iel.ne.0)

	  iel = iel + 1
	  id = iel
	  nnie = 0
	  imid = 0

	  do ili = 1,nie
	   ia = ie(ili)
	   do l = 1,nm
	    if (am(ia,l).eq.id) then
	     ip = 0
	     do it = 1,nm
	      if(am(ia,it).eq.1.and.am(l,it).eq.id-1.
     .	  or.am(l,it).eq.1.and.am(ia,it).eq.id-1) ip = 1
	     enddo
	     if (ip.eq.0) then
	      am(ia,l) = 0
	      am(l,ia) = 0
	      nnie = nnie + 1
	      nv = nv + 1

            if (nv.gt.nml.or.nie+nnie.gt.nml) then
             write(5,*)'limite nml ultrapassado. nv, nie, nnie'
     .	   ,nv, nie, nnie
             stop
            endif
	
	      iv(1,nv) = min(ia,l)
	      iv(2,nv) = max(ia,l)
	      ie(nie+nnie) = l
	      imid = 1
	     endif
	    endif
	   enddo
	  enddo
	  nie = nie + nnie
	 if (imid.eq.0.and.id.gt.id1) ied = iel
	 if (imid.eq.0.and.id.gt.id1) iel = 0
	 enddo
 
c	reconstroi a matriz de vizinhança
c=====================================================

	 iel = ied + 1
	 iel = ied*0+2
	 do while (iel.ne.0)
	  id = 0
	  do iiv = 1,nv
	   i1 = iv(1,iiv)
	   j1 = iv(2,iiv)
	   if(am(i1,j1).eq.0) then
	    id = 1
	    ip = 0
	    il = 1
          do while (ip.eq.0)
	     do it = 1,nm
	      if(am(i1,it).eq.il.and.am(j1,it).eq.iel-il.
     .      or.am(i1,it).eq.iel-il.and.am(j1,it).eq.il) ip = 1
	     enddo
	     if (ip.eq.1) am(i1,j1) = iel
	     if (ip.eq.1) am(j1,i1) = iel
	     il = il + 1  
	     if (il.gt.iel-1) ip = npm+1
	    enddo
	   endif
	  enddo
	  iel = iel + 1
	  if (iel.gt.2*ied.or.id.eq.0) iel = 0
	 enddo

c	calcula a distância entre duas matrizes sucessivas 
c==================================================================

	 dist = 0

	 do i = 1,nm-1
	  do j = i,nm
	   dist = dist + (cm(i,j) - am(i,j))**2
	  enddo
	 enddo

	 dist = sqrt(dist)

c	identificação dos clusters
c==================================================================
	 do i = 1,nm
	  icc(i) = 0
	  inc(i) = 0
	 enddo

	 id = 1	 					   
	 nc = 0

	 do i = 1,nm
	  if(nc.lt.nm) then
	   ic = 0
	   do j = 1,nm
	    if(icc(j).eq.0) then
	     if(am(i,j).ne.0.or.i.eq.j) then
	      icc(j) = id
	      nc = nc + 1
	      ic = 1
	     endif
	    endif
	   enddo
	   if (ic.eq.1) id = id + 1
	  endif
	 enddo

	 incm = 0
       do i = 1,nm
	  inc(icc(i)) = inc(icc(i)) + 1
	  incm = max(incm,icc(i))
	 enddo

	 nden = 0
	 do i = 1,incm
	  nden = nden + inc(i)*(inc(i)-1)/2
	 enddo

c	renumera os sitios
c==================================================================

	 do i = 1,nm
	  iord(i,1) = i
	  iord(i,2) = icc(i)
	 enddo

	 call orderset(iord,nm,4,2)

	 iwc = 0

	 do i = 1,nm
	  iord(i,3) = i
	  ila(i) = i
	 enddo

20	 iw = 0
	 iwc = iwc + 1
	  do i = 1,nm
	   if (iord(i,1).ne.iord(i,3)) then
	    i1 = iord(i,1)
	    i3 = iord(i,3)
	    j1 = i
	    j2 = ila(i1)
	    do j = 1,nm
	     if(j.ne.j2.and.j.ne.j1)then
	      iw = 1
	      mtemp = am(j1,j)
	      am(j1,j) = am(j2,j)
 	      am(j2,j) = mtemp
	      am(j,j1) = am(j1,j)
	      am(j,j2) = am(j2,j)
	      mtemp = bm(j1,j)
	      bm(j1,j) = bm(j2,j)
 	      bm(j2,j) = mtemp
	      bm(j,j1) = bm(j1,j)
	      bm(j,j2) = bm(j2,j)
	     endif
	    enddo
	    iord(ila(i1),3) = i3
	    iord(i,3) = i1
	    mtemp = ila(i1)
	    ila(i1) = ila(i3)
	    ila(i3) = mtemp

	   endif
	  enddo
	 if (iw.eq.1) goto 20
 
	 write(6,511)itera+0,(iord(k,2),k=1,nm) 	  
	 write(10,512)itera+0,dist,dist/max(1,nden)
	 write(16)itera+0,(iord(k,2),k=1,nm) 
           write(9,511)itera+0,(iord(k,4),k=1,nm) 	  
50	 continue

	 igrava = igrava + 1

	 if(igrava.eq.ngrava)then

	  open (unit=7,file=saida3)
!!	  open (unit=9,file=saida5)
	  open (unit=11,file=saida7)
	      open (unit=9,file=saida5,access='append')
!!	  write(9,511)itera
	  write(7,*)' matriz de vizinhança renumerada',itera
	  write(11,*)' matriz de vizinhança com link eliminado renumerada'
     . ,itera
	  igrava = 0
	  do i = 1,nm
!!	   write(9,511)iord(i,4)
	   write(7,510)(bm(i,j),j=1,nm)
	   write(11,510)(am(i,j),j=1,nm)
	  enddo
	 endif

       close(unit=5)
       close(unit=6)
       close(unit=7)
       close(unit=9)
       close(unit=10)
       close(unit=11)

      enddo

	open (unit=7,file=saida3)
!!	open (unit=9,file=saida5)
	open (unit=11,file=saida7)
	    open (unit=9,file=saida5,access='append')  
!!	write(9,511)itera
	write(7,*)' matriz de vizinhança renumeradaa',itera
	write(11,*)' matriz de vizinhança com link eliminado renumerada'
     .,itera
	igrava = 0
	do i = 1,nm
!!	 write(9,511)iord(i,4)
	 write(7,510)(bm(i,j),j=1,nm)
	 write(11,510)(am(i,j),j=1,nm)
	enddo

      close(unit=7)
      close(unit=9)
!      close(unit=11)

      close(unit=16)

      nlink = itera
	write(*,*)nlink,nlink1
      open (unit=6,file=saida2,access='append')

      write(6,510)
      write(6,511)100,(iord(k,4),k=1,nm) 	  
 
      close(unit=6)

c	calcula modularidade Q 
c==================================================================

c	identifica valores das coordenadas de cada cluster 
c==================================================================
	open (unit=9,file=saida5) !,access='append')  
	open (unit=16,file='tempdendo1s.dat',form='unformatted')

      write(11,510)
	ncomu =1
	do i = 1,nlink1

	 do j = 1,nm
	  ie(j) = 0
	  lar(j) = 0
	 enddo

	 read (9,*)itera,(ie(k1),k1=1,nm)
	 read (16)itera,(lar(k1),k1=1,nm)
	 do j = 1,nm
	  k = ie(j)
	  iq(k) = lar(j)
	 enddo
	 
	 if (i.eq.1) then

	  qq = functionqq(Em,iq,nm,ncomu,nlink1)
	  write(11,*)itera,qq, ncomu
	  write(*,512)itera,qq

	 elseif	(lar(nm).gt.ncomu) then

	  ncomu = lar(nm)

	  qq = functionqq(Em,iq,nm,ncomu,nlink1)
	  write(11,*)itera,qq, ncomu
	  write(*,512)itera,qq

	 endif

	enddo

      close(unit=11)
      close(unit=9)
      close(unit=16)

c	constroi dendograma 
c==================================================================

c	identifica valores das coordenadas de cada cluster 
c==================================================================

	open (unit=16,file='tempdendo1s.dat',form='unformatted')
	open (unit=17,file='tempdendo2s.dat',form='unformatted')

	itera = 0
	ig(0) = 1
	xlm = 0.5*(1.+ float(nm))

	do k = 1,nm
	 yc(k) = xlm
	enddo

	write (17)itera,(yc(k1),k1=1,nm)

	do i = 1,nlink
	 read (16)itera,(lar(k1),k1=1,nm)
	 xlm = float(lar(1))
	 ii = 1
	 xne = 1.
	 ig(i) = 1
	 do j = 2,nm
	  if(lar(j).eq.lar(j-1))then

	   xlm = xlm*xne/(xne+1) + float(j)/(xne+1)
	   xne = xne + 1

	   if(j.eq.nm) then
	    do k = ii,j
	     yc(k) = xlm
	    enddo
	   endif

	  else

	   ig(i) = ig(i) + 1
	   do k = ii,j-1
	    yc(k) = xlm
	   enddo
	   xlm = float(j)
	   ii = j
	   xne = 1.
	   if(j.eq.nm) yc(k) = xlm

	  endif

	 enddo

	 write (17)itera,(yc(k1),k1=1,nm)

	 do k = 1,nm
	  yc(k) = 0.
	 enddo

	enddo

      close(unit=16)
      close(unit=17)

c	insere linhas extras nos valores de bifurcação e escreve dendograma
c==================================================================

	open (unit=17,file='tempdendo2s.dat',form='unformatted')

	do i = 0,nlink-1
	 read (17)itera,(yc(k1),k1=1,nm)
	 write(8,530)itera,(yc(k1),k1=1,nm)
	 if(ig(i).lt.ig(i+1))write(8,530)itera+1,(yc(k1),k1=1,nm) 
	enddo

	read (17)itera,(yc(k1),k1=1,nm)
	write(8,530)itera,(yc(k1),k1=1,nm)

!!	open (unit=9,file=saida5)
	    open (unit=9,file=saida5,access='append')
	do i = 1,nm
	 write(9,511)iord(i,4)
	enddo

	write(9,511)

      close(unit=8)
      close(unit=9)

	goto 10

500   format(a26)
505   format(a8)
510   format(10000i2)
511   format(i6,10000i4)
512   format(i6,2(2x,e13.6))
520   format(10000(2x,e11.3))     
530   format(i5,1x,10000(2x,e11.3))     
	
1000	stop
      end

c======================================================
c======================================================
      subroutine orderset(set,n,m,icol)
c     it orders, descending, a set of n numbers, conserving the respective
c     positions of other sets
c	m indicates the number of columns in the set
c     icol indicates the column which will be considered for the ordering  
      parameter(npm=5000)

      integer set(npm,m),x1 
      itera = 0
10    icont = 0

      itera = itera + 1

      do  20  j= 1,n-1

c	write(*,*)j,set(j,icol),set(j+1,icol)
c      to order in ascending order use the following command
      if (set(j,icol).gt.set(j+1,icol)) then
c      to order in descending order use the following command
c      if (set(j,icol).lt.set(j+1,icol)) then

      icont = 1
      do k = 1,m
      x1 = set(j,k)
      set(j,k) = set(j+1,k)
      set(j+1,k) = x1
      enddo

      endif

20    continue

      if (icont.ne.0) go to 10

      return
      
      end      


c======================================================
c======================================================
c
      function rannyu()
      implicit double precision (a-h,o-z)
c        real*8 rannyu, twom12
      parameter (twom12 = 1/4096.d0)
      common /rnyucm/ mk1,mk2,mk3,mk4,km1,km2,km3,km4
c
c     this is rannyu as modified by a. sokal 9/26/85.
c     it is linear congruencial with modulus m = 2**48,incremen_tempoc=1,
c     and multiplier a = (2**36)*mk1 + (2**24)*mk2 + (2**12)*mk3 + mk4.
c     the multiplier is stored in common (see subroutine setrn)
c     and is set to a = 31167285 (recommended by knuth, vol. 2,
c     2nd ed., p. 102).
c
      i1 = km1*mk4 + km2*mk3 + km3*mk2 + km4*mk1
      i2 = km2*mk4 + km3*mk3 + km4*mk2
      i3 = km3*mk4 + km4*mk3
      i4 = km4*mk4  +  1
      km4 = mod(i4, 4096)
      i3 = i3 + i4/4096
      km3 = mod(i3, 4096)
      i2 = i2 + i3/4096
      km2 = mod(i2, 4096)
      km1 = mod(i1 + i2/4096, 4096)
      rannyu = twom12*(dble(km1)+twom12*(dble(km2)+
     .         twom12*(dble(km3)+twom12*(dble(km4)))))
      return
      end

c======================================================
c======================================================

c======================================================
	function functionqq(am,iq,nm,ncomu,nlink)
      
      parameter(npm=5000,nmaxcomu=100)
	integer am(npm,npm)
      integer iq(npm)
      integer iaa(nmaxcomu,nmaxcomu),ia2(nmaxcomu,nmaxcomu),ja(nmaxcomu)
	real xaa(nmaxcomu,nmaxcomu),xa2(nmaxcomu,nmaxcomu),xa(nmaxcomu)
	qq = 0.

	do i = 1,ncomu
	 ja(i) = 0
	 do j = 1,ncomu
	  iaa(i,j) = 0
	 enddo
	enddo

	do i = 1,nm-1
	 do j = i+1,nm
	  if (am(i,j).eq.1) then
	   iaa(iq(i),iq(j)) = iaa(iq(i),iq(j)) + 1
	   iaa(iq(j),iq(i)) = iaa(iq(i),iq(j))
	  endif
	 enddo
	enddo

	do i = 1,ncomu
	 do j = 1,ncomu
	  xaa(i,j) = float(iaa(i,j))
	  if(i.ne.j)xaa(i,j)=xaa(i,j)/2
	 enddo
	enddo

	do i = 1,ncomu
	 do j = 1,ncomu
	  ia2(i,j) = 0
	  xa2(i,j) = 0
	  do k = 1,ncomu
	   ia2(i,j) = ia2(i,j) + iaa(i,k)*iaa(k,j)
	   xa2(i,j) = xa2(i,j) + xaa(i,k)*xaa(k,j)
	  enddo
	 enddo
	enddo

	do i = 1,ncomu
	 ja(i) = 0
	 xa(i) = 0
	 do j = 1,ncomu
	  ja(i) = ja(i) + ia2(i,j)
	  xa(i) = xa(i) + xa2(i,j)
	 enddo
	enddo

	write(*,*)'ncomu',ncomu
	do i = 1,ncomu
	 write(*,*)'i,ja(i),xa(i)',i,ja(i),xa(i)
	 do j = 1,ncomu
	  write(*,*)i,j,iaa(i,j),ia2(i,j),xaa(i,j),xa2(i,j)
	 enddo
	enddo
!	pause

	do i = 1,ncomu
	   qq = qq + xaa(i,i)/float(nlink) - xa(i)/(float(nlink)**2)
	enddo

	functionqq = qq

	return
	end
c======================================================



