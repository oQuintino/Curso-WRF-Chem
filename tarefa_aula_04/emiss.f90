program gera

implicit none

integer                            :: i,j,ispec,ihr,isrc,n,k,nsap99
integer                            :: cx,cy,cz,nx,ny,nz,nradm

parameter (cx=159,cy=109,cz=3)                                                     ! modificar
parameter (nx=cx, ny=cy, nz=cz-1, NRADM=32, nsap99=13)

character*1 dummy

real,dimension(0:23)               :: hrsplt_co,hrsplt_nox
real, dimension(0:23)              :: hrsplt_mp, hrsplt_voc
real,dimension(cx,cy)              :: veic_co,veic_co2, veic_ch4
real,dimension(cx,cy)              :: termo_co
real,dimension(cx,cy)              :: termo2_co
real,dimension(cx,cy)              :: termo3_co
real,dimension(cx,cy)              :: refinaria_co
real,dimension(cx,cy)              :: trem_co, trem_co2, trem_ch4
real,dimension(cx,cy)              :: veic,veic1,veic2,veic3,veic4a,veic4b,veic4c,veic5,veic6,termo,termo2,termo3,refina,ferro
character*8, dimension(13)         :: snam


! Variaveis para formato WRF/Chem
real,dimension(nx,ny,nz, 0:23)     :: e_co,e_co2,e_ch4

!***********saida do dados************
integer                            :: cont,cont1,cont2,cont3
character*9,dimension(3)           :: ename
real,dimension(nx,nz,ny,NRADM,0:23):: EM3RD
real,dimension(nx,nz,ny)           :: EM3RS

!this array is a split of the daily total emissions into hourly fractions
!em UTC (3h), mas outubro horario verao (2h)
!Ciclo diario emissao CO - base dado Olimpio, experimento Hewllet
!Ciclo diario emissao - medida do tunel 2011
!Perfil de SO2 igual ao de NOx
data hrsplt_co/0.019,0.012,0.008,0.004,0.003,0.003,0.006,0.017,0.047,0.074,0.072,0.064,0.055,0.052,0.051,   &
                    0.048,0.052,0.057,0.068,0.087,0.085,0.057,0.035,0.024/

!Ciclo diario emissao NOx
data hrsplt_nox/0.019,0.015,0.012,0.010,0.008,0.009,0.015,0.030,0.048,0.053,0.051,0.061,0.064,0.064,0.061,  &
                     0.060,0.060,0.065,0.065,0.066,0.056,0.044,0.035,0.027/

!Esse vetor informa os constituintes a serem salvos no formato WRF/Chem
data ename/ &
      'e_co  ', 'e_co2 ', 'e_ch4 '/

! Declaração de variáveis do namelist
integer, parameter :: n_tipos = 8, n_substancias = 3

real :: distancia_media(n_tipos), frac_veiculos(n_tipos), &
  emissoes_CO2(n_tipos), emissoes_CO(n_tipos), emissoes_CH4(n_tipos), &
  fatores_de_emissao_trem(n_substancias), &
  intensidades_de_uso(n_tipos), acrescimo_motocicletas

! Namelist para controle das entradas
namelist /entrada/ distancia_media, frac_veiculos, emissoes_CO2, emissoes_CO, emissoes_CH4, &
  intensidades_de_uso, fatores_de_emissao_trem, acrescimo_motocicletas

! Necessários para cálculos
real :: emissao_CO2_por_veiculo(n_tipos), &
  emissao_CO_por_veiculo(n_tipos), &
  emissao_CH4_por_veiculo(n_tipos)

! Leitura das entradas via arquivo
open(unit=10, file='namelist.emis', status='old')
read(10, nml=entrada)
close(10)

! Aplicando o cenário
frac_veiculos(8) = frac_veiculos(8) + acrescimo_motocicletas / 100
frac_veiculos(7) = frac_veiculos(7) * (1 - acrescimo_motocicletas / 100)
frac_veiculos(6) = frac_veiculos(6) * (1 - acrescimo_motocicletas / 100)
frac_veiculos(5) = frac_veiculos(5) * (1 - acrescimo_motocicletas / 100)
frac_veiculos(4) = frac_veiculos(4) * (1 - acrescimo_motocicletas / 100)
frac_veiculos(3) = frac_veiculos(3) * (1 - acrescimo_motocicletas / 100)
frac_veiculos(2) = frac_veiculos(2) * (1 - acrescimo_motocicletas / 100)
frac_veiculos(1) = frac_veiculos(1) * (1 - acrescimo_motocicletas / 100)

!leitura do mapa  (saidaxkm.pgm) com numero de veiculos na celula de x*xkm

open(20,file='output_vehicular',status='old')                                              ! modificar ****************
do j=1,cy
       read(20,*)(veic(i,j),i=1,cx)
enddo


!Abrir e ler um arquivo de saida das termeletricas
open(77,file='output_refinary',status='old')                                               ! modificar **************

do j=1,cy
       read(77,*)(refina(i,j),i=1,cx)
enddo


open(30,file='output_diesel',status='old')                                                 ! modificar *************
do j=1,cy
       read(30,*)(termo(i,j),i=1,cx)
enddo



open(40,file='output_fueloil',status='old')                                                ! modificar ************
do j=1,cy
       read(40,*)(termo2(i,j),i=1,cx)
enddo



open(50,file='output_naturalgas',status='old')                                              ! modificar ***********
do j=1,cy
       read(50,*)(termo3(i,j),i=1,cx)
enddo

!leitura das ferrovias by Reis
!Considera-se para as ferrovias a quantidade de litros
!de combustivel quimados por ponto de grade

open(60,file='output_ferrovia',status='old')                                               ! modificar ***********
do j=1,cy
       read(60,*)(ferro(i,j),i=1,cx)
       ferro(i,j)=ferro(i,j)/24 !dividindo por 24 para conventer l/dia para l/hora
enddo

emissao_CO_por_veiculo = emissoes_CO * intensidades_de_uso
emissao_CO2_por_veiculo = emissoes_CO2 * intensidades_de_uso
emissao_CH4_por_veiculo = emissoes_CH4 * intensidades_de_uso

!multiplicando o numero de veiculos pelo fator de correcao, Jorge 18.2
!e fracao de veiculos correspondente ao tipo de veiculo na RMSP.
do ihr=0, 23

        do j=1,cy
		do i=1,cx

		! fracao frota veicular - DENATRAN
		veic1(i,j) = veic(i,j) * 18.2 * frac_veiculos(1)  ! Gasolina C
		veic2(i,j) = veic(i,j) * 18.2 * frac_veiculos(2)  ! Alcool
		veic3(i,j) = veic(i,j) * 18.2 * frac_veiculos(3)  ! Flex
		veic4a(i,j) = veic(i,j) * 18.2 * frac_veiculos(4)  ! Caminhoes
		veic4b(i,j) = veic(i,j) * 18.2 * frac_veiculos(5)! Urbanos
		veic4c(i,j) = veic(i,j) * 18.2 * frac_veiculos(6)  ! Rodoviarios
		veic5(i,j) = veic(i,j) * 18.2 * frac_veiculos(7)  ! Taxis
		veic6(i,j) = veic(i,j) * 18.2 * frac_veiculos(8)  ! Motocicletas


		! Multiplicando pelo fator de emissao, e considerando que os leves rodam
		! por dia 46.6 km (hewlett) e os pesados 100km. (Foi alterado!)
		! Aterada a quilometragem baseada nas informacoes contidas em:
		! http://www.sptrans.com.br/ganhosambientais/ (site da SPTrans)
		! A nova quilometragem media por dia fica:
		! veiculos a gasolina rodam: 41,09 km/dia
		! veiculos a alcool+flex rodam: 41,09 km/dia
		! veiculos a diesel (caminhoes) rodam: 109,58 km/dia
		! veiculos a diesel (onibus-EURO II) rodam: 164,38 km/dia
		! veiculos a diesel (onibus-EURO III) rodam: 164,38 km/dia
		! veiculos a gas natural rodam: 41,09 km/dia
		! veiculos motocicletas rodam: 136,98 km/dia
		! Fatores medios de emissao CETESB,2008 referentes ao ano de 2007, e SPTrans.
		! O fator relativo a concentracao de VOC foi baseado no HC. O antigo era:
		! VOC leves = 3.12 g/km , VOC pesados = 2.29 g/km
		! FE experimento tuneles + Cetesb, 2011

		!Fe co2 = 600.49 os dados da CETESB são proximos
		!Fe ch4 = 0.6 dado CTESB 2020



	veic_ch4(i,j) = veic1(i,j) * emissao_CH4_por_veiculo(1) + &
        veic2(i,j) * emissao_CH4_por_veiculo(2) + &
        veic3(i,j) * emissao_CH4_por_veiculo(3) + &
	veic4a(i,j) * emissao_CH4_por_veiculo(4) + &
        veic4b(i,j) * emissao_CH4_por_veiculo(5) + &
        veic4c(i,j) * emissao_CH4_por_veiculo(6) + &    !conferir
	veic5(i,j) * emissao_CH4_por_veiculo(7) + &
        veic6(i,j) * emissao_CH4_por_veiculo(8)

	veic_co2(i,j) = veic1(i,j) * emissao_CO2_por_veiculo(1) + &
        veic2(i,j) * emissao_CO2_por_veiculo(2) + &
        veic3(i,j) * emissao_CO2_por_veiculo(3) + &
	veic4a(i,j) * emissao_CO2_por_veiculo(4) + &
        veic4b(i,j) * emissao_CO2_por_veiculo(5) + &
        veic4c(i,j) * emissao_CO2_por_veiculo(6) + &    !conferir
	veic5(i,j) * emissao_CO2_por_veiculo(7) + &
        veic6(i,j) * emissao_CO2_por_veiculo(8)


	veic_co(i,j) = veic1(i,j) * emissao_CO_por_veiculo(1) + &
        veic2(i,j) * emissao_CO_por_veiculo(2) + &
        veic3(i,j) * emissao_CO_por_veiculo(3) + &
	veic4a(i,j) * emissao_CO_por_veiculo(4) + &
        veic4b(i,j) * emissao_CO_por_veiculo(5) + &
        veic4c(i,j) * emissao_CO_por_veiculo(6) + &
	veic5(i,j) * emissao_CO_por_veiculo(7) + &
        veic6(i,j) * emissao_CO_por_veiculo(8)


		veic_ch4(i,j)                            = veic_ch4(i,j)/16                 !    mol/dia
		veic_co2(i,j)                            = veic_co2(i,j)/44                 !    mol/dia
		veic_co(i,j)                             = veic_co(i,j)/28                  !    mol/dia

		!Aplica o perfil diurno
		veic_ch4(i,j)                            = veic_ch4(i,j)*hrsplt_co(ihr)     !    mol/km^2/hr
		veic_co2(i,j)                            = veic_co2(i,j)*hrsplt_co(ihr)     !    mol/km^2/hr
		veic_co(i,j)                             = veic_co(i,j)*hrsplt_co(ihr)      !    mol/km^2/hr


		!Adicionando emissao dos trens by Reis
	        trem_co(i,j) = ferro(i,j) * fatores_de_emissao_trem(1) / 28
	        trem_co2(i,j) = ferro(i,j) * fatores_de_emissao_trem(2) / 44
	        trem_ch4(i,j) = ferro(i,j) * fatores_de_emissao_trem(3) / 16



	        ! Calculando em apenas dois niveis verticais!
	        do k=1, 2

			e_co(i,j,k,ihr)=  veic_co(i,j)   + termo_co(i,j)   + termo2_co(i,j)   + termo3_co(i,j)  + refinaria_co(i,j)+trem_co(i,j)
			e_co2(i,j,k,ihr)= veic_co2(i,j)  + trem_co2(i,j)
			e_ch4(i,j,k,ihr)= veic_ch4(i,j)  + trem_ch4(i,j)


			EM3RD(i,k,j,11,ihr)=e_co(i,j,k,ihr)
			EM3RD(i,k,j,31,ihr)=(e_co2(i,j,k,ihr)) ! Add by REIS
			EM3RD(i,k,j,32,ihr)=(e_ch4(i,j,k,ihr)) ! Add by REIS

	        enddo !k
	        end do  !j
        enddo   !i
enddo   !ihr

!******************escrevendo os resultados**********
open (unit=50, file='wrfem_00to12z_d01',form='formatted')                      ! modificar

do cont=1,2
        if(cont.EQ.1)then
		write(*,*)'Salvando : wrfem_00to12z_d01'
		cont1=50   !Arquivo a ser aberto
		cont2=0   !hora inicial
		cont3=11  !hora final
        else
		write(*,*)'Fechou: wrfem_00to12z_d01'
		write(*,*)'Salvando : wrfem_12to24z_d01'
		close (unit=50)
		open (unit=7, file='wrfem_12to24z_d01',form='formatted')                  ! modificar
		cont1=7  !arquivo a ser aberto
		cont2=12 !hora inicial
		cont3=23 !hora final
        endif

        write (cont1,*)NRADM !Escreve a qtd de poluentes
        write (cont1,*)ename !escreve o nome dos poluentes
        do ihr=cont2,cont3
		write(cont1,*)ihr !escreve a hora
		do n=1,NRADM
		do i=1,nx
		do k=1,nz
		do j=1,ny


		! Escrevendo...
		EM3RS(i,k,j)=EM3RD(i,k,(cy-j),n,ihr) ! esta escrevendo invertido
		! variacao horaria  das emissoes


		enddo
		enddo
		enddo
		write(cont1,*)EM3RS
		enddo   !Poluentes
        enddo   !hora
enddo   !Arquivos
! formatos de saida dos dados

200    format (5(F20.3,1X))
400    format (A9,I3,E10.3)

! end of program
write(*,*)'******** Fim do Programa *********'
close (unit=7)
close (5)
stop
end

