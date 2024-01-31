! modified : 12-03-2020
implicit none

real*8,      dimension(:,:), allocatable   :: a
integer,     dimension(:),   allocatable   :: nelem
character*2, dimension(:),   allocatable   :: elem

real*8          ::   e,e0,v,eta
real*8          ::   v0 , b , bp  , etot    ! mur
real*8          ::   bv0, bb, bbp , betot    ! Birch 
real*8          ::   pas,vmax,vmin
real            ::   pi,sp,pf, ti, st, tf
real            ::   matm,ma,mm
integer         ::   j,vfree,z,nb,natom,nl,io,ord,i,n

character*4     ::   volume  ,bulk ,Bpp , ENG
character*4     ::   bvolume ,bbulk,bBpp, bENG
character*4     ::   choix  , nbc 
character*8     ::   test,strain
character*16    ::   phase
character*2     ::   eleme,tst
character*1     ::   vari  ,fdata
character*5     ::   mult,fitm
logical         ::   test_exist


write(*,*) ' ================================================================='
write(*,*) '         set  input data for Gibbs2                               '
write(*,*) '      https://aoterodelaroza.github.io/gibbs2/                    '
write(*,*) '  --------------------------------------------------------        '
write(*,*) '' 
write (*,*)' Necessary  input paramaters obtained from your ab initio code '
write(*,*) '' 
write(*,*) '         The bulmk modulus        : B    (GPa)                         '
write(*,*) '         The prussure derivative  : Bp                            '
write(*,*) '         The volume               : V0   ( a.u^3)                         '
write(*,*) '         The total energy at V0   : E0   (Ry  )                       '
write(*,*) '' 
write(*,*) '         MM, the molecularmass of the primitive cell (in a.m.u )      '
write(*,*) '         VFREE,the total number of atoms in the primitive cell.   '
write(*,*) '' 
write(*,*) ' ********************* 07/01/2024 pg@G.Benabdellah *****************'


INQUIRE(file='datatest',exist=test_exist)

fdata='F'

if( test_exist ) then
    open( 245, file='datatest',status='old')
    read(245,*) fdata
    close(245)
endif

if(fdata.eq.'T'.or.fdata.eq.'t') then
    open(unit=90,file='volume.out',status='old',err=7000)
    read(90,*)
    read(90,*) volume ,bulk,Bpp,ENG,v0,b,bp,etot
    read(90,*) bvolume ,bbulk,bBpp,bENG,bv0,bb,bbp,betot
    close(90)
else 
	!modif
    write(*,*)                                   
    write(*,*)          
    write(*,'(a)', advance='no')'---> Enter the volume (in  a.u^3)  : V0=   '  
    read(*,*) v0
    write(*,*)
    write(*,'(a)', advance='no')'---> Enter the bulmk modulus (in GPa ) : B=  '
    read(*,*) b
    write(*,*)
    write(*,'(a)', advance='no')'---> Enter the prussure derivative of B: Bp= '
    read(*,*) bp
    write(*,*)
    write(*,'(a)', advance='no')'---> Enter the equilibrium total energy at V0 ( in Ry) : E0= ' 
    read(*,*) etot
    write(*,*)
endif

223 write(*,*)
  write(*,*)
  !write(*,*)'#======================================================='
  write(*,*)
  write(*,*)'for  Equation of state: Murnaghan       enter:  m  '
  write(*,*)'for  Equation of state: Birch-Murnaghan enter:  mb ' 
  write(*,*)
  !write(*,*)'#======================================================='
  write(*,'(a)', advance='no')'---> Enter your choose (  m | mb) :  '
  read(*,*) choix
 
  
 if(choix.eq.'m'.or.choix.eq.'M') then
     n=100
     fitm='murn'
     strain=''
    write(*,*)
    write(*,*) '---| Equilibrium volume V0 = ', v0
    write(*,*)
    write(*,'(a)', advance='no') '---> Eneter the Number of fitted points:(default n=100) : n = '
    read(*,*) n
   
   if( n.lt.0) then
	 n=100
   end if

 allocate(a(n,2) )    
   
    vmin=v0*0.85
    vmax=v0*1.15
    pas=(vmax-vmin)/(float(n))
    e0=etot
    v=vmin
  
   do i=1,n
      !Equation of state: Murnaghan  
      e=e0+(b*v/bp*(1/(bp-1)*(v0/v)**bp +1)-b*v0/(bp-1))/14703.6 
      ! write(*,*)pas, V,E
      a(i,2)=e
      a(i,1)=v
      v=v+pas
   enddo

  else if(choix.eq.'mb'.or.choix.eq.'MB') then
     if(fdata.eq.'T'.or.fdata.eq.'t') then
      v0 = bv0 ; b=bb ; bp=bbp ; etot=betot
     endif
      n=100
      fitm='bm4'
      ord = 4
      strain='strain'
     ! write(*,*) volume ,bulk,Bpp,ENG,v0,b,bp,etot
      write(*,*) '---| Equilibrium volume V0 = ',v0
      write(*,*)
      write(*,'(a)', advance='no') '---> Enter the Number of fitted points:(default n=100) : n= '
      read(*,*) n
      if( n.lt.0) then
	    n=100
      end if
      
    allocate(a(n,2)) 
      vmin=v0*0.85
      vmax=v0*1.15
      pas=(vmax-vmin)/(float(n))
      e0=etot
      v=vmin

   do i=1,n
      !Equation of state: Birch-Murnaghan 
      eta = (v0/v)**(1.0/3.0) 
      e= e0 + 9.0/16.0*(b/14703.6)*v0*((eta**2-1)**3*bp + (eta**2-1)**2*(6-4*eta**2))
      a(i,2)=e
      a(i,1)=v
      v=v+pas
   enddo
      
 else
     goto 223 
 endif
     !write(*,*) 'volume v0=',v0
 

 if(fdata.eq.'F'.or.fdata.eq.'f') then
  
    write(*,*)
    write(*,'(a)', advance='no' ) '---> Enter the total number of atoms in the primitive cell: VFREE = '
     read(*,*) vfree
    write(*,*)
    write(*,'(a)', advance='no')'---> Enter :The molecular Masse of the primitive cell in a.m.u: mm = '
    read(*,*) ma
    write(*,*)
     mm=ma
    write(*,*)
    write(*,'(a)', advance='no')'---> Enter the phase name (free format) : phase_name =  '
    read(*,*) phase
    write(*,*)'          ============='
  goto 212
endif

    write(*,*)
    write(*,*)'>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>'
    write(*,*)

open(34,file='atome')
open(35,file='atnumber')

nl=0
    do
       read(34,*, IOSTAT=io)  tst
      if (io > 0) then
	   WRITE(*,*) 'Check input.  Something was wrong'
	   exit
      else if (io < 0) then
	   exit
      else
	  nl = nl+1
     end if
    end do
close(34)
close(35)

    allocate(elem(nl)) 
    allocate(nelem(nl)) 

open(35,file='atnumber')
open(34,file='atome')

      nb=0
    do i=1,nl
      read(34,*) elem(i) 
      read(35,*) nelem(i)
      nb=nb+nelem(i) 
    enddo
close(34)
close(35)
    write(*,*) '---| The total number of atoms in the cell:', nb
     vfree=nb
    write(*,*) '-----------------------------------------------------------------------------'
    write(*,*) '  element |  Z           |  N. elements   |    mass_atomic   | N*masse_atomic'
    write(*,*) '----------|--------------|----------------|------------------|---------------'
    
    ma=0.0
    do i=1,nl
      eleme=elem(i)
      natom=nelem(i)  
      call find_za_ma(eleme,z,matm)

      write(*,*) '  ', eleme,'      |', z,' | ',natom,'  | ',matm,' | ', natom*matm
      ma=ma + natom*matm
    enddo
    write(*,*) '-----------------------------------------------------------------------------'
    
    write(*,*)
    write(*,*)'---| The calcculated molecular Mass  in a.m.u is >>>  mm=', ma
    write(*,*)'&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&'
    write(*,*)
    write(*,*)
    write(*,*) 

    open(445,file='phase')
    read(445,*) phase
    close(445)

    write(*,*)'---| The name of phase is: ',phase
    !write(*,*)'          ============='
    
212 write(*,*)
    open(100,file='case.ing' , status='unknown' )
    write(100,111) ma
    111 format ('mm', F10.4 )
    write(100,112)vfree
    112 format ('vfree ',i2)
    write(*,*) ''
    write(*,*) '&&&&& the pressure range: P_init; P_step ;P_final &&&&&'
    write(*,*) ' ' 
    write(*,'(a)', advance='no' ) '--> pressure range (GPa): P_init = '
    read (*,*) pi  
   write(*,'(a)', advance='no') '--> pressure range (GPa): P_step = '
   read (*,*) sp
   write(*,'(a)', advance='no') '--> pressure range (GPa): P_final = '
   read (*,*) pf
   write(100,222)   pi,sp,pf
   write(*,*)
   write(*,*) '&&&&&& the temperature range (K): T_init; T_step ; T_final &&&&&'
   write(*,*)
   write(*,'(a)', advance='no') '--> temperature range: T_init = '
   read (*,*) ti  
   write(*,'(a)', advance='no') '--> temperature range: T_step = '
   read (*,*) st
   write(*,'(a)', advance='no') '--> temperature range: T_final= '
   read (*,*) tf
   write(100,203) ti,st,tf
   write(100,224)  phase ,fitm
  
    !write(*,*)'==================================================================='
    write(*,*)
    write(*,*) ' The units of Gibbs2 : volumes (in bohr^3  ) and energies (in Hartree ).'
    write(*,*)'==================================================================='
    do i=1,n
    a(i,2)=a(i,2)*1/2.0              ! Convert Ry to Ha ; Ha=1/2*Ry
    a(i,1)=a(i,1)*1/1.0
    write(100,*) (a(i,j) ,j=1,2)      !  
    !write(*,*) (a(i,j) ,j=1,2) 
    enddo
    write(100,*)'endphase'
    close(100)
    write(*,*)
    write(*,*)
    write(*,*) '==========================================================='
    !write(*,*) ' les resultats sont enregistres dans le fichier: case.ing  '
    write(*,*) '     out data  in file:   case.ing  '
    write(*,*) '==========================================================='
    write(*,*)
    write(*,*)
     
goto 999
7001 write(*,*) '...Oooops.... ERROR....: must be integer '
stop
7000 write(*,*)
    write(*,*)
    write(*,*) '==========================================================================='

    write(*,*) 'Error: The files  " case.outputeos " case.struct"   not found '
    write(*,*) '      you must run this program in a directory containing this file '
    write(*,*) 
    write(*,*) '==========================================================================='
    write(*,*)
	  STOP

999 write(*,*)
    write(*,*)



222 format ('pressure', 3F10.2 )
203 format ('temperature', 3F10.2 )
224 format ('phase ',A10 ,'   fit   ' , A10 )
1000 FORMAT(A10)

end
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    

subroutine find_za_ma(name,za,ma)
      character*2 name
      character*2 nam
      real  ma
      integer za
nam = name(1:2) 
    if (nam.eq.'H')   za= 1
    if (nam.eq.'He')  za= 2
    if (nam.eq.'Li')  za= 3
    if (nam.eq.'Be')  za= 4
    if (nam.eq.'B')   za= 5
    if (nam.eq.'C')   za= 6
    if (nam.eq.'N')   za= 7
    if (nam.eq.'O')   za= 8
    if (nam.eq.'F')   za= 9
    if (nam.eq.'Ne')  za= 10
    if (nam.eq.'Na')  za= 11
    if (nam.eq.'Mg')  za= 12
    if (nam.eq.'Al')  za= 13
    if (nam.eq.'Si')  za= 14
    if (nam.eq.'P')   za= 15
    if (nam.eq.'S')   za= 16
    if (nam.eq.'Cl')  za= 17
    if (nam.eq.'Ar')  za= 18
    if (nam.eq.'K')   za= 19
    if (nam.eq.'Ca')  za= 20
    if (nam.eq.'Sc')  za= 21
    if (nam.eq.'Ti')  za= 22
    if (nam.eq.'V')   za= 23
    if (nam.eq.'Cr')  za= 24
    if (nam.eq.'Mn')  za= 25
    if (nam.eq.'Fe')  za= 26
    if (nam.eq.'Co')  za= 27
    if (nam.eq.'Ni')  za= 28
    if (nam.eq.'Cu')  za= 29
    if (nam.eq.'Zn')  za= 30
    if (nam.eq.'Ga')  za= 31
    if (nam.eq.'Ge')  za= 32
    if (nam.eq.'As')  za= 33
    if (nam.eq.'Se')  za= 34
    if (nam.eq.'Br')  za= 35
    if (nam.eq.'Kr')  za= 36
    if (nam.eq.'Rb')  za= 37
    if (nam.eq.'Sr')  za= 38
    if (nam.eq.'Y')   za= 39
    if (nam.eq.'Zr')  za= 40
    if (nam.eq.'Nb')  za= 41
    if (nam.eq.'Mo')  za= 42
    if (nam.eq.'Tc')  za= 43
    if (nam.eq.'Ru')  za= 44
    if (nam.eq.'Rh')  za= 45
    if (nam.eq.'Pd')  za= 46
    if (nam.eq.'Ag')  za= 47
    if (nam.eq.'Cd')  za= 48
    if (nam.eq.'In')  za= 49
    if (nam.eq.'Sn')  za= 50
    if (nam.eq.'Sb')  za= 51
    if (nam.eq.'Te')  za= 52
    if (nam.eq.'I')   za= 53
    if (nam.eq.'Xe')  za= 54
    if (nam.eq.'Cs')  za= 55
    if (nam.eq.'Ba')  za= 56
    if (nam.eq.'La')  za= 57
    if (nam.eq.'Ce')  za= 58
    if (nam.eq.'Pr')  za= 59
    if (nam.eq.'Nd')  za= 60
    if (nam.eq.'Pm')  za= 61
    if (nam.eq.'Sm')  za= 62
    if (nam.eq.'Eu')  za= 63
    if (nam.eq.'Gd')  za= 64
    if (nam.eq.'Tb')  za= 65
    if (nam.eq.'Dy')  za= 66
    if (nam.eq.'Ho')  za= 67
    if (nam.eq.'Er')  za= 68
    if (nam.eq.'Tm')  za= 69
    if (nam.eq.'Yb')  za= 70
    if (nam.eq.'Lu')  za= 71
    if (nam.eq.'Hf')  za= 72
    if (nam.eq.'Ta')  za= 73
    if (nam.eq.'W')   za= 74
    if (nam.eq.'Re')  za= 75
    if (nam.eq.'Os')  za= 76
    if (nam.eq.'Ir')  za= 77
    if (nam.eq.'Pt')  za= 78
    if (nam.eq.'Au')  za= 79
    if (nam.eq.'Hg')  za= 80
    if (nam.eq.'Tl')  za= 81
    if (nam.eq.'Pb')  za= 82
    if (nam.eq.'Bi')  za= 83
    if (nam.eq.'Po')  za= 84
    if (nam.eq.'At')  za= 85
    if (nam.eq.'Rn')  za= 86
    if (nam.eq.'Fr')  za= 87
    if (nam.eq.'Ra')  za= 88
    if (nam.eq.'Ac')  za= 89
    if (nam.eq.'Th')  za= 90
    if (nam.eq.'Pa')  za= 91
    if (nam.eq.'U')   za= 92
    if (nam.eq.'Np')  za= 93
    if (nam.eq.'Pu')  za= 94
    if (nam.eq.'Am')  za= 95
    if (nam.eq.'Cm')  za= 96
    if (nam.eq.'Bk')  za= 97
    if (nam.eq.'Cf')  za= 98
    if (nam.eq.'Es')  za= 99
    if (nam.eq.'Fm')  za= 100
    if (nam.eq.'Md')  za= 101
    if (nam.eq.'No')  za= 102
    if (nam.eq.'Lr')  za= 103
    if (nam.eq.'Rf')  za= 104
    if (nam.eq.'Db')  za= 105
    if (nam.eq.'Sg')  za= 106
    if (nam.eq.'Bh')  za= 107
    if (nam.eq.'Hs')  za= 108
    if (nam.eq.'Mt')  za= 109

if (nam.eq.'H')   ma= 1.0079
if (nam.eq.'He')  ma= 4.0026
if (nam.eq.'Li')  ma= 6.941
if (nam.eq.'Be')  ma= 9.0122
if (nam.eq.'B')   ma= 10.811
if (nam.eq.'C')   ma= 12.0107
if (nam.eq.'N')   ma= 14.0067
if (nam.eq.'O')   ma= 15.9994
if (nam.eq.'F')   ma= 18.9984
if (nam.eq.'Ne')  ma= 20.1797
if (nam.eq.'Na')  ma= 22.9897
if (nam.eq.'Mg')  ma= 24.305
if (nam.eq.'Al')  ma= 26.9815
if (nam.eq.'Si')  ma= 28.0855
if (nam.eq.'P')   ma= 30.9738
if (nam.eq.'S')   ma= 32.065
if (nam.eq.'Cl')  ma= 35.453
if (nam.eq.'Ar')  ma= 39.948
if (nam.eq.'K')   ma= 39.0983
if (nam.eq.'Ca')  ma= 40.078
if (nam.eq.'Sc')  ma= 44.9559
if (nam.eq.'Ti')  ma= 47.867
if (nam.eq.'V')   ma= 50.9415
if (nam.eq.'Cr')  ma= 51.9961
if (nam.eq.'Mn')  ma= 54.938
if (nam.eq.'Fe')  ma= 55.845
if (nam.eq.'Co')  ma= 58.9332
if (nam.eq.'Ni')  ma= 58.6934
if (nam.eq.'Cu')  ma= 63.546
if (nam.eq.'Zn')  ma= 65.39
if (nam.eq.'Ga')  ma= 69.723
if (nam.eq.'Ge')  ma= 72.64
if (nam.eq.'As')  ma= 74.9216
if (nam.eq.'Se')  ma= 78.96
if (nam.eq.'Br')  ma= 79.904
if (nam.eq.'Kr')  ma= 83.8
if (nam.eq.'Rb')  ma= 85.4678
if (nam.eq.'Sr')  ma= 87.62
if (nam.eq.'Y')   ma= 88.9059
if (nam.eq.'Zr')  ma= 91.224
if (nam.eq.'Nb')  ma= 92.9064
if (nam.eq.'Mo')  ma= 95.94
if (nam.eq.'Tc')  ma= 98.
if (nam.eq.'Ru')  ma= 101.07
if (nam.eq.'Rh')  ma= 102.9055
if (nam.eq.'Pd')  ma= 106.42
if (nam.eq.'Ag')  ma= 107.8682
if (nam.eq.'Cd')  ma= 112.411
if (nam.eq.'In')  ma= 114.818
if (nam.eq.'Sn')  ma= 118.71
if (nam.eq.'Sb')  ma= 121.76
if (nam.eq.'Te')  ma= 127.6
if (nam.eq.'I')   ma= 126.9045
if (nam.eq.'Xe')  ma= 131.293
if (nam.eq.'Cs')  ma= 132.9055
if (nam.eq.'Ba')  ma= 137.327
if (nam.eq.'La')  ma= 138.9055
if (nam.eq.'Ce')  ma= 140.116
if (nam.eq.'Pr')  ma= 140.9077
if (nam.eq.'Nd')  ma= 144.24
if (nam.eq.'Pm')  ma= 145.
if (nam.eq.'Sm')  ma= 150.36
if (nam.eq.'Eu')  ma= 151.964
if (nam.eq.'Gd')  ma= 157.25
if (nam.eq.'Tb')  ma= 158.9253
if (nam.eq.'Dy')  ma= 162.5
if (nam.eq.'Ho')  ma= 164.9303
if (nam.eq.'Er')  ma= 167.259
if (nam.eq.'Tm')  ma= 168.9342
if (nam.eq.'Yb')  ma= 173.04
if (nam.eq.'Lu')  ma= 174.967
if (nam.eq.'Hf')  ma= 178.49
if (nam.eq.'Ta')  ma= 180.9479
if (nam.eq.'W')   ma= 183.84
if (nam.eq.'Re')  ma= 186.207
if (nam.eq.'Os')  ma= 190.23
if (nam.eq.'Ir')  ma= 192.217
if (nam.eq.'Pt')  ma= 195.078
if (nam.eq.'Au')  ma= 196.9665
if (nam.eq.'Hg')  ma= 200.59
if (nam.eq.'Tl')  ma= 204.3833
if (nam.eq.'Pb')  ma= 207.2
if (nam.eq.'Bi')  ma= 208.9804
if (nam.eq.'Po')  ma= 209.
if (nam.eq.'At')  ma= 210.
if (nam.eq.'Rn')  ma= 222.
if (nam.eq.'Fr')  ma= 223.
if (nam.eq.'Ra')  ma= 226.
if (nam.eq.'Ac')  ma= 227.
if (nam.eq.'Th')  ma= 232.0381
if (nam.eq.'Pa')  ma= 231.0359
if (nam.eq.'U')   ma= 238.0289
if (nam.eq.'Np')  ma= 237.
if (nam.eq.'Pu')  ma= 244.
if (nam.eq.'Am')  ma= 243.
if (nam.eq.'Cm')  ma= 247.
if (nam.eq.'Bk')  ma= 247.
if (nam.eq.'Cf')  ma= 251.
if (nam.eq.'Es')  ma= 252.
if (nam.eq.'Fm')  ma= 257.
if (nam.eq.'Md')  ma= 258.
if (nam.eq.'No')  ma= 259.
if (nam.eq.'Lr')  ma= 262.
if (nam.eq.'Rf')  ma= 261.
if (nam.eq.'Db')  ma= 262.
if (nam.eq.'Sg')  ma= 266.
if (nam.eq.'Bh')  ma= 264.
if (nam.eq.'Hs')  ma= 277.
if (nam.eq.'Mt')  ma= 268.

if(za.eq.0.or.za.gt.109) then
za=0
ma=0
endif

if(ma.eq.0.0.or.ma.gt.260.) then
za=0
ma=0
endif
  if(za.gt.0 ) then
  if( za.lt.110) then
!   write(*,*)''
!   write(*,*) 'the number atomic of   ',nam,'   is z= ', za
!   write(*,*) 'the atomic masse of    ',nam,'   is ma=   ',ma
 !  write(*,*) '=================='
endif  
endif
      return
      end

