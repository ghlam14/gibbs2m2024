implicit none
     real rr, test
     integer i,nc ,nii,ii,nl,io ,k
     character*8 acar(5000,2),pinit
open(550,file='case.eos')
do i=1,8
  read(550,*)
enddo
read(550,*) pinit
write(*,*)
write(*,*) 'initial pressure is: ', pinit
write(*,*)
close(550)
open(55,file='case.eos')
open(19,file='p')
if( pinit .eq. '0.0000' ) then
call system( "sed '/^ *$/d' case.eos | awk '$1+0==0' | awk '/# Phase/{print ""; print ""} {print}'> vT") 
else
call system( "sed '/^ *$/d' case.eos | awk '$1+0=="//pinit//"' | awk '/# Phase/{print ""; print ""} {print}'> vT") 
endif
open(210,file='vT')
open(21,file='T')
open(211,file='vT')
!==================================nombre de lignes============= 
 do i=1,9
  read(211,*)
  enddo
    
 nl=0
 DO
 !WRITE(*,*) test
   READ(211,*,IOSTAT=io)  test
  
   IF (io > 0) THEN
      WRITE(*,*) 'Check input.  Something was wrong'
      EXIT
   ELSE IF (io < 0) THEN
      WRITE(*,*)  'The total nbr T is ', nl
        EXIT
   ELSE
      nl = nl+1
   END IF
END DO 
  close(211)
!==================================================================  



  do i=1,7
  read(55,*)
  read(210,*)
  enddo
  
open(14, file='num')
 read(14,*) nii
 ii=nii
close(14)

  read(210,*)
  read(210,*)
  read(55,*) 
  do  i=1,nii
     read(55,*) acar(i,1)
     write(19,*)acar(i,1)
     
  enddo
  do i=1,nl
  read(210,*) rr,acar(i,2)
  write(21,*)acar(i,2)
  enddo
if(nl.lt.10) then
k=int( float(nl)/5.) 
else
k=int( float(nl)/10.)  
endif
 write(*,*) 'step=',k 
close(210)
close(21)
close(19)
close(55)

if ( acar(1,1).eq.'0.00' ) acar(1,1) ='0'
if ( acar(1,2).eq.'0.00' ) acar(1,2) ='0'
if ( acar(i,1).eq.'0.00' ) acar(i,1) ='0'
if ( acar(i,2).eq.'0.00' ) acar(i,2) ='0'

if( nl.lt.2 ) then

if (nii .lt.2) then
call system( "sed '/^ *$/d' case.eos | awk '$1+0=="//acar(1,1)// "' | awk '/# Phase/{print ""; print ""} {print}'>> "//'T_eos_P='&
//acar(1,1)//"")
do i=2,nii
call system ( "grep  '#'   case.eos  > "//'T_eos_P='//acar(i,1)//"")
call system( "sed '/^ *$/d' case.eos | awk '$1+0=="//acar(i,1)// "' | awk '/# Phase/{print ""; print ""} {print}'>> "//'T_eos_P='&
//acar(i,1)//"")
enddo
endif



call system( "sed '/^ *$/d' case.eos | awk '$2+0=="//acar(1,2)// "' | awk '/# Phase/{print ""; print ""} {print}'>> "//'P_eos_T='&
//acar(1,2)//"")


do i=k,nl,k
call system ( "grep  '#'   case.eos  > "//'P_eos_T='//acar(i,2)//"")
call system( "sed '/^ *$/d' case.eos | awk '$2+0=="//acar(i,2)// "' | awk '/# Phase/{print ""; print ""} {print}'>> "//'P_eos_T='&
//acar(i,2)//"")
enddo
endif
end 

	