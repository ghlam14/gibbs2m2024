#!/bin/tcsh -f
echo ""
echo ""

if( ! -e gibbs2 ) then
echo ""
echo "************************************* "
echo "#         Download Gibbs2              "
echo "           please wait ...               "
echo "************************************* "
echo ""
echo ""

git clone https://github.com/aoterodelaroza/gibbs2.git
echo ""
endif
echo "************************************* "
echo "#         Install Gibbs2              "
echo "           01/2024               "
echo "************************************* "
echo ""
echo ""
echo ""
sleep 1
foreach list (d_eos  d_gibs.f90      plot_gibbs2  set_gbbs2.f90  set_gibbs2 )
cp $list gibbs2/src/
end


cd gibbs2/src/

cat <<EOF>> Makefile  
gibs:	
	
	\$(FC) set_gbbs2.f90  -o setupgibbs2 
	\$(FC) d_gibs.f90     -o  d_gibs 
EOF

echo "************************************* "
echo "#     choose: Fortran Compiler                "
echo "************************************* "
echo ""
echo ""
echo "The Intel(R) fortran compiler (ifort) "
echo "The GNU fortran compiler     (gfortran)"
echo ""
cat<<EOF > Makefile.inc

  #-------------------- 
  # gfortran Compiler 
  #-------------------

ifeq (\$(DEBUG),1)
  FC = gfortran
  FCFLAGS = -g -fbounds-check -Wall -Wunused-parameter -ffpe-trap=invalid -fbacktrace -fdump-core
  LDFLAGS = 
  AR = ar
  EXE =
else
  FC = gfortran
  FCFLAGS = -O3
  LDFLAGS = 
  AR = ar
  EXE =
endif

EOF



echo -n " Choose your compiler (defaut: gfortran):  (ifort/ gfortran) :  "
    set yn = ($<)
    if ($yn == '' ) then
    set yn = gfortran 
    endif
echo ""  
echo "" 
    if ($yn == ifort ) then
echo "================= "
echo " ifort  Compiler "
echo "=================" 
sleep 1
cat <<EOF > Makefile.inc
  #----------------------- 
  # ifort  Compiler 
  #---------------------- 
## The Intel(R) fortran compiler (ifort)
ifeq (\$(DEBUG),1)
 FC = ifort
 FCFLAGS = -g -CU -C -traceback -fpe0 -debug
 LDFLAGS = 
 AR = ar
 EXE =
else
 FC = ifort
 FCFLAGS = -O3 
 LDFLAGS = -O3
 AR = ar
 EXE =
endif
EOF

endif
echo "" 
echo "" 
if ($yn == gfortran ) then
echo "=================== "
echo " gfortran Compiler "
echo "===================" 
sleep 1
endif


echo "================================="
echo "         make gibbs2 "
echo "================================"
sleep 1
make
make gibs
echo "" 
echo "" 
echo "================================="
echo "         make debug  gibbs2 "
echo "================================"
sleep 2
 make debug
 echo "" 
 echo "" 
make veryclean
echo "================================="
echo "         install Gibbs2          "
echo "================================ "
echo "" 

sleep 2

if (! -e gibbs2) then
goto error1

else
mkdir -p ../bin
cd ../bin/


foreach i ( set_gibbs2 setupgibbs2 d_gibs gibbs2 gibbs2_dbg plot_gibbs2  d_eos)
  ln -s -f  ../src/$i ./$i
  chmod +x $i
end
setenvironment:
#make sure these files exist
touch ~/.bashrc
        cp ~/.bashrc ~/.bashrc.saveg
        cp ~/.bashrc ~/.bashrc.saved_$$


set gib = `grep "# added by Gibbs2 "  ~/.bashrc | wc -l`
if ($gib > 0) then
	sed -i '/# added by Gibbs2  BEGIN/,/# added by Gibbs2 END/d' ~/.bashrc 
 	    
endif

set bindir=`cd ../bin; pwd`

cat <<EOF >>~/.bashrc	
           
## added by Gibbs2  BEGIN  ##   
 export PATH=${bindir}:\$PATH
## added by Gibbs2 END ##

EOF


cat <<EOF

   *********************************************************
   
   Your   environment for Gibbs2   is now configured.

          You have to restart your shell 
     
---QUICK Run--------------------------------
   Start :    set_gibbs2
   ======
       or:    gibbs2   case.ing   case.out
       
------------------------------------
       
Note:All programs are original  copyright for:
     Author:Alberto Otero-de-la-Roza, Víctor Luaña and David Abbasi.
     Contact:alberto@carbono.quimica.uniovi.es
     Contact:victor@carbono.quimica.uniovi.es
     Version:1.0 (jan. 2011)
*********************************************************** 
EOF
endif
    
        rm ~/.bashrc.saveg
exit(0)
error1:
echo ">>>>>"
echo ">>   Gibbs2 not compiled, check your compiler, (need :Fortran compiler)"
echo ">>>>>"
exit(1)

