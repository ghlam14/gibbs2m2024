required packages : ( git , gfortran | ifort  , gnuplot , epstopdf , pdfcrop).

Alternatively, clone the git repository for the latest version of the code (last modiefed: 12/01/2024):
   
    git clone https://github.com/ghlam14/gibbs2m2024.git
 
to install:

       cd gibbs2m2024/
       
       chmod +x install.sh
              
       ./install.sh

To run exemple:
for wien2k user: need   .struct file and .outputeos  files

after volume  optimization (to  get  case.outputeos file ) you can run  the gibbs2  by the following command line:
# ( to set and run gibbs2 ) 
    set_gibbs2            
# to plot the results
    plot_gibbs2            

