https://camo.githubusercontent.com/f96fc394972e4982d57efb3f4bc61184351ea2cba8241661b95eeef94c1c68a4/68747470733a2f2f70726f66696c652d636f756e7465722e676c697463682e6d652f456c41546f6f6c732f636f756e742e737667
required packages : ( git , gfortran | ifort  , gnuplot , epstopdf , pdfcrop).

Alternatively, clone the git repository for the latest version of the code (last modiefed: 12/01/2024):
   
    git clone https://github.com/ghlam14/gibbs2m2024.git
 
to install:

       cd gibbs2m2024/
       
       chmod +x install.sh
              
       ./install.sh
To run only a simple exemple (quick run) for more option, my should read the userguide :
      https://aoterodelaroza.github.io/gibbs2/quickstart/


To run exemple:
for wien2k user: need   .struct file and .outputeos  files

after volume  optimization (to  get  case.outputeos file ) you can run  the gibbs2  by the following command line:

# to set and run gibbs2 
    set_gibbs2            
# to plot the results
    plot_gibbs2            

