# Eurodeer_paper
This github page includes all scripts that were created for the application of Sequence Analysis Methods (SAM) on ecological data. In the future this page will be updated and documented more extensively when all tools are properly developed. 

## Following scripts are included
### SAM
* **seqdef2.R**   
This sequence definition includes all the important parameters that need to be set to generate our trees.
* **plotseq.R**   
Plots the sequences according to the order of the labels after clustering.
* **SAM.R**   
General function to plot trees (includes the former two functions: seqdef2.R and plotseq.R). 
* **SAMext.R** (to be improved)   
Extended function to plot trees.

### Bootstrapping
* **jboot.R** 	
Bootstrap function
* **parjboot.R** 	
Parallel version of bootstrap function

### plots 
* **fittedSpline.R**  
Function to plot fitted splines.
* **glines.R** (gradient lines)
Function to plot lines with decreasing and increasing width.
* **glegend.R**    
Function to plot the legend of glines. 
* **gradient.R**   
Function to add a color gradient legend within plotting window.

### Colors
* **addalpha.R**    
Add transparency to colors.
* **cole.R** (color explorer)    
Plot colors to explore their visibility when combined (mixed randomly or ordered). 
* **color.scales.R** (to be improved)   
Different nice color scales.
* **darklight.R** (to be improved - sometimes error when 'darken' is used)    
Make colors slightly darker or lighter.

### Data imputation
* **impute_mv.R**     
Impute missing values (biological meaning). 
* **impute_nd.R**     
Impute nodata (no biological meaning).  

### Others
* **mkdir.R** 	
* **roundUp.R**	
* **varmax.R** 	   
