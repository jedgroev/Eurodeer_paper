mkdir <-
function(dirx='/media/jedgroev/DATA/JOHANNES_DATA/PhD/My_Papers/Paper_SAM_EuroDEER/HRE_animals/results',x, mvtr = mv_tr, setdir=TRUE){
# This function creates directories from a certain start directory (here results). mvtr refers to the fact if a proportion of missing values is removed and setdir if the new directory is accessed. 

# dirx is the directory from where to start 
# x = directory name e.g. bootstrap 
# mvtr = how many missing values are removed
# imputed = set in the beginning and is TRUE when imputations have been performed (i.e. missing values are reassigned to forest and open habitat)
# setdir if you want to go to the newly created directory 
if(imputed==TRUE){
setwd(dirx)
if(dir.exists(paste0(x,'_mv',mvtr))==FALSE){dir.create(paste0(x,'_mv',mv_tr))}
if(setdir == TRUE){setwd(paste0(x,'_mv',mvtr))}
} else { if (imputed==FALSE) {
if(dir.exists(paste0(x,mvtr))==FALSE){dir.create(paste0(x,mv_tr))}
if(setdir == TRUE){setwd(paste0(x,mvtr))}
}}
}
