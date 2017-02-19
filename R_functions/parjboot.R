parjboot <-
function (x,clustertype='MPI',numCores=3, bootpar=c(10, 15,'all')){
# x is srvv - list of data.frames structured as the input for jboot
# clustertype = MPI or SOCK
# numCores = number of cores

# bootpar are the arguments for jboot, respectively rep (10), clusters (15) and subset (all). Note that the number of clusters cannot be higher than the number of sequences.  
# rep = number of bootstraps
# clusters = number of clusters that are evaluated
# subset has three values: 'all', 'simulations', 'real'   

if(clustertype == 'SOCK') 
{
# Parallel with SOCK 
library(snow)
library(snowfall)
        snowfall::sfInit(parallel=TRUE, cpus=numCores, type="SOCK")
        # Exporting needed data and loading required packages on workers
        snowfall::sfLibrary("TraMineR",character.only = TRUE)
snowfall::sfLibrary("fpc",character.only = TRUE)
snowfall::sfExport("seqdef2","plotseq","fixes","days","bootnr","seq_length","svv_length","clusters_nr","eps")# Distribute calculation: will return values as a list object
# Do parallel computation   
        result <- snowfall::sfLapply(x, jboot, rep=as.numeric(bootpar[1]), clusters=as.numeric(bootpar[2]), subset=bootpar[3])
        # Destroy cluster
        snowfall::sfStop()
} else { 
if(clustertype == 'MPI') {
# Parallel with MPI         
        library(parallel)
#numCores <- 3
        cl <- parallel::makeCluster(numCores, type='MPI')
        parallel::clusterExport(cl, list("seqdef2","plotseq","fixes","days","bootnr","seq_length","svv_length","clusters_nr","eps"))
parallel::clusterEvalQ(cl, library(TraMineR))
        parallel::clusterEvalQ(cl, library(fpc))
        result <- parallel::parLapply(cl, x, jboot, rep=as.numeric(bootpar[1]), clusters=as.numeric(bootpar[2]), subset=bootpar[3])
        parallel::stopCluster(cl)
   #     mpi.exit()
   #     print(unlist(res))
}}
return(result)

# EXAMPLE 
# srvv_sub <- srvv[names(srvv)[1:4]]
# result <- parjboot(srvv_sub, 'SOCK', numCores=3, bootpar=c(10,15,'real'))
}
