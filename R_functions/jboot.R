jboot <-
function(x, rep=10, clusters=clusters_nr, setseed=100, subset = 'all'){
# x is svv
# rep is number of bootstraps
# clusters is number of clusters that are evaluated
# setseed is to make it replicable
# subset has three values: 'all', 'simulations', 'real'   

require(TraMineR)
require(fpc)
if (subset == 'simulations'){
x <- x[which(x[,svv_length] == eps[which(eps$s.labels == 'missing'),c('s.alphabet')]),]
} else {
if(subset == 'real'){
x <- x[which(x[,svv_length] != eps[which(eps$s.labels == 'missing'),c('s.alphabet')]),]
} else { x <- x
}
}

seq_rand_50 <- seqdef2(x,len=seq_length)
# states 
s.states <- data.frame(s.alphabet=c(seqstatl(x[,1:seq_length], format='STS')), stringsAsFactors=FALSE)
eps_l <- as.data.frame(merge(s.states,eps,type='left',by='s.alphabet'), stringsAsFactors=FALSE)
### Hamming_Distance
if ('xx' %in% eps_l$s.alphabet | 'missing' %in% eps_l$s.alphabet | 'NULL' %in% eps_l$s.alphabet | 'NA' %in% eps_l$s.alphabet | 'NaN' %in% eps_l$s.alphabet | '-9999' %in% eps_l$s.alphabet | 'nan' %in% eps_l$s.alphabet){
hd_rand_50 <- seqdist(seq_rand_50, method = "HAM", with.missing = TRUE) # , sm = Autocor_50[[i]], with.missing = TRUE        
} else {
hd_rand_50 <- seqdist(seq_rand_50, method = "HAM")
        }
colnames(hd_rand_50) <- rownames(x[[1]])
rownames(hd_rand_50) <- rownames(x[[1]])
ward_rand_50 <- hclust(as.dist(hd_rand_50), method = "ward.D2") 


# Generate bootmean table
bootmean_50 <- data.frame(value=NA)
bootmean_50 <- cbind(data.frame(mean=NA,sd=NA,median=NA,clusters=NA,stringsAsFactors=FALSE),bootmean_50[,rep('value',each=clusters_nr)])
colnames(bootmean_50) <- c(colnames(bootmean_50)[which(substr(colnames(bootmean_50),1,5) != 'value')], paste0('value_',c(1:clusters_nr)))

# Generate partition table
partition_50 <- data.frame(partition=rep(NA, length(x[,1])))
partition_50 <- partition_50[,rep('partition',each=clusters_nr-1)]
colnames(partition_50) <- c(paste0('partition',c(1:(clusters_nr-1))))

for (j in 2:clusters_nr){
j.boot<-clusterboot(as.dist(hd_rand_50), clustermethod=disthclustCBI, k=j, B = rep, seed = setseed, count = TRUE, cut="number", method="ward.D2") # calculate bootmean for 2 to 20 clusters and extract those values from the clusterboot result 
values<-j.boot$bootmean 
bootmean_50[j-1,1] <- mean(values)
bootmean_50[j-1,2] <- sd(values)
bootmean_50[j-1,3] <- median(values)
bootmean_50[j-1,4] <- j
for (h in 1:j)
{
bootmean_50[[j-1,h+4]] =  values[h] # insert values ("values") in the data frame
}

values4 <- as.vector(j.boot$partition) # calculate partition for 2 to 20 clusters and extract those values from the clusterboot result
for (g in 1:length(x[,1]))
{
partition_50[g,j-1] = values4[g] # insert vectors ("values4") in the data frame 
rownames(partition_50) <- rownames(x)
}

}
b.seqdef <- seq_rand_50
b.hamming <- hd_rand_50
b.ward <- ward_rand_50
b.bootmean <- bootmean_50
b.partition <- partition_50
return(list(b.seqdef,b.hamming,b.ward,b.bootmean,b.partition))

# EXAMPLES 
# BOOTSTRAPPING USING PARALLEL
# no_cores <- detectCores()
# cl<-makeCluster(no_cores)
# boot_res <- mclapply(srvv, function(x) jboot(x,rep=1000))
# stopCluster(cl)
# BOOTSTRAPPING WITHOUT PARALLEL
# boot_res <- lapply(srvv, function(x) jboot(x,rep=10))

# EXTRACT RESULTS FROM BOOT_RES
# seqdef_res <- lapply(boot_res, function(x) x[[1]])
# hd_res <- lapply(boot_res, function(x) x[[2]])
# bootmean_res<- lapply(boot_res, function(x) x[[3]])
# partition_res <- lapply(boot_res, function(x) x[[4]])
}
