plotseq <-
function(x, labs, out_seqdef2=seq_rand_50){ # plots the sequences according to ordered sequences 
# unlike the seqIplot, this adjusted version plots the sequences according to the order of the labels after clustering. This in combination with the tree gives you a strong visualisation of the clustering 

# x = sequences with variables, svv   
# labs = labels_rand_50, ordered labels of the dendrogram
# out_seqdef2 = seq_rand_50, sequence definition with variables
labels_rand_501 <- data.frame(labels_rand_50=labs, ID=1:length(labs))
id_rand_50 <- as.vector(rownames(x))
rand_50 <- labels_rand_501[match(id_rand_50, labels_rand_501$labels_rand_50), ]
add_to_dvs_rand_50 <- cbind(x, rand_50)
seqIplot(out_seqdef2, border = NA, sortv = add_to_dvs_rand_50$ID, withlegend = FALSE, group = NULL, ylab = NA, yaxis = FALSE, xlab = NA, xaxis = FALSE)

# To run this function you need information about the label order after clustering, the sequence dataframe, a seqdef object: 

# seq_rand_50 <- seqdef2(svv[[1]],len=seq_length) ## Define sequence object 
# hd_rand_50 <- seqdist(seq_rand_50, method = "HAM") ## Hamming Distance
#colnames(hd_rand_50) <- rownames(svv) ## Set row and colnames
#rownames(hd_rand_50) <- rownames(svv) ## Set row and colnames
#ward_rand_50 <- hclust(as.dist(hd_rand_50), method = "ward.D2") ## Ward clustering
#dend_rand_50 <- as.dendrogram(ward_rand_50) ## transform into dendrogram object
#labels_rand_50 <- labels(dend_rand_50) ## get the labels from dendrogram to have the order of the sequences
#labels(dend_rand_50) <- (rep("",length(svv[[1]][,1]))) ## remove labels for visualisation purposes
#ll <- layout(matrix(c(1,1,2,2,2,2),2,3)) ## set layout
#par (mar = c(1,1,1,1), oma = c(6,2,1,2), cex = 0.5, bg=colors()[c(204)]) ## set par
#### PLOT TREE AND SEQUENCES 
#plot(dend_rand_50, horiz=TRUE) ## plot tree  
#seq_rand_50 <- seqdef2(svv[[1]],len=svv_length) ## redefine the seqdef with seqdef2 in order to include variables in the plot 
#plotseq(svv[[1]], labels_rand_50, seq_rand_50) ## plot reordered sequences 
#axis(1, at=seq(from=0, to= seq_length, by= seq_length/days), labels=c(0:days)) ## add an axes with daily lines

# Similarily this can be ran over a list of dataframes 
#seq_rand_50 <- lapply(svv, function(x) seqdef2(x,len=seq_length))
### Hamming_Distance
#hd_rand_50 <- lapply(seq_rand_50, function(x) seqdist(x, method = "HAM")) # , sm = Autocor_50[[i]], with.missing = TRUE
#for (i in 1:length(svv)){
#colnames(hd_rand_50[[i]]) <- rownames(svv[[i]])
#rownames(hd_rand_50[[i]]) <- rownames(svv[[i]])
#}
### Perform the clustering (ward.D2) 
#ward_rand_50 <- lapply(hd_rand_50, function(x) hclust(as.dist(x), method = "ward.D2")) 
#dend_rand_50 <- lapply(ward_rand_50, function(x) as.dendrogram(x)) 
#labels_rand_50 <- lapply(dend_rand_50, function(x) labels(x)) ## get the labels from dendrogram to have the order of the sequences
#for (i in 1:length(svv)) {
#labels(dend_rand_50[[i]]) <-(rep("",length(svv[[i]][,1])))
#}
#ll <- layout(matrix(c(1,1,2,2,2,2),2,3)) ## set layout
#par (mar = c(1,1,1,1), oma = c(6,2,1,2), cex = 0.5, bg=colors()[c(204)]) ## set par
### PLOT TREE AND SEQUENCES 
#lapply(dend_rand_50, function(x) plot(x, horiz=TRUE)) ## plot tree  
#seq_rand_50 <- lapply(svv, seqdef2(x,len=svv_length))  ## redefine the seqdef with seqdef2 in order to include variables in the plot 
#lapply(svv, function(x) plotseq(x, labels_rand_50, seq_rand_50) ## plot reordered sequences 
#axis(1, at=seq(from=0, to= seq_length, by= seq_length/days), labels=c(0:days)) ## add an axes with daily lines
}
