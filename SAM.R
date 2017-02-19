SAM <-
function(x, plot=TRUE, ep, subset = 'all'){
# This function runs SAM on a dataframe containing sequences according to the structure in svv_sp or svv. Clustering of Ward.D2 is used. Hamming Distance is computed. Missing Values are automatically set. 
# input is a table with sequences structure as 1 row per sequence
# plot = TRUE if you want the result to be plotted
# ep is the environmental parameter that is the input of the sequences (e.g. tcd50)

if (subset == 'simulations'){
x[[1]] <- x[[1]][which(x[[1]][,svv_length] == eps[which(eps$s.labels == 'missing'),c('s.alphabet')]),]
} else {
if(subset == 'real'){
x[[1]] <- x[[1]][which(x[[1]][,svv_length] != eps[which(eps$s.labels == 'missing'),c('s.alphabet')]),]
} else { x[[1]] <- x[[1]]
}
}

# Sequence definition using only habitat types 
seq_rand_50 <- seqdef2(x[[1]],len=seq_length)

# states 
s.states <- data.frame(s.alphabet=c(seqstatl(x[[1]][,1:seq_length], format='STS')), stringsAsFactors=FALSE)
eps_l <- as.data.frame(merge(s.states,eps,type='left',by='s.alphabet'), stringsAsFactors=FALSE)
### Hamming_Distance
if ('xx' %in% eps_l$s.alphabet | 'missing' %in% eps_l$s.alphabet | 'NULL' %in% eps_l$s.alphabet | 'NA' %in% eps_l$s.alphabet | 'NaN' %in% eps_l$s.alphabet | '-9999' %in% eps_l$s.alphabet | 'nan' %in% eps_l$s.alphabet){
hd_rand_50 <- seqdist(seq_rand_50, method = "HAM", with.missing = TRUE) # , sm = Autocor_50[[i]], with.missing = TRUE        
} else {
hd_rand_50 <- seqdist(seq_rand_50, method = "HAM")
    }
colnames(hd_rand_50) <- rownames(x[[1]])
rownames(hd_rand_50) <- rownames(x[[1]])
### Perform the clustering (ward.D2) 
ward_rand_50 <- hclust(as.dist(hd_rand_50), method = "ward.D2") 
dend_rand_50 <- as.dendrogram(ward_rand_50) 
labels_rand_50 <- labels(dend_rand_50) # get the labels from dendrogram to have the order of the sequences
# remove labels for plotting
labels(dend_rand_50) <-(rep("",length(x[[1]][,1]))) 
# save dend object in order to plot 
to_plot <- dend_rand_50 #%>% color_labels(k = 6) #%>%  color_branches(k = 10)# 
# rasters 
raster_rand_50 <- x[[2]] 

# points 
locs_rand_50 <- x[[3]] 
if(plot == TRUE)
{
s.states <- data.frame(s.alphabet=c(seqstatl(x[[1]][,1:svv_length], format='STS')), stringsAsFactors=FALSE)
eps_l <- as.data.frame(merge(s.states,eps,type='left',by='s.alphabet'), stringsAsFactors=FALSE)
eps_l <- eps_l[order(eps_l$s.order),]
ll <- layout(matrix(c(1,2,2,3),1,4))
par (mar = c(1,1,1,1), oma = c(6,2,1,2), cex = 0.5, bg=colors()[c(204)])

# Plot tree
plot(dend_rand_50, horiz=TRUE)
seq_rand_50 <- seqdef2(x[[1]],len=svv_length)

# Plot sequences 
plotseq(x[[1]], labels_rand_50, seq_rand_50)
axis(1, at=seq(from=0, to= seq_length, by= seq_length/days), labels=c(0:days))

#To add lines for each day 
for (j in seq(fixes,seq_length,fixes))
{
points(x=rep(j,as.vector(length(x[[1]][,1]))), y=seq(0.000000,as.vector(length(x[[1]][,1])),as.vector(length(x[[1]][,1]))/(as.vector(length(x[[1]][,1])-1))), type="l", col = "black")
}
#To add lines for covariates
for (j in seq(seq_length,svv_length,fixes/2))
{
points(x=rep(j,as.vector(length(x[[1]][,1]))), y=seq(0.000000,as.vector(length(x[[1]][,1])),as.vector(length(x[[1]][,1]))/(as.vector(length(x[[1]][,1])-1))), type="l", col = "black")
}


# Plot raster
mtext(names(x[2]), outer = TRUE, cex = 1,padj=1.5)
colr <- unique(x[[2]]@data@values) # colors for raster
colr <- data.frame(s.alphabet=colr[!is.na(colr)])
colp <- join(eps_l, colr, type='inner', by= 's.alphabet')[,c('s.alphabet','s.palet')]
colr <- join(eps_l, colr, type='inner', by= 's.alphabet')[,c('s.palet')]
plot(x[[2]], axes=FALSE, box=FALSE, legend=FALSE,col=colr)

# Plot points
colnames(colp) <- c(ep,'s.palet')
colp <- join(x[[3]]@data, colp, type='left', by=ep)[,c('s.palet')] 
points(x[[3]]$lon3035,x[[3]]$lat3035,type='l',col=addalpha('grey30',0.5), cex=0.3)
# points(x[[3]]$lon3035,x[[3]]$lat3035,type='p',col=colp, pch=19, cex=0.5)
scale<- roundUp((extent(x[[2]])@xmax-(extent(x[[2]])@xmin))/3)

position<- c((extent(x[[2]])@xmin+(scale/8)),(extent(x[[2]])@ymin-(scale/2)))
positionx <- (extent(x[[2]])@xmin+(extent(x[[2]])@xmax-extent(x[[2]])@xmin)/1.5) 
positiony <- (extent(x[[2]])@ymin-(scale/1.5))
arrow <- north.arrow(positionx,positiony,scale/8,lab='N',cex.lab=1,tcol='black', col='black')

scalebar(d=scale, type='bar', divs=4, below='m',lwd=2,xy=position)

# plot legend
par(fig = c(0, 1, 0, 1), oma = c(0, 0, 0, 0), mar = c(0, 0, 0, 0), new = TRUE)
plot(0, 0, type = "n", bty = "n", xaxt = "n", yaxt = "n")
legend("bottom", eps_l$s.labels, xpd = TRUE,inset = c(0,0), bty = "n", pch = 15, ncol = 5, cex = 1,box.col=colors()[c(214)],bg=colors()[c(214)],col = eps_l$s.palet)
}
    return(list(hd_rand_50,ward_rand_50,dend_rand_50,labels_rand_50,raster_rand_50,locs_rand_50))

# EXAMPLES 
# SAM WITHOUT PLOTTING
# SAM_res <- lapply(svv_sp, function(x) SAM(x, FALSE))

# EXTRACT OUTPUTS FROM SAM_RES 
# hd_res <- lapply(SAM_res, function(x) x[[1]])
# ward_res <- lapply(SAM_res, function(x) x[[2]])
# dend_res<- lapply(SAM_res, function(x) x[[3]])
# labels_res <- lapply(SAM_res, function(x) x[[4]])
# raster_res <- lapply(SAM_res, function(x) x[[5]])
# locs_res <- lapply(SAM_res, function(x) x[[6]])

    # SAM WITH PLOTTING
    # Subset from list 
# subset <- srvv_sp[names(svv)] 
## pdf("trees_res.pdf")
# SAM_res <- lapply(subset, function(x) SAM(x, TRUE, ep='tcd50'))
## dev.off()

    # SAM applied on 1 data frame 
# SAM_res_subset <- SAM(subset[[2]],TRUE, ep='tcd50')
    }
