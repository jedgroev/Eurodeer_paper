fittedSpline <-
function(dep, ind, yourdata, group){
y <- yourdata[, dep];x <- yourdata[, ind]
plot(yourdata[,ind], yourdata[,dep], type='p', col= 'white', ylim=c(0,1), xlab='number of clusters', ylab='bootstrap - median')
for (i in 1:length(unique(yourdata[,group])))
{
subset <- yourdata[which(yourdata[,group] == unique(yourdata[,group])[i]),]
points(subset[,ind],subset[,dep],type='p', col=col_po[which(col_po$p_open_t==subset$p_open_t[1]),1], ylim=c(0,1), pch=19)
}
data.spl <- with(data, smooth.spline(yourdata[,ind], yourdata[,dep]))
lines(data.spl, col = "blue")
return(data.spl)
}
