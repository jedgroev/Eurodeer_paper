gradient <-
function(x, y, colors, labels= c(0,1),pos='upperright', xwidth=2, ywidth=8,bars=5, tdist=4) {
# x = x axis values
# y = y axis values
# colors = vector of colors
# pos = position in the plot - 'upperright', 'upperleft'
# width = width of the bar - if 2 it is 1/10th of the plot width 
# bars = number of bars to distinguish in the gradient 
# tdist = distance of text from gradient bar  
# set 1/10th away from border plot
# labels = vector with min and max value  
x_dist <- max(x)/20
y_dist <- max(y)/20
if(pos=='upperright'){
x_coord_max <- max(x) - (x_dist)  
x_coord_min <- max(x) - (x_dist*xwidth) 
} else { 
if(pos=='upperleft'){
x_coord_max <- min(x) + (x_dist*xwidth) 
x_coord_min <- min(x) + (x_dist)  
}
}

x_coord <- c(x_coord_max,x_coord_max, x_coord_min, x_coord_min)
y_coord <- list(data.frame())
for (j in length(colors):1){
y_coord_min <- max(y) - y_dist*ywidth 
y_coord_max<- max(y) - y_dist
y_coord_var <- seq(y_coord_min,y_coord_max,((y_coord_max - y_coord_min)/length(colors)))[j]
y_coord[[j]] <- c(y_coord_min, y_coord_var, y_coord_var, y_coord_min)
polygon(x_coord, y_coord[[j]], col= colors[j], border=FALSE)
}     
polx <- c(min(x_coord), min(x_coord), max(x_coord), max(x_coord))
for (i in 1:bars){
poly<-c(min(do.call(rbind,y_coord)),min(do.call(rbind,y_coord)) + (max(do.call(rbind,y_coord))-min(do.call(rbind,y_coord)))/bars*i,
min(do.call(rbind,y_coord)) + (max(do.call(rbind,y_coord))-min(do.call(rbind,y_coord)))/bars*i, min(do.call(rbind,y_coord)))
if (length(colors) > 20){
polygon(polx,poly, border=colors[(seq((length(colors)/bars),length(colors),length(colors)/bars)-round((5/100*length(colors))))][i])
}
}
polx<-c(min(x_coord),min(x_coord), max(x_coord),max(x_coord))
poly<-c(min(do.call(rbind,y_coord)),max(do.call(rbind,y_coord)),max(do.call(rbind,y_coord)),min(do.call(rbind,y_coord)))
polygon(polx,poly)
text(max(polx)+tdist, max(poly), max(labels[2]))
text(max(polx)+tdist, min(poly), min(labels[1]))

# test 
# x <- 1:100
# y <- (1:100)/5
# colors <- colorRampPalette(c('grey90','grey20'))(100)
# plot(x,y, col=colors)
# gradient(x,y, col=colors)
# if there are less than 20 colors no bars are included
# colors <- colorRampPalette(c('grey90','grey20'))(10)
# plot(x,y, col=colors)
# gradient(x,y, col=colors)
}
