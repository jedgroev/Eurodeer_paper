gradient <-
function(x,y,colors,len=c(0.1,0.2),pos=NULL,dist=c(0.1,0.1),place='upperright', bars=5, tdist=0.04,labels= c(0,1), title=NULL,tdisttitle=0.04, font=1) {
# x = x axis values
# y = y axis values
# colors = vector of colors
# len = width of the bar given as a proportion of x and y axis  
# pos = maximum x and y coordinate
# if pos is null dist and place can be used to set the position of the 
# dist = the distance from the border 
# place = placement of the bar in the plot window (upperright, lowerleft, upperleft, lowerright)
# bars = number of bars to distinguish in the gradient 
# tdist = distance of text from gradient bar  
# labels = vector with min and max value

# If pos is null than dist (distance from border) and place (upperright, lowerleft, upperleft, lowerright) is used to determine the position 
if (length(pos) == 0 & length(dist) != 0){
if (place == 'upperright') {
x_coord_max <- max(x) - (max(x)*dist[1])
x_coord_min <- x_coord_max - (max(x)*len[1])
y_coord_max <- max(y) - (max(y)*dist[2]) 
y_coord_min <- y_coord_max - (max(y)*len[2])

} else { if (place == 'lowerleft') { 
x_coord_max <- min(x) + (max(x)*dist[1]) + (max(x)*len[1])
x_coord_min <- x_coord_max - (max(x)*len[1])
y_coord_max <- min(y) + (max(y)*dist[2]) + (max(y)*len[2]) 
y_coord_min <- y_coord_max - (max(y)*len[2])

} else { if (place == 'upperleft') { 
x_coord_max <- min(x) + (max(x)*dist[1]) + (max(x)*len[1])
x_coord_min <- x_coord_max - (max(x)*len[1])
y_coord_max <- max(y) - (max(y)*dist[2]) 
y_coord_min <- y_coord_max - (max(y)*len[2])

} else { if (place == 'lowerright') { 
x_coord_max <- max(x) - (max(x)*dist[1])
x_coord_min <- x_coord_max - (max(x)*len[1])
y_coord_max <- min(y) + (max(y)*dist[2]) + (max(y)*len[2]) 
y_coord_min <- y_coord_max - (max(y)*len[2])
}}}}
x_coord <- c(x_coord_max,x_coord_max, x_coord_min, x_coord_min) # x coordinates 
} else { # pos is used 
x_coord_max <- pos[1]
x_coord_min <- x_coord_max - (max(x)*len[1])
x_coord <- c(x_coord_max,x_coord_max, x_coord_min, x_coord_min)
y_coord_max <- pos[2] 
y_coord_min <- y_coord_max - (max(y)*len[2])
}

# plot colored polygons of gradient 
y_coord <- list(data.frame()) # y coordinates (dynamic)
for (j in length(colors):1){
y_coord_var <- seq(y_coord_min,y_coord_max,((y_coord_max - y_coord_min)/length(colors)))[j]
y_coord[[j]] <- c(y_coord_min, y_coord_var, y_coord_var, y_coord_min)
polygon(x_coord, y_coord[[j]], col= colors[j], border=FALSE)
}

# plot bars 
polx <- c(min(x_coord), min(x_coord), max(x_coord), max(x_coord))
for (i in 1:bars){
poly<-c(min(do.call(rbind,y_coord)),min(do.call(rbind,y_coord)) + (max(do.call(rbind,y_coord))-min(do.call(rbind,y_coord)))/bars*i,
min(do.call(rbind,y_coord)) + (max(do.call(rbind,y_coord))-min(do.call(rbind,y_coord)))/bars*i, min(do.call(rbind,y_coord)))
if (length(colors) > 20){
polygon(polx,poly, border=colors[(seq((length(colors)/bars),length(colors),length(colors)/bars)-round((5/100*length(colors))))][i])
}
}

# plot the border of the gradient 
polx<-c(min(x_coord),min(x_coord), max(x_coord),max(x_coord))
poly<-c(min(do.call(rbind,y_coord)),max(do.call(rbind,y_coord)),max(do.call(rbind,y_coord)),min(do.call(rbind,y_coord)))
polygon(polx,poly)

# plot labels 
text((max(polx) + (max(x)*tdist)), max(poly), max(labels[2]), cex=0.9)
text((max(polx) + (max(x)*tdist)), min(poly), min(labels[1]), cex=0.9)
if (length(title) != 0){
text(sum(unique(polx))/2,max(poly)+tdisttitle,title, xpd=NA, font=font)
}

# EXAMPLE 1 
# x <- 1:100
# y <- (1:100)/5
# colors <- colorRampPalette(c('grey90','grey20'))(100)
# plot(x,y, col=colors)
# gradient(x,y, col=colors)

# EXAMPLE 2 
# If there are less than 20 colors no bars are plotted 
# colors <- colorRampPalette(c('grey90','grey20'))(10)
# plot(x,y, col=colors)
# gradient(x,y, col=colors)

# EXAMPLE 3  
# x <- 1:100
# y <- (1:100)/5
# colors <- colorRampPalette(c('grey90','grey20'))(100)
# plot(x,y, col=colors)
# Add gradient using pos (values of x and y axis) or using dist (distance from border) and place (upperright, upperleft,...)
# gradient(x,y, col=colors, pos=c(21,18)) # using pos 
# gradient(x,y, col=colors, dist=c(0.1,0.1), place = 'upperleft') # using dist and place 
}
