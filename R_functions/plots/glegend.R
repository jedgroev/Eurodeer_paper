glegend <-function(x,y,w,w_minmax=NULL,pos=NULL,dist=c(0.1,0.1),place='upperright',len=0.35,tdist=0.04,labels=NULL,title=NULL,tdisttitle=0.04,colors='black',alpha=1, font=1) {
# use this functions to add a legend for lines with a gradient 
# x = x-axis values
# y = y-axis values
# w = width of the segments of the line
# w_minmax = min and max value to rescale w 
# pos = position of the legend bar - maximum value on the x and y axis
# if pos is null dist and place can be used to set the position of the 
# dist = distance from the border 
# place = four options: upperright, lowerright, upperleft, lowerleft 
# len = length of the legend bar: the proportion of the x-axis (35% = 0.35) 
# tdist = distance of labels from the legend bar 
# labels = minimum and maximum value of the legend bar
# title = title of the legend bar 

# function to add the alpha level to a vector of colors 
addalpha <- function(col, alpha=1){
if(missing(col))
stop("Please provide a vector of colours.")
apply(sapply(col, col2rgb)/255, 2, 
                    function(x) 
                    rgb(x[1], x[2], x[3], alpha=alpha))  
}

# rescaling w (necessary when the values of w are very small or big) and calculate min and max value
if (length(w_minmax) != 0){
require(plotrix)
w2 <- rescale(w,w_minmax)
w_min <- min(w2)
w_max <- max(w2)
} else {
w_min <- min(w)
w_max <- max(w)
}

# if a position is not provided dist (distance from border) and place (upperleft, upperright,...) can be used to determine the position of the legend in the plot 
if (length(pos) == 0 & length(dist) != 0){
if (place == 'upperright') {
pos1 <- max(x) - (max(x)*dist[1])
pos2 <- max(y) - (max(y)*dist[2])
} else { if (place == 'lowerleft') { 
pos1 <- min(x) + (max(x)*dist[1]) + (max(x)*len)
pos2 <- min(y) + (max(y)*dist[2])
} else { if (place == 'upperleft') { 
pos1 <- min(x) + (max(x)*dist[1]) + (max(x)*len)
pos2 <- max(y) - (max(y)*dist[2])
} else { if (place == 'lowerright') { 
pos1 <- max(x) - (max(x)*dist[1]) 
pos2 <- min(y) + (max(y)*dist[2])
}}}}
pos <- c(pos1,pos2)
}

# calculate line widths and corresponding x and y values to be plotted
lwdleg <- seq(w_min,w_max, (w_max-w_min)/99) # line widths 
end <- pos[1] # end position on x axis  
start <- end - (max(x)*len) # start position on x axis 
xpos <- seq(start, end , (end-start)/(length(lwdleg)-1)) # x values  
ypos <- rep(pos[2],length(xpos)) # y values 

# if there is only one color make a vector of it with length of argument x
if(length(colors) == 1){
colors <- rep(colors, length(lwdleg))
}
# if there is only one transparency level make a vector of it with length of argument x
if(length(alpha) == 1){
alpha <- rep(alpha, length(lwdleg))
}

# plot legend 
for (k in 1:length(lwdleg)){
lines(xpos[k:(k+1)], ypos[k:(k+1)], lwd=lwdleg[k], col=addalpha(colors[k],alpha[k]))
}

# add title 
if(length(title) != 0){ # if there is a title plot title 
text(sum(start,end)/2,pos[2]+(max(y)*tdisttitle),title, xpd=NA,cex=1, font=font)
} 

# add labels 
if(length(labels) == 0){
text(start,pos[2]+(max(y)*tdist),round(min(w),2), xpd=NA,cex=0.9)
text(end,pos[2]+(max(y)*tdist),round(max(w),2), xpd=NA,cex=0.9)
} else {if(length(labels) != 0){
text(start,pos[2]+(max(y)*tdist),min(labels), xpd=NA, cex=0.9)
text(end,pos[2]+(max(y)*tdist),max(labels), xpd=NA, cex=0.9)
}}

# EXAMPLE 1
# y <- 1:100
# x <- 1:100/20
# w <- seq(0.01, 2.5, 2.5/100)
# plot(x,y,  col='white')
# glines(x,y,w)
# glegend(x,y,w, place='upperleft')

# EXAMPLE 2
# y <- 1:100
# x <- 1:100/20
# w <- seq(0.01, 2.5, 2.5/100)
# plot(x,y,  col='white')
# glines(x,y,w,w_minmax = c(1,10))
# glegend(x,y,w,w_minmax = c(1,10), place='lowerright')

# EXAMPLE 3  
# y <- 1:100
# x <- 1:100/20
# w <- sample(seq(0.01, 2.5, 2.5/100))
# colors <- colorRampPalette(c('red','grey20'))(100)
# alpha <- rep(c(0.6,0.7,0.8,0.9,1),20)
# plot(x,y, col='white')
# glines(x,y,w,w_minmax=c(1,10),colors=colors, alpha=alpha)
# glegend(x,y,w,w_minmax = c(1,10),pos=c(1.7,95),len=0.30,colors=colors,alpha=alpha)
}
