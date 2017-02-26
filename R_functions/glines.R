glines <-
function(x,y,w,w_minmax=NULL,colors='black',alpha=1){
# lines with a changing thickness in R 

# x = x-axis values
# y = y-axis values
# w = width of the segments of the line
# w_minmax = min and max value to rescale w 
# colors = the colors that are used for the lines
# alpha = transparency default is set to no alpha 

# function to add the alpha level to a vector of colors 
addalpha <- function(col, alpha=1){
if(missing(col))
stop("Please provide a vector of colours.")
apply(sapply(col, col2rgb)/255, 2, 
                    function(x) 
                    rgb(x[1], x[2], x[3], alpha=alpha))  
}

# if there is only one color make a vector of it with length of argument x
if(length(colors) == 1){
colors <- rep(colors, length(x))
}
# if there is only transparency level make a vector of it with length of argument x
if(length(alpha) == 1){
alpha <- rep(alpha, length(x))
}

# Rescale w (necessary when the values of w are very small or big)
if (length(w_minmax) != 0){
require(plotrix)
w <- rescale(w,w_minmax)
}

# plot segments  
for (k in 1:length(w)){
lines(x=x[k:(k+1)],y=y[k:(k+1)],col=addalpha(colors[k],alpha[k]),lwd= w[k])
}

# EXAMPLE 1 
# y <- rnorm(1:100,100)
# x <- 1:100/20
# w <- seq(0.01, 2.5, 2.5/100)
# plot(x,y, col='white')
# glines(x,y,w)

# EXAMPLE 2
# y <- 1:100
# x <- 1:100/20
# w <- seq(0.01, 2.5, 2.5/100)
# plot(x,y,  col='white')
# glines(x,y,w,w_minmax = c(1,10))

# EXAMPLE 3
# y <- 1:100
# x <- 1:100/20
# w <- rnorm(1:100,100)
# plot(x,y, col='white')
# glines(x,y,w, w_minmax = c(1,20))

# EXAMPLE 4  
# y <- 1:100
# x <- 1:100/20
# w <- rnorm(seq(0.01, 2.5, 2.5/100))
# colors <- colorRampPalette(c('red','grey20'))(100)
# alpha <- rep(c(0.6,0.7,0.8,0.9,1),20)
# plot(x,y, col='white')
# glines(x,y,w,w_minmax=c(1,10),colors=colors, alpha=alpha)
}
