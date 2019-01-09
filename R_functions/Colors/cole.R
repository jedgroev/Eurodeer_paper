cole <- function(x,y, random = TRUE, type= TRUE)  { 
# x = vector of colors 
# y = how many times colors are repeated
# random = are the colors plotted randomly (T) or not (F)
# type = plot as bars (T) or as points (F) 
# use set.seed() if you want it to be repeatable 
if (random == TRUE) 
    {
    if(type == FALSE)
        {   
        plot(c(rep(rep(1,length(x)),y)), col = sample(rep(x,y)), axes= FALSE, type ='p', pch = 19, xlab=' ',ylab=' ')
        }   
    else
        {
        barplot(c(rep(rep(1,length(x)),y)), col = sample(rep(x,y)),space=c(rep(rep(0,length(x)),y)), border = FALSE, axes= FALSE)
        }
    
    } 

else # if random is false than colors are not randomized
    {

    if(type == FALSE)   
        {
        plot(c(rep(rep(1,length(x)),y)), col = rep(x,y), axes= FALSE, type ='p', pch = 19, xlab=' ',ylab=' ')
        }

    else 
        {
        barplot(c(rep(rep(1,length(x)),y)), col = rep(x,y),space=c(rep(rep(0,length(x)),y)), border = FALSE, axes= FALSE)
        }

    }

}
