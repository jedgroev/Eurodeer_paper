varmax <-
function(x, var, missing){
# dataset
# variable to get the most present value from
# vector of missing values
clc1 <- count(x[,c('unique_id',var)])
sort <- clc1[order(clc1$unique_id, -clc1$freq),]
sort <- sort[! sort[,c(var)] %in% missing,]
var <- sort[!duplicated(sort$unique_id),]
}
