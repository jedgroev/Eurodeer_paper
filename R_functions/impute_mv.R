impute_mv <-
function(data, p=10, impval=1, na='xx', seed=1234, subset = 901, seqlen=seq_length){
# This function imputes a proportion of the fixes to forest. The proportion is set to 10% standard because 10% based on literature. It is possible to apply the function both on a data frame as on a vector. 
# data = data.frame or vector  
# p = percentage of missing values (na) that needs to be imputed 
# na = what code represents the missing value
# impval = which value to impute to 
# seed = replicator 
# seqlen = length of the sequence
# subset = the row from which to start the imputation
set.seed = seed

if(class(data) != 'data.frame'){
perc <- ceiling(length(data)/100*p) 
if(length(data[data==na]) == 0){ 
data <- data 
} else {
if(length(vec[vec==na]) <= perc){
data[data==na] <- impval
} else {
if(length(data[data==na]) > perc)
to_change <- sample(which(data==na), perc)
data[to_change] <- impval
}
}
}

if(class(data) == 'data.frame'){
perc <- ceiling(length(data[1,1:seqlen])/100*p) 
for (i in subset:length(data[,1]))
{
if(length(data[i,1:seqlen][data[i,1:seqlen]  == 'xx']) == 0) # if no missing values than the same vector is returned!
{ 
data[i,1:seqlen] <- data[i,1:seqlen] 
} else {
if(length(data[i,1:seqlen][data[i,1:seqlen]  == 'xx']) <= perc) # if missing values is less than the percentage than all missing values are substituted by forest 
{
vec <- data[i,1:seqlen] 
vec[vec==na] <- impval
data[i,1:seqlen] <- vec 
} else {
if(length(data[i,1:seqlen][data[i,1:seqlen]  == 'xx'])  > perc) # if the number of missing values is more than the percentage than only the percentage is substituted 
{
vec <- data[i,1:seqlen] 
to_change <- sample(which(vec==na), perc)
vec[to_change] <- impval
data[i,1:seqlen] <- vec 
}
}
}

}
}

return(data)
}
