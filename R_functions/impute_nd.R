impute_nd <-
function(data,na='nd',subset=901, method='resample', seed=1234, seqlen=seq_length){
# This function is especially designed for missing values of the type 'nd' meaning that the missing value is due to the sampling design and not because of gps acquisition failure. This type of missing value has no biological meaning what so ever. We decided on two different methods to impute. Or the most common value for a specific fix. Or we resample based on the observed observations for the certain timestamp. Because missing values corresponding to acquisition failures have a biological meaning, it may also be that instead of an observation that a missing value (that has a biological meaning - e.g. acquisition failure) is imputed. 

# data = data.frame  
# na = what code represents the missing value
# seed = replicator 
# method = there are two methods: resample and max
# seqlen = length of the sequence
# subset = the row from which to start the imputation

for (i in subset:length(data[,1])) # subset the rows on which the imputation needs to be performed
{
if (method == 'max') # in this case the method that is used is selecting the most common value as the impute value
{

if(length(unlist(table(data[i,]==na))) == 2) # subset the sequences which have no data (nd)
{
for (j in 1:fixes)
{
a <- table(unlist(data[i,seq(j,seqlen,fixes)]))
a <- a[names(a) %in% c(0,1)]
a <- a[max(a) == a]
t <- seq(j,seqlen,fixes)
if(length(a) == 1)
{
data[i,t[data[i,t] == na]] <- a    
} else { 
if(length(a) == 2 | length(a) == 0)
{
z <- table(unlist(data[i,]))  
z <- z[names(z) %in% c(0,1)]
z <- z[max(z) == z]
data[i,t[data[i,t] == na]] <- names(z)    
}
}

}
}
} else {


if (method == 'resample') # a resampling is performed based on the proportion of observed values for certain time fixes 
{
set.seed(seed)
if(length(unlist(table(data[i,1:seqlen]==na))) == 2) # subset the sequences which have no data (nd)
{
for (j in 1:fixes)
{
vec <- data[i,seq(j,seqlen,fixes)]
to_change <- which(vec ==na)
vec[to_change] <- sample(vec[vec != na],length(to_change))
data[i,seq(j,seqlen,fixes)] <- vec 
}
}
}
}

}

return(data)
}
