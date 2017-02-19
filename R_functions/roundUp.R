roundUp <-
function(x){
to <- 10
if(x > 0 & x < 100){
round_any(x,to)
} else { 
if(x >100 & x<1000){
round_any(x,(to*20))
} else {if (x>1000) {
round_any(x,(to*50))
}
}}}
