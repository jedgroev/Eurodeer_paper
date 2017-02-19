color.scales <-
function(color=c('forest','season','season6','season12','nice','nice6','red','blue','daynight'),colnum=2,plot=FALSE)
{
# different color scales using nice combinations: forest, red, blue (binary)
# season and nice have minimum 6 colors and can be extended

# Adjustable color scales 
if(color == 'forest'){
col<-colorRampPalette(c('darkolivegreen1','forestgreen'))(colnum)
}
if(color == 'red'){
col<-colorRampPalette(c('tomato2','rosybrown1'))(colnum)
}
if(color == 'blue'){
col<-colorRampPalette(c('skyblue','slateblue4'))(colnum)
}


if(color == 'season' & colnum >= 6){
col<-colorRampPalette(c('aliceblue','lightgreen','darkgoldenrod1','red','brown','paleturquoise4'))(colnum)
}
if(color == 'nice' & colnum >= 6){
col<-colorRampPalette(c('darksalmon','firebrick1','firebrick4','darkolivegreen','darkseagreen1','khaki3'))(colnum)
}

# Fixed color scales 
if(color == 'season6'){
col<-colorRampPalette(c('aliceblue','lightgreen','darkgoldenrod1','red','brown','paleturquoise4'))(6)
}
if(color == 'season12'){
col<-colorRampPalette(c('aliceblue','lightgreen','darkgoldenrod1','red','brown','paleturquoise4'))(12)
}
if(color == 'nice6'){
col<-colorRampPalette(c('darksalmon','firebrick1','firebrick4','darkolivegreen','darkseagreen1','khaki3'))(6)
}
if(color == 'daynight'){
col<-colorRampPalette(c('white','blue','black'))(3)
}
if(plot == TRUE){
cole(col,random=FALSE)
}
return(col)
}
