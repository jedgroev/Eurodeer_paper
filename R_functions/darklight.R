darklight <-
function(color, factor=1.4, type='darken')
{
col <- col2rgb(color)
if(type == 'darken'){
col <- col*factor
col <- rgb(t(col), maxColorValue=255)
col
} else {
if(type == 'lighten'){
col <- col/factor
col <- rgb(t(col), maxColorValue=255)
col
}
}
}
