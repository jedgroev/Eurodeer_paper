export <- function(func, file, res2 = 300, ratio=1.5, type='pdf'){
# creates figures in pdf, png and tif at same dimensions.
# func is the function called 
# file = filename
# res2 = resolution
# ratio = ratio with length
# type = pdf, png, tif
dims=480/72*res2
  # PNG
  if(type=='png'){
  png(paste0(file,'.png'),res=res2,width=dims*ratio,height=dims)
  func
  dev.off()}
  # TIF 
  if(type=='tif'){
  tiff(paste0(file,'.tif'),res=res2,width=dims*ratio,height=dims)
  func
  dev.off()}
  # PDF  
  if(type=='pdf'){
  pdf(paste0(file,'.pdf'),width=(dims/res2)*ratio, height=dims/res2)
  func
  dev.off()}
  # EXAMPLE
  # x <- 1:10
  # y <- 1:10
  # plot(x,y)
  # export(func=plot(x,y),'test',type='pdf')
  # export(func=plot(x,y),'test',type='tif')
  # export(func=plot(x,y),'test',type='png')
}
