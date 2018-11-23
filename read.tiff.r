plot.tiff = function(path, add=FALSE)
{
  require('tiff')
  tiff = readTIFF(path, native=T) # read the file
  res = dim(tiff)[2:1] # get the resolution, [x, y]
  if (!add) # initialize an empty plot area if add==FALSE
    plot(1,1,xlim=c(1,res[1]),ylim=c(1,res[2]),asp=1,type='n',xaxs='i',yaxs='i',xaxt='n',yaxt='n',xlab='',ylab='',bty='n')
  rasterImage(tiff,1,1,res[1],res[2])
}