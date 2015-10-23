library(raster)
setwd("/Users/liziqi/Desktop/ts_tiff")
files <- list.files()

r = raster("/Users/liziqi/Desktop/Raster brick for lower yenesei/class_00_02_1km.tif")

b0 = brick()
for (i in (1:length(files))) {
	b = brick(files[i])
	b0=brick(addLayer(b0,b))
	
}

b1=projectRaster(b0,r,method='ngb')
b1=mask(b1,r)
writeRaster(b1, filename="merra_ts.grd", bandorder='BIL', overwrite=TRUE)
