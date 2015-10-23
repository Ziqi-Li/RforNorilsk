library(raster)
setwd("F:\\Raster brick for lower yenesei")
files <- list.files(path="F:\\Waterloo LST\\Nor_dud_iga_2002_2011", full.names=T, recursive=FALSE)
class <- raster("F:\\class&dem\\00_02_classified_plus_trees.img")
class1km = raster("F:\\class&dem\\class_1km.img")
class1km[class1km==0]=NA
b =brick()
for (i in (1:length(files))) {
	curYearFolder <- list.files(path= files[i], full.names=T, recursive=FALSE,pattern = "\\001.tif$")
	year <-c(2002:2011)
	month <- c("01","10","11","12","02","03","04","05","06","07","08","09")
	for (j in (1:length(curYearFolder))) {
		curName <- paste0("uw_","monthly_","lst_",year[i],month[j],".tif")
		prjed = projectRaster(raster(curYearFolder[j]),class1km)
		temp = mask(prjed,class1km)
		b = addLayer(b,temp)
		#writeRaster(temp,filename=curName, format="GTiff", overwrite=TRUE)
		print (curYearFolder[j])
		print (curName)

	}
}
