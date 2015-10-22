library(raster)
library(plyr)
setwd("F:\\Raster brick for lower yenesei")
files <- list.files()

#read all bricks
b = brick()
for (i in (1:length(files))){
	b=addLayer(b, raster(files[i]))
}

df = as.data.frame(b,xy=T)
i = 6
j = 1
a=df
#Average Year
year = c(2002:2011)
year = lapply(year,FUN = toString)
year = unlist(year)
repeat{
	a[,year[j]] = rowMeans(df[,i:(i+11)],na.rm=T)
	i=i+12
	j=j+1
  	if(j==11){
    		break
  }
}

k = ddply(a[,c(4,6:125)], .(class_85_87_1km), colwise(mean,na.rm=T))
k = k[complete.cases(k),]
k = ddply(a[,c(4,126:135)], .(class_85_87_1km), colwise(mean,na.rm=T))
k$class_85_87_1km[k$class_85_87_1km==1]="water"
k$class_85_87_1km[k$class_85_87_1km==2]="soil"
k$class_85_87_1km[k$class_85_87_1km==3]="tundra"
k$class_85_87_1km[k$class_85_87_1km==4]="forest"
k$class_85_87_1km[k$class_85_87_1km==5]="urban"
k = k[complete.cases(k),]
Molten <- melt(k, id.vars = "class_85_87_1km")
colnames(Molten) <- c("class", "year","temperature")
pl = ggplot(Molten, aes(x = year, y = temperature, group = class,color=class)) + geom_line(size = 0.7)+geom_point()
pl+geom_smooth(data=Molten,aes(x = year, y = temperature, group = class,color=class),method=lm,se=FALSE)+scale_color_manual(values=c("darkgreen", "black", "green", "red", "blue"))



#Average Winter/Summer
ws = c("2002w","2002s","2003w","2003s","2004w","2004s","2005w","2005s","2006w","2006s"
,"2007w","2007s","2008w","2008s","2009w","2009s","2010w","2010s","2011w","2011s")
j = 1
repeat{
	a[,ws[j]] = rowMeans(df[,i:(i+11)],na.rm=T)
	if (winter){
		i=i+
	}
	j=j+1
  	if(j>20){
    		break
  }
}



#rasteize
spg <- a
coordinates(spg) <- ~ x + y
gridded(spg) <- TRUE
rasterDF <- raster(spg["temp"])
plot(rasterDF)