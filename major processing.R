library(raster)
library(plyr)
library(ggplot2)
library(reshape)
library(gstat)
setwd("/Users/liziqi/Desktop/Raster brick for lower yenesei")
files <- list.files(pattern = ".tif")

#read all bricks
b = brick()
b2=brick()
for (i in (1:length(files))){
	r = raster(files[i])
	#rdf = as.data.frame(r,xy=T)
	#colnames(rdf)=c("x","y","t")
	#rdf = rdf[complete.cases(rdf),]
	#model <- gstat(id = "t", formula = t~1, locations = ~x+y, data=rdf, 
            #nmax=7, set=list(idp = .5))
	#z <- interpolate(r, model)
	#z <- mask(z,)
	#b2=addLayer(b2, z)
	b=addLayer(b,r)
}
b1=brick("modis.grd")
b1=stack(raster("merra_grid_no.tif"),b1)
b2=brick("merra_ts.grd")
b3=brick("merra_t2m.grd")
b=brick(stack(b1,b2[[277:396]],b3[[277:396]]))


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

#Annual
colnames(a)[3] = "class_00_02_1km"
k = ddply(a[,c(3,126:135)], .(class_00_02_1km), colwise(mean,na.rm=T))
k$class_00_02_1km[k$class_00_02_1km==1]="water"
k$class_00_02_1km[k$class_00_02_1km==2]="soil"
k$class_00_02_1km[k$class_00_02_1km==3]="tundra"
k$class_00_02_1km[k$class_00_02_1km==4]="forest"
k$class_00_02_1km[k$class_00_02_1km==5]="urban"
k = k[complete.cases(k),]
cl = read.csv("norilsk_climate.csv")
rbi <- ddply(cl, .(Year), summarise,t2m=mean(tm, na.rm = TRUE),rr=mean(RR, na.rm = TRUE))
station = subset(subset(rbi,Year<=2011),Year>=2002)
m = t(station)
colnames(m) = station$Year
s = m[2,]
s=s+273.15
s = as.data.frame(t(s))
s$class_00_02_1km = "t2m"
k = rbind(s,k)
k = k[c(11,1:10)]

Molten <- melt(k, id.vars = "class_00_02_1km")
colnames(Molten) <- c("class", "year","temperature")
pl = ggplot(Molten, aes(x = year, y = temperature, group = class,color=class)) + geom_line(size = 0.7)+geom_point()
pl+geom_smooth(data=Molten,aes(x = year, y = temperature, group = class,color=class),method=lm,se=FALSE)+scale_color_manual(values=c("darkgreen", "grey","black", "green", "red", "blue"))

#by Month
k = ddply(a[,c(3,6:125)], .(class_00_02_1km), colwise(mean,na.rm=T))
k = k[complete.cases(k),]
k$class_00_02_1km[k$class_00_02_1km==1]="water"
k$class_00_02_1km[k$class_00_02_1km==2]="soil"
k$class_00_02_1km[k$class_00_02_1km==3]="tundra"
k$class_00_02_1km[k$class_00_02_1km==4]="forest"
k$class_00_02_1km[k$class_00_02_1km==5]="urban"

cl = read.csv("norilsk_climate.csv")
rbi <- ddply(cl, .(Year,Month), summarise,t2m=mean(tm, na.rm = TRUE),rr=mean(RR, na.rm = TRUE))
station = subset(subset(rbi,Year<=2011),Year>=2002)
m = t(station)
s = m[3,]
s=s+273.15
s = as.data.frame(t(s))
s$class_00_02_1km = "t2m"
s = s[c(121,1:120)]
colnames(s) = colnames(k)
k = rbind(s,k)
Molten <- melt(k, id.vars = "class_00_02_1km")
colnames(Molten) <- c("class", "year","temperature")
pl = ggplot(Molten, aes(x = year, y = temperature, group = class,color=class)) + geom_line(size = 0.7)+geom_point()
pl+geom_smooth(data=Molten,aes(x = year, y = temperature, group = class,color=class),method=lm,se=FALSE)+scale_color_manual(values=c("darkgreen", "grey","black", "green", "red", "blue"))



#Merra
merra_ts = k[,126:549]
merra_t2m = k[,550:(ncol(k)-1)]
merra_ts_0211 = merra_ts[,277:396]
merra_t2m_0211 = merra_t2m[,277:396]
modis_0211 = k[,6:125]

#test with 29
a = subset(df,merra_grid_no==30)



p = cbind(a$x,a$y)
for (j in (0:11)){
	mts = a[,seq((127+j), (235+j), by=12)]
	mt2m = a[,seq((247+j), (355+j), by=12)]
	modis = a[,seq((7+j), (115+j), by=12)]
	
	res = summary(lm(t(modis[1,])~t(mt2m[1,])))$r.squared
	for (i in (2:nrow(mt2m))){
		if (!all(is.na(modis[i,])) & length(modis[i,])>=6){
			temp = summary(lm(t(modis[i,])~t(mt2m[i,]),na.action=na.exclude))$r.squared
			res = rbind(res,temp)
		}
		else{
			res = rbind(res,c(NA))
		}
	}
	p = cbind(p,res)
}



a = df
p = cbind(a$x,a$y)
j=3
	mts = a[,seq((127+j), (235+j), by=12)]
	mt2m = a[,seq((247+j), (355+j), by=12)]
	modis = a[,seq((7+j), (115+j), by=12)]
	res=c(NA,NA)

	for (i in (1:nrow(mt2m))){
		if (!all(is.na(modis[i,])) & !all(is.na(mt2m[i,]))){
			temp = coef(lm(t(modis[i,])~t(mt2m[i,])))
			res = rbind(res,temp)
		}
		else{
			res = rbind(res,c(NA,NA))
		}
	}
	p = cbind(p,res[2:dim(res)[1],])

q = as.data.frame(b3,xy=T)
q=cbind(q,p[,c(3,4)]
q$new = q["MERRA_t2m_1981.4"]*q["t(mt2m[i, ])"]+q["(Intercept)"]




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
spg <- m
coordinates(spg) <- ~ x + y
gridded(spg) <- TRUE
rasterDF <- raster(spg["new"])
plot(rasterDF)