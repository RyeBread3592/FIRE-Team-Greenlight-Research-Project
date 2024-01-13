library("terra")

transit <- vect("G:/Shared drives/2023 FIRE-SA/FALL OUTPUT/Team Greenlight/LYNX_Blue_Line_Route/LYNX_Blue_Line_Route.shp")
plot(transit)

#connect all lines together
lr<-aggregate(transit, dissolve=TRUE)

#change projection system with latitude and longitude units
lr_project<-project(lr, "+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs ")

#find centroid
lrc<-centroids(lr_project, inside=FALSE)

#create a buffer with 10 km radius
pts_buffer<-buffer(lrc, width = 10000)

#make a map
plot(pts_buffer)
lines(lr_project, col="red")
points(lrc, col="blue")

r<-rast("G:/Shared drives/2023 FIRE-SA/FALL OUTPUT/Team Greenlight/NASAMODIS Data/2000.nc4")

#Check how many values are in the raster (This is the same across all years)
names(r)

install.packages("ncdf4")
library(ncdf4)
nc<-nc_open("G:/Shared drives/2023 FIRE-SA/FALL OUTPUT/Team Greenlight/NASAMODIS Data/2000.nc4")

library("lubridate")
time<-as.Date(nc$dim$time$vals, origin='2000-01-01')
month<-month(time)

crs(r)
circle_project<-project(pts_buffer, 
                        crs(r))

vars<-names(r)
output<-c()

#extracting the first layer of the raster 46 layers in total
for (i in 1:46) {
  
  #pull out one layer of the raster
  rl<-r[[i]]
  
  dfr1<-terra::extract(rl, circle_project)
  
  ndvi1<-mean(dfr1[,2])
  
  output<-rbind(output, ndvi1)
}
d<-cbind(output,month)
write.csv(d, "G:/Shared drives/2023 FIRE-SA/FALL OUTPUT/Team Greenlight/OUTPUT/2000_PollutionData.csv")
