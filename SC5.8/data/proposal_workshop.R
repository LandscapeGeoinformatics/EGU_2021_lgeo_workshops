.libPaths( c( "C:/r_packages" , .libPaths() ) )
install.packages("cowplot")
install_github("marchtaylor/sinkr")
install_github("ffilipponi/rtsa")
require(devtools)
library(raster)
library(Kendall)
library(spatialEco)
require(rgdal)
library(lubridate)
library(zoo)
require(wql)
library(dplyr)

##import raster data
dir<-("C:/estonia_2010") 
rlist=list.files(dir, pattern="prec_2", full.names=T) ##list of images based on a pattern
n<-list.files(dir, pattern="prec_2")
r<-stack(rlist)
object.size(r)
r@layers
hist(r)
max(r)
r <- reclassify(r, c(65534,65536,NA))
hist(r)
plot(r)


r_temp<-list.files(dir, pattern="cut.tif", full.names=T)
tempe_r<-stack(r_temp)
plot(tempe_r)

##rename the rasters 
l_names<-r@data@names
r@data@names<-substr(r@data@names,13,19)
r@data@names
class(r@data@names)
plot(r)
class(r)

names(tempe_r)<-substr(names(tempe_r),13,19)
names(tempe_r)
names(tempe_r) <- gsub("X","temp_",names(tempe_r))
names(tempe_r)

##convert raster to polygons 
r_poly<-rasterToPolygons(r[[1]])
class(r_poly)
plot(r_poly)
crs(r_poly)
crs(r)
polylist = lapply(as.list(r[[1:12]]), rasterToPolygons)
head(polylist)

temp_poly = rasterToPolygons(tempe_r[[3:11]])
head(temp_poly)
class(temp_poly)

##Create a base shapefile
shp_base<-polylist[[1]]
shp_base$id<-1:length(polylist[[1]])
head(shp_base)
writeOGR(shp_base, "C:/estonia_2010/shp_base.shp","shp_base",  driver="ESRI Shapefile")

#create a dataframe with all columsn from shapefiles
merge_shp<-as.data.frame(polylist)
str(merge_shp)
merge_shp$id<-1:length(merge_shp$X2010_01)
str(merge_shp)
class(merge_shp)

temp_poly_df<-as.data.frame(temp_poly)
str(temp_poly_df)
temp_poly_df$id<-1:length(temp_poly_df$temp_2010_05)
str(temp_poly_df)

## organizing the data for the mannkendall analysis
require(tidyr)
long_DF <- merge_shp %>% gather(date, prec_value, "X2010_01":"X2010_12")
class(long_DF)
str(long_DF)
long_DF$date <- gsub("X","",long_DF$date)
long_DF$date <- gsub("_","-",long_DF$date)
str(long_DF)
long_DF$date <- as.Date(paste(long_DF$date,"-01",sep=""),"%Y-%m-%d")
str(long_DF)
unique(long_DF$date)

long_temp <- temp_poly_df %>% gather(date, temp_value, "temp_2010_03":"temp_2010_11")
class(long_temp)
str(long_temp)
head(long_temp)
long_temp$date <- gsub("temp_","",long_temp$date)
long_temp$date <- gsub("_","-",long_temp$date)
str(long_temp)
head(long_temp)
long_temp$date <- as.Date(paste(long_temp$date,"-01",sep=""),"%Y-%m-%d")
str(long_temp)
unique(long_temp$date)
head(long_temp)

### mann kendall analysis
coef.fcn = function(newdata) {
  res<-mannKen(newdata$prec_value)
  return(as.data.frame(res))
}
require(dplyr)

lm_coefs = long_DF %>% 
  group_by(id) %>%
  do(coef.fcn(.))
lm_coefs

coef.fcn_temp = function(newdata) {
  res<-mannKen(newdata$temp_value)
  return(as.data.frame(res))
}
lm_coefs_temp = long_temp %>% 
  group_by(id) %>%
  do(coef.fcn_temp(.))
lm_coefs_temp


###categorize the results in positive, negative or no-trend
class(lm_coefs)
lm_coefs[,"trend"] <- as.character(NA)
class(lm_coefs$trend)
head(lm_coefs)
lm_coefs$trend <-ifelse(lm_coefs$sen.slope <0 & lm_coefs$p.value<0.05, "1", 
                        ifelse(lm_coefs$sen.slope >0 & lm_coefs$p.value<0.05, "2", "0"))

lm_coefs
unique(lm_coefs$trend)

class(lm_coefs_temp)
lm_coefs_temp[,"trend_temp"] <- as.character(NA)
class(lm_coefs_temp$trend_temp)
head(lm_coefs_temp)
sum(lm_coefs_temp$p.value <0.05)
lm_coefs_temp$trend_temp <-ifelse(lm_coefs_temp$sen.slope <0, "1", 
                        ifelse(lm_coefs_temp$sen.slope >0 , "2", "0"))

head(lm_coefs_temp)
unique(lm_coefs_temp$trend_temp)

##### combine the results in shapefile format
shp_comb<-merge(shp_base,lm_coefs, by="id")
names(shp_comb)
shp_comb<-shp_comb[,-(2:8)]
head(shp_comb)

shp_comb<-merge(shp_comb,lm_coefs_temp, by="id")
names(shp_comb)
shp_comb<-shp_comb[,-(3:8)]
names(shp_comb)
class(shp_comb)


#### bivariate map
require(sf)
require(biscale)

shp_sf<-st_as_sf(shp_comb)
class(shp_sf$trend)
shp_sf$trend<-as.numeric(as.character(shp_sf$trend))
class(shp_sf$trend)
shp_sf$trend_temp<-as.numeric(as.character(shp_sf$trend_temp))
class(shp_sf$trend_temp)

data <- bi_class(shp_sf, x = trend, y = trend_temp, style = "jenks", dim = 2)
class(data)
st_write(data, paste0(dir, "/", "data_nc.shp"), delete_layer = TRUE)


require(ggplot2)
map<-ggplot()+
  geom_sf(data=data, aes(fill=bi_class), color=NA)+
  bi_scale_fill(pal = "DkBlue", dim = 2) +
  labs(title = "Climate trends in Estonia") +
  theme_bw()+
  theme(legend.position = "none")
map


legend <- bi_legend(pal = "DkBlue",
                    dim = 2,
                    xlab = "Precipitation ",
                    ylab = "Temperature",
                    size = 11)
legend
#require(cowplot)
finalPlot <- ggdraw() +
  draw_plot(map) + 
 draw_plot(legend, 0.05, 0.7, 0.15, 0.15)
finalPlot

ggsave("C:/estonia_2010/finalPlot.png")
