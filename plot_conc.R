library(readr)
library(dplyr)
library(rasterVis)
library(rtiff)
library(RColorBrewer)


setwd("~/Documents/ADMSUrban/test3/monitoring_sites/output/")
###########################################################

out <- read_csv("test3_gst.csv")%>% 
  rename(X = 'X(m)', Y = 'Y(m)', NOx_as_NO2 = 'Conc|ug/m3|NOx|<All sources>|-|  1hr', NO2 = 'Conc|ug/m3|NO2|<All sources>|-|  1hr',
         O3 = 'Conc|ug/m3|O3|<All sources>|-|  1hr', CO = 'Conc|mg/m3|CO|<All sources>|-|  1hr', VOC = 'Conc|ug/m3|VOC|<All sources>|-|  1hr', 
         SO2 = 'Conc|ug/m3|SO2|<All sources>|-|  1hr', PM2_5  = 'Conc|ug/m3|PM2.5|<All sources>|-|  1hr', PM10 = 'Conc|ug/m3|PM10|<All sources>|-|  1hr') %>% 
  mutate(NO = (NOx_as_NO2 - NO2)/46*30, NOx_true = NO + NO2) %>% 
  #rank first by day and hour, then descending Y and ascending X for filling the raster object from topleft
  arrange(Day, Hour, desc(Y), X) 



#create an empty raster object, eg. xmn = xfirst-xinc/2 
xy <- read.table("~/Documents/beijing/emissions/MEIC2013/xy", sep = "")
summary(xy)

co <- raster(ncol = 60, nrow = 80, xmn = 499355-1000/2, ymn = 491949-1000/2, 
             xmx = 558355+1000/2, ymx = 570949+1000/2)
crs(co) <- "+proj=lcc +lat_1=20 +lat_2=50 +lat_0=35 +lon_0=110 +x_0=0 +y_0=0 +ellps=WGS84 +units=m +no_defs"
values(co) <- out$CO[1:4800]

dev.new()
levelplot(co, main = "CO concentration at step 1", margin = F, colorkey = list(title = "(mg/m3)"), pretty = T,
          par.settings = rasterTheme(region = brewer.pal("YlOrRd", n = 9)))
dev.print(pdf, "CO_map.pdf")

pop <- raster("~/Documents/beijing/pop/BaseYear_1km/baseYr_total_2000.tif")

#crop with a spatial object
p <- as(extent(115,117,39,41), 'SpatialPolygons')
crs(p) <- crs(pop)
pop.sub <- crop(pop, p)

#reproject to match with CO conc map (same crs, extent and resolution)
pop.new <- projectRaster(pop.sub, co)

dev.new()
levelplot(pop.new, main = "Population", margin = F, colorkey = list(title = "(counts)"), 
          par.settings = rasterTheme(region = brewer.pal("Blues", n = 9)))
dev.print(pdf, "Population_map.pdf")


#raster calculation
exp <- overlay(pop.new, co, fun = function(x,y) x*y)

dev.new()
levelplot(exp, main = "CO Exposure at step 1", margin = F,  
          par.settings = rasterTheme(region = rev(brewer.pal("RdYlGn", n = 9))))
dev.print(pdf, "Exposure.pdf")

p_total <- sum(values(pop.new))
weighted_co <- sum(values(exp/p_total))


co.masked <- mask(co, mask = co > 0.5, maskvalue = 0)
pop.masked <- mask(pop.new, mask = co.masked)
pop.frac <- sum(values(pop.masked), na.rm = T)/p_total
