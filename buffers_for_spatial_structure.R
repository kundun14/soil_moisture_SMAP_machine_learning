#install.packages("chirps")
#library("remotes")
#install_github("ropensci/chirps", build_vignettes = TRUE)
#vignette("Overview", package = "chirps")

library(rgdal)
library(raster)
library(maptools)
library(rgdal)
library(sf)
library(tiverse)
library(GSIF)
library(tidyverse)


#PUNTOS
raster_har = raster("./raster_humedad.tif") # a SpatialPixelsDataFrame
#lista de puntos xy extraidos del raster

puntos_df = data.frame(xyFromCell(raster_har, c(1:ncell(raster_har)))) # a SpatialPointsDataFrame 
#colnames(puntos_df) = c("x_coord",  "y_coord")

xy = puntos_df[,c(1,2)]

#spatialPointsDataFrame
puntos_pdf = SpatialPointsDataFrame(coords = xy,data= puntos_df,
                                    proj4string = CRS("+proj=utm +zone=18 +south +datum=WGS84 +units=m +no_defs"))

#SpatialPixelsDataFrame

pix_df=as(raster_har, "SpatialPixelsDataFrame")

#grid.dist0 = GSIF::buffer.dist(meuse["zinc"], meuse.grid[1], as.factor(1:nrow(meuse)))

grid.dist0 = GSIF::buffer.dist(puntos_pdf, pix_df, as.factor(1:nrow(puntos_pdf))) # crea 483 spatialpixelsdataframe
dist0_stack =stack(grid.dist0) # stackear

layer_n = paste(rep("buffer", 483), 1:483, sep = "_" )
names(dist0_stack) = layer_n

#guardar el stack

writeRaster(dist0_stack,
            filename = "buffers",
            overwrite= T)

#dataframe

# NPixel suelos

NPix.df = data.frame(seq(1:ncell(dist0_stack)))
colnames(NPix.df) = "NPix"

#Lista de variables

buf = list()
for (i in 1:nlayers(dist0_stack)) {
  buf[[i]]= as.data.frame(dist0_stack[[i]])  
}

for (i in 1:length(buf)){
  colnames(buf[[i]]) = names(dist0_stack[[i]])
} 

#COORDINADAS X Y por pixel para cada rasterlayer de MOD
#xyfromCell()
#xyFromCell(MOD, c(1:ncell(MOD))

xy.df = data.frame(xyFromCell(dist0_stack, c(1:ncell(dist0_stack))))
colnames(xy.df) = c("x_coord", "y_coord")

###bind

buf_DF= cbind(NPix.df,xy.df, buf)
dim(buf_DF)

##guardar DF 
write.csv(buf_DF, "buff.csv")



# stack  = stack("buffers")
# 
# writeRaster(stack[[200]],"buf_200.tif" )
