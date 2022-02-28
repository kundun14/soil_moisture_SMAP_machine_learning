# descripcion

# spatial operations: stacking, remsampling and cropping 
# of soil covariates from soilGrids

# dataframe from rasterstack of soil covariates

#setwd("D:/QGIS_R_SMAP/SOIL_GRIDS_250/soil_grids")
#getwd()

library(rgdal)
library(raster)
library(sp)
library(dplyr)

soil.list.files = list.files(".", # "." directorio
                            recursive = FALSE, 
                            full.name = FALSE,
                            pattern = "\\.tif$")

#raster_base <- raster("raster_humedad_base.tif")
#bbox<-readOGR(dsn="D:/QGIS_R_SMAP/SOIL_GRIDS_250/bbox",layer="bbox")
#borde = bbox(bbox)
#proyectar raster projectRaster()

#csr.utm18s.wgs84 = "+proj=utm +zone=18 +south +datum=WGS84 +units=m +no_defs" 

#rasterizar la lista de tiffs

soil.list.files.r = lapply(X = soil.list.files, 
                               FUN = raster)

#raster.utm = projectRaster(raster, crs = csr.utm18s.wgs84)
#soil.list.files.r.utm <- lapply(soil.list.files.r, 
#                                function(x) projectRaster(x, crs=csr.utm18s.wgs84))

#proyectar vector pTransform
#bbox_utm = spTransform(bbox, csr.utm18s.wgs84)
#borde.utm = bbox(bbox_utm)

#stackear

soil.stack = stack(soil.list.files.r.utm)

# cero es NA 
values(soil.stack)[values(soil.stack) <= 0] = NA

#resampling a 1000m #raster_base <- raster("raster_humedad_base.tif")

soil.stack.res.bi = resample(soil.stack, raster_base, method = "bilinear")

#CROP, recortar el twi.stack a la misma extension del raster_base

soil.stack.crop = crop(soil.stack.res.bi, raster_base)

#xportar rasters
#setwd("D:/QGIS_R_SMAP/SOIL_GRIDS_250/soil_grids_export")

nombres.soil = names(soil.stack.crop)
tiff.soil = c(".tif",".tif",".tif",".tif",".tif",".tif",
        ".tif")
nombres.soil.tif = paste(nombres.soil, tiff.soil, sep = "")

writeRaster(soil.stack.crop, 
            filename =  nombres.soil.tif,
            bylayer=TRUE, 
            format='GTiff')

#dataframe

# NPixel suelos
soil.NPix.df = data.frame(seq(1:ncell(soil.stack.crop)))
colnames(soil.NPix.df) = "NPix"

#Lista de variables

soil_ = list()
for (i in 1:nlayers(soil.stack.crop)) {
  soil_[[i]]= as.data.frame(soil.stack.crop[[i]])  
}

for (i in 1:length(soil_)){
  colnames(soil_[[i]]) = names(soil.stack.crop[[i]])
} 

#COORDINADAS X Y por pixel para cada covariable
#xyfromCell()
#xyFromCell(MOD, c(1:ncell(MOD))

xy_list = list()
for (i in 1:(nlayers(MOD))) {
  xy_list[[i]] = data.frame(xyFromCell(MOD, c(1:ncell(MOD[[i]]))))  
}

for (i in 1:length(xy_list)){
  colnames(xy_list[[i]]) = c("x_coord", "y_coord")
}
###bind

soil_DF= cbind(soil.NPix.df, soil_)

##guardar DF 

write.csv(soil_DF, "soil.csv", row.names=FALSE, quote=FALSE) 

crs(twi.stack)
