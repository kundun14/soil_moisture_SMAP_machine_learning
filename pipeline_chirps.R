#paquetes necesarios
library(sp)
library(raster)
library(rgdal)
library(dplyr)
library(ggplot2)

#contruir una lista de todos los rasters
list.files = list.files(path = "./chrips_data",
                        recursive = TRUE, 
                        full.name = TRUE, # directorio relativo raster
                        pattern = "\\.tif$")

# converir los eleentos de la lista a rasters
list.files.raster = lapply(list.files, raster) #coordenadas geograficas pero sin crs
#stackear la lista
stack = stack(list.files.raster) 
#crs(stack) = "+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0"# AÑADIR CRS
#writeRaster(stack[[1]],filename = './prueba.tif',overwrite= T)

# eliminar valores menores a 0
#values(stack)[values(stack) <= 0] = NA #  valor flag


# Extraccion, formato y adicion de fechas a cada raster
stack.date.NoFormat = substr(names(stack),
                             start = 2,
                             stop = 9)
stack.fechaFormato = as.POSIXct(stack.date.NoFormat,
                                format = "%Y%m%j",
                                tz = "UTC")

stack.fecha.df = as.data.frame(as.Date(c(stack.fechaFormato)))

stack.Z= setZ(stack, stack.fecha.df[,1],
              name = "Fecha")
#names(stack.Z)= seq(1:nlayers(stack.Z)) #nombres de las layers


# #ordenar stack por fechas
# fechas.humedad.noSort = smap.stack.fecha.df[,1]
# fechas.humedad.sort = sort.int(fechas.humedad.noSort, index.return=TRUE)$x #vector de fechas ordenadas
# smap.stack.Z.sort = smap.stack.Z[[sort.int(fechas.humedad.noSort, index.return=TRUE)$ix]] #stack ordenado cronologicamente

#crop a area de estudio
raster_har = raster("./raster_humedad.tif") # raster humedad del suelo
shape = readOGR(dsn = "./geodata_aux/crop_layer.shp") # cargar shape 
shape = spTransform(shape, crs(stack.Z))


# persiann_ccs_bi =  stack.Z[[1]] %>%
#   crop(., shape)  %>% # crop a area mas pequeña
#   projectRaster(., crs = crs(raster_har)) %>%
#   resample(., raster_har, method = "bilinear" ) %>% #codigo para remuestrear a la resolucion de raster_har disaggregate o resample FUN
#   crop (., raster_har) # raster_har es el raster smap #
# 
# writeRaster(persiann_ccs_bi,filename = './bi.tif',overwrite= T)

chirp =  stack.Z %>%
  crop(., shape)  %>% # crop a area mas pequeña
  projectRaster(., crs = crs(raster_har)) %>%
  resample(., raster_har, method = "ngb" ) %>% #codigo para remuestrear a la resolucion de raster_har disaggregate o resample FUN
  crop (., raster_har) # raster_har es el raster smap # 

#adjuntar fecha a raster
chirp= setZ(chirp, stack.fecha.df[,1],
                   name = "Fecha")

#exportar stack
writeRaster(chirp,
            filename = "./stacks/chirp",
            overwrite= T)

#exportar fechas
write.table(stack.fecha.df,
            "./stacks/chirp_fecha.csv",
            row.names = F,
            col.names = F)

#writeRaster(persiann_ccs_ngb,filename = './ngb.tif',overwrite= T)


# Construcion del dataframe

#fechas
fechas.stack.df = list()
for (i in 1:(nlayers(chirp))) {
  fechas.stack.df[[i]] = data.frame(rep(getZ(chirp[[i]]),
                                        times= ncell(chirp)))  
}

for (i in 1:length(fechas.stack.df)){
  colnames(fechas.stack.df[[i]]) = "Fecha"
}


#valores
valores = list()
for (i in 1:nlayers(chirp)) {
  valores[[i]]= as.data.frame(chirp[[i]])  
}

for (i in 1:length(valores)){
  colnames(valores[[i]]) = "chirp"
} 

# NPixel
NPix.df = list()
for (i in 1:nlayers(chirp)) {
  NPix.df[[i]] = data.frame(seq(1:ncell(chirp[[i]])))
}

for (i in 1:length(NPix.df)){
  colnames(NPix.df[[i]]) = "NPix"
} 

#COORDINADAS X Y por pixel para cada rasterlayer de MOD
#xyfromCell()
#xyFromCell(MOD, c(1:ncell(MOD))

xy_list = list()
for (i in 1:(nlayers(chirp))) {
  xy_list[[i]] = data.frame(xyFromCell(chirp, c(1:ncell(chirp[[i]]))))  
}

for (i in 1:length(xy_list)){
  colnames(xy_list[[i]]) = c("x_coord", "y_coord")
}

# N pixel, coordenadas, fecha y valores para cada rasterlayer de MOD

list_chirp = list()
for (i in 1: length(valores)) {
  list_chirp[[i]]= cbind(NPix.df[[i]], xy_list[[i]], valores[[i]], fechas.stack.df[[i]])
}

chirp_df = bind_rows(list_chirp) #  junta  dfs de la lista en uno solo df
write.csv(chirp_df, "./chirp.csv", row.names=FALSE, quote=FALSE)  #guardar DF

#todos los df tienen n filas
#dimensiones deMOD_df 783426 x 5
#resp_DF_0_spline = na.omit(resp_DF_spline) # eliminar NAs del DF

#write.csv(smap_df, "smap_1.csv", row.names=FALSE, quote=FALSE)  #guardar DF
#exportar las fechas disponibles de humedad para usarlas en MODIS
#write.csv(as.data.frame(fechas.smap.noRep), "fecha_humedad_df.csv", row.names=FALSE, quote=FALSE)

#usar  fechas de smap para interpolar con datos del MODIS
#writeRaster(chirp[[1]],filename = 'chirps1_prueba.tif',overwrite= T) # raster para probar con qgis



