# data del producto SMAP-Sentinel a 1Km de resolución espacial
# 

#paquetes necesarios
library(sp)
library(raster)
library(rgdal)
library(dplyr)

#contruir una lista de todos los rasters
smap.list.files = list.files(path = "./data/data_SMAP_2017_02Jun_2021",
                      recursive = TRUE, 
                      full.name = TRUE, # directorio relativo raster
                      pattern = "\\.tif$")

# converir los eleentos de la lista a rasters

smap.list.files.raster = lapply(smap.list.files, raster) 

#stackear la lista

smap.stack = stack(smap.list.files.raster)

# eliminar valores menores a 0

values(smap.stack)[values(smap.stack) <= 0] = NA 


# Extraccion, formato y adicion de fechas a cada raster

smap.stack.date.NoFormat = substr(names(smap.stack),
                         start = 22,
                         stop = 29)
smap.stack.fechaFormato = as.POSIXct(smap.stack.date.NoFormat,
                                      format = "%Y%m%j",
                                      tz = "UTC")

smap.stack.fecha.df = as.data.frame(as.Date(c(smap.stack.fechaFormato)))
smap.stack.Z= setZ(smap.stack, smap.stack.fecha.df[,1],
     name = "Fecha")
names(smap.stack.Z)= seq(1:nlayers(smap.stack.Z))


#ordenar stack por fechas

fechas.humedad.noSort = smap.stack.fecha.df[,1]
fechas.humedad.sort = sort.int(fechas.humedad.noSort, index.return=TRUE)$x #vector de fechas ordenadas
smap.stack.Z.sort = smap.stack.Z[[sort.int(fechas.humedad.noSort, index.return=TRUE)$ix]] #stack ordenado cronologicamente

#crop a area de estudio
#raster_har = raster("D:/MODIS_R/raster_humedad.tif") # raster humedad del suelo

smap = 
  smap.stack.Z.sort %>%
  crop (., raster_har) %>% # raster_har es el raster smap
  resample(., raster_har, method = "bilinear") 

#smap= setZ(smap, smap.stack.fecha.df[,1],name = "Fecha")


#stack_smap = stack("smap_2") # leer stack desde folder
#fechas_MOD = read.csv(file = 'fechas_NDVI_pre.csv',header = F,col.names = "Fecha") 
#stack_MOD = setZ(stack_MOD, fechas_MOD, "Fecha") # fechas de las layer

#eliminar layes con NAs
#layers con 483 NAs se eliminan
#cellStats(is.na(smap.stack.Z.sort), sum)

smap.stack.Z.sort0 = smap.stack.Z.sort[[which(cellStats(is.na(smap.stack.Z.sort), sum) < 483)]]

#eliminar layers con fechas repetidas

layers.repetidos = c(13,17,20,24,27,30,34,37,43,46,48,51,54,57,60,63,67,70,73,76,79,82,87,90,93,96,99,102,105,108,111,114,117,120,123,126,129,131,134,137,140,143,146,149,152,155,158,162,164,167)
smap = dropLayer(smap.stack.Z.sort0,layers.repetidos ) #stack sin repeticiones de fecha
fechas.smap.noRep = getZ(smap)

#guardar stack en disco

writeRaster(smap,
            filename = 'smap_1',
            overwrite= T)

#guardar fechas

write.table(fechas.smap.noRep, 
            "fechas_smap_1.csv",
            row.names = F,
            col.names = F)

# Construcion del dataframe

#fechas

fechas.stack.df = list()
for (i in 1:(nlayers(smap))) {
  fechas.stack.df[[i]] = data.frame(rep(getZ(smap[[i]]),
                                        times= ncell(smap)))  
}

for (i in 1:length(fechas.stack.df)){
  colnames(fechas.stack.df[[i]]) = "Fecha"
}


#humedad

humedad = list()
for (i in 1:nlayers(smap)) {
  humedad[[i]]= as.data.frame(smap[[i]])  
}

for (i in 1:length(humedad)){
  colnames(humedad[[i]]) = "Humedad"
} 

# NPixel

NPix.df = list()
for (i in 1:nlayers(smap)) {
  NPix.df[[i]] = data.frame(seq(1:ncell(smap[[i]])))
}

for (i in 1:length(NPix.df)){
  colnames(NPix.df[[i]]) = "NPix"
} 

#COORDINADAS X Y por pixel para cada rasterlayer de MOD
#xyfromCell()
#xyFromCell(MOD, c(1:ncell(MOD))

xy_list = list()
for (i in 1:(nlayers(smap))) {
  xy_list[[i]] = data.frame(xyFromCell(smap, c(1:ncell(smap[[i]]))))  
}

for (i in 1:length(xy_list)){
  colnames(xy_list[[i]]) = c("x_coord", "y_coord")
}

# N pixel, coordenadas, fecha y valores para cada rasterlayer de MOD

list_humedad = list()
for (i in 1: length(humedad)) {
  list_humedad[[i]]= cbind(NPix.df[[i]], xy_list[[i]], humedad[[i]], fechas.stack.df[[i]])
}

smap_df = bind_rows(list_humedad) #  junta  dfs de la lista en uno solo df
#todos los df tienen n filas
#dimensiones deMOD_df 783426 x 5
#resp_DF_0_spline = na.omit(resp_DF_spline) # eliminar NAs del DF

write.csv(smap_df, "smap_1.csv", row.names=FALSE, quote=FALSE)  #guardar DF
#exportar las fechas disponibles de humedad para usarlas en MODIS
#write.csv(as.data.frame(fechas.smap.noRep), "fecha_humedad_df.csv", row.names=FALSE, quote=FALSE)

#usar  fechas de smap para interpolar con datos del MODIS
#writeRaster(smap[[100]],filename = 'smap_prueba.tif',overwrite= T) # raster para probar con qgis



