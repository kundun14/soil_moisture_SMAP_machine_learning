# paquetes
library(sp)
library(raster)
library(rgdal)
library(rasterVis)
library(zoo)
library(tidyverse)

# importar rasters como lista

list.files.modis = list.files(path = "./img_mod/NDVI",
                              recursive = TRUE, 
                              full.name = TRUE,
                              pattern = "\\.tif$")

#importar fechas de rasters
#cargar fechas desde dos archivos posiblemente superpuestos

#fecha_1= getZ(get(load("D:/MODIS_R/NDVI/MOD13A1_NDVI_65_2015_161_2021_RData.RData")))
#fecha_2= getZ(get(load("D:/MODIS_R/NDVI/MOD13A1_NDVI_161_2021_241_2021_RData.RData")))
#fecha_2 = fecha_2[!(fecha_2 %in% fecha_1)] # eliminar fechas coincidentes de fecha_2
#fechas_modis = c(fecha_1,fecha_2)

fechas_MOD = getZ(get(load("./img_mod/NDVI/NDVI.RData")))

# stackear 

stack_0 = stack(list.files.modis)
stack_0 = setZ(stack_0, fechas_MOD, "Fecha") # fechas de las layers


#pre- procesamiento general

#CROP
shape = readOGR(dsn = "./geodata_aux/crop_layer.shp") # cargar shape 
stack_0 = crop(stack_0, shape) # cropear el tile 
stack_1 = stack_0 * 0.0001 # factor de correccion
values(stack_1)[values(stack_1) <= 0] = NA # solo considerar suelo y vegetacion
stack_MOD = setZ(stack_1, fechas_MOD, "Fecha") # fechas de las layers

#GUARDAR STACK pre-procesado

# writeRaster(stack_0,
#           filename = "NDVI_pre",
#           overwrite= T)
# 
# write.table(fechas_modis, 
#             "fechas_NDVI_pre.csv",
#             row.names = F,
#             col.names = F) 

#################################################################333
#nas_MOD

#cargar stack y fechas
# 
# stack_MOD = stack("NDVI_pre") # leer stack desde folder
# 
# fechas_MOD = read.csv(file = 'fechas_NDVI_pre.csv',
#                       header = F,
#                       col.names = "Fecha") 

#stack_MOD = setZ(stack_MOD, fechas_MOD, "Fecha") # fechas de las layers

#stack_MOD = setZ(stack_MOD, fechas_MOD, "Fecha")


#BRICK NAS DIARIOS
#raster NA

csr = crs(stack_MOD)
raster_na = raster()
raster_na = projectRaster(raster_na, crs = csr) #utm 18s wgs84
extent(raster_na) = extent(stack_MOD) # mismo extent que ndvi
res(raster_na) = res(stack_MOD)  # misma resolucion que ndvi
n = ncell(stack_MOD)
nas = rep(c(NA),times = ncell(stack_MOD))
raster_na = setValues(raster_na, nas) # llenamos el raster con NAs

# fechas diarias na

fechas.diarias.na = seq(as.Date(getZ(stack_MOD[[1]])),
                        as.Date(getZ(stack_MOD[[nlayers(stack_MOD)]])), 
                        by = "day")

# stack dairio na

stack.diario.nas = stack(lapply(1:length(fechas.diarias.na),
                                function(x) raster_na)) # crea  stack diario de nas

stack.diario.nas = setZ(stack.diario.nas,
                        fechas.diarias.na, 
                        "Fecha") # asigna fechas al stack diario de nas

# BRICK NA + MOD DIARIOS

#fechas_MOD = as.Date(as.vector(fechas_MOD)) #df a vector con as.vector()

fechas_na_No_MOD = fechas.diarias.na[!(fechas.diarias.na %in% fechas_MOD)] #matching %in%

#dropear los rasterlayes de stack.diario.nas q coincidan con 

stack_na_No_MOD = stack.diario.nas[[which(!(fechas.diarias.na %in% fechas_MOD))]] #

# MEZCLAR stacks

stack_na_MOD = stack(stack_MOD,
                     stack_na_No_MOD)

fechas_na_MOD = c(fechas_MOD,
                  fechas_na_No_MOD)

#ordenar los raster basados en las fechas

MOD_dates = sort.int(fechas_na_MOD, index.return=TRUE)$x #$x  las fechas ordenadas


stack_na_MOD_sort = stack_na_MOD[[sort.int(fechas_na_MOD,index.return=TRUE)$ix]] # stack ordenado por fechas

MOD = setZ(stack_na_MOD_sort,MOD_dates, "Fecha")


#write stack ordenado con nas y mod data

writeRaster(MOD,
            filename = './raster_stacks/NDVI/NDVI_na_sort',
            overwrite= T)

#writeRaster(MOD[[1]],filename = './raster_stacks/NDVI/NDVI_na_sort_1.tif',overwrite= T)

# write.table(MOD_dates, 
#             "fechas_NDVI_na_sort.csv",
#             row.names = F,
#             col.names = F)


######################################################
#spline_har
#cargar data

# MOD = stack("NDVI_na_sort")
# 
# MOD_dates = read.csv(file = 'fechas_NDVI_na_sort.csv',
#                      header = F,
#                      col.names = "Fecha")
# 
# MOD = setZ(MOD,MOD_dates,"Fecha")


#INTERPOLACION SPLINE DIARIA

#FUNCION DE INTERPOLACION ENTRE PIXELES DEL RASTERSTACK

spline_int_f = function(x) {  
  z  = which(is.na(x))
  nz = length(z)
  nx = length(x)
  if (nz > 0 & nz < nx) { 
    x[z] = spline(x = 1:nx, y = x, xout = z, method = "natural")$y
  }
  x
}

#INTERPOLACION

MOD_sp = calc(MOD, spline_int_f) # funcion para inter spline 3
MOD_sp = setZ(MOD_sp, MOD_dates, "Fecha") # resultados negeativos
values(MOD_sp)[values(MOD_sp) <= 0] = NA # solo considerar suelo y vegetacion


#HARMONIZAR:pixel, resolucion, extencion y crs smap = pixel MODIS

raster_har = raster("./geodata_aux/raster_humedad.tif") # raster humedad del suelo

MOD =
  MOD_sp %>%
  crop (., raster_har) %>% # raster_har es el raster smap
  resample(., raster_har, method = "bilinear")


#archivos para EXPORTAR -->

writeRaster(MOD,
            filename = "./raster_stacks/NDVI/NDVI_har", 
            overwrite= T)

write.table(MOD_dates, 
            "./raster_stacks/NDVI/NDVI_fechas.csv", 
            row.names = F,
            col.names = F)

#writeRaster(MOD[[1]],filename = './raster_stacks/NDVI/NDVI_har_1.tif',overwrite= T)

###########################################
#df

#cargar data
# 
# MOD = stack("./raster_stacks/NDVI")
# 
# MOD_dates = read.csv(file = './raster_stacks/NDVI.csv',
#                      header = F,
#                      col.names = "Fecha")
# 
#MOD = setZ(MOD,MOD_dates,"Fecha")


#extraer DF

MOD = setZ(MOD,MOD_dates,"Fecha")
#NAs = sum(!is.na(MOD))  
#getValues(MOD[[5]])

#lista de  fechas de cada rasterlayer de MOD

fechas_list = list()
for (i in 1:(nlayers(MOD))) {
  fechas_list[[i]] = data.frame(rep(getZ(MOD[[i]]),
                                    times = ncell(MOD)))  
}

for (i in 1:length(fechas_list)){
  colnames(fechas_list[[i]]) = "Fecha"
}

#lista de  valores de  pixeles  para cada rasterlayer de MOD
#NDVI (reemplazar por nombre de variable)

var_list = list()
for (i in 1:nlayers(MOD)) {
  var_list[[i]]= as.data.frame(MOD[[i]])  
}

for (i in 1:length(var_list)){
  colnames(var_list[[i]]) = "NDVI" #### cambiar nombre de variable
} 

# NPixel

NPix.list = list()
for (i in 1:nlayers(MOD)) {
  NPix.list[[i]] = data.frame(seq(1:ncell(MOD[[i]])))
}


for (i in 1:length(NPix.list)){
  colnames(NPix.list[[i]]) = "NPix"
}

#COORDINADAS X Y por pixel para cada rasterlayer de MOD
#xyfromCell()
#xyFromCell(MOD, c(1:ncell(MOD))

xy_list = list()
for (i in 1:(nlayers(MOD))) {
  xy_list[[i]] = data.frame(xyFromCell(MOD, c(1:ncell(MOD[[i]]))))  
}

for (i in 1:length(xy_list)){
  colnames(xy_list[[i]]) = c("x_coord", "y_coord")
}

# N pixel, coordenadas, fecha y valores para cada rasterlayer de MOD

list_MOD = list()
for (i in 1: length(var_list)) {
  list_MOD[[i]]= cbind(NPix.list[[i]], xy_list[[i]], var_list[[i]], fechas_list[[i]])
}

MOD_df = bind_rows(list_MOD) #  junta  dfs de la lista en uno solo df
#todos los df tienen n filas
#dimensiones deMOD_df 783426 x 5
#resp_DF_0_spline = na.omit(resp_DF_spline) # eliminar NAs del DF

#GUAARDAR DF ANTES DE PROCESAMIENTO

write.csv(MOD_df, "./data_frames/NDVI_pre_outliers.csv", row.names=FALSE, quote=FALSE)  #guardar DF

#humedad.csv = read.csv(file = 'NDVI.csv') # leer archivo
# revisar los df exportados
#linear.df = read.csv(file = 'D:/MODIS_R/SAVI_LINEAR.csv') 
#spline.df = read.csv(file = 'D:/MODIS_R/SAVI_SPLINE.csv') 
#guardar raster de prueba
#writeRaster(MOD[[100]],filename = './NDVI_prueba.tif',overwrite= T)

######################################################

#TIME PLOTS

MOD_df = read_csv("./data_frames/NDVI_pre_outliers.csv")

#time plots per pixel antes de correcion de outliers

mod_arr = MOD_df %>% arrange(NPix)
mod_arr_sub = mod_arr %>%  filter(NPix %in% c(1:4)) # pixels 1 a 4

#TIME PLOT--- UN PANEL PARA CADA PIXEL 

p = ggplot(mod_arr_sub, aes(Fecha, NDVI, group = NPix)) +
  geom_line(color="purple") + 
  ggtitle(" NDVI MOD09GA \n Pre outliers detection \n Área de estudio\n 2017-2021") +
  facet_wrap(~ NPix)

# existen outliers producto de errores del sensor, etc


#convertir de long to wide con pixels como variables

mod = mod_arr[-(2:3)] # eliminar x y y coords
mod_wider = pivot_wider(mod, names_from = NPix,
            values_from = NDVI, 
            names_prefix = "Pixel_")

#convertir cada columna a  time-series

library("xts")
library("forecast")

fecha = dplyr::pull(mod_wider, Fecha) #  vector de fechas

mod_wider_ts = lapply(mod_wider[,-1], xts, order.by = fecha) #  lista por pixel
#class(mod_wider_ts$Pixel_1) # clase xtr/zoo

# series de tiempo con outliers reemplazados mediante STL loess (locally weighted regression and scatterplot smoothing
mod_wider_ts_clean = lapply(mod_wider_ts, 
                            tsclean,
                            replace.missing = F, 
                            lambda = NULL)

#aplicar transformaciones inversas (juntar la listas en un df y hacer wide to long)

#lista to df
mod_df_clean = bind_rows(mod_wider_ts_clean) # unir los df de la lista por fecha

fecha_df = as.data.frame(fecha, col.names = "fecha")

mod_df_clean_fecha = mod_df_clean %>% 
  add_column(fecha_df) %>% 
  relocate("fecha")

#xts to dbl

#long to wide

cols = colnames(mod_df_clean_fecha[2:ncol(mod_df_clean_fecha)])
  
mod_long_clean = pivot_longer(mod_df_clean_fecha,
                              cols = cols,
                              names_to = "NPIX", 
                              names_prefix = "Pixel_",
                              values_to = "NDVI")


