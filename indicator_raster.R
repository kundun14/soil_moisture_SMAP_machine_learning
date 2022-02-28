# crea un raster indicador donde  

# # pixels validos son 1

# # pixels con errores son 0

# # # la finalidad de este proceso es filtrar las covariables, seleccionándolas
# # # si el valor del raster indicador es 1 e ignorándolas de si es cero (non válidos)


library(raster) # nueva veresion terra
library(rasterVis)
library(sp)
library(raster)
library(rgdal)

raster_har = raster("raster_humedad.tif") # raster humedad del suelo



#eliminar pixels 
pix = 1:483
pix_error = as.integer(c(3,6,7,10,26,30,31,32,53,54,55,59,77,93,96,100,
                         104,119,126,127,147,148,149,150, 169,170,171, 174, 
                         197,198,199, 207,208,230,231,232,245,251,254,255,
                         256,270,274,277,280,297,300,301,302,303,304,305,
                         318,319,320,325,326,327,328,329,330,339,
                         341,342,349,352,354, 355,356, 357,361, 363,364,
                         365, 369,370,371, 372,373, 375,376, 377, 380,
                         386,387,388,389,392,390, 393,394,395, 396, 397,
                         398,399, 405, 406, 407 ,410, 411,412,415, 416,417,
                         418,419,464,  465,466,469, 473,477,478, 479,482,
                         483,24,25,29,72,81,82,94,116,123,124,125, 139, 140, 
                         142, 172, 176, 222, 234, 246,253,257,279, 281, 293, 
                         316, 317, 338, 340, 348, 350,351,
                         8, 9, 49, 23, 57, 60, 83, 97,60, 121, 146, 153, 163, 164, 
                         173, 215, 220, 221, 229, 243, 247 , 275, 292, 266, 264, 269, 
                         298, 299, 307,323,324, 334, 334, 337, 353, 366, 367, 437, 436, 
                         441, 442, 443, 446, 423 , 459, 460, 472 ))

pix_validos = pix[!(pix %in% pix_error)] 


#indexar en raster a los pix_validos y dales como valor 1 y a los no validos valor 0

raster_har[pix_validos] = 1 # pixels validos son 1
raster_har[pix_error] = 0 # pixels con errores son 0

raster::writeRaster(raster_har,
            filename = "humedad_pixeles_validos_monitoreo.tif",
            overwrite= T)

raster_har[raster_har == 0] = NA # set NA a los valores invalidos

# extraer los puntos solo en los pixeles validos 

ras = raster("smap_prueba.tif")
pts = as(ras, 'SpatialPointsDataFrame')
writeOGR(pts, ".", "smap_points", driver="ESRI Shapefile")

pol = rasterToPolygons(raster_har) # raster to polygon
writeOGR(pol, ".", "pol/poligono_pixeles_eliminados", driver="ESRI Shapefile")


#raster_indicador_na = raster::values(raster_har)[raster::values(raster_har) = 0] = NA 

raster::writeRaster(raster_har,
                    filename = "humedad_pixeles_validos_monitoreo_NA.tif",
                    overwrite= T)
