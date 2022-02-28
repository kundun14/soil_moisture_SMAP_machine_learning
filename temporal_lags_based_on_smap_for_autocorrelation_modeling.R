library(tidyverse)
library(lubridate)
library(xts)
library(timetk)
library(tidyquant)

smap = readr::read_csv("smap0.csv", col_names = TRUE)
smap0 = smap %>% 
  arrange(NPix) %>% 
  filter(!(Fecha == as_date("2016-01-31"))) %>% 
  filter(!(Fecha == as_date("2016-12-13")))

#split
smap1 = split(smap0 , f = smap0$NPix )
#extraer fechas decada df
#fecha = lapply(smap1, pull , Fecha)
smap2 = lapply(smap1, function(x) x %>% select(Fecha,Humedad,))
#fecha = dplyr::pull(smap2[[1]], Fecha)

#LAGS
# smap3 = lapply(smap2, function(x) x %>% tk_xts(silent = TRUE)) # convertir a xts cada df
# smap4 = lapply(smap3, function(x) x %>% lag.xts(k = 1:5)) # crear 5 lags 
#con 5 lags se inutilizan  filas de datos 

#lags correcto
smap_lags5 = lapply(smap2, function(x) x %>% tq_mutate(select = Humedad, mutate_fun = lag.xts, k = 1:5))


# volver al formato origianal con NPiX  anexando los nuevos valores encontrados

x = 1:483 # 483 dfs
smap_lag5_NPix = map2(smap_lags5, x, ~cbind(.x, NPix = .y)) # añadir columna NPix a cada df para fututo joint on otros dataframaes de variables

#lista de df a df
smap_lags = bind_rows(smap_lag5_NPix)
#cambiar nombre de columnas
lag_nombres = paste(rep("lag"), 1:5, sep = "_" )
colnames(smap_lags)[3:7]  = lag_nombres

write.csv(smap_lags, "smap_lags.csv", row.names=FALSE, quote=FALSE)  #guardar DF
