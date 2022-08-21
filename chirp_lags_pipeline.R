library(tidyverse)
library(lubridate)
library(xts)
library(timetk)
library(tidyquant)

data_chirp = read_csv("chirp.csv") %>%
  dplyr::select(-c("x_coord", "y_coord"))
  
# data_chirp =dplyr::select(data_10PMP, c("NPix", "Fecha", "chirp"))
#data_persian =dplyr::select(data_10PMP, c("NPix", "Fecha", "persian"))


#split
split_chirp = split(data_chirp , f = data_chirp$NPix )
#extraer fechas decada df
#fecha = lapply(smap1, pull , Fecha)
select_chirp = lapply(split_chirp, function(x) x %>% select(Fecha,chirp))
#fecha = dplyr::pull(smap2[[1]], Fecha)

#LAGS
# smap3 = lapply(smap2, function(x) x %>% tk_xts(silent = TRUE)) # convertir a xts cada df
# smap4 = lapply(smap3, function(x) x %>% lag.xts(k = 1:5)) # crear 5 lags 
#con 5 lags se inutilizan  filas de datos 

#lags 30 dias antes
chirp_lags = lapply(select_chirp, function(x) x %>% tq_mutate(select = chirp, mutate_fun = lag.xts, k = 1:7))


# volver al formato origianal con NPiX  anexando los nuevos valores encontrados

x = 1:483 # 483 dfs
chirp_lags_NPix = map2(chirp_lags, x, ~cbind(.x, NPix = .y)) # añadir columna NPix a cada df para fututo joint on otros dataframaes de variables

#lista de dfs a df
chirp_lags_df = bind_rows(chirp_lags_NPix)
#cambiar nombre de columnas
lag_nombres = paste(rep("chirp_lag"), 1:7, sep = "_" )
colnames(chirp_lags_df)[3:9]  = lag_nombres
#adjuntar promedios de los tres ultimos lags
chirp_lags_tib = as_tibble(chirp_lags_df) %>%
  mutate(chirp_3lags = (chirp_lag_1  + chirp_lag_2 + chirp_lag_3 )/3) %>%
  select(c(1,10:11))

#save
write.csv(chirp_lags_tib, "chirp_lags.csv", row.names=FALSE, quote=FALSE)  #guardar DF


#####
# data_final <- data_final %>%
#   group_by(cars) %>%
#   mutate(last3_average = (lag(speed, 1) + lag(speed, 2) + lag(speed, 3))/3)