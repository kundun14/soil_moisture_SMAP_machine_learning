#crear dummiy variables para los meses

library(dplyr)

data_arr = read_csv("matriz_regresion_arrange.csv") # extaer NPix y fecha


data_arr_fecha = data_arr %>% 
  dplyr::select(c(NPix, Fecha))

mes_abr = c ("Ene","Feb","Mar","Abr","May","Jun", "Jul", "Ag", "Sep","Oct","Nov", "Dic")

data_mes= data_arr_fecha %>% 
  mutate(mes = lubridate::month(Fecha))
# 
data_mes$mes_ab = mes_abr[data_mes$mes]  # adjuntar nombres abreviados para cada mes

#join mes a data sin nas

#data_mes %>%

  # group_by(mes) %>%
  # mutate(mes_dummy = +(DayofMonth < 6 | 
  #                        (DayofMonth > (max(DayofMonth) - 3) &
  #                           max(DayofMonth) > 18))) # Change to appropriate number