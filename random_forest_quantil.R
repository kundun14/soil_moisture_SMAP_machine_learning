# ajuste y seleccion de modelo
library(randomForest)
library(ranger)
library(caret)

#manejo de  datos
library(tidyverse)
#transformacion de logit a escala normal para la comparacion de predicciones
#library(boot)

#data
# Q se carga como string, hay q convertila a factor

X_train = read_csv("split_quantil_sel/X_train.csv",
                   col_type = cols(Q = col_factor(levels = c("Q1", "Q2", "Q3", "Q4")))
                   )

y_train = read_csv("split_quantil_sel/y_train.csv")

#log_chirp   es el promedio tranformado (log) de los datos chirps 
#de los 3 dias anteriores chirp3_lag


#mtry = sqt(N°cov)*1, sqt(N°cov)*1.5, sqt(N°cov)*2, sqt(N°cov)*3
#ntree  = 100, 150, 200, 250, 500, 750 and 1000


#ranger
train = bind_cols(y_train, X_train)

#formula

dn0 = paste(colnames(X_train), collapse="+")
fm0 = as.formula(paste("Humedad_log  ~ ", dn0)) #Humedad ~ chirp + ...
fm0 # formula de regresion

# mtry: number of variables to try at each split (default to m = sqrt(p))
# num.trees: number of trees to build (default to 500)
# min.node.size: minimum number of observations in each node (default to 5)


set.seed(1234) #FIJA GENERADOR DE NUMERO ALEATOREOS

# MODELO CON 10 ARBOLES

rf_10 = ranger(formula = fm0,
                data= train,
                num.trees= 10,
                mtry= ncol(X_train)/3,
                oob.error = TRUE)
# 
# OOB prediction error (MSE):       0.3952917 
# R squared (OOB):                  0.2440949
# 
# MODELO CON 100 ARBOLES

rf_100 = ranger(formula = fm0,
              data= train,
              num.trees= 100,
              mtry= ncol(X_train)/3,
              oob.error = TRUE)
 
# OOB prediction error (MSE):       0.3250759 
# R squared (OOB):                  0.37836663

# MODELO CON 500 ARBOLES

rf_500 = ranger(formula = fm0,
                data= train,
                num.trees= 500,
                mtry= ncol(X_train)/3,
                oob.error = TRUE)
 
# OOB prediction error (MSE):       0.3209381 
# R squared (OOB):                  0.3862791


# MODELO CON 1000 ARBOLES

rf_1000 = ranger(formula = fm0,
                data= train,
                num.trees= 1000,
                mtry= ncol(X_train)/3,
                oob.error = TRUE)

# OOB prediction error (MSE):       0.3200785 
# R squared (OOB):                  0.3879231

#TUNING

#mtry: numero de variables en cada particion de un nodo

hyper_grid = expand.grid(mtry = c(sqrt(ncol(X_train))*1,
                                   sqrt(ncol(X_train))*1.5, 
                                   sqrt(ncol(X_train))*2, 
                                   sqrt(ncol(X_train))*3),
                          min.node.size= seq(5,25, by=5),# a mayor min.node.size arbole mas pequeños
                          OOB_RMSE   = 0,
                          r2 = 0,
                          oob.error= TRUE)

#modelos (num.trees = 500)

for(i in 1:nrow(hyper_grid)) {
  
  # train model
  model = ranger(
    formula         = fm0, 
    data            = train, 
    num.trees       = 500,
    mtry            = hyper_grid$mtry[i],
    min.node.size   = hyper_grid$min.node.size[i],
    seed            = 1234
  )
  
  # add OOB error to grid
  hyper_grid$OOB_RMSE[i] = sqrt(model$prediction.error)
  hyper_grid$r2[i] = model$r.squared
}

hyper_grid %>% 
  dplyr::arrange(OOB_RMSE) %>%
  head(10)

#mejor modelo (num.trees = 500)

# mtry         min.node.size  OOB_RMSE        r2       oob.error
# 3.162278            25     0.5580571    0.4044657      TRUE
# 

model_grid_500 = ranger(
  formula         = fm0, 
  data            = train, 
  num.trees       = 500,
  mtry            = 3,
  min.node.size   = 25 ,
  seed            = 1234,
  write.forest= TRUE,
  oob.error = TRUE,
  importance = "impurity") #variance of the responses for regression 

# importancia de las variables para predicir la humedad de suelo

importance(model_grid_500)


#######################

#RANDOM FORET EN CARET CON K FOLD CV

