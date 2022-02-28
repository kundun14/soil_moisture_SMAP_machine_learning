
# ajuste y seleccion del modelos
library(randomForest)
library(ranger)
library(caret)

#manejo de  datos
library(tidyverse)
#transformacion de logit a escala normal para la comparacion de predicciones
library(boot)

#data
# Q se carga como string, hay q convertila a factor

X_train = read_csv("split_buff/X_train_buff.csv",
                   col_type = cols(Q = col_factor(levels = c("Q1", "Q2", "Q3", "Q4")))
                   )

y_train = read_csv("split_buff/y_train_buff.csv")

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

set.seed(1234)

rf_10 = ranger(formula = fm0,
                data= train,
                num.trees= 10,
                mtry= ncol(X_train)/3,
                oob.error = TRUE)

# OOB prediction error (MSE):       0.5225048 
# R squared (OOB):                  0.3286341

rf_100 = ranger(formula = fm0,
              data= train,
              num.trees= 100,
              mtry= ncol(X_train)/3,
              oob.error = TRUE)

# OOB prediction error (MSE):       0.437954 
# R squared (OOB):                  0.4372733

rf_200 = ranger(formula = fm0,
                data= train,
                num.trees= 200,
                mtry= ncol(X_train)/3,
                oob.error = TRUE)

# OOB prediction error (MSE):       0.4348556 
# R squared (OOB):                  0.4412544

rf_500 = ranger(formula = fm0,
                data= train,
                num.trees= 500,
                mtry= ncol(X_train)/3,
                oob.error = TRUE)

# OOB prediction error (MSE):       0.4311569 
# R squared (OOB):                  0.4460069

rf_750 = ranger(formula = fm0,
                data= train,
                num.trees= 750,
                mtry= ncol(X_train)/3,
                oob.error = TRUE)
 
# OOB prediction error (MSE):       0.4315713 
# R squared (OOB):                  0.4454744

rf_1000 = ranger(formula = fm0,
                data= train,
                num.trees= 1000,
                mtry= ncol(X_train)/3,
                oob.error = TRUE)

#OOB prediction error (MSE):       0.4304632 
# R squared (OOB):                  0.4468982


#TUNING

#mtry: numero de variables en cada particion de un nodo

hyper_grid = expand.grid( mtry = c(sqrt(ncol(X_train))*1,
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

# mtry      min.node.size  OOB_RMSE        r2 
# 3          25            0.6488100      0.4591161    

model_grid_500 = ranger(
  formula         = fm0, 
  data            = train, 
  num.trees       = 500,
  mtry            = 3.162278,
  min.node.size   = 25 ,
  seed            = 1234,
  write.forest= TRUE,
  oob.error = TRUE,
  importance = "impurity")

# importancia de las variables para predicir la humedad de suelo

importance(model_grid_500)
# log_chirp      cden       cic        da       soc     ALR_1     ALR_2   MFD_FD8 
# 1991.9282  710.4486  677.6915  822.5372  641.6332 1083.5551  800.1034  739.9923 
# log_evi         Q 
# 1331.4297 1599.626

#OTROS MODELOS

#        mtry    min.node.size num.trees   max.depth  OOB_RMSE   r2 oob.error
# 1   3.162278            25      1000        24     0.6485530     0.4595444      
# 2   3.162278            25      1000        36     0.6485607     0.4595316      
# 3   4.743416            25      1000        24     0.6486308     0.4594147      
# 4   4.743416            25      1000        36     0.6486332     0.4594107  
# 5   3.162278            25       500        24     0.6488067     0.4591216      
# 6   3.162278            25       500        36     0.6488100     0.4591161      
# 7   4.743416            25       500        36     0.6491096     0.4586165      
