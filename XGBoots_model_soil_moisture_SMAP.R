# install.packages("xgboost")
# install.packages("tidyverse")
# install.packages("caret")


# ajuste y seleccion del modelos
library(xgboost)
library(caret)
#manejo de  datos
library(tidyverse)
#data
# Q se carga como string, hay q convertila a factor

X_train = read_csv("X_train.csv",
                   col_type = cols(Q = col_factor(levels = c("Q1", "Q2", "Q3", "Q4")))
                   )

y_train = read_csv("y_train.csv")



#xgboots solo acepta variables numericas
#transformacion a dummies de la variable Q

Q = X_train %>% dplyr::select(Q)

#transformar dataframe a matrixmodel
#ademas no es necesario estandarizar las variables

Q_dum = model.matrix(~Q-1, data= Q) %>% # crear 4 variables indicadoras
  as_tibble()
colnames(Q_dum)[1] <- c("Q1")
colnames(Q_dum)[2] <- c("Q2")
colnames(Q_dum)[3] <- c("Q3")
colnames(Q_dum)[4] <- c("Q4")

X_train_dum =  bind_cols(X_train, Q_dum) %>% select(-Q)

X_train_dum$Q1 = as.integer(X_train_dum$Q1)
X_train_dum$Q2 = as.integer(X_train_dum$Q2)
X_train_dum$Q3 = as.integer(X_train_dum$Q3)

#HACER LO MISMO CON LA DATA DE EVALUACION (data_tets) para la evaluaciona final del modelo
#aplicar funcion predict()


#convertir a xgb.DMatrix (#xgb.DMatrix crea una matriz para entrenar el modelo)

train_D = xgb.DMatrix(label=as.matrix(y_train),
                      data=as.matrix(X_train_dum))

#en la funcion xgboost solo se usara como argumento train_D
#no es necesario usar explicitamente la variable de respuesta (label) 
#si la data esta en formato xgb.DMatrix

# xgb.DMatrix.save(train_D, 'xgb.DMatrix.data') #guardar la data
# train_D = xgb.DMatrix('xgb.DMatrix.data') #cargar la data


set.seed(1234) # es neceario in xgboots?

#PRIMERO CORRER XGB CON LOS PARAMETROS EN DEFAULT

grid_0 = list(booster = "gbtree", objective = "reg:squarederror", 
           eta=0.3, gamma=0, max_depth=6, min_child_weight=1, 
           subsample=1, colsample_bytree=1, eval_metric= "rmse")

#subsample: seleccion aleatoria de un porcentaje de muestras para entrenamiento de un arbol (controla el overfitting de cada arbol)
#colsample_bytree : seleccion aleatoria de un variables para entrenamiento de un arbol (reduce la correlacion entre arboles)
#max_depth; profunidad maxima de los arboles (puede ir de 6 a 32)
#eta: learning rate
#gamma (controla el overfitting de cada arbol)
#objective = "reg:squarederror" RMSE dafault
#eval_metric= "rmse"default si objective = "reg:squarederror"

#k folds CV con parámetros en default

xgbcv = xgb.cv( params = grid_0, data = train_D, nrounds = 200, 
                 nfold = 10, showsd = T, stratified = F, print_every_n = 25,
                early_stopping_rounds = 10 , maximize = F)

#data xgb.DMatrix
#nrounds: numero maximo de iteraciones a probar (n de arboles)
#nfold: folds o grupos de validacion
#showsd: muestra la desviacion estandar de la estimacion de error
#early.stop.round: funcion de detencion, k intero, si el performance no mejora en k rounds, para el proceso.


xgbcv$best_iteration #97

#train-rmse:0.501286 test-rmse:0.653614 
#aun peor que el modelo de ramdon forest


#GRID SEACH


#data
train = bind_cols(y_train, X_train_dum)
#caret formula
dn0 = paste(colnames(X_train_dum), collapse="+")
fm0 = as.formula(paste("Humedad_log  ~ ", dn0)) #Humedad ~ chirp + ...
fm0 # formula de regresion

#caret CV
set.seed(1234)

train_control = trainControl(method = "cv", number = 5)


grid = expand.grid(nrounds = 1000, # numero de iteraciones (arboles)
                         max_depth = 6,
                         eta = 0.05,
                         gamma = 0.1,
                         colsample_bytree = 0.3,
                         min_child_weight = 0,
                         subsample =0.5)

xgb_cv = train(fm0, data = train, method = "xgbTree",
                trControl=train_control,
                tuneGrid = grid)

#reg:squarederror Regression with squared loss (Default)


#MODELO 1
#The final values used for the model were nrounds = 200, max_depth = 6, eta =
#0.1, gamma = 0.1, colsample_bytree = 1, min_child_weight = 0 and subsample = 1.
#RMSE 0.6488753, R2 0.4593114 # aparentemente es te RMSE es muy optimista



# for(i in 1:nrow(hyper_grid)) {
#   
#   # train model
#   model = ranger(
#     formula         = fm0, 
#     data            = train, 
#     num.trees       = 500,
#     mtry            = hyper_grid$mtry[i],
#     min.node.size   = hyper_grid$min.node.size[i],
#     seed            = 1234
#   )
#   
#   # add OOB error to grid
#   hyper_grid$OOB_RMSE[i] = sqrt(model$prediction.error)
#   hyper_grid$r2[i] = model$r.squared
# }
# 
# hyper_grid %>% 
#   dplyr::arrange(OOB_RMSE) %>%
#   head(10)

#MLR

#una vez halaldo el mejor modelo debe ajustarse a toda la data de entrenamiento

modelo = xgboost(data = train_D , max_depth = 6,
               eta = 0.1, nthread = 2, nrounds = 200,
               objective = "reg:squarederror")


