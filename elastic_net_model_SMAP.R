library(caret)
library(tidyverse)
library(glmnet)

#data
data = read_csv("data/data_m1.csv")

# var x y y 

X =  dplyr::select(data, -Humedad)
y =  dplyr::select(data, Humedad)

#formula

dn0 = paste(colnames(X), collapse="+")
fm0 = as.formula(paste("Humedad ~ ", dn0)) #Humedad ~ chirp + ...
fm0 # formula de regresion

# Parametros de control del entrenamiento

train_control = trainControl(method = "repeatedcv",
                              number = 10,
                              repeats = 10,
                              search = "random",
                              verboseIter = TRUE)

# Entrenamiento del modelo

elastic_net_model = train(fm0,
                           data = cbind(y, X),
                           method = "glmnet",
                           preProcess = c("center", "scale"),
                           tuneLength = 25,
                           trControl = train_control)

elastic_net_model$results

#parametros CV
#alpha = 0.169, lambda = 0.00201 on full training set
#si alpha < 0.5 ridge es mas preponderante

# Check multiple R-squared

y_hat_enet = predict(elastic_net_model, X)
rsq_enet = cor(y, y_hat_enet)^2


# ElasticNet model con parametros estimados

X =  dplyr::select(data, -Humedad) %>% as.matrix()
y =  dplyr::select(data, Humedad) %>% as.matrix()

elasnet_cv = glmnet(X, y, alpha = 0.169, lambda = 0.00201,
                     standardize = TRUE)

elasnet_cv$beta

#PLOTS 


