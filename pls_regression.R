library(dplyr)
library(pls)

data = read_csv("data/data_m1.csv")
X =  dplyr::select(data, -Humedad)

#formula

dn0 = paste(colnames(X), collapse="+")
fm0 = as.formula(paste("Humedad ~ ", dn0)) #Humedad ~ chirp + ...
fm0 # formula de regresion


pls_model = plsr(fm0, data = data, validation = "CV")

# Find the number of dimensions with lowest cross validation error
cv = RMSEP(pls.model)
cv_dims = which.min(cv$val[estimate = "adjCV", , ]) - 1

# Rerun the model
pls_model_cv = plsr(fm0, data = data, ncomp = cv_dims)

#plot

coefficients = coef(pls_model_cv)
sum.coef = sum(sapply(coefficients, abs))
coefficients = coefficients * 100 / sum.coef # normalizacion
coefficients = sort(coefficients[, 1 , 1])

coeficientes_norm = as.data.frame(tail(coefficients, 5)) %>%
  rename(Coeficientes_Normalizados =  "tail(coefficients, 5)") %>%
  rownames_to_column()  %>%
  rename("Variables" ="rowname")


#barplot(tail(coefficients, 8)) 


ggplot(coeficientes_norm, aes(x = Variables, y = Coeficientes_Normalizados)) + 
  stat_summary(fun = "mean", geom = "bar") +
  ggtitle(" Importancia de variables \n mediante regresión de mínimos cuadrados parciales") +
  theme(plot.title = element_text(hjust = 0.5),
        plot.subtitle = element_text(hjust = 0.5))
