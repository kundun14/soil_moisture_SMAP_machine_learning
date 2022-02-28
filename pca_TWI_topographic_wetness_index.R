library(FactoMineR)
library(factoextra)
library(ggplot2)
library(tidyverse)

#estandarizar las variables para que tengan media 0 y desviación estándar 1
# "rotation" proporciona los loadings de los componentes principales 
# (cada columna contiene el vector de loadings de cada componente principal).

# A PCA yields two metrics that are relevant for data exploration: 
# Firstly, how much variance each component explains (scree plot), and 
# secondly how much a variable correlates with a component (factor loading)
#cos2   It shows the importance of a principal component for a given observatio
# los valores de los PCAs para vada observacion son llamados factor scores


#data
data = read_csv("data/data_m1.csv")

# var X y y 

X =  dplyr::select(data, -Humedad)
#summary(X)

#construir PCA 

X_pca = PCA(X, scale.unit=TRUE, ncp=6) #scale.unit = estandarizar #correlation PCA
#ya que las variables estan en diferentes escalas es necesario estandarizarlas
# default Single Value Descoposition

#RESULTADOS PCA

summary(X_pca) # eigenvalores,varianza explicada y acumulada

X_pca_des= dimdesc(X_pca, axes = c(1:6)) # descripcon de componentes a nivel de significacia 0.05, confianza 0.95

X_pca_des$call$X

#X_pca_trans = X_pca_des$call # matriz X transformada al espacio de PCs dim1, faltan las demas dims

fviz_eig(X_pca)

X_pca$eig

# SELECCION DE COMPONENTES 

eig_val = as.data.frame(get_eigenvalue(X_pca)) %>%
  rownames_to_column()

write_csv(eig_val, "./pca/eig_valores_pca.csv")

# CORRELACION ENTRE VARIABLES Y COMPONENTES

var = get_pca_var(X_pca)
X_pca_corr=var$cor # correlaciones entre las variables y los PC

# library(corrplot)
# corrplot::corrplot(var$cos2, is.corr=FALSE)
# corrplot::corrplot(var$contrib, is.corr=FALSE)    

#summary(X_pca, nbelements=Inf, file="PCA.txt") # exporta los resultados como txt

#PLOTS e interpretaciones
# plot(X_pca, label = "none")
# plot(X_pca, type="l")

#  CIRCULO DE CORRELACIONES 

# CLUSTER DE FACTORES
#KMEANS
set.seed(123)
res.km = kmeans(var$coord, centers = 5, nstart = 25)
grp = as.factor(res.km$cluster)

#PLOTS de variables

# CIRCULO DE CORRELACIONES / PLOT DE VARIABLES 

# The closer a variable is to the circle of correlations,
# the better we can reconstruct this variable from the
# first two components (and the more important it is to
# interpret these components); the closer to the center
# of the plot a variable is, the less important it is for the
# first two components.

#from two synthetic variables (pc1 Y pC2), we are able to
#summarise most of the information provided by the 12 initial variables.


library(RColorBrewer)
pal_spec = brewer.pal(8, "Dark2") %>%
  colorRampPalette() # paleta de colores espectrales

var_plot_12 = fviz_pca_var(X_pca, col.var = grp,
             palette = pal_spec(8),
             repel = TRUE, # Avoid text overlapping
             axes = c(1, 2), # choose PCs to plot
             legend.title = "Factores")

var_plot_12 + ggtitle(" Círculo de correlaciones \n Primer y segundo componentes principales") +
  theme(plot.title = element_text(hjust = 0.5),plot.subtitle = element_text(hjust = 0.5))

var_plot_34 = fviz_pca_var(X_pca, col.var = grp,
                           palette = pal_spec(8),
                           repel = TRUE, # Avoid text overlapping
                           axes = c(3, 4), # choose PCs to plot
                           legend.title = "Factores")

var_plot_34 + ggtitle(" Círculo de correlaciones \n Tercer y cuarto componentes principales") +
  theme(plot.title = element_text(hjust = 0.5),plot.subtitle = element_text(hjust = 0.5))

var_plot_56 = fviz_pca_var(X_pca, col.var = grp,
                           palette = pal_spec(8),
                           repel = TRUE, # Avoid text overlapping
                           axes = c(5, 6), # choose PCs to plot
                           legend.title = "Factores")

var_plot_56 + ggtitle(" Círculo de correlaciones \n Quinto y sexto componentes principales") +
  theme(plot.title = element_text(hjust = 0.5),plot.subtitle = element_text(hjust = 0.5))


#grafica de correlaciones entre la humedad y los componentes principales


#LOADINGS

ncomp_cv = estim_ncp(X,scale=TRUE)  # generalized cross-validation approximation
