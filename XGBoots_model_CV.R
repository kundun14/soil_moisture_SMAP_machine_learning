
grid_1 = list(booster = "gbtree", objective = "reg:squarederror", 
              eta=0.05, gamma=0.2, max_depth=6, min_child_weight=0, 
              subsample=0.5, colsample_bytree=0.3) #eval_metric= "rmse"
#colsample_bytree=0.3 remuestreo de las variables tipo random forest

xgbcv1 = xgb.cv( params = grid_1, data = train_D, nrounds = 1000, 
                nfold = 10, showsd = T, stratified = F, print_every_n = 100,
                early_stopping_rounds = 10 , maximize = F)


# booster = "gbtree", objective = "reg:squarederror", 
# eta=0.1, gamma=0.1, max_depth=6, min_child_weight=0, 
# subsample=0.5, colsample_bytree=0.3
# optimo nrounds es  [320] con train-rmse:0.561253+0.003192	test-rmse:0.648365+0.003432
#sobre ajuste?

xgbcv1$best_iteration
#320 arboles son los que mejores resultados producen dados los otros hiperparametros 

