### activating packagies
library('Rcpp')
library('ABCoptim')
library('randomForest')


### making a function out of Random forest models. 
##ABC by default goes for minimization
## z is a vector, first value is the number of randomly selected descr.,
##second is classwt for inactive class, third is number of inactives in tr.set
##There will be several optimizations
## 1) BA 2) Sens 3)Spec 4)Prec

### make a standard model using initial parameters
sad <- function(z) {
#col_selection <- c(sample(seq(from = 2, to = ncol(df), by = 1),ncol(df)-1 ) )

rf_multiple <- randomForest(x=df[-test_set_IDs, 2:ncol(df)	],
 y=as.factor(df[-test_set_IDs, 1]),
 xtest=df[test_set_IDs, 2:ncol(df)],
 ytest=as.factor(df[test_set_IDs, 1]),
 ntree=500, mtry = z[1], replace=FALSE, classwt=c(z[2],0.5),
 strata=as.factor(df[, 1]), sampsize=c(z[3],training_actives),
 keep.forest=TRUE, keep.inbag = TRUE, do.trace=50, importance=TRUE, nPerm=1) 


return(rf_multiple)
}

initial_par <- c( floor(sqrt(ncol(df)-1) ) ,0.5,training_inactives) #1 or 11 depending on the final solution
rfstd <- sad(initial_par)
save(rfstd, file = paste(output_dir, "\\rfstandard_final", ".Rdata", sep = ""))


############################################################################### 1 BA 

sad <- function(z) {
#col_selection <- c(sample(seq(from = 2, to = ncol(df), by = 1),ncol(df)-1 ) )

rf_multiple <- randomForest(x=df[-test_set_IDs, 2:ncol(df)	],
 y=as.factor(df[-test_set_IDs, 1]),
 xtest=df[test_set_IDs, 2:ncol(df)],
 ytest=as.factor(df[test_set_IDs, 1]),
 ntree=500, mtry = z[1], replace=FALSE, classwt=c(z[2],0.5),
 strata=as.factor(df[, 1]), sampsize=c(z[3],training_actives),
 keep.forest=TRUE, keep.inbag = TRUE, do.trace=50, importance=TRUE, nPerm=1) 

return(round( (rf_multiple$confusion[6]+rf_multiple$confusion[5])/2,2 ) )
#return(rf_multiple)
}

### making optimization
initial_par <- c( floor(sqrt(ncol(df)-1) ) ,0.5,training_inactives)
w <- 0
write.csv(w, file = 'C:/start.csv')

optimization_result_BA <- abc_optim(initial_par, sad, FoodNumber = 10,
lb = c(10, 0.1, training_actives),
ub = c(45, 0.5, training_inactives),
limit = 0.6*(10/2)*3,
maxCycle = 5,
optiinteger = FALSE, criter = 3,
parscale = rep(1, length(par)),
fnscale = 1)

write.csv(w, file = 'C:/stop.csv')
save(optimization_result_BA, file = paste(output_dir, "\\optimization_result_BA_final", ".Rdata", sep = ""))
write.csv(c(floor(optimization_result_BA[[7]][1]),round(optimization_result_BA[[7]][2],3), floor(optimization_result_BA[[7]][3])), file = paste(output_dir, "\\optimization_result_BA_final", ".csv", sep = ""))
### make BA optimized model

sad <- function(z) {
#col_selection <- c(sample(seq(from = 2, to = ncol(df), by = 1),ncol(df)-1 ) )

rf_multiple <- randomForest(x=df[-test_set_IDs, 2:ncol(df)	],
 y=as.factor(df[-test_set_IDs, 1]),
 xtest=df[test_set_IDs, 2:ncol(df)],
 ytest=as.factor(df[test_set_IDs, 1]),
 ntree=500, mtry = z[1], replace=FALSE, classwt=c(z[2],0.5),
 strata=as.factor(df[, 1]), sampsize=c(z[3],training_actives),
 keep.forest=TRUE, keep.inbag = TRUE, do.trace=50, importance=TRUE, nPerm=1) 

#return(round( (rf_multiple$confusion[6]+rf_multiple$confusion[5])/2,2 ) )
return(rf_multiple)
}

rfmodel_BA <- sad(c(floor(optimization_result_BA[[7]][1]),
round(optimization_result_BA[[7]][2],3), floor(optimization_result_BA[[7]][3])) )

### save optimization model
save(rfmodel_BA, file = paste(output_dir, "\\rfmodel_BA_final", ".Rdata", sep = ""))


