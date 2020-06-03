load("chao.RData")
library(varbvs)
library(glmnet)
load("PCA.RData")

vbvs = varbvs(X=newX,y=newY,family='gaussian',Z=new_Z)

rmse = apply(vbvs$residuals ^ 2,MARGIN=2, function(x) sqrt(mean(x)))

plot(vbvs$logodds, rmse, type='b',col='black',xlab="logodds",
     ylab="rmse")

LB = vbvs$mu - 1.96 * sqrt(vbvs$s)
UB = vbvs$mu + 1.96 * sqrt(vbvs$s)
non_zero = (UB < 0) | (LB > 0)
non_zero_count = colSums(non_zero)

plot(vbvs$logodds, rev(non_zero_count), type='b',col='black',xlab="logodds",
     ylab="non-zero variables")

plot(vbvs$logodds, colMeans(vbvs$alpha), type='b',col='black',xlab="logodds",
     ylab=expression(posterior ~  ~ probability ~ of ~ pi))





fit.glmnet <- cv.glmnet(x=newX,y=newY, family = "gaussian", alpha = 1, 
                        lambda = 10^(seq(0, -2, -0.05)),nfolds = 20)


plot(fit.glmnet$lambda, fit.glmnet$cvm, xlab="lambda",ylab="cross-validation error",type='l',col='blue',ylim=c(0.95,1.3))
lines(fit.glmnet$lambda, fit.glmnet$cvup)
lines(fit.glmnet$lambda, fit.glmnet$cvlo)


plot(fit.glmnet$lambda, fit.glmnet$nzero,xlab="lambda",ylab="non-zero variabls",type='l',col='blue')

