###################################################################
### When does the order of the factors in AN(C)OVA model matter? ## 
###################################################################

### the order of factors does not matter in ANOVA with balanced design
y=rnorm(8) # some fictive response variable
f1=factor(c(1,1,1,1,2,2,2,2)) # factor f1 
f2=factor(c(1,2,3,4,1,2,3,4)) # factor f2
mod1=lm(y~f1+f2)
mod2=lm(y~f2+f1) # change the order of factors in the model formula
anova(mod1) # balanced design for anova
anova(mod2) # order of factors doesn't matter

### the order of factors in ANOVA matters when design is unbalanced
f1=factor(c(1,1,1,1,2,2,2,2)) # factor f1 
f2=factor(c(1,2,3,1,2,3,1,2)) # new factor f2, leading to unbalanced design
# 6 cells, 8 observations, cells (1,1) and (2,2) contain 2 observations, the rest 1
mod3=lm(y~f1+f2) # the same MeanSq for f1 as in anova(lm(y~f1)) 
mod4=lm(y~f2+f1) # unbalanced additive anova
anova(mod3) # the p-value for f2 (second factor in the formula) is correct
anova(mod4) # the p-value for f1 *second factor in the formula) is correct
## we see that the order of factors in unbalance anova matters
# for mod3: the first line f1 is test omega1(f1) within Omega(f1+f2) 
# the second line f2 is the test omega2(f2\f1) within Omega(f1+f2)
# the second line f2 is the right test for f2, with f1 taken into account 
# for unbalanced designs omega2(f2\f1) is not the same as omega2(f2)
# the best way to perform the test for f2 is 
modf1=lm(y~f1); anova(modf1,mod3) # or anova(modf1,mod4) # the same

### the order of factors in ANCOVA matters 
x3=rnorm(8,0.2,0.8) # create a continuous (pseudo)-variable 
mod1a=lm(y~f1+f2+x3) # x3 is last when testing for the relevance of x3
mod2a=lm(y~x3+f1+f2) # f2 is last when testing for the relevance of f2
# the p-values for f1, f2 and x3 are different for mod1a and mod2a
anova(mod1a) # although the design is balanced
anova(mod2a) # the order of factors matters in ancova
summary(mod1a) # gives the correct p-value for the continuous variable x3
summary(mod2a) # gives the correct p-value for the continuous variable x3

# the same relevant p-values can be derived by testing submodels within 
# the full model. The relevant p-value for x3  
mod1b=lm(y~f1+f2) # submodel (without x3) of the model mod1a=lm(y~f1+f2+x3)
anova(mod1b,mod1a) # relevant p-value for x3, anova(mod1b,mod2a) gives the same
# the relevant p-value for f2  
mod2b=lm(y~f1+x3) # create submodel without f2 inside mod2a=lm(y~x3+f1+f2)
anova(mod2b,mod2a) # relevant p-value for f2, anova(mod2b,mod1a) gives the same
# to get all these relevant p-values at once, just run 
drop1(mod1a,test="F") # this works for all the models anova,ancova

mod1c=lm(y~f1*x3); anova(mod1c)
drop1(mod1c,test="F") # only interaction p-value in model with interaction 

## Conclusions:
## order of factors does not matter in anova with balanced design
## order of factors in anova matters when design is unbalanced
## order of factors in ancova matters 
## for continuous variables p-values from summary are also usable
## drop1 gives only p-value for interaction term for models with interaction


#######################################################################
##### Lasso for the data set mtcars (show this one at the lecture) #####
########################################################################

mtcars # dataset mtcars: mpg is the response
x=as.matrix(mtcars[,-1])
y=mtcars[,1]

train=sample(1:nrow(x),0.67*nrow(x)) # train by using 2/3 of the x rows 
x.train=x[train,]; y.train=y[train]  # data to train
x.test=x[-train,]; y.test = y[-train] # data to test the prediction quality

# Prediction by using the linear model
# first fit linear model on the train data
lm.model=lm(mpg~cyl+disp+hp+drat+wt+qsec+vs+am+gear+carb,data=mtcars,subset=train)
y.predict.lm=predict(lm.model,newdata=mtcars[-train,]) # predict for the test rows
mse.lm=mean((y.test-y.predict.lm)^2); mse.lm # prediction quality by the linear model

# Now apply lasso for selecting the variables and prediction 
library(glmnet) 
lasso.model=glmnet(x.train,y.train,alpha=1) # alpha=1 for lasso
#more options: standardize=TRUE, intercept=FALSE,nlambda=1000
lasso.cv=cv.glmnet(x.train,y.train,alpha=1,type.measure="mse",nfolds=5)
# option nfolds=5 means 5-fold cross validation. By default, the method 
# performs 10-fold cross validation to choose the best lambda.
# plots
plot(lasso.model,label=T,xvar="lambda") #standardize=T,type.coef="2norm",xvar="norm") "coef"
#plot(lasso.cv$glmnet.fit,xvar="lambda",label=T) # the same plot
plot(lasso.cv) 
# With label="T" in plot commando you see which curve corresponds 
# to which coefficients. The glmnet plot above shows the shrinkage of 
# the lasso coefficients as you move from the right to the left, 
# but unfortunately, it is not clearly labelled. 
# Lasso contrasts with ridge regression, which flattens out 
# everything, but does not zero out any of the regression coefficients.

lambda.min=lasso.cv$lambda.min; lambda.1se=lasso.cv$lambda.1se; 
lambda.min; lambda.1se # best lambda by cross validation
coef(lasso.model,s=lasso.cv$lambda.min) # cyl,hp,wt,am and carb are relevant
coef(lasso.model,s=lasso.cv$lambda.1se) # only cyl,hp and wt are releveant

# lambda.min is the value of lambda that gives minimum mean cross-validated 
# error. The other lambda saved is lambda.1se, which gives the most regularized 
# (reduced) model such that error is within one standard error of the minimum. 

lasso.pred1=predict(lasso.model,s=lambda.min,newx=x.test) 
lasso.pred2=predict(lasso.model,s=lambda.1se,newx=as.matrix(x.test))
mse1.lasso=mean((y.test-lasso.pred1)^2); mse1.lasso
mse2.lasso=mean((y.test-lasso.pred2)^2); mse2.lasso

# By default, the glmnet function standardizes all the independent 
# variables, but here the dependent variable can also be standardized 
# by the function standardize=function(x){(x-mean(x))/sd(x)}).
# Then one may want tomomit the intercept in the lm and glmnet models, 
# because all the variables have already been standardized to a zero mean. 
############## end of the example mtcars #############################

