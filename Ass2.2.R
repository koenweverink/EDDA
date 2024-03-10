library(readr)

# load Birthweight.csv
birthweight <- read.csv("C:/Users/koenw/Desktop/Master - Computational Science/Block 4/Experimental Design and Data Analysis/EDDA_Assignments/Birthweight.csv")

# Skip the columns ID, mage35, lowbwt, smoker
birthweight <- birthweight[,-c(1, 6, 15, 16)]
head(birthweight)

# Fit the linear regression model
model <- lm(Birthweight ~ ., data=birthweight)

# Diagnostic plots
# Assuming 'model' is your linear model object
par(mfrow=c(2,2)) # Set up the plotting area for four plots

# Increase label and title sizes
label_size = 2 # Adjust this value as needed
axis_size = 1.5  # Adjust this value as needed
title_size = 2   # Adjust this value as needed
point_size = 2 # Adjust this value as needed for the size of points in the plot

# 1. Residuals vs Fitted
plot(fitted(model), residuals(model),
     xlab="Fitted values", ylab="Residuals",
     main="Residuals vs Fitted",
     cex.lab=label_size, cex.axis=axis_size, cex.main=title_size, cex=point_size)
abline(h=0, col="red") # Add a horizontal line at y=0

# 2. Normal Q-Q
qqnorm(residuals(model),
       xlab="Theoretical Quantiles", ylab="Standardized Residuals",
       main="Normal Q-Q",
       cex.lab=label_size, cex.axis=axis_size, cex.main=title_size)
qqline(residuals(model), col="red") # Add a reference line

# 3. Scale-Location (Spread-Location)
plot(fitted(model), sqrt(abs(residuals(model))),
     xlab="Fitted values", ylab="Sqrt(|Residuals|)",
     main="Scale-Location",
     cex.lab=label_size, cex.axis=axis_size, cex.main=title_size, cex=point_size)

# 4. Residuals vs Leverage
plot(hatvalues(model), residuals(model),
     xlab="Leverage", ylab="Residuals",
     main="Residuals vs Leverage",
     cex.lab=label_size, cex.axis=axis_size, cex.main=title_size, cex=point_size)
abline(h=0, col="red") # Add a horizontal line at y=0

par(mfrow=c(1,1))
cooks_distances <- cooks.distance(model)
plot(cooks_distances, pch=19, cex.main=3, cex.axis=2, cex.lab=2, main="Cook's distances", xlab="Index", ylab="Cook's distance")

### Collinearity diagnostics
library(car)

vif_values <- vif(model)
print(vif_values)

# Check for high VIF values
high_vif <- vif_values[vif_values > 5]
if(length(high_vif) > 0) {
  print("There are variables with high multicollinearity:")
  print(high_vif)
} else {
  print("No significant multicollinearity detected among the variables.")
}

############# B #################

# use the step down method to reduce the amount of explanatory variables
model_reduced <- step(model, direction="backward")


# Diagnostic plots
par(mfrow=c(2,2)) # Set up the plotting area for four plots

# 1. Residuals vs Fitted
plot(fitted(model_reduced), residuals(model_reduced),
     xlab="Fitted values", ylab="Residuals",
     main="Residuals vs Fitted",
     cex.lab=label_size, cex.axis=axis_size, cex.main=title_size, cex=point_size)
abline(h=0, col="red") # Add a horizontal line at y=0

# 2. Normal Q-Q
qqnorm(residuals(model_reduced),
       xlab="Theoretical Quantiles", ylab="Standardized Residuals",
       main="Normal Q-Q",
       cex.lab=label_size, cex.axis=axis_size, cex.main=title_size)
qqline(residuals(model_reduced), col="red") # Add a reference line

# 3. Scale-Location (Spread-Location)
plot(fitted(model_reduced), sqrt(abs(residuals(model_reduced))),
     xlab="Fitted values", ylab="Sqrt(|Residuals|)",
     main="Scale-Location",
     cex.lab=label_size, cex.axis=axis_size, cex.main=title_size, cex=point_size)

# 4. Residuals vs Leverage
plot(hatvalues(model_reduced), residuals(model_reduced),
     xlab="Leverage", ylab="Residuals",
     main="Residuals vs Leverage",
     cex.lab=label_size, cex.axis=axis_size, cex.main=title_size, cex=point_size)
abline(h=0, col="red") # Add a horizontal line at y=0

par(mfrow=c(1,1))
cooks_distances <- cooks.distance(model_reduced)
plot(cooks_distances, pch=19, cex.main=3, cex.axis=2, cex.lab=2, main="Cook's distances", xlab="Index", ylab="Cook's distance")


# Collinearity diagnostics
vif_values_reduced <- vif(model_reduced)
print(vif_values_reduced)

summary(model_reduced)


############# C #################

average_data <- data.frame(
  Length = mean(birthweight$Length, na.rm = TRUE),
  Headcirc = mean(birthweight$Headcirc, na.rm = TRUE),
  Gestation = mean(birthweight$Gestation, na.rm = TRUE),
  mage = mean(birthweight$mage, na.rm = TRUE),
  mppwt = mean(birthweight$mppwt, na.rm = TRUE),
  fheight = mean(birthweight$fheight, na.rm = TRUE)
)

# Then, use these average values to estimate the confidence interval for the expected birthweight
conf_interval <- predict(model_reduced, newdata = average_data, interval = "confidence", level = 0.95)

# conf_interval now contains the lower and upper bounds of the 95% CI
print(conf_interval)

pred_interval <- predict(model_reduced, newdata = average_data, interval = "prediction", level = 0.95)

# pred_interval now contains the lower and upper bounds of the 95% prediction interval
print(pred_interval)



############# D #################

# Apply the LAS method to the first model
install.packages("glmnet")
library(glmnet)

# Remove the response variable from the data
x = as.matrix(birthweight[,-2])

# Only the response variable
y <- as.double(as.matrix(birthweight[,2]))

# train by using 2/3 of the data
train = sample(1:nrow(x), nrow(x)*2/3)

# data for training
x.train=x[train,]; y.train=y[train]

# data for testing
x.test=x[-train,]; y.test=y[-train]

lasso.mod=glmnet(x.train,y.train,alpha=1)
cv.lasso=cv.glmnet(x.train,y.train,alpha=1,type.measure='mse')

par(mfrow=c(1,1), mar=c(5, 5, 10, 2) + 0.1)
plot(lasso.mod, label=T, xvar="lambda", cex.axis=2, cex.lab=1.5)
title(main="Lasso coefficients", line=3.5, cex.main=3)  # Increase 'line' to push title further up

plot(cv.lasso, cex.axis=2, cex.lab=1.5) # the best lambda by cross-validation
title(main="Lambda by cross-validation", line=3.5, cex.main=3)  # Increase 'line' to push title further up

plot(cv.lasso$glmnet.fit,xvar="lambda",label=T, cex.axis=2, cex.lab=1.5)
title(main="Path of Lasso Coefficients as Lambda Varies", line=3.5, cex.main=2.5)  # Increase 'line' to push title further up


lambda.min=cv.lasso$lambda.min; lambda.1se=cv.lasso$lambda.1se
coef(lasso.mod,s=cv.lasso$lambda.min) #beta’s for the best lambda
y.pred=predict(lasso.mod,s=lambda.min,newx=x.test) #predict for test
mse.lasso=mean((y.test-y.pred)^2) #mse for the predicted test rows
print(mse.lasso)
