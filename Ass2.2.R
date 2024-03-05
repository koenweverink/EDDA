# load Birthweight data
birthweight <- read.csv("C:\\Users\\koenw\\Desktop\\Master - Computational Science\\Block 4\\Experimental Design and Data Analysis\\EDDA_Assignments\\Birthweight.csv")

head(birthweight)

# Fit the linear regression model
model <- lm(Birthweight ~ Length + Headcirc + Gestation + mage + mnocig + mheight + mppwt + fage + fedyrs + fnocig + fheight, data=birthweight)

# Diagnostic plots
par(mfrow=c(2,2)) # Arrange plots in 2x2 grid
plot(model) # This generates the 4 basic diagnostic plots

install.packages("car")
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