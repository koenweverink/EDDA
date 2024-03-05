library(broom)
library(dplyr)
library(car)
library(ggplot2)
################ PART A ############

# Define the number of fish
num_fish <- 80

# Define the levels for rate and method
rate_levels <- c(1, 2, 3, 4)
method_levels <- c("A", "B")

# Create a data frame to hold the combinations of rate and method
combinations <- expand.grid(Rate=rate_levels, Method=method_levels)

# Repeat each combination 10 times to distribute the 80 fish evenly
assignments <- combinations[rep(1:nrow(combinations), each = num_fish / length(combinations)), ]

# Randomize the order to ensure a random distribution
set.seed(123) # Setting a seed for reproducibility
assignments <- assignments[sample(1:nrow(assignments)), ]

# Add an ID for each fish
assignments$FishID <- 1:num_fish

# View the first few rows of the randomized assignments
head(assignments)

################ PART B ############

# Read the data from a text file (adjust the path as necessary)
data <- read.table("~/Downloads/hemoglobin.txt", header = TRUE, sep = "")

# Perform two-way ANOVA
model <- aov(hemoglobin ~ rate * method, data = data)

# Use broom to tidy the model, then dplyr to round the numeric columns
summary(model)

################ Check assumptions of ANOVA ############
# 1. Homogeneity of variances
plot(model, which = 1)

# 2. Normality of residuals
plot(model, which = 2)

# Use boxplots to visually check for outliers in each group
ggplot(data, aes(x = interaction(rate, method), y = hemoglobin)) +
  geom_boxplot() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  labs(x = 'Rate and Method Combination', y = 'Hemoglobin', title = "Boxplots for Combination of Rate and Method")

################ PART C ############
# Calculate mean hemoglobin for each combination of rate and method
mean_hemo_by_combination <- aggregate(hemoglobin ~ rate + method, data, mean)

# Find the combination with the highest mean hemoglobin
highest_hemo_combination <- mean_hemo_by_combination[which.max(mean_hemo_by_combination$hemoglobin),]
print(highest_hemo_combination)

# Calculate mean hemoglobin for rate 3 using method A
mean_hemo_rate3_methodA <- mean(data$hemoglobin[data$rate == 3 & data$method == "A"])
print(mean_hemo_rate3_methodA)

# Calculate mean hemoglobin for each rate
mean_hemo_by_rate <- aggregate(hemoglobin ~ rate, data, mean)
# Find the rate with the highest mean hemoglobin
highest_hemo_rate <- mean_hemo_by_rate[which.max(mean_hemo_by_rate$hemoglobin),]
print(highest_hemo_rate)

# Calculate estimated mean hemoglobin value for each rate
mean_hemoglobin_by_rate <- aggregate(hemoglobin ~ rate, data, mean)
# Print the mean hemoglobin values
print(mean_hemoglobin_by_rate)

################ PART D ############
# Perform one-way ANOVA
fit_one_way <- aov(hemoglobin ~ rate, data = data)
summary(fit_one_way)

# Calculate estimated mean hemoglobin value for each rate
mean_hemoglobin_by_rate <- aggregate(hemoglobin ~ rate, data, mean)
# Print the mean hemoglobin values
print(mean_hemoglobin_by_rate)

################ PART E ############
kruskal.test(hemoglobin ~ rate, data = data)
