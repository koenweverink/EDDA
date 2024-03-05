#Load Ice_cream_.csv
Ice_cream <- read.csv("C:\\Users\\koenw\\Desktop\\Master - Computational Science\\Block 4\\Experimental Design and Data Analysis\\EDDA_Assignments\\Ice_cream.csv")

#print the first 6 rows of the data
head(Ice_cream)

#####################################   A   #####################################
#Get the mean of video column
mu <- mean(Ice_cream$video)
print(mu)

hist(Ice_cream$video, main="Histogram of video column", xlab="Video", col="lightblue", border="black", 
     cex.lab=1.5,  # Enlarges the x and y labels
     cex.main=2.5,   # Enlarges the main title
     cex.axis=1.7) # Enlarges the axis tick labels


#Plot a boxplot of the video column
boxplot(Ice_cream$video, main="Boxplot of video column", xlab="Video", col="lightblue", border="black",
    cex.lab=2,  # Enlarges the x and y labels
    cex.main=2.5,   # Enlarges the main title
    cex.axis=1.7) # Enlarges the axis tick labels

#Check if sample is normally distributed
shapiro.test(Ice_cream$video)

#Check if sameple is normally distributed using a qqplot and include a line to show the normal distribution
qqnorm(Ice_cream$video,
       main="Normal Q-Q Plot of video column",
       xlab="Theoretical Quantiles",
       ylab="Sample Quantiles",
       pch=19,
       cex.lab=1.5,  # Enlarges the x and y labels
       cex.main=2.5,   # Enlarges the main title
       cex.axis=1.7) # Enlarges the axis tick labels
qqline(Ice_cream$video)

#Construct a 97% confidence interval for the mean of the video column
CI <- t.test(Ice_cream$video, conf.level = 0.97)
print(CI)

#Calculate the sample size needed to provide that the length of the 97%-CI is at most 3
alpha <- 0.03
z_score <- qnorm(1 - alpha/2)
error <- 3

min_sample_size <- ((z_score)^2 * sd(Ice_cream$video)^2) / ((error/2)^2)
# rounding
min_sample_size_round_up <- ceiling(min_sample_size)
print(min_sample_size_round_up)

#Compute a bootstrap 97%-CI for the mean of the video column
library(boot)
boot_mean <- function(data, indices) {
  #Calculate mean of the sampled indices
  return(mean(data[indices]))
}
bootstrap_results <- boot(data=Ice_cream$video, statistic=boot_mean, R=1000)

bootstrap_ci <- boot.ci(bootstrap_results, type="perc", conf=0.97)
print(bootstrap_ci)

#print the ttest CI and the bootstrap CI to compare them 
print(CI)
print(bootstrap_ci$percent[4:5])


#####################################   B   #####################################

#Perform a one-sided t-test to test the hypothesis that the mean of the video column is more than 50
ttest <- t.test(Ice_cream$video, alternative = "greater", mu = 50)
print(ttest)

#Perform a one-sided t-test to test the hypothesis that the mean of the video column is more than 51
ttest <- t.test(Ice_cream$video, alternative = "greater", mu = 51)
print(ttest)


#####################################   C   #####################################
#check if the median of the video column is bigger than 50 by using a sign test
sign_test <- binom.test(sum(Ice_cream$video > 50), length(Ice_cream$video), p=0.5, alternative="greater")
print(sign_test)

#check if the median of the video column is bigger than 50 by using a rank sum test
wilcox_test <- wilcox.test(Ice_cream$video, mu=50, alternative="greater")
print(wilcox_test)


# Count the number of scores less than 42
scores_less_than_42 <- sum(Ice_cream$video < 42)

# Total number of scores
total_scores <- length(Ice_cream$video)

# Testing if the fraction of scores less than 42 is at most 25%
binom_test_result <- binom.test(scores_less_than_42, total_scores, p = 0.25, alternative = "less")

print(binom_test_result)

#####################################   D   #####################################
n_bootstrap = 1000

# Store bootstrap means
bootstrap_mins <- numeric(n_bootstrap)

# Bootstrap
set.seed(123)
for(i in 1:n_bootstrap) {
  sample_data <- sample(Ice_cream$video, replace = TRUE, size = length(Ice_cream$video))
  bootstrap_mins[i] <- min(sample_data)
}

# Calculate the 95% confidence interval for the mean
conf_interval <- quantile(bootstrap_mins, probs = c(0.025, 0.975))

print(conf_interval)

#####################################   E   #####################################
#extract the female and video column from the data
retval <- Ice_cream[, c(2,4)]

female <- subset(retval, female=='1')
male <- subset(retval, female=='0')

# get the mean of the video column of female
mu_fem <- mean(female$video)
mu_male <- mean(male$video)
print(mu_fem); print(mu_male)

#Perform a two-sample t-test to test the hypothesis that the mean of the video column of female is different from the mean of the video column of male
ttest <- t.test(female$video, male$video, alternative = "less")
print(ttest)

#perform a wilcox rank-sum test to test the hypothesis that the mean of the video column of female is different from the mean of the video column of male
wilcox_test <- wilcox.test(female$video, male$video, alternative="less")
print(wilcox_test)

#perform a kolmogorov-smirnov test to test the hypothesis that the distribution of the video column of female is different from the mean of the video column of male
ks_test <- ks.test(female$video, male$video, alternative="less")
print(ks_test)

var_test = var.test(female$video, male$video, alternative='two.sided')
print(var_test)
#####################################   F   ################################
#extract the video and puzzle column from the data
retval <- Ice_cream[, c(4,5)]
head(retval)

#Check if the video and puzzle column are correlated
correlation <- cor(retval)
print(correlation)

#Perform Pearson's correlation test to test the hypothesis that the video and puzzle column are correlated
cor_test <- cor.test(retval$video, retval$puzzle, method="pearson")
print(cor_test)

#check if score of the video column is higher than the score of the puzzle column
ttest <- t.test(retval$puzzle, retval$video, alternative = "greater")
print(ttest)

#perform a wilcox rank-sum test to test the hypothesis that the score of the video column is higher than the score of the puzzle column
wilcox_test <- wilcox.test(retval$puzzle, retval$video, alternative="greater")
print(wilcox_test)
