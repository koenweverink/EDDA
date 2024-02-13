#Load Ice_cream_.csv
Ice_cream <- read.csv("C:\\Users\\koenw\\Desktop\\Master - Computational Science\\Block 4\\Experimental Design and Data Analysis\\Assignments\\Ice_cream.csv")

#print the first 6 rows of the data
head(Ice_cream)

#####################################   A   #####################################
#Get the mean of video column
mu <- mean(Ice_cream$video)
print(mu)

#Plot a histogram of the video column
hist(Ice_cream$video, main="Histogram of video column", xlab="Video", col="lightblue", border="black")

#Plot a boxplot of the video column
boxplot(Ice_cream$video, main="Boxplot of video column", xlab="Video", col="lightblue", border="black")

#Check if sample is normally distributed
shapiro.test(Ice_cream$video)

#Check if sameple is normally distributed using a qqplot
qqnorm(Ice_cream$video)

#Construct a 97% confidence interval for the mean of the video column
CI <- t.test(Ice_cream$video, conf.level = 0.97)

#Calculate the sample size needed to provide that the length of the 97%-CI is at most 3
n <- (qt(0.97, df = length(Ice_cream$video) - 1) * sd(Ice_cream$video) / 3)^2

#Print the sample size
print(n)

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


#####################################   D   #####################################
