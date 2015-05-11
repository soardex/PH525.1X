# TODO: Week_3_2
# 
# Author: fizzafruit
###############################################################################

file <- paste(getwd(), "/extdata/babies.txt", sep="")
file
babies = read.table(file, header = TRUE)

bwt.nonsmoke = babies$bwt[babies$smoke == 0]
bwt.smoke = babies$bwt[babies$smoke == 1]

# t-test for difference between means
# get the p-value and confidence interval
mytest <- t.test(bwt.nonsmoke, bwt.smoke)
mytest$p.value
mytest$conf.int

# compute the average length of the confidence interval
CIs = replicate(1000, t.test(sample(bwt.nonsmoke, 30), sample(bwt.smoke, 30))$conf.int)
mean(CIs[2,] - CIs[1,])

# get population level difference
popdiff = mean(bwt.nonsmoke) - mean(bwt.smoke)

# proportion of times lower bound of the confidence interval
# less than popdiff and the upper bound greater than
# popdiff
mean(CIs[1,] < popdiff & CIs[2,] > popdiff)

# power calculations
N <- 15
alphas <- c(0.1, 0.05, 0.01)
B <- 1000

reject <- function(N, alpha = 0.05) {
	ns <- sample(bwt.nonsmoke, N)
	s <- sample(bwt.smoke, N)
	pval <- t.test(ns, s)$p.value
	pval < alpha
}

power <- sapply(alphas, function(alpha) {
	rejections <- replicate(B, reject(N, alpha=alpha))
	mean(rejections)
})
power

# plot the power calculations
plot(log10(alphas), power, xlab="log10 (base 10) alpha", type="b")


file2 <- paste(getwd(), "/extdata/assoctest.csv", sep="")
file2
d = read.csv(file2, header = TRUE)

# create a table and test chi-square
tab = table(d)
chisq.test(tab)

# compute fisher test
fisher.test(tab)