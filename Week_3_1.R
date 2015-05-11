# TODO: Week_3_1
# 
# Author: fizzafruit
###############################################################################

file <- paste(getwd(), "/extdata/babies.txt", sep="")
file
babies = read.table(file, header = TRUE)

bwt.nonsmoke = babies$bwt[babies$smoke == 0]
bwt.smoke = babies$bwt[babies$smoke == 1]

# check population difference in means
mean(bwt.nonsmoke) - mean(bwt.smoke)
sd(bwt.nonsmoke)
sd(bwt.smoke)

# first way (long method) to compute t-value of non-smoking and smoking mothers

N <- 20
dat.ns = sample(bwt.nonsmoke, N)
dat.s = sample(bwt.smoke, N)

X.ns = mean(dat.ns)
sd.ns = sd(dat.ns)
X.s = mean(dat.s)
sd.s = sd(dat.s)
sd.diff = sqrt(sd.ns^2 / N + sd.s^2 / N)
tval = (X.ns - X.s) / sd.diff
tval

# second way (short method)
t.test(dat.ns, dat.s)$statistic

# compute t-value for the first 30 weights
spl.ns = bwt.nonsmoke[1:30]
spl.s = bwt.smoke[1:30]

# or with this
spl.ns = head(bwt.nonsmoke, 30)
spl.s = head(bwt.smoke, 30)

t.test(spl.ns, spl.s)$statistic

# two sided test
# to compute this probability is to take one minus the area 
# under the standard normal curve between -abs(tval) and abs(tval)
pval = 1 - pnorm(abs(tval)) + pnorm(-abs(tval))
pval

# because of the symmetry of the standard normal distribution
# a simpler way to calculate the probability of t-value under the null
# could have a larger absolute value than tval
# the area outside of -|tval| and |tval| is equal to 2 times the area
# to the left of -|tval|
2 * pnorm(-abs(tval))

