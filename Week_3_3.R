# TODO: Week_3_3
# 
# Author: fizzafruit
###############################################################################

file <- paste(getwd(), "/extdata/babies.txt", sep="")
file
babies = read.table(file, header = TRUE)

bwt.nonsmoke = babies$bwt[babies$smoke == 0]
pop.var = var(bwt.nonsmoke)


# using Monte Carlo simulation and calculating variance
N <- 50
tg <- function(){
	var(sample(bwt.nonsmoke, N))
}

vars = replicate(1000, tg())
hist(vars, breaks = 100)
abline(v = pop.var, col = "blue")
mean(vars > pop.var * 1.5)

# set seed to 0 so we will get the same random sample again
set.seed(0)

# get samples for both variables
smokers <- sample(babies$bwt[babies$smoke == 1], N)
nonsmokers <- sample(babies$bwt[babies$smoke == 0], N)

# get the observe difference
obs <- mean(smokers) - mean(nonsmokers)

# create 1000 replicated permutations to generate a null distribution
# joined the two groups, shuffled the samples, and took 50 in one group
# and 50 on the second group
avgdiff <- replicate(1000, {
	all <- sample(c(smokers,nonsmokers))
	smokersstar <- all[1:N]
	nonsmokersstar <- all[(N+1):(2*N)]
	return(mean(smokersstar) - mean(nonsmokersstar))
})

# calculate a probability of seeing such a large different between means
# under the null hypothesis
mean(abs(avgdiff) > abs(obs))

# same as above but for medians
obs <- median(smokers) - median(nonsmokers)
avgdiff <- replicate(1000, {
	all <- sample(c(smokers,nonsmokers))
	smokersstar <- all[1:N]
	nonsmokersstar <- all[(N+1):(2*N)]
	return(median(smokersstar) - median(nonsmokersstar))
})
mean(abs(avgdiff) > abs(obs))
