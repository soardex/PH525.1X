# TODO: Week_2_2
# 
# Author: fizzafruit
###############################################################################


# extract the lifeExp from year 1952
gs = split(gapminder$lifeExp, gapminder$year)
x <- gs[["1952"]]

# alternative extraction
dat1952	= gapminder[gapminder$year == 1952,]
x = dat1952$lifeExp

# get the proportion of countries in 1952 that have life
# expectancy less than or equal to 40
mean(x <= 40)

# both are same get lifeExp between 40 to 60 years
mean(x <= 60) - mean(x <= 40)
mean(x > 40 & x < 60)

# create custom function
prop = function(q) {
	mean(x <= q)
}
prop(40)

# build a range q's min..max length is 20
qs = seq(from = min(x), to = max(x), length = 20)
qs

# apply to the vector the function prop
props = sapply(qs, prop)

# plot the qs against props
plot(qs, props)

# one liner without defining function name
# called inline or anonymous function
props = sapply(qs, function(q) mean(x <= q))

# plot with ecdf pre-built function
plot(ecdf(x))

# get population from year 1952
datPop1952 = gapminder[gapminder$year == 1952,]
pop = datPop1952$pop

# get standard deviation of log10 population
logpop = log10(pop)
sd(logpop)

# population standard deviation
sqrt(mean((logpop - mean(logpop)) ^ 2))

# q-q plot comparing to the quantiles of a normal distribution
x <- logpop
qqnorm(x)

# standardize the x vector
z = (x - mean(x)) / sd(x)
z

# plot the z
qqnorm(z)

# get the largest value in vector
max(z)

# create an identity line or diagonal line
abline(0, 1)

# get the Normal distribution cumulative density function
F = function(q) pnorm(q, mean=mean(x), sd=sd(x))
# the function relies on knowing the fact that 1 million = 10^6
# which means if I input 7 it would be 10^7 or 10 million
F(6)

# get the length of vector
n = length(x)

# get the proportion from the normal approximation times
# the total number of countries or vector size
(F(7) - F(6)) * n

# get the quantiles of the standard normal distribution
ps = ((1:n) - 0.5) / n
ps

# re-creating or same plot as qqnorm
plot(qnorm(ps), sort(x))

# get the quantile of the standard normal distribution which matches to
# the smallest number in x (which is sorted sort(x))
min(qnorm(ps))
qnorm(ps[1])