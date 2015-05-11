# TODO: Week_2_1
# 
# Author: fizzafruit
###############################################################################

# load file
file <- paste(getwd(), "/extdata/femaleMiceWeights.csv", sep="")
file
dat <- read.csv(file)

# get the second column from the 1..12 values
dat[1:12, 2]

# get the difference of the mean of 13..24 index against mean of 1:12
mean(dat[13:24, 2]) - mean(dat[1:12, 2])

# make a strip chart of weight
s = split(dat[,2], dat[,1])
stripchart(s, vertical = TRUE, col = 1:2)

# add means to the plot
abline(h = sapply(s, mean), col = 1:2)

# get the sum of hf weigh that is less than mean of chow
d = split(dat$Bodyweight, dat$Diet)
sum(d[["hf"]] < mean(d[["chow"]]))

# get the sum of chow weigh that is greater than mean of hf
sum(d[["chow"]] > mean(d[["hf"]]))

# assign the value of list to a vector
highfat = s[["hf"]]
highfat

# choose random 6 values from highfat
sample(highfat, 6)

# allow multiple draws of the same observation
sample(highfat, 6, replace = TRUE)

# turns logical vector results into numeric values
# TRUE turned into 1, FALSE turned into 0
as.numeric(highfat > 30)

# check the proportion of highfat over 30
# both returns the same
mean(highfat > 30)
mean(as.numeric(highfat > 30))

# get new file
file2 <- paste(getwd(), "/extdata/femaleControlsPopulation.csv", sep="")
file2
population = read.csv(file2)

# extract the first column vector
population = population[,1]

# get the mean of population
mean(population)

# get the mean of sample population
mean(sample(population, 12))

# replicate the mean using for loop and store on a vector
# lastly print the head of vector
sampleMean = replicate(10000, mean(sample(population, 12)))
head(sampleMean)

# plot the sampleMean
plot(sampleMean)

# sample of null distribution
# plot and print head of the difference of between two random samples
null = replicate(10000, mean(sample(population, 12)) - mean(sample(population, 12)))
head(null)
plot(null)

# create the histogram
hist(null)

# add the diff to the histogram
diff = mean(dat[13:24, 2]) - mean(dat[1:12, 2])
abline(v = diff, col = "red")

# add the negative of the diff to the histogram
abline(v = -diff, col = "red")

# get one-tailed probability of seeing as big a difference
mean(null > abs(diff))

# get two-tailed probability of seeing as big a difference
mean(abs(null) > abs(diff))
