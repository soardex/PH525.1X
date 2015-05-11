# TODO: Week_1
# 
# Author: fizzafruit
###############################################################################

file <- paste(getwd(), "/extdata/msleep_ggplot2.csv", sep="")
file
tab = read.csv(file)

# check the class of the data
class(tab)

# what does the top of the dataframe look like
head(tab)

# get the dimensions of the dataframe
dim(tab)

# extract the column names
colnames(tab)

# get the value of the column of the first data
tab$sleep_total[1]

# two ways to get quartile
summary(tab$sleep_total)
quantile(tab$sleep_total)

# subsetting data showing only first 2 rows
tab[c(1,2),]

# subsetting data showing only total sleep greater than 18 
tab[tab$sleep_total > 18,]

# get the mean of animals with total sleep greater than 18
x <- tab$sleep_total[c(tab$sleep_total > 18)]
mean(x)

# function gives us the numeric index that satisfies a logical question
which(tab$sleep_total > 18)

# access and print the value of index 1 result of which
tab$sleep_total[which(tab$sleep_total > 18)[1]]

# combine two logical vectors
which(tab$sleep_total > 18 & tab$sleep_rem < 3)

# sort the column vectors
sort(tab$sleep_total)

# extract the index of the sorted vector
order(tab$sleep_total)

# same as sort as to explain how order works
tab$sleep_total[order(tab$sleep_total)]

# check their rank
# ties are resolved by giving the numbers the average of their ranks
rank(c(1,2,2,3))

# sample of rank
rank(tab$sleep_total)

# find index that match the vector
match(c("Cow", "Owl monkey", "Cheetah"), tab$name)

# get the values after checking match
idx = match(c("Cow", "Owl monkey", "Cheetah", "Cotton rat"), tab$name)
tab[idx,]

# grouping same and remove duplicate in alphabetical order
vec = c("red","blue","red","green","green","yellow","orange")
fac = factor(vec)
fac
levels(fac)

# redefine the alphabetical order return from factor
fac2 = factor(vec, levels=c("blue", "green", "yellow", "orange", "red"))
fac2
levels(fac2)

# get the total number of group
table(tab$order == "Rodentia")

# split base on order of species
# outputs are turned into list
s = split(tab$sleep_total, tab$order)
s

# list are indexed with double square brackets[[]]
s[[17]]
s[["Rodentia"]]

# using functions on list
mean(s[["Rodentia"]])

# other ways of applying function repeatedly to a vector or list
# lapply returns a list
lapply(s, mean)

# sapply simplify the output
sapply(s, mean)

# shortcut for using split and sapply
tapply(tab$sleep_total, tab$order, mean)

# get standard deviation with tapply and sd
tapply(tab$sleep_total, tab$order, sd)
