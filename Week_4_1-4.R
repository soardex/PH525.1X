# TODO: Week_4_1-4
# 
# Author: fizzafruit
###############################################################################

# load RData file
filenamed <- paste(getwd(), "/extdata/skew.RData", sep="")
filenamed
load(filenamed)

# check the dimesion
dim(dat)

# set up a grid for 3x3 = 9 plots
# create a multifigure grid filled in row-by-row
par(mfrow = c(3,3))


#install and use package "UsingR"
install.packages("UsingR")

library(UsingR)
data(father.son)
plot(father.son$fheight, father.son$sheight)

# calculate the correlation between these two vectors
cor(father.son$fheight, father.son$sheight)

# get the mouse pointer value
identify(father.son$fheight, father.son$sheight)

x = father.son$fheight
y = father.son$sheight
n = nrow(father.son)

# plot scaled x and y
plot(scale(x), scale(y))

# add horizontal and vertical lines
abline(h=0, v=0)

# get average of scaled x times scaled y
mean(scale(x) * scale(y))

# load data
library(UsingR)
data(nym.2002)

# get the fastest time divided by median time
time = sort(nym.2002$time)
min(time / median(time))

# install and use dplyr
install.packages("dplyr")
library(dplyr)

# if the file doesn't exist then download
filename <- paste(getwd(), "/extdata/msleep_ggplot2.csv", sep="")
filename

install.packages("downloader")
library(downloader)
url <- "https://raw.githubusercontent.com/genomicsclass/dagdata/master/inst/extdata/msleep_ggplot2.csv"
if (!file.exists(filename)) download(url, filename)

# read the csv
msleep <- read.csv(filename)

# usage of pipe operator
# example code of before using pipe operator
# e.g. head(select(msleep, name, sleep_total))
msleep %>%
	select(name, sleep_total) %>%
	head

# add column of the proportion of REM sleep to total sleep
# group the animals by taxonomic order
# summarize by the median REM proportion
# arrange by the median REM proportion
# get the head
msleep %>%
	mutate(rem_proportion = sleep_rem / sleep_total) %>%
	group_by(order) %>%
	summarize(median_rem_proportion = median(rem_proportion)) %>%
	arrange(median_rem_proportion) %>%
	head

# load dataset that is built in R
data(ChickWeight)

# plot the data
plot(ChickWeight$Time, ChickWeight$weight, col=ChickWeight$Diet)

# reshape function change the dataset from a long to wide shape
chick = reshape(ChickWeight,idvar=c("Chick","Diet"),timevar="Time",direction="wide")
head(chick)

# omit / remove NA's
chick = na.omit(chick)

# average of weigh of day 4 with an outlier 3000
# then average of it divided average of weigh of day 4
# without outlier
cs = c(chick$weight.4, 3000)
mean(cs) / mean(chick$weight.4)

# getting the median
median(cs) / median(chick$weight.4)

# getting the standard deviation
sd(cs) / sd(chick$weight.4)

# add outlier on day 21
cs21 = c(chick$weight.21, 3000)

# calculate the correlation of day 4 and day 21
cor(cs, cs21) / cor(chick$weight.4, chick$weight.21)

# display using stripchart
stripchart(chick$weight.4 ~ chick$Diet, method="jitter", vertical=TRUE)

# get chicks on day 4 with diet 1 and 4
x <- chick$weight.4[chick$Diet == 1]
y <- chick$weight.4[chick$Diet == 4]

# perform t test
t.test(x, y)

# perform wilcox test
wilcox.test(x, y)

# do a t test with addition value of 200 in x
xa <- c(x, 200)
t.test(xa, y)$p.value

# difference between t-test statistic
t.test(x, y + 10)$statistic - t.test(x, y + 100)$statistic
