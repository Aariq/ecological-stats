#the dbinom function outputs the probability of X successes in n trials if each success has probability of success p
#dbinom(x, n, p, log = F), use log = T if you want logged probabilities
barplot(dbinom(0:20, 4, 0.1), xlab = "# events", ylab = "probability", main = "", names.arg = 0:20)

barplot(dbinom(0:20, 20, 0.1), xlab = "# events", ylab = "probability", main = "", names.arg = 0:20)

barplot(dbinom(0:20, 20, 0.6), xlab = "# events", ylab = "probability", main = "", names.arg = 0:20)

barplot(dbinom(0:20, 4, 0.6), xlab = "# events", ylab = "probability", main = "", names.arg = 0:20)


###########################
#We can use the binomial distribution to calculate the support for a given model, e.g., the likelihood of values of p, given values of N & k...
#Recall: L(H|R) is proportional to P(R|H)
#likelihood profiles for p
vals<-seq(0,1,0.01) #sequence of possible values for p
logLiks<-log(2*vals*(1-vals)) #log(2p(1-p))

plot(vals,logLiks,xlab="value of p",ylab = "log-Likelihood",type="l")

#we can also use the dbinom function to get the likelihood of different values of p for some number of events in N trials
logLiks.2<-dbinom(1,2,vals,log=T) #1 success in 2 trials, with probability of success p = vals
points(vals,logLiks.2,col="red")


########################
#Obtain the probability of orchid data given a binomial model using the function dbinom(x, size, prob, log)
#x = # events, k
#size = # trials, N
#prob = probability of an event in a single trial
#log = T or F
calypso = read.csv("/Users/leonebrown/Dropbox/2019_BIO133/Ecological Statistics and Data/data/orchid.seedlings.csv", header = T)
head(calypso) #number of seedlings found in year 0 as a function of the number of seedlings found 1, 2, 3, and 4 years prior. 

#seedlings in year0 is our x, or number of events
#our size, N, depends on how many seeds we think we started out with, depending on the lag (of 1, 2, 3, or 4 years)

# we can use the binomial distribution here to calculate the probability of the data, given an arbitrary model
# here our "model" has two parts: (1) our hypothesis about the lag, i.e., how many years (2) our binomial probability of seeds per seedling

# Take the case where the lag is 4, and prob = 0.05
#dbinom(x, size, prob, log=F)
ln.pdat.m1 = dbinom(x = calypso$seedlings.0, size = calypso$seeds.m4, prob = 0.05, log = TRUE)
ln.pdat.m1 # show the list of probabilities for each observation, given the model
sum(ln.pdat.m1) # log-likelihood of the whole data set

# compare to a lag of 4 and seeds per seedling of 0.04
ln.pdat.m2 = dbinom(x = calypso$seedlings.0, size = calypso$seeds.m4, prob = 0.04, log = TRUE)
ln.pdat.m2 # show the list of probabilities of each observation, given the model
sum(ln.pdat.m2) # log-likelihood of the whole data set

# compare to a lag of 3 and seeds per seedling of 0.05
ln.pdat.m3 = dbinom(x = calypso$seedlings.0, size = calypso$seeds.m3, prob = 0.05, log = TRUE)
ln.pdat.m3 # show the list of probabilities of each observation, given the model
sum(ln.pdat.m3) # log-likelihood of the whole data set

# this is inefficient but you will learn about writing loops in R on Monday



