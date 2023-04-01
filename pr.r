library(proportion)
ciAllx(778,1497,0.05)
library(binom)
binom.confint(778, 1497, conf.level=0.95, method="asymptotic") # Wald CI
zCI<- function(x, conf.level=0.95){
  # x: vector of the sample values
  mean(x)+c(-1,1)*qnorm((1+conf.level)/2)*sqrt(var(x)/length(x)) }

UN <- read.table("http://stat4ds.rwth-aachen.de/data/UN.dat", header=TRUE) # We revisit the UN data file
names(UN)

t.test(UN$GDP, conf.level=0.95)$conf.int


#### Linear regression model ####

Races<-read.table("http://stat4ds.rwth-aachen.de/data/ScotsRaces.dat", header=TRUE)
head(Races,3) # timeM for men, timeW for women
matrix(cbind(mean(Races$timeW),sd(Races$timeW),mean(Races$climb),sd(Races$climb),mean(Races$distance), sd(Races$distance)),nrow=2)
pairs(~timeW + climb + distance, data = Races)
fit <- lm(timeW~distance, data = Races)  ### Linear model where we are plotting timeW with respect to distance
summary(fit)
plot(fit)
cor(Races[,c("timeW", "distance", "climb")])
cor(cbind(Races[-41,]$timeW, Races[-41,]$distance, Races[-41,]$climb))

fit_dc <- lm(timeW~distance + climb, data = Races)
summary(fit_dc)
plot(fit_dc)
summary(lm(timeW~distance + climb+distance:climb,data=Races))

Florida<-read.table("http://stat4ds.rwth-aachen.de/data/Florida.dat", header=TRUE)
head(Florida,2)
summary(lm(Crime~HS, data = Florida))
summary(lm(Crime~HS + Urban, data = Florida))
cor(Florida$HS, Florida$Urban)
cor(Florida$Crime, Florida$Urban)

res <- residuals(fit_dc)
fitval <- fitted(fit_dc)
leverage <- hatvalues(fit_dc)
cooks <- cooks.distance(fit_dc)

tail(sort(cooks), 3)
out<-cbind(Races$race, Races$timeW, fitval, res, leverage, cooks, rank(cooks))
out[c(1,41,68),] # print output for the 1st, 41th and 68th observations
# **largest Cook's distance = 9.07 has rank 68 of 68 observations

fit.dc2<-lm(timeW~distance + climb,data=Races[-41,])# re-fit without observ.41
summary(fit.dc2)
cor(Races[-41,]$timeW,fitted(fit.dc2)) # multiple correlation
(sigma(fit.dc2))^2
(sd(Races[-41,]$timeW))^2


##### testing hypothesis
prop.test(524, 1008, p = 0.5, alt = "two.sided", conf.level = 0.95, correct = FALSE)

Polid<-read.table("http://stat4ds.rwth-aachen.de/data/Polid.dat", header=TRUE)
t.test(Polid$ideology[Polid$race=="hispanic"], mu=4.0, alt="two.sided")


###### Estimation i.e. calculation the maximum log-likelihood for the normal distribution 
norm.data <- rnorm(100,1,2)

normal.lik<-function(theta,y){
  mu <- theta[1]
  sigma2 <- theta[2]
  n <- length(y)
  logl<- -n/2*log(2*pi) -n/2*log(sigma2) - (2*sigma2)^(-1)*sum((y-mu)^2)
  return(-logl)
}

optim(c(0,1),normal.lik,y=norm.data,method="BFGS")


###### Calculating the confidence interval for the following:
library(proportion)
ciAllx(778,1497,0.05)
library(binom)
binom.confint(778, 1497, conf.level=0.95, method="asymptotic") # Wald CI
binom.coverage(0.5, 1500, conf.level = 0.95, method = "asymptotic")
binom.confint(778, 1497, conf.level=0.95, method="wilson") # Score CI
binom.coverage(0.5, 1500, conf.level = 0.95, method = "wilson")
binom.coverage(0.25, 20, conf.level=0.95, method="asymptotic")
binom.coverage(0.25, 20, .level=0.95, method="wilson")
binom.sim(M=1000, n=20, p=0.25, conf.level=0.95, methods="asymptotic")
binom.sim(M=1000, n=20, p=0.25, conf.level=0.95, methods="wilson")

#### Does prayer help coronary surgery patients
prop.test(c(315,304),c(604,597),conf.level=0.95, correct=FALSE)

##### t.test
Anorexia <- read.table("http://stat4ds.rwth-aachen.de/data/Anorexia.dat", header=TRUE)
head(Anorexia,3) # first3 lines in Anorexiadatafile
change<-Anorexia$after - Anorexia$before
summary(change[Anorexia$therapy=="cb"]) # cb is cognitive behavioral therapy
sd(change[Anorexia$therapy=="cb"]) # standard deviation of weight changes
hist(change[Anorexia$therapy=="cb"]) # histogram of weight changevalues
t.test(change[Anorexia$therapy=="cb"],conf.level=0.95)$conf.int
t.test(change[Anorexia$therapy=="cb"],conf.level=0.99)$conf.int


Anorexia<-read.table("http://stat4ds.rwth-aachen.de/data/Anorexia.dat", header=TRUE)
change<-Anorexia$after-Anorexia$before


#### TO find the mean, standard deviation and time series analysis
library(MCMCpack)
fit<-MCMCregress(change[Anorexia$therapy=="cb"]~1,mcmc=5000000,
                 b0=0,B0=10^{-15},c0=10^{-15},d0=10^{-15})
# mean has normal prior dist. with mean b0=0, variance 1/B0 (std.dev. > 31 million)
# variance has inverse gamma prior distribution (c0/2=shape, d0/2=scale)
summary(fit)


####### Highest density intervals
library(HDInterval)

pi1<-rbeta(10000000,12.0,1.0) # posterior for 11 successes, 0 failures, uniform prior
pi2<-rbeta(10000000,1.0,2.0) # posterior for 0 success, 1 failure, uniform prior
hdi(pi1-pi2,credMass=0.95)
hist(pi1-pi2) # posterior (not shown) is highly skewed to left
plot(density(pi1-pi2)) # density approximation


####### Hypothesis testing
prop.test(524,1008,p=0.50,alt="two.sided",conf.level=0.95, correct=FALSE)

Polid<-read.table("http://stat4ds.rwth-aachen.de/data/Polid.dat", header=TRUE)
t.test(Polid$ideology[Polid$race=="hispanic"], mu=4.0, alt="two.sided")
