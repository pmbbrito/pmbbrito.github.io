# Paulo  Brito  29.4.2017
# Growth cost of volatility for Portugal

# if you run the script more than once you may have to clean up the environment
rm(list = ls())

# upload libraries
library(pwt9)
data(pwt9.0)
attach(pwt9.0)
library(tseries)
library(zoo)


# get series for year, population and real GDP for Portugal 1950:2014 
year<-year[country=="Portugal"]
pop=pop[country=="Portugal"]
rgdp=rgdpna[country=="Portugal"]

# real GDP per person
rgdp.per.person<-rgdp/pop

# Create a time series of real.gdp.per.person in Portugal starting at 1970:
rgdp.pc <- window(ts(rgdp.per.person,start=1950,frequency=1),start=c(1970))
year <- window(ts(year,start=1950,frequency=1),start=c(1970))

# plot it 
plot(rgdp.pc)

# determine and plot the growth rates 
growth.rate<-diff(log(rgdp.pc), lag=1, differences=1)*100
plot(growth.rate,ylim=c(-7,11))

# average growth rate 
mgr <- mean(growth.rate)

# build and plot  the histogram
br = seq(-6, 10, by=2) 
h <- hist(growth.rate, breaks = br, plot=FALSE)
plot(h,ylim=c(-7,12))

h$counts=h$counts/sum(h$counts)
diff(h$breaks)*h$density

# rate levels and relative frequencies
rate.level<-c(-0.05,-0.03,-0.01,0.01,0.03,0.05,0.07,0.09)
rbind(rate.level,h$counts)

rate.cut = cut(growth.rate, br , right=FALSE)
rate.freq = table(rate.cut)
rate.prob = table(rate.cut)/44

# Certanty equivalent  growth rate: for a CRRA function 
eqcert <- function(s){
  if(s == 1) {(exp(sum(rate.prob*log(1+rate.level)))-1)*100} 
  else  { (((1-s)*sum(rate.prob*(((1+rate.level)^(1-s))/(1-s))))^(1/(1-s))-1 )*100}
}

plot(Vectorize(eqcert),1,10, xlab="CRRA", ylab="g",type="l",ylim=c(1,2.5), 
     main="Portugal 1970-2014: CE(g) and  E(g)"
     )
abline(h=mgr,col='red')