# QFGB 8925 Simulation Applications
# Vivek Yadlapalli
# Quick and Dirty LCG Generator

##PROBLEM 1

#a.	100 Random numbers between 1 and 1000 and plot a histogram
im<- 134456
ia<- 8121
ic<- 28411
idum<- -1000
icount<- 1
numran<- 100
ranx<-rep(0,numran)
#
while (icount <= numran) {
  idum<-(ia*idum+ic)%%im
  ranx[icount]<-idum/im
  icount <- icount + 1
}
#
hist(ranx)
#
# Scaling to 1:1000
#
ran1000<-as.integer(1000*ranx)+1

ran1000

hist(ran1000)
max(ran1000)
min(ran1000)


# QFGB 8925 Simulation Applications
# Vivek Yadlapalli
# Quick and Dirty LCG Generator

# b.	10,000 Random numbers between 0 and 1 and plot a histogram
im<- 134456
ia<- 8121
ic<- 28411
idum<- -1000
icount<- 1
numran<- 10000
ranx<-rep(0,numran)
#
while (icount <= numran) {
  idum<-(ia*idum+ic)%%im
  ranx[icount]<-idum/im
  icount <- icount + 1
}
#ranx  gives use a hist of 10000 random numbers between 0 and 1
hist(ranx)

##PROBLEM 2
#a. Confirmation of uniform distribution for 10000 trials of roulette
im<-134456
ia<-8121
ic<-28411
idum<-2
icount<-1
numran<-10000
ranx<-rep(0,numran)
#
while (icount <= numran) {
  idum<-(ia*idum+ic)%%im
  ranx[icount]<-idum/im
  icount<-icount+1
}
#
hist(ranx)

#
# Scaling to 1:36
#
ran36<-as.integer(36*ranx)+1

#
ran36[1]
ran36[2]

hist(ran36)
max(ran36)
min(ran36)


#10 plays of roulette

im<- 134456
ia<- 8121
ic<- 28411
idum<- -1000
icount<- 1
numran<- 10
ranx<-rep(0,numran)
#
while (icount <= numran) {
  idum<-(ia*idum+ic)%%im
  ranx[icount]<-idum/im
  icount <- icount + 1
}
#ranx  gives use a hist of 10000 random numbers between 0 and 1
hist(ranx)

ran36_10<-as.integer(36*ranx)+1

ran36_10

hist(ran36_10)
max(ran36_10)
min(ran36_10)


#100 plays of roulette

im<- 134456
ia<- 8121
ic<- 28411
idum<- -1000
icount<- 1
numran<- 100
ranx<-rep(0,numran)
#
while (icount <= numran) {
  idum<-(ia*idum+ic)%%im
  ranx[icount]<-idum/im
  icount <- icount + 1
}
#ranx  gives use a hist of 10000 random numbers between 0 and 1
hist(ranx)

ran36_100<-as.integer(36*ranx)+1

ran36_100

hist(ran36_100)
max(ran36_100)
min(ran36_100)

#1000 plays of roulette

im<- 134456
ia<- 8121
ic<- 28411
idum<- -1000
icount<- 1
numran<- 1000
ranx<-rep(0,numran)
#
while (icount <= numran) {
  idum<-(ia*idum+ic)%%im
  ranx[icount]<-idum/im
  icount <- icount + 1
}
#ranx  gives use a hist of 10000 random numbers between 0 and 1
hist(ranx)

ran36_1000<-as.integer(36*ranx)+1

ran36_1000

hist(ran36_1000)
max(ran36_1000)
min(ran36_1000)

## Payoffs for a fair Roulette

payoff<-function(t){
    pay=0
    for(i in 1:length(t)){
      if(t[i]==36){
        pay[i] = 35
      }
      else{
        pay[i]=-1
      }
    }
    return(pay)
}

sum(payoff(ran36_10))
mean(payoff(ran36_10))
sd(payoff(ran36_10))

sum(payoff(ran36_100))
mean(payoff(ran36_100))
sd(payoff(ran36_100))

sum(payoff(ran36_1000))
mean(payoff(ran36_1000))
sd(payoff(ran36_1000))

##c. Unfair Roulette (37 slots)
#10 plays
im<- 134456
ia<- 8121
ic<- 28411
idum<- -1000
icount<- 1
numran<- 10
ranx<-rep(0,numran)
#
while (icount <= numran) {
  idum<-(ia*idum+ic)%%im
  ranx[icount]<-idum/im
  icount <- icount + 1
}
#ranx  gives use a hist of 10000 random numbers between 0 and 1
hist(ranx)

uran37_10<-as.integer(37*ranx)

uran37_10

hist(uran37_10)
max(uran37_10)
min(uran37_10)

#100 plays
im<- 134456
ia<- 8121
ic<- 28411
idum<- -1000
icount<- 1
numran<- 100
ranx<-rep(0,numran)
#
while (icount <= numran) {
  idum<-(ia*idum+ic)%%im
  ranx[icount]<-idum/im
  icount <- icount + 1
}
#ranx  gives use a hist of 10000 random numbers between 0 and 1
hist(ranx)

uran37_100<-as.integer(37*ranx)

uran37_100

hist(uran37_100)
max(uran37_100)
min(uran37_100)

#1000 plays
im<- 134456
ia<- 8121
ic<- 28411
idum<- -1000
icount<- 1
numran<- 1000
ranx<-rep(0,numran)
#
while (icount <= numran) {
  idum<-(ia*idum+ic)%%im
  ranx[icount]<-idum/im
  icount <- icount + 1
}
#ranx  gives use a hist of 10000 random numbers between 0 and 1
hist(ranx)

uran37_1000<-as.integer(37*ranx)

uran37_1000

hist(uran37_1000)
max(uran37_1000)
min(uran37_1000)

##playoffs of unfair roulette
sum(payoff(uran37_10))
mean(payoff(uran37_10))
sd(payoff(uran37_10))

sum(payoff(uran37_100))
mean(payoff(uran37_100))
sd(payoff(uran37_100))

sum(payoff(uran37_1000))
mean(payoff(uran37_1000))
sd(payoff(uran37_1000))

##Problem 3
#here ranx is the u input into the inverse normal function
im<- 134456
ia<- 8121
ic<- 28411
idum<- -1000
icount<- 1
numran<- 10000
ranx<-rep(0,numran)
#
while (icount <= numran) {
  idum<-(ia*idum+ic)%%im
  ranx[icount]<-idum/im
  icount <- icount + 1
}
a0 = 2.50662823884
a1 = -18.61500062529
a2 = 41.39119773534
a3 = -25.44106049637
b0 = -8.47351093090
b1 = 23.08336743743
b2 = -21.06224101826
b3 = 3.13082909833
c0 = 0.3374754822726147
c1 = 0.9761690917186
c2 = 0.1607979714918209
c3 = 0.0276438810333863
c4 = 0.0038405729373609
c5 = 0.0003951896511919
c6 = 0.0000321767881768
c7 = 0.0000002888167364
c8 = 0.0000003960315187

INF <- function(u){
  y<-u-0.5
  if(abs(y)<0.42){
    r <- y*y
    x <- y*(((a3*r+a2)*r+a1)*r+a0)/((((b3*r+b2)*r+b1)*r+b0)*r+1)
  }
  else{
    r<-u
    if(y>0){
      r<-1-u
    }
    r<-log(-log(r))
    x<-c0+r*(c1+r*(c2+r*(c3+r*(c4+r*(c5+r*(c6+r*(c7+r*c8)))))))
    if(y<0){
      x<--x
    }
  }
  return(x)
}
out<-list()
for (i in 1:length(ranx)){
  out[i] <- INF(ranx[i])
}
##Normal Distribution
hist(as.numeric(out))

##Problem 4
#Determining implied volatility of Visa Call Option stock using Black-Scholes

S0<-125
Strike<-125
rf<-0.05
T<-0.5 
C<-47.55

#implied volatility 
volatility <- sqrt(2*3.14/T)*C/Strike

