# Examination of efficacy of several drugs on a population's life expectancy
options(scipen=999)
rates=read.csv("https://raw.githubusercontent.com/AdrianKuklaPL/actuarial-science/main/data/yearly_death_rates.csv", header = TRUE);View(rates)
age = rates[,1];q = rates[,2]
plot(age,log(q),main="Plot of mortality rates",lwd=2,col=2,type="l",xlab="Age last birthday",ylab="Log of mortality ates")

#Creating a function for complete expectation of life
expect <- function(x,p) {
  e=0
  surviv=1
  for (i in (x+1):length(q)) {
    if (i==length(q)) {q[i]=1;surviv <- surviv*(1-q[i])} else {surviv <- surviv*(1-q[i]*p/100)}
    e <- e+surviv
  }
  return(e+0.5)
}

#Simulation technique in R to estimate future life expectancies
x=0;p=100
life=0
for (i in 1:10000) {
  alive=1;age=x
  while (alive==1) {
    if ((q[age+1]*p/100)>runif(1,0,1)) {alive=0} else {age=age+1}
    if (age+1==length(q)) {age=age-1;alive=0} else {age=age}
  }
  life[i] <- age + 0.5 - x
}
length(subset(life,life>=10))/length(life)
quantile(life,0.025);quantile(life,0.975)

#Cumulative multiplicative 0.5% fall in mortality rates
qnew=0
for (i in 1:length(q)) {
  qnew[i]=q[i]*0.995^(i-1)
  if(i==length(q)) {qnew[i]=1}
}
print(qnew)

#Creating a vector E and m of population data and simulating annual number of deaths
prop <- 0.75;pop <- 4500000
ws <- c(5,5.5,5.5,5.5,6,6,6.5,7,7,7,7,6.5,6,5.5,5,4,2.5,1.5,1)
E <- pop*ws/sum(ws)

m=0;age=seq(1,91,5);t=0;qnew=0
for (i in age) {
  t = t + 1
  qnew=prop*q;qnew[length(q)]=1
  if (t==length(E)) {m[t] = mean(qnew[(age[t]):(length(qnew))])} else {m[t] = mean(qnew[i:(i+4)])}
}
m

deaths=0
for (j in 1:10000) {
  d=0
  for (i in 1:length(E)) {
    d=d+rbinom(1,E[i],m[i])
  }
  deaths[j]=d
}
mean(deaths)

#Mortality Statistics
w <- c(10.5,11,12,13.5,14,13.5,11.5,9,4,1)
m <- c(0.0006,0.0002,0.0015,0.0015,0.002,.005,0.01,0.0250,0.075,0.18)
pop <- c(675,585,660,760,635,520,395,225,95,50)*1000
CMR <- sum(m*pop)/sum(pop)
E <- sum(pop)*w/sum(w)
DSR <- sum(E*m)/sum(E);print(DSR);print(DSR*1000)

#Graph of weights
w1 <- pop/sum(pop)*100; w2 <- w; w3 <- w2 + 1; w4 <- w2 + 2; w5 <- w2 + 5
w1 <- w1/sum(w1); w2 <- w2/sum(w2); w3 <- w3/sum(w3); w4 <- w4/sum(w4); w5 <- w5/sum(w5)
age <- c(4.5,14.5,24.5,34.5,44.5,54.5,64.5,74.5,84.5,94.5)
plot(age,w1,col="green",pch=16,xlab="age",ylab="Proportion of Population",main="Comparing weights for different age profiles",ylim=c(0,.17))
points(age,w2,col="blue",pch=16)
points(age,w3,col="red",pch=16)
points(age,w4,col="black",pch=16)
points(age,w5,col="gold",pch=16)
legend(20,.08,col = c("green","blue","red","black","gold"), pch=c(16,16,16,16,16), cex= 1,
       legend = c("Ireland","Europe","Europe+1","Europe+2","Europe+3"))

#Examining the sensitivity of DSR to annual mortality rates
w=c(10.5,11,12,13.5,14,13.5,11.5,9,4,1)
m=c(0.0006,0.0002,0.0015,0.0015,0.002,0.005,0.01,0.0250,0.075,0.18)
E=sum(pop)*w/sum(w)
DSR=0
for (k in 1:10000){
  d=0
  for (i in 1:length(E)){
    d=d+rpois(1,E[i]*m[i])
  }
  DSR[k]=d/sum(E)
}
mean(DSR)
quantile(DSR,c(.025,.975))

#Calculating the average age of the population for a given set of population weights
AvgAge<-function(pop,age,ws)
{ageprof <- 0
for (i in 0:24) {
  w=ws+0.5*i;E=sum(pop)*w/sum(w)
  ageprof[i+1]=sum(age*E)/sum(E)
}
barplot(ageprof,ylab="Average Age",xlab="Future Year",main="Barplot of Average Age in Ireland")
return(ageprof)
}
AvgAge(pop,age,w)

#Function of age ratio of over 65s to under 65s
AgeRatio<-function(ws)
{ratio <- 0
for (i in 0:24) {
  w=ws+0.5*i
  a <- sum(w[8:10])+0.5*w[7]
  b <- sum(w[1:6])+0.5*w[7]
  ratio[i+1]=(a/b)
}
barplot(ratio,ylab="Ratio of people 65 or over to under 65",xlab="Future Year",main="Barplot of Retirement Ratio in Ireland")
return(ratio)
}
AgeRatio(w)

#SMRatio Function
SMRatio <- function(pop,d,sm)
{ratio <- sum(d)/sum(pop*sm)
return(ratio)
}
population <- c(50,60,80,100,100,85,70,50,30,10)*1000
deaths <- c(20,50,150,450,600,700,2100,2250,2700,1500)
rates <- c(0.0005,0.001,0.0025,0.005,0.0075,0.01,0.025,0.05,0.1,0.2)
SMRatio(population,deaths,rates)

#Simulation exercise of distribution of SMRatio
E=population
m=deaths/E
SMR=0
for (k in 1:10000){
  d=0
  for (i in 1:length(E)){
    d=d+rpois(1,E[i]*m[i])
  }
  SMR[k]=SMRatio(population,d,rates)
}
mean(SMR); sd(SMR); quantile(SMR,c(.025,.975))

#ACF Function
ACF <- function(pop,sm,ws)
  #pop is the population of interest, sm are the standard mortality rates and ws are the weights in standard population
{Es <- sum(pop)*(ws)/sum(ws)
num <- sum(Es*sm)/sum(Es)
denom <- sum(pop*sm)/sum(pop)
ACF <- num/denom
return(ACF)}
population <- c(675,585,660,760,635,520,395,225,95,50)*1000
rates <- c(0.0005,0.001,0.0025,0.005,0.0075,0.01,0.025,0.05,0.1,0.2)
weights <- c(10.5,11,12,13.5,14,13.5,11.5,9,4,1)
ACF(population,rates,weights)

#IndirectSMR Function using ACF*CMR
IndirectSMR1 <- function(pop,sm,ws,d)
  #pop is the population of interest, sm are the standard mortality rates and ws are the weights in standard population,d is the total deaths in pop of interest
{Es <- sum(pop)*ws/sum(ws)
num <- sum(Es*sm)/sum(Es)
denom <- sum(pop*sm)/sum(pop)
ACF <- num/denom
CMR <- sum(d)/sum(pop)
ratio <- ACF*CMR
return(ratio)}
IndirectSMR1(population,rates,weights,deaths)

#IndirectSMR using CMRs*SMRatio
IndirectSMR2 <- function(pop,sm,ws,d)
  #pop is the population of interest, sm are the standard mortality rates and ws are the weights in standard population,d is the total deaths in pop of interest
{Es <- sum(pop)*ws/sum(ws)
CMRs <- sum(Es*sm)/sum(Es)
SMRatio <- sum(d)/sum(pop*sm)
ratio <- CMRs*SMRatio
return(ratio)
}
IndirectSMR2(population,rates,weights,deaths)

options(scipen=999)
set.seed(574665)
rates=read.csv("https://raw.githubusercontent.com/AdrianKuklaPL/actuarial-science/main/data/yearly_death_rates.csv", header = TRUE);View(rates)
age = rates[,1];qx = rates[,2];qy = rates[,3];qz = rates[,4]

Meanx=0;SDx=0;BottomQx=0;TopQx=0
years=c(0,20,30,40,50,60,70,80)
for (j in 1:length(years)) {
x=years[j];p=100
life=0
for (i in 1:10000) {
  alive=1;age=x
  while (alive==1) {
    if ((qx[age+1]*p/100)>runif(1,0,1)) {alive=0} else {age=age+1}
    if (age+1==length(qx)) {age=age-1;alive=0} else {age=age}
  }
  life[i] <- age + 0.5 - x
}
Meanx[j] <- mean(life)
SDx[j] <- sd(life)
BottomQx[j] <- quantile(life,0.025)
TopQx[j] <- quantile(life,0.975)
}

Meany=0;SDy=0;BottomQy=0;TopQy=0
years=c(0,20,30,40,50,60,70,80)
for (j in 1:length(years)) {
  x=years[j];p=100
  life=0
  for (i in 1:10000) {
    alive=1;age=x
    while (alive==1) {
      if ((qy[age+1]*p/100)>runif(1,0,1)) {alive=0} else {age=age+1}
      if (age+1==length(qy)) {age=age-1;alive=0} else {age=age}
    }
    life[i] <- age + 0.5 - x
  }
  Meany[j] <- mean(life)
  SDy[j] <- sd(life)
  BottomQy[j] <- quantile(life,0.025)
  TopQy[j] <- quantile(life,0.975)
}

Meanz=0;SDz=0;BottomQz=0;TopQz=0
years=c(0,20,30,40,50,60,70,80)
for (j in 1:length(years)) {
  x=years[j];p=100
  life=0
  for (i in 1:10000) {
    alive=1;age=x
    while (alive==1) {
      if ((qz[age+1]*p/100)>runif(1,0,1)) {alive=0} else {age=age+1}
      if (age+1==length(qz)) {age=age-1;alive=0} else {age=age}
    }
    life[i] <- age + 0.5 - x
  }
  Meanz[j] <- mean(life)
  SDz[j] <- sd(life)
  BottomQz[j] <- quantile(life,0.025)
  TopQz[j] <- quantile(life,0.975)
}

Means <- cbind(years,Meanx,Meany,Meanz)
SDs <- cbind(years,SDx,SDy,SDz)
BottomQs <- cbind(years,BottomQx,BottomQy,BottomQz)
TopQs <- cbind(years,TopQx,TopQy,TopQz)
write.csv(Means,"C:\\Users\\kukla\\Desktop\\Means.csv",row.names = FALSE)
write.csv(SDs,"C:\\Users\\kukla\\Desktop\\SDs.csv",row.names = FALSE)
write.csv(BottomQs,"C:\\Users\\kukla\\Desktop\\BottomQs.csv",row.names = FALSE)
write.csv(TopQs,"C:\\Users\\kukla\\Desktop\\TopQs.csv",row.names = FALSE)

#Simulating annual number of deaths
popx <- c(2,2,2,2,4,4,4,4)*10000
popy <- c(4,4,3,3,2,2,1,1)*10000
popz <- c(3,3,3,3,3,3,3,3)*10000
dx <- c(200,400,800,1000,4000,6000,8000,10000);mx=dx/popx
dy <- c(350,820,1140,1500,1850,2900,1800,2000);my=dy/popy
dz <- c(600,660,905,1120,2245,4000,4500,5600);mz=dz/popz

deathsx=0
for (j in 1:10000) {
  d=0
  for (i in 1:length(popx)) {
    d=d+rbinom(1,popx[i],qx[i])
  }
  deathsx[j]=d
}
mean(deathsx)

#Examining the sensitivity of DSR to annual mortality rates
Ey=popx
DSR=0
for (k in 1:10000){
  d=0
  for (i in 1:length(Ey)){
    d=d+rbinom(1,Ey[i],my[i])
  }
  DSR[k]=d/sum(Ey)
}
mean(DSRy)
quantile(DSRy,c(.025,.975))


#Pricing benefit pension scheme.
#See how effects will change when the q changes. By changing q.
#See how to stabilize costs by changing features of particular pension scheme. 
#There's probably 4 or 5 different features to look at.

rates=read.csv("https://raw.githubusercontent.com/AdrianKuklaPL/actuarial-science/main/data/male&female-mortality-rates.csv", header = TRUE)
rates1=read.csv("https://raw.githubusercontent.com/AdrianKuklaPL/actuarial-science/main/data/new_male&female-mortality-rates.csv", header = TRUE)
#Separate rates for males and females for original and new mortality rates
ages<-rates[,1]
q<-rates[,c(2:3)]
qm<-rates[,2]
qf<-rates[,3]

qnew <- rates1[,c(2,3)]
qmnew <- rates1[,2]
qfnew <- rates1[,3]

#Using Original Rates
#Assumptions to change
SA <- 6000
incr <- .05  
int <- .04
retage <- 65
  
p=0
for (j in 1:1000){#understand cost of book of business
  cost=0
  for (i in 1:2000) { #2000 policyholders, replicates book of business
    if(i<=1000) {q=qm} else {q=qf}
    alive=1;age=retage;t=0  
    while (alive==1) { #simulating person's live
      cost <-  cost + (SA*(1+incr)^t)/(1+int)^t
      t <- t+1
      if (runif(1,0,1)<q[age-65+1]) {alive=0} else (age=age+1)
    }
  }
  
  p[j] <- cost/1000000 #records cost per million
}
currentcost <- mean(p)
sd(p)
max(p)
min(p)
IQR(p)
COV <- sd(p)/mean(p);COV
quantile(p,.025);quantile(p,.975)

#Males
SA <- 6000
incr <- .05  
int <- .04
retage <- 65

pmale=0
for (j in 1:1000){#understand cost of book of business
  cost=0
  for (i in 1:1000) { #1000 male policyholders, replicates book of business
    alive=1;age=retage;t=0  
    while (alive==1) { #simulating person's live
      cost <-  cost + (SA*(1+incr)^t)/(1+int)^t
      t <- t+1
      if (runif(1,0,1)<qm[age-65+1]) {alive=0} else (age=age+1)
    }
  }
  
  pmale[j] <- cost/1000000 #records cost per million
}
mean(pmale)
sd(pmale)

#Females
SA <- 6000
incr <- .05  
int <- .04
retage <- 65

pfemale=0
for (j in 1:1000){#understand cost of book of business
  cost=0
  for (i in 1:1000) { #1000 female policyholders, replicates book of business
    alive=1;age=retage;t=0  
    while (alive==1) { #simulating person's live
      cost <-  cost + (SA*(1+incr)^t)/(1+int)^t
      t <- t+1
      if (runif(1,0,1)<qf[age-65+1]) {alive=0} else (age=age+1)
    }
  }
  
  pfemale[j] <- cost/1000000 #records cost per million
}
mean(pfemale)
sd(pfemale)

col2rgb(c("deepskyblue2","yellow"))
c1 <- rgb(0,178,238,max = 255,alpha = 80, names="blue")
c2 <- rgb(255,255,0,max = 255,alpha = 80, names="yellow")
a <- min(c(pmale,pfemale))-0.001;b <- max(c(pmale,pfemale))
hm <- hist(pmale,breaks=pretty(a:b,n=30),plot=FALSE)
hf <- hist(pfemale,breaks=pretty(a:b,n = 30),plot=FALSE)
plot(hm,xlim=c(110,160),ylim=c(0,400), col = c1,xlab = "Cost of Pension Scheme per 1000 persons",ylab="Number of Pensioners",main="Total Pension Cost split by Gender",
     cex.lab=1.5,cex.main=1.7,cex.axis=1.5)
legend(123,400,col = c(c1,c2), pch=c(15,15), cex=1.3,
      legend = c("Male Pension Cost","Female Pension Cost"))
plot(hf,ylim=c(0,400), col = c2,add=TRUE)

dev.off()
plot.new()

#Using Improved Rates 
SA <- 6000
incr <- .05  
int <- .04
retage <- 65

pnew=0
for (j in 1:1000){#understand cost of book of business
  cost=0
  for (i in 1:2000) { #2000 policyholders, replicates book of business
    if(i<=1000) {q=qmnew} else {q=qfnew}
    alive=1;age=retage;t=0  
    while (alive==1) { #simulating person's live
      cost <-  cost + (SA*(1+incr)^t)/(1+int)^t
      t <- t+1
      if (runif(1,0,1)<q[age-65+1]) {alive=0} else (age=age+1)
    }
  }
  
  pnew[j] <- cost/1000000 #records cost per million
}
newcost <- mean(pnew)
sd(pnew)
min(pnew);max(pnew)
hist(pnew)
IQR(pnew)
quantile(pnew,.025);quantile(pnew,.975)
#want 267 million euro - new cost is at 331 million
(newcost - currentcost) / currentcost #24% increase in cost 
(max(pnew)-max(p))/max(p)
(min(pnew)-min(p))/min(p)

#Males
SA <- 6000
incr <- .05  
int <- .04
retage <- 65

pmale2=0
for (j in 1:1000){#understand cost of book of business
  cost=0
  for (i in 1:1000) { #1000 male policyholders, replicates book of business
    alive=1;age=retage;t=0  
    while (alive==1) { #simulating person's live
      cost <-  cost + (SA*(1+incr)^t)/(1+int)^t
      t <- t+1
      if (runif(1,0,1)<qmnew[age-65+1]) {alive=0} else (age=age+1)
    }
  }
  
  pmale2[j] <- cost/1000000 #records cost per million
}
mean(pmale2)
sd(pmale2)

#Females
SA <- 6000
incr <- .05  
int <- .04
retage <- 65

pfemale2=0
for (j in 1:1000){#understand cost of book of business
  cost=0
  for (i in 1:1000) { #1000 female policyholders, replicates book of business
    alive=1;age=retage;t=0  
    while (alive==1) { #simulating person's live
      cost <-  cost + (SA*(1+incr)^t)/(1+int)^t
      t <- t+1
      if (runif(1,0,1)<qfnew[age-65+1]) {alive=0} else (age=age+1)
    }
  }
  
  pfemale2[j] <- cost/1000000 #records cost per million
}
mean(pfemale2)
sd(pfemale2)

#Males vs Females
a <- min(c(pmale2,pfemale2))-0.001;b <- max(c(pmale2,pfemale2))+20
hm2 <- hist(pmale2,breaks=pretty(a:b,n=30),plot=FALSE)
hf2 <- hist(pfemale2,breaks=pretty(a:b,n = 30),plot=FALSE)
plot(hm2,xlim=c(140,190),ylim=c(0,400), col = c1,xlab = "Cost of Pension Scheme per 1000 persons",ylab="Number of Pensioners",main="Total Pension Cost split by Gender",
     cex.lab=1.5,cex.main=1.7,cex.axis=1.5)
legend(153,400,col = c(c1,c2), pch=c(15,15), cex=1.3,
       legend = c("Male Pension Cost","Female Pension Cost"))
plot(hf2,ylim=c(0,400), col = c2,add=TRUE)

#Current vs Future
a <- min(c(p,pnew))-0.001;b <- max(c(p,pnew))
hold <- hist(p,breaks=pretty(a:b,n=30),plot=FALSE)
hnew <- hist(pnew,breaks=pretty(a:b,n = 30),plot=FALSE)
plot(hold,xlim=c(250,350),ylim=c(0,300), col = c1,xlab = "Cost of Pension Scheme per 2000 persons",ylab="Number of Pensioners",main="Comparison of Current and Future Pension Costs",
     cex.lab=1.5,cex.main=1.7,cex.axis=1.5)
legend(273,290,col = c(c1,c2), pch=c(15,15), cex=1.3,
       legend = c("Current Pension Scheme","Future Pension Scheme"))
plot(hnew,ylim=c(0,400), col = c2,add=TRUE)

(mean(pfemale2)-mean(pfemale))/mean(pfemale)
(mean(pmale2)-mean(pmale))/mean(pmale)



#Rebalancing Cost Structure #1
#Assumptions
SA <- 9000
incr <- 0
int <- .04
retage <- 65

pchng=0
for (j in 1:1000){#understand cost of book of business
  cost=0
  for (i in 1:2000) { #2000 policyholders, replicates book of business
    if(i<=1000) {q=qmnew} else {q=qfnew}
    alive=1;age=retage;t=0  
    while (alive==1) { #simulating person's live
      cost <-  cost + (SA*(1+incr)^t)/(1+int)^t
      t <- t+1
      if (runif(1,0,1)<q[age-65+1]) {alive=0} else (age=age+1)
    }
  }
  
  pchng[j] <- cost/1000000 #records cost per million
}
changedcost <- mean(pchng)
changedcost
sd(pchng)
max(pchng)
min(pchng)
IQR(pchng)
COV <- sd(pchng)/mean(pchng);COV
quantile(pchng,.025);quantile(pchng,.975)

#Rebalancing Cost Structure #2
#Assumptions
SA <- 7000
incr <- .0215
int <- .04
retage <- 65

pchng1=0
for (j in 1:1000){#understand cost of book of business
  cost=0
  for (i in 1:2000) { #2000 policyholders, replicates book of business
    if(i<=1000) {q=qmnew} else {q=qfnew}
    alive=1;age=retage;t=0  
    while (alive==1) { #simulating person's live
      cost <-  cost + (SA*(1+incr)^t)/(1+int)^t
      t <- t+1
      if (runif(1,0,1)<q[age-65+1]) {alive=0} else (age=age+1)
    }
  }
  
  pchng1[j] <- cost/1000000 #records cost per million
}
changedcost2 <- mean(pchng1)
changedcost2
mean(pchng1)
sd(pchng1)
max(pchng1)
min(pchng1)
IQR(pchng1)
COV <- sd(pchng1)/mean(pchng1);COV
quantile(pchng1,.025);quantile(pchng1,.975)

#Rebalancing Cost Structure #3
#Assumptions
SA <- 6500
incr <- .035
int <- .04
retage <- 65

pchng2=0
for (j in 1:1000){#understand cost of book of business
  cost=0
  for (i in 1:2000) { #2000 policyholders, replicates book of business
    if(i<=1000) {q=qmnew} else {q=qfnew}
    alive=1;age=retage;t=0  
    while (alive==1) { #simulating person's live
      cost <-  cost + (SA*(1+incr*t))/(1+int)^t
      t <- t+1
      if (runif(1,0,1)<q[age-65+1]) {alive=0} else (age=age+1)
    }
  }
  
  pchng2[j] <- cost/1000000 #records cost per million
}
changedcost3 <- mean(pchng2)
changedcost3
mean(pchng2)
sd(pchng2)
max(pchng2)
min(pchng2)
IQR(pchng2)
COV <- sd(pchng2)/mean(pchng2);COV
quantile(pchng2,.025);quantile(pchng2,.975)

#Rebalancing Cost Structure #4
#Assumptions
SA <- 5000
incr <- .075
int <- .04
retage <- 65

pchng3=0
for (j in 1:1000){#understand cost of book of business
  cost=0
  for (i in 1:2000) { #2000 policyholders, replicates book of business
    if(i<=1000) {q=qmnew} else {q=qfnew}
    alive=1;age=retage;t=0  
    while (alive==1) { #simulating person's live
      cost <-  cost + (SA*(1+incr*t))/(1+int)^t
      t <- t+1
      if (runif(1,0,1)<q[age-65+1]) {alive=0} else (age=age+1)
    }
  }
  
  pchng3[j] <- cost/1000000 #records cost per million
}
changedcost4 <- mean(pchng3)
changedcost4
mean(pchng3)
sd(pchng3)
max(pchng3)
min(pchng3)
IQR(pchng3)
COV <- sd(pchng3)/mean(pchng3);COV
quantile(pchng3,.025);quantile(pchng3,.975)

#Rebalancing Cost Structure #5
#Assumptions
SA <- 4500
incr <- .055
int <- .04
retage <- 65

pchng4=0
for (j in 1:1000){#understand cost of book of business
  cost=0
  for (i in 1:2000) { #2000 policyholders, replicates book of business
    if(i<=1000) {q=qmnew} else {q=qfnew}
    alive=1;age=retage;t=0  
    while (alive==1) { #simulating person's live
      cost <-  cost + (SA*(1+incr)^t)/(1+int)^t
      t <- t+1
      if (runif(1,0,1)<q[age-65+1]) {alive=0} else (age=age+1)
    }
  }
  
  pchng4[j] <- cost/1000000 #records cost per million
}
changedcost5 <- mean(pchng4)
changedcost5
mean(pchng4)
sd(pchng4)
max(pchng4)
min(pchng4)
IQR(pchng4)
COV <- sd(pchng4)/mean(pchng4);COV
quantile(pchng4,.025);quantile(pchng4,.975)

#Rebalancing Cost Structure #6
#Assumptions
SA <- 7000
incr <- .024
int <- .04
retage <- 66

pchng5=0
for (j in 1:1000){#understand cost of book of business
  cost=0
  for (i in 1:2000) { #2000 policyholders, replicates book of business
    if(i<=1000) {q=qmnew} else {q=qfnew}
    alive=1;age=retage;t=0  
    while (alive==1) { #simulating person's live
      cost <-  cost + (SA*(1+incr)^t)/(1+int)^t
      t <- t+1
      if (runif(1,0,1)<q[age-65+1]) {alive=0} else (age=age+1)
    }
  }
  
  pchng5[j] <- cost/1000000 #records cost per million
}
changedcost6 <- mean(pchng5)
changedcost6
mean(pchng5)
sd(pchng5)
max(pchng5)
min(pchng5)
IQR(pchng5)
COV <- sd(pchng5)/mean(pchng5);COV
quantile(pchng5,.025);quantile(pchng5,.975)

#Current vs Future with improvements
a <- min(c(p,pchng5))-0.001;b <- max(c(p,pchng5))
hold <- hist(p,breaks=pretty(a:b,n=20),plot=FALSE)
hnew <- hist(pchng5,breaks=pretty(a:b,n = 20),plot=FALSE)
plot(hold,xlim=c(250,280),ylim=c(0,180), col = c1,xlab = "Cost of Pension Scheme per 2000 persons",ylab="Number of Pensioners",main="Comparison of Current and Future Pension Costs",
     cex.lab=1.5,cex.main=1.7,cex.axis=1.5)
legend(250,170,col = c(c1,c2), pch=c(15,15), cex=1.2,
       legend = c("Current Pension Scheme","Future Pension Scheme"))
plot(hnew,ylim=c(0,400), col = c2,add=TRUE)

(mean(pfemale2)-mean(pfemale))/mean(pfemale)
(mean(pmale2)-mean(pmale))/mean(pmale)

#Rebalancing Cost Structure #7
#Assumptions
rates2=read.csv("https://raw.githubusercontent.com/AdrianKuklaPL/actuarial-science/main/data/m-thly_death_rates.csv", header = TRUE);View(rates2)
qmthlym <- rates2[,1]
qmthlyf <- rates2[,2]
SA <- rates2[,3]
incr <- .055
int <- .04
retage <- 65
m <- 2

pchng6=0
for (j in 1:1000){#understand cost of book of business
  cost=0
  for (i in 1:2000) { #2000 policyholders, replicates book of business
    if(i<=1000) {q=qmthlym} else {q=qmthlyf}
    alive=1;age=retage;t=1  
    while (alive==1) { #simulating person's live
      cost <-  cost + (SA[t])/(1+int)^(t/m-1/m)
      t <- t+1
      if (runif(1,0,1)<q[age-65+1]) {alive=0} else (age=age+1)
    }
  }
  
  pchng6[j] <- cost/1000000 #records cost per million
}
changedcost7 <- mean(pchng6)
changedcost7
mean(pchng6)
sd(pchng6)
max(pchng6)
min(pchng6)
IQR(pchng6)
COV <- sd(pchng6)/mean(pchng6);COV
quantile(pchng6,.025);quantile(pchng6,.975)
