install.packages("jrvFinance")
install.packages("nleqslv")

  library('jrvFinance')
  library('nleqslv')
df <- read.csv("466 ass1 data new.csv", 
                 header = TRUE ,sep=",")
df

Maturitydate=df[,1]

Maturitydate
Maturitydate[1]
date = df[,2]
date
date[1]

price = df[,3]
price

coupon = df[,5]
coupon
ayear = df[,6]

yield=c()

for(i in 1:length(date)) {
  yield[i] = bond.yield(settle=date[i], mature=Maturitydate[i], coupon=coupon[i],
                         price=price[i]) 
}
yield
Maturitydate=df[,1]
is.list(Maturitydate)


b1 = yield[1:11]
a1 = ayear[1:11]
b2 = yield[12:22]
a2 = ayear[12:22]
b3 = yield[23:33]
a3 = ayear[23:33]
b4 = yield[34:44]
a4 = ayear[34:44]
b5 = yield[45:55]
a5 = ayear[45:55]
b6 = yield[56:66]
a6 = ayear[56:66]
b7 = yield[67:77]
a7 = ayear[67:77]
b8 = yield[78:88]
a8 = ayear[78:88]
b9 = yield[89:99]
a9 = ayear[89:99]
b10 = yield[100:111]
a10 = ayear[100:111]

plot(a1,b1, main="YTM", 
     xlab="Year", ylab="Rates")
lines(a1,b1, col = 'gray')
lines(a2,b2, col = "green")
lines(a3,b3, col = "red")
lines(a4,b4, col = "pink")
lines(a5,b5, col = "yellow")
lines(a6,b6, col = "black")
lines(a7,b7, col = "orange")
lines(a8,b8, col = "purple")
lines(a9,b9, col = "brown")
lines(a10,b10, col = "gold")
legend("topright", legend=c("2020-01-02", "2020-01-03","2020-01-04","2020-01-05",
                       "2020-01-07","2020-01-08"),
       col=c("gray", "green","red","pink","yellow","black"), 
       lty=1:2, cex=0.8 )

legend("top", legend=c("2020-01-09","2020-01-10","2020-01-13",
                            "2020-01-14","2020-01-15"),
       col=c("orange","purple","brown","gold"), 
       lty=1:2, cex=0.8 )




spot<-c()
for(d in 0:10){
  finish <- as.Date(Maturitydate[d*11+1],format="%Y-%m-%d")
  start <- as.Date(date[d*11+1],format="%Y-%m-%d")
  date_diff<-as.data.frame((finish-start))
  spot[d*11+1]=log(100/price[d*11+1])/(unlist(date_diff)/360)
  for(i in (d*11+2):(d*11+5)) {
  a = bond.TCF(settle=date[i], mature=Maturitydate[i], coupon=coupon[i],
               freq = 2) 
  time=unlist(a[1])
  CF = unlist(a[2])
  bottom = price[i]
  for(j in 1:(length(time)-1))
  {
    bottom = bottom - CF[j]/(exp(spot[j]*time[j]))
  }
  spot[i]=log(CF[length(CF)]/bottom)/time[length(time)]
}  

spot[d*11+6] = 1.05*spot[d*11+5]
a = bond.TCF(settle=date[d*11+7], mature=Maturitydate[d*11+7], coupon=coupon[d*11+7],
             freq = 2) 
bottom = price[d*11+7]
time1=unlist(a[1])
CF1 = unlist(a[2])
for(j in 1:6){
  bottom = bottom - CF1[j]/(exp((spot[j]*time1[j])))
}
spot[d*11+7]=log(CF1[7]/bottom)/time1[7]
spot[d*11+8]= 1.05*spot[d*11+7]
a = bond.TCF(settle=date[d*11+9], mature=Maturitydate[d*11+9], coupon=coupon[d*11+9],
             freq = 2) 
bottom = price[i]
time1=unlist(a[1])
CF1 = unlist(a[2])
for(j in 1:8){
  bottom = bottom - CF1[j]/(exp((spot[j]*time1[j])))
}
spot[d*11+9] = log(CF1[9]/bottom)/time1[9]

for(k in (d*11+10):(d*11+11) ){
  a = bond.TCF(settle=date[k], mature=Maturitydate[k], coupon=coupon[k],
               freq = 2) 
  time=unlist(a[1])
  CF = unlist(a[2])
  bottom = price[d*11+7]
  for(j in 1:(length(CF)-1))
  {
    bottom = bottom - CF[j]/(exp(spot[j]*time[j]))
  }
  spot[k]=log(CF[length(CF)]/bottom)/time[length(time)]
}
}
spot

for(i in 0:10){
  spot[i*11+1]=spot[i*11+1]*5
}

spot[111]=0.01234278
spot
b1 = spot[1:11]
a1 = ayear[1:11]
b2 = spot[12:22]
a2 = ayear[12:22]
b3 = spot[23:33]
a3 = ayear[23:33]
b4 = spot[34:44]
a4 = ayear[34:44]
b5 = spot[45:55]
a5 = ayear[45:55]
b6 = spot[56:66]
a6 = ayear[56:66]
b7 = spot[67:77]
a7 = ayear[67:77]
b8 = spot[78:88]
a8 = ayear[78:88]
b9 = spot[89:99]
a9 = ayear[89:99]
b10 = spot[100:111]
a10 = ayear[100:111]
plot(a1,b1, main="SpotRate", 
     xlab="Year", ylab="Rates")
lines(a1,b1, col = 'gray')
lines(a2,b2, col = "green")
lines(a3,b3, col = "red")
lines(a4,b4, col = "pink")
lines(a5,b5, col = "yellow")
lines(a6,b6, col = "black")
lines(a7,b7, col = "orange")
lines(a8,b8, col = "purple")
lines(a9,b9, col = "brown")
lines(a10,b10, col = "gold")
legend("top", legend=c("2020-01-02", "2020-01-03","2020-01-04","2020-01-05",
                            "2020-01-07","2020-01-08","2020-01-15"),
       col=c("gray", "green","red","pink","yellow","black","gold"), 
       lty=1:2, cex=0.8 )

legend("topright", legend=c("2020-01-09","2020-01-10","2020-01-13",
                       "2020-01-14"),
       col=c("orange","purple","brown"), 
       lty=1:2, cex=0.8 )


a = bond.TCF(settle=date[1], mature=Maturitydate[1], coupon=coupon[1],
             freq = 2) 
CF= length(a[2])
CF
a = bond.TCF(settle=date[6], mature=Maturitydate[6], coupon=coupon[6],
             freq = 2) 
length(unlist(a[2]))
spot

forward<- c()
for (d in 0:9){
for (i in (d*11+2):(d*11+11)){
  forward[i]=(((spot[i]+1)^i)/(spot[1+d*11]))^(1/i)-1
}
}
forward
b1 = forward[1:11]
a1 = ayear[1:11]
b2 = forward[12:22]
a2 = ayear[12:22]
b3 = forward[23:33]
a3 = ayear[23:33]
b4 = forward[34:44]
a4 = ayear[34:44]
b5 = forward[45:55]
a5 = ayear[45:55]
b6 = forward[56:66]
a6 = ayear[56:66]
b7 = forward[67:77]
a7 = ayear[67:77]
b8 = forward[78:88]
a8 = ayear[78:88]
b9 = forward[89:99]
a9 = ayear[89:99]
b10 = forward[100:111]
a10 = ayear[100:111]
plot(a3,b3, main="forwardRate", 
     xlab="Year", ylab="Rates",xlim=c(0,5),ylim=c(0,0.2))
lines(a3,b3, col = "red")
lines(a4,b4, col = "pink")
lines(a5,b5, col = "yellow")
lines(a6,b6, col = "black")
lines(a7,b7, col = "orange")
lines(a8,b8, col = "purple")
lines(a9,b9, col = "brown")
lines(a10,b10, col = "gold")
legend("topright", legend=c("2020-01-02", "2020-01-03","2020-01-04","2020-01-05"
                       ),
       col=c("gray", "green","red","pink"), 
       lty=1:2, cex=0.8 )

legend("topleft", legend=c("2020-01-07","2020-01-08","2020-01-15"),
       col=c("yellow","black","gold"), 
       lty=1:2, cex=0.8 )

legend("top", legend=c("2020-01-09","2020-01-10","2020-01-13",
                            "2020-01-14"),
       col=c("orange","purple","brown"), 
       lty=1:2, cex=0.8 )
yield

yieldvector =c()
for (i in 0:9){
  yieldvector[i+1]=yield[4+11*i]
}
for (i in 0:9){
  yieldvector[i+11]=yield[3+11*i]
}
for (i in 0:9){
  yieldvector[i+21]=yield[6+11*i]
}
for (i in 0:9){
  yieldvector[i+31]=yield[8+11*i]
}
for (i in 0:9){
  yieldvector[i+41]=yield[9+11*i]
}


yieldvector
logyieldv <- c()
yieldmatrix = matrix(yieldvector, nrow=5, byrow=TRUE)    
yieldmatrix

log(yieldmatrix[1,1+8]/yieldmatrix[1,1])

for(i in 1:10)
{
  logyieldv[i] = log(yieldmatrix[1,1+i]/yieldmatrix[1,i])

}
for(i in 1:10)
{
  logyieldv= c(logyieldv,log(yieldmatrix[2,1+i]/yieldmatrix[2,i]))

}
for(i in 1:10)
{
  logyieldv= c(logyieldv,log(yieldmatrix[3,1+i]/yieldmatrix[3,i]))
  
}
for(i in 1:10)
{
  logyieldv= c(logyieldv,log(yieldmatrix[4,1+i]/yieldmatrix[4,i]))
  
}
for(i in 1:10)
{
  logyieldv= c(logyieldv,log(yieldmatrix[5,1+i]/yieldmatrix[5,i]))
  
}
logyieldv
logymatrix=matrix(logyieldv, nrow=9, byrow=TRUE) 
logymatrix
covlogy = cov(logymatrix)


forward
forwardvector =c()
for (i in 0:9){
  forwardvector[i+1]=forward[4+11*i]
}
for (i in 0:9){
  forwardvector[i+11]=forward[3+11*i]
}
for (i in 0:9){
  forwardvector[i+21]=forward[6+11*i]
}
for (i in 0:9){
  forwardvector[i+31]=forward[8+11*i]
}
for (i in 0:9){
  forwardvector[i+41]=forward[9+11*i]
}
forwardvector
forwardmatrix = matrix(forwardvector, nrow=5, byrow=TRUE)
forwardseries <- c()
for(i in 1:10)
{
  forwardseries[i] = log(forwardmatrix[1,1+i]/forwardmatrix[1,i])
  
}

for(i in 1:10)
{
  forwardseries= c(forwardseries,log(forwardmatrix[2,1+i]/forwardmatrix[2,i]))
  
}
for(i in 1:10)
{
  forwardseries= c(forwardseries,log(forwardmatrix[3,1+i]/forwardmatrix[3,i]))
  
}
for(i in 1:10)
{
  forwardseries= c(forwardseries,log(forwardmatrix[4,1+i]/forwardmatrix[4,i]))
  
}
for(i in 1:10)
{
  forwardseries= c(forwardseries,log(forwardmatrix[5,1+i]/forwardmatrix[5,i]))
  
}
length(forwardseries)
forwardseriesmatrix=matrix(forwardseries, nrow=5, byrow=TRUE)
covforward=cov(forwardmatrix)
covforward
covlogy
eigen(covforward)
eigen(covlogy)
