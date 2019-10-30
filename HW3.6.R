# Import input
loca="C:/Users/Duong Hung/OneDrive - University Of Houston/Msds classes materials/Fall semester/Math 6357-01 Linear models"
setwd(loca)
fil_nam="HW3.6.txt"
df =  file(fil_nam, open = "r") #open file
dat=NULL
k=0
name=c('B1','B2','B3')
while (length(oneLine <- readLines(df, n = 1, warn = FALSE)) > 0) {
  num_lis = (strsplit(oneLine, " "))
  num_lis = as.numeric(num_lis[[1]])
  for (i in 1:length(num_lis)){
    dat$brand[k+i]=name[i]
    dat$week[k+i]=num_lis[i]
  }
  k=k+3
} 
close(df)
dat=data.frame(dat)
dat=dat[order(dat$brand),]
a=3
n=5
print(dat)
y_dot=tapply(dat$week, dat$brand, mean)
tapply(dat$week, dat$brand, length)
y2dot=sum(dat$week)
N=length(dat$week)
y2dot_bar=y2dot/N
values=data.matrix(dat)
SSt=0
for (i in 1:N){
  SSt=SSt+(dat$week[i]-y2dot_bar)^2
}

SStreat=0
for (a in 1:a){
  SStreat=SStreat+(y_dot[a]-y2dot_bar)^2
}
SStreat=n*SStreat
SSe=SSt-SStreat
MStreat=SStreat/(a-1)
MSe=SSe/(N-a)
F0=MStreat/MSe
oneway.test(week~brand,data = dat)
print(qf(0.95,a-1,N-a))

for (i in 1:a){
  print(a)
  LL=y_dot[i]-qt(0.975,N-a)*sqrt(MSe/n)
  UL=y_dot[i]+qt(0.975,N-a)*sqrt(MSe/n)
  print(paste(LL,'<',y_dot[i],'<',UL))
}

diff23=y_dot[2]-y_dot[3]
ci23=qt(0.995,N-a)*sqrt(2*MSe/n)
LL23=diff23-ci23
UL23=diff23+ci23
print(paste(LL23,'<',diff23,'<',UL23))

e=dat$week
fv=c(rep(y_dot[1],5),rep(y_dot[2],5),rep(y_dot[3],5))
e=dat$week-fv
qqnorm(e)
qqline(e)

plot(fv,e, main = 'Residual vs Predicted',xlab = 'Predicted',ylab = 'Residuals')
abline(0,0)