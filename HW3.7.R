# Import input
loca="C:/Users/Duong Hung/OneDrive - University Of Houston/Msds classes materials/Fall semester/Math 6357-01 Linear models"
setwd(loca)
fil_nam="HW3.7.txt"
df =  file(fil_nam, open = "r") #open file
dat=NULL
k=0
name=c('B1','B2','B3','B4','B5')
while (length(oneLine <- readLines(df, n = 1, warn = FALSE)) > 0) {
  num_lis = (strsplit(oneLine, " "))
  num_lis = as.numeric(num_lis[[1]])
  for (i in 1:length(num_lis)){
    dat$batch[k+i]=name[i]
    dat$calcium[k+i]=num_lis[i]
  }
  k=k+5
} 
close(df)
dat=data.frame(dat)
dat=dat[order(dat$batch),]
a=5
n=5
print(dat)
y_dot=tapply(dat$calcium, dat$batch, mean)
tapply(dat$calcium, dat$batch, length)
y2dot=sum(dat$calcium)
N=length(dat$calcium)
y2dot_bar=y2dot/N
values=data.matrix(dat)
SSt=0
for (i in 1:N){
  SSt=SSt+(dat$calcium[i]-y2dot_bar)^2
}
SStreat=0
for (i in 1:a){
  SStreat=SStreat+(y_dot[i]-y2dot_bar)^2
}
SStreat=n*SStreat
SSe=SSt-SStreat
MStreat=SStreat/(a-1)
MSe=SSe/(N-a)
F0=MStreat/MSe
oneway.test(calcium~batch,data = dat)
print(qf(0.95,a-1,N-a))
print((MStreat-MSe)/n)

e=dat$calcium
fv=c(rep(y_dot[1],5),rep(y_dot[2],5),rep(y_dot[3],5),rep(y_dot[4],5),rep(y_dot[5],5))
e=dat$calcium-fv
qqnorm(e)
qqline(e)

plot(fv,e, main = 'Residual vs Predicted',xlab = 'Predicted',ylab = 'Residuals')
abline(0,0)

plot(rep(1:5,each=5),e, main = 'Residual vs Batch',xlab = 'Batch',ylab = 'Residuals')
abline(0,0)
