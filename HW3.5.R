# Import input
loca="C:/Users/Duong Hung/OneDrive - University Of Houston/Msds classes materials/Fall semester/Math 6357-01 Linear models"
setwd(loca)
fil_nam="HW3.5.txt"
df =  file(fil_nam, open = "r") #open file
dat=NULL
k=1
while (length(oneLine <- readLines(df, n = 1, warn = FALSE)) > 0) {
  num_lis = (strsplit(oneLine, " "))
  num_lis = as.numeric(num_lis[[1]])
  for (i in 2:length(num_lis)){
    dat$dens[k]=num_lis[i]
    dat$temp[k]=num_lis[1]
    k=k+1
  }
} 
close(df)
print(dat)

y_dot=tapply(dat$dens, dat$temp, mean)
tapply(dat$dens, dat$temp, length)
y2dot=sum(dat$dens)
N=length(dat$dens)
y2dot_bar=y2dot/N
values=data.matrix(dat)
SSt=0
for (i in 1:N){
  SSt=SSt+(dat$dens[i]-y2dot_bar)^2
}
    
SStreat=0
for (a in 1:4){
  SStreat=SStreat+(y_dot[a]-y2dot_bar)^2
}
SStreat=4.5*SStreat
SSe=SSt-SStreat
MStreat=SStreat/3
MSe=SSe/(N-4)
F0=MStreat/MSe
print(oneway.test(dens~temp,data = dat))

e=dat$dens
e[1:5]=e[1:5]-y_dot[1]
e[6:9]=e[6:9]-y_dot[2]
e[10:14]=e[10:14]-y_dot[3]
e[15:18]=e[15:18]-y_dot[4]
qqnorm(e)
qqline(e)

fv=c(rep(y_dot[1],5),rep(y_dot[2],4),rep(y_dot[3],5),rep(y_dot[4],4))
plot(fv,e, main = 'Residual vs Predicted',xlab = 'Predicted',ylab = 'Residuals')
abline(0,0)
