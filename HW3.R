# Import input
loca="C:/Users/Duong Hung/OneDrive - University Of Houston/Msds classes materials/Fall semester/Math 6357-01 Linear models"
setwd(loca)
data=read.table("HW3.4.txt")
names(data)=c('Sham','1h','2h','4h')
data=data.frame(data)
for (i in 1:4){
  print(i)
  print(sum(data[i]))
}
y_dot= colMeans(data)
y2dot=sum(data)
y2dot_bar=y2dot/80
values=data.matrix(data)
SSt=0
for (a in 1:4){
  for (i in 1:20){
    SSt=SSt+(values[i,a]-y2dot_bar)^2
  }
}
SStreat=0
for (a in 1:4){
  SStreat=SStreat+(y_dot[a]-y2dot_bar)^2
}
SStreat=20*SStreat
SSe=SSt-SStreat
MStreat=SStreat/3
MSe=SSe/(80-4)
F0=MStreat/MSe

e=values
for (a in 1:4) {e[,a]=e[,a]-y_dot[a]}
qqnorm(e[,1])
qqline(e[,1])
pdata=NULL
fv=NULL
for (a in 1:4){
  for (i in 1:20){
    index=i+(a-1)*10
    pdata[index]=e[i,a]
    fv[index]=y_dot[a]
  }
}
plot(fv,pdata, main = 'Residual vs Predicted',xlab = 'Predicted',ylab = 'Residuals')
abline(0,0)
