# Import input
loca="C:/Users/Duong Hung/OneDrive - University Of Houston/Msds classes materials/Fall semester/Math 6357-01 Linear models"
setwd(loca)
fil_nam="HW3.3.txt"
df =  file(fil_nam, open = "r") #open file
dat=NULL
k=0
name=c('BO1','BO2')
while (length(oneLine <- readLines(df, n = 1, warn = FALSE)) > 0) {
  num_lis = (strsplit(oneLine, " "))
  num_lis = as.numeric(num_lis[[1]])
  a=length(num_lis)
  for (i in 1:a){
    dat$bo[k+i]=name[i]
    dat$int[k+i]=num_lis[i]
  }
  k=k+a
} 
close(df)

dat=data.frame(dat)
dat=dat[order(dat$bo),]
a=length(num_lis)
n=length(dat$bo)
print(dat)
bo1=dat[(dat$bo=='BO1'),]$int
bo2=dat[(dat$bo=='BO2'),]$int
dif_ord=bo1-bo2
qqnorm(dif_ord)
qqline(dif_ord)