# Import input
loca="C:/Users/Duong Hung/OneDrive - University Of Houston/Msds classes materials/Fall semester/Math 6357-01 Linear models"
setwd(loca)
data=read.table("CH06PR18.txt")
#Rename data
names(data)=c('Y','X1','X2','X3','X4')

# Plot the scatter plot of all variables. Comment on the plots.
par(mfrow=c(2,2))
for (i in 1:4){
  plot(data[,i+1],data$Y,xlab = paste('X',i,sep = ""), ylab = 'Y',col='black',pch=14+i)
}
#Fit a regression line to the data. Using the hypothesis testing method to determine the
#final model at the significance level of α =0.05. Provide the parameter estimates, standard
#errors and 95% confidence intervals of the parameters.

#Correlation matrix
cor_mtx=cor(data) 
print(cor_mtx)
#Build matrix X
one_vec=data.frame(c(rep(1,81)))
names(one_vec)='1'
X=data.matrix(cbind(one_vec,data[,2:5]))
#Build matrix Y
Y=data.matrix(data$Y)
#Calculate beta_hat
beta=solve(t(X)%*%X)%*%t(X)%*%Y
print(beta)
#Calculate fitted values
Y_hat=X%*%beta
#Calculate residuals
res=Y-Y_hat
#print(res)
#Calculate matrix H
h_mtx=X%*%solve(t(X)%*%X)%*%t(X)
i_mtx=diag(81)
#residual sum of squares
sse=t(Y)%*%(i_mtx-h_mtx)%*%Y

mse=sse/(81-5)
s=mse^0.5
c_mtx=solve(t(X)%*%X)
#Distribution of beta hat
var_b=mse[1,1]*c_mtx
#standard error for each beta
ses=NULL
for (i in 1:5){
  ses[i]=sqrt(var_b[i,i])
}
print(ses)
sig_lev=0.05
t_val=qt(1-sig_lev/2,81-5)
ci=array(0,c(5,2))
for (i in 1:5){
  ci[i,1]=beta[i]-t_val*ses[i]
  ci[i,2]=beta[i]+t_val*ses[i]
}
print(ci)

fit=lm(Y~X1+X2+X3+X4,data = data)
plot(fit)
confint(fit)
#6.23
x_0=c(1,1,1)
x_1=c(4,6,12)
x_2=c(10,11.5,12.5)
x_3=c(0.1,0,0.32)
x_4=c(80000,120000,340000)
new_dta=matrix(c(x_0,x_1,x_2,x_3,x_4),nrow = 3,ncol = 5)
fit_Y=new_dta%*%beta
print(fit_Y)
new_dta=data.frame(new_dta)
predict(fit,new_dta,interval = 'confidence',level = 0.95)
predict(fit,new_dta,interval = 'prediction',level = 0.95)
