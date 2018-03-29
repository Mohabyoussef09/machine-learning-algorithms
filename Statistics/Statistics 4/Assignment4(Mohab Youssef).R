age = c(58, 69, 43, 39, 63, 52, 47, 31, 74, 36)
cholestrol = c(189, 235, 193, 177, 154, 191, 213, 165, 198, 181)
data_matrix=matrix(c(age,cholestrol),nrow = 10,ncol = 2)
d=as.data.frame(data_matrix)
reg = lm(d$V2 ~ d$V1)
b0=reg$coefficients[1]
b1=reg$coefficients[2]

#no1
regression=paste("y=",toString(b0),"+",toString(b1),"X")
print(paste("Regression Line:",regression))

#no2

plot(d$V2 ~ d$V1, data = data_matrix,xlim=range(1:100), ylim = range(100:250), main="Regression line", xlab = "Age", ylab = "Cholestrol Level")
abline(b0,b1)

#no3
res=resid(reg)
plot(d$V1,res,xlab = "Age",ylab = "Residuals", main="Residual Error")
abline(0,0)
#no4
#calc ssx
ssx=0
mean_age=mean(d$V1)
for(i in 1:10)
{
  ssx=ssx+(d$V1[i]-mean_age)^2
}

ssy=0
mean_cholestrol=mean(d$V2)
for(i in 1:10)
{
  ssy=ssy+(d$V2[i]-mean_cholestrol)^2
}


ssxy=0
mean_cholestrol=mean(d$V2)
for(i in 1:10)
{
  ssxy=ssxy+((d$V2[i]-mean_cholestrol)*((d$V1[i]-mean_age)))
}

r=ssxy/(sqrt(ssx)*sqrt(ssy))
r2=r**2
print(paste("Goodness of fit",r2))
print("since r is in range +0.5 so from slide 10 this means that there are weak positive correlation")




#no5
sse=0
#slide 22 since e=y-y^ -->y^=y-e
estimated_val=matrix(0,nrow = 10,ncol = 1)
for(i in 1:10)
{
  estimated_val[i]=d$V2[i]-res[i]
}
for(i in 1:10)
{
  sse=sse+(d$V2[i]-(estimated_val[i]))^2
}

var_error=sse/(10-2)
sd_error=sqrt(var_error)
print(paste("Standard Deviation of Error = ",sd_error))

#no6
sb=sd_error/sqrt(ssx)
alpha=(1-0.95)/2
t = qt(alpha, df=8)
#t=2.30600  # from t distribution table look at value of df/p -->10-2=8 and alpha value (0.025)

#slide 19
from=b1+t*sb
to=b1-t*sb


range_B1=paste("95% Confidence Interval for B1 from",from,"to",to)
print(range_B1)
#confint(o,age,0.95)

#no7

t_calc = (b1 - 0) / sb
t_alpha = qt(0.95, 8)

print(paste("t_calc=",t_calc,", t_alpha=",t_alpha))
if(t_calc<t_alpha)
{
  print ("failed to reject H0")
}else{
  print("Reject H0")
}
# t_calc < t_alpha => failed to reject H0 


#no8

r_alpha = qt(0.975, 8)
r_calc = r * sqrt( (8)/(1-r*r) )
print(paste("r_calc=",r_calc,", r_alpha=",r_alpha))
if(r_calc<r_alpha)
{
  
  print ("we cannot reject the hypothesis")
}else{
  print ("we can reject the hypothesis")
}
# Since r_calc<r_alpha we cannot reject the hypothesis