#DS1=EuStockMarkets$DAX
#DS2=EuStockMarkets$SMI
library(moments)

d=as.data.frame(EuStockMarkets)
DS1=d$DAX
len_DS1=length(DS1)

d=as.data.frame(EuStockMarkets)
DS2=d$SMI
len_DS2=length(DS2)

#mean of DS1
mean_DS1=0

for(i in 1:len_DS1)
{
  mean_DS1=mean_DS1+DS1[i]
  
}
mean_DS1=mean_DS1/len_DS1
#mean_DS1=mean(DS1) -->mean using built in function

#varience of DS1
var_DS1=0
for(i in 1:len_DS1)
{
  var_DS1=var_DS1+(DS1[i]-mean_DS1)^2
}
var_DS1=var_DS1/(len_DS1-1)
#var_DS1=var(DS1) -->variance using built in function



##mean and varience for DS2
#mean of DS2
mean_DS2=0
for(i in 1:len_DS2)
{
  mean_DS2=mean_DS2+DS2[i]
  
}
mean_DS2=mean_DS2/len_DS2
#mean_DS2=mean(DS2)-->mean using built in function

#varience of DS2
var_DS2=0
for(i in 1:len_DS2)
{
  var_DS2=var_DS2+(DS2[i]-mean_DS2)^2
}
var_DS2=var_DS2/(len_DS2-1)
#var_DS2=var(DS2) -->variance using built in function


#mean of both DS1 union DS2

mean_DS1_Ds2=0
for(i in 1:len_DS1)
{
  mean_DS1_Ds2=mean_DS1_Ds2+DS1[i]
}

for(i in 1:len_DS2)
{
  mean_DS1_Ds2=mean_DS1_Ds2+DS2[i]
}

mean_DS1_Ds2=mean_DS1_Ds2/(len_DS1+len_DS2)
#mean_DS1_Ds2 = (len_DS1*mean_DS1 + len_DS2*mean_DS2) / (len_DS1 + len_DS2)

#varience of both DS1 union DS2

var_DS1_Ds2=((len_DS1*var_DS1)+(len_DS2*var_DS2)+(len_DS1*(mean_DS1-mean_DS1_Ds2)^2)+(len_DS2*(mean_DS2-mean_DS1_Ds2)^2))/(len_DS1+len_DS2)

#calc skewness and kurtosis of DS1

#skewness using loops 
skewness_DS1=0
for(i in 1:len_DS1)
{
  skewness_DS1=skewness_DS1+((DS1[i]-mean_DS1)^3)/(sqrt(var_DS1))^3
}
skewness_DS1=skewness_DS1*(len_DS1/((len_DS1-1)*(len_DS1-2)))

#skewness_DS1=skewness(DS1) #skewness using built in function


#kurtosis using loops
kurtosis_DS1=0
for(i in 1:len_DS1)
{
  kurtosis_DS1=kurtosis_DS1+((DS1[i]-mean_DS1)^4)/(sqrt(var_DS1))^4
}
kurtosis_DS1=kurtosis_DS1*((len_DS1*(len_DS1+1))/((len_DS1-1)*(len_DS1-2)*(len_DS1-3)))
#kurtosis_DS1=kurtosis(DS1) #kurtosis using built in function


combined_dataset=as.vector(cbind(DS1,DS2))

hist(combined_dataset,main="Histogram for Combined dataset",xlab="Combined Dataset",probability=TRUE,breaks=seq(min(combined_dataset),max(combined_dataset),l=10))
boxplot(combined_dataset)






