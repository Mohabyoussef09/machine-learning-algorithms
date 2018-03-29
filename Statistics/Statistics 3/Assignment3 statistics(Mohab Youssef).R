d=as.data.frame(EuStockMarkets)
DS=d$DAX

#split the data to two halfs
len_DS=length(DS)
half_data=(len_DS/2)
DS1 = DS[1:half_data]
DS2 = DS[(half_data+1):len_DS]

#mean & variance for the first half of data
mean_first_half = mean(DS1)
variance_first_half = var(DS1)

#mean & variance for the second  half of data
mean_second_half = mean(DS2)
variance_second_half = var(DS2)

#define q=m2-m1
q = mean_second_half-mean_first_half
var_q=variance_first_half/half_data + variance_second_half/half_data
n=length(DS)/2
t = (q - 0) / (sqrt(var_q) / sqrt(n))
t_alpha = qt(1 - 0.025, df=n-1)

if(abs(t) > t_alpha)
{
  print("True")
  print("reject the null hypothsis H0")
}else{
  print("False")
  print("not reject the null hypothsis H0")
}

