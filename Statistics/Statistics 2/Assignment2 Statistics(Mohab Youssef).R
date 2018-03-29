#part 1
die = sample(1:6,size=1000,replace=TRUE)
hist(die,freq=TRUE,main="Histogram for throwing a Die 1000 times",xlab="Die Faces",breaks=seq(min(die),max(die),l=100))

#part 2
die1 = sample(1:6,size=1000,replace=TRUE)
die2 = sample(1:6,size=1000,replace=TRUE)

avg_two_dies=matrix(0,nrow = 1,ncol = 1000)
mean_two_dies=0
for( i in 1:1000)
{
  avg_two_dies[1,i]=(die1[i]+die2[i])/2
  mean_two_dies=mean_two_dies+avg_two_dies[1,i]
  
}

#histogram of the average 
hist(avg_two_dies,freq=TRUE,main="Histogram for average of  throwing two Dies 1000 times",xlab="Die Faces",breaks=seq(min(avg_two_dies),max(avg_two_dies),l=100))
#mean
mean_two_dies=mean_two_dies/1000
#mean_two_dies=mean(avg_two_dies) mean using built in function

#varience
var_two_dies=0
for( i in 1:1000)
{
  var_two_dies=var_two_dies+(avg_two_dies[1,i]-mean_two_dies)^2
}
var_two_dies=var_two_dies/(1000-1)
#var_two_die=var(avg_two_dies)

#part 3

die_matrix=matrix(0,nrow = 10,ncol = 1000)
mean_ten_dies=0
for(i in 1:10)
{
  die_matrix[i,1:1000]=sample(1:6,size=1000,replace=TRUE)
}

avg_ten_dies=matrix(0,nrow = 1,ncol = 1000)

for( i in 1:1000)
{
  for(j in 1:10)
  {
    avg_ten_dies[1,i]=avg_ten_dies[1,i]+(die_matrix[j,i])  
  }
  avg_ten_dies[1,i]=avg_ten_dies[1,i]/10
  mean_ten_dies=mean_ten_dies+avg_ten_dies[1,i] #preparing the mean of 10 dies
  
  
}


hist(avg_ten_dies,freq=TRUE,main="Histogram for average of throwing ten Dies 1000 times",xlab="Die Faces",breaks=seq(min(avg_ten_dies),max(avg_ten_dies),l=100))

#mean of 10 dies
mean_ten_dies=mean_ten_dies/1000
#mean_ten_dies = mean(avg_ten_dies) mean using built in function
#var of 10 dies
var_ten_dies=0
for( i in 1:1000)
{
  var_ten_dies=var_ten_dies+(avg_ten_dies[1,i]-mean_ten_dies)^2
}
var_ten_dies=var_ten_dies/(1000-1)
#var_ten_dies = var(avg_ten_dies)