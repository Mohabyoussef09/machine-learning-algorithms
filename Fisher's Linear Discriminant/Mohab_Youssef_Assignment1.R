require("jpeg")
require("MASS")

read_images<-function()
{
  setwd("D:\\Nile University\\Machine Learning - Assignment 1\\Assignment 1 Dataset\\Assignment 1 Dataset\\Train")
  images = list.files()
  im = matrix(ncol=144, nrow = 182)
  i=1
  for(image in images)
  {
    im[i, 1:144]= as.vector(readJPEG(image))
    i = i+1
  }
  return(im)
}

w_list= matrix(ncol=144, nrow = 26)
w0_list=matrix(ncol=1, nrow = 26)

start=1
end=7

im=read_images()
for(i in 1:26)
{
  v1=as.vector(matrix(0,nrow=12,ncol=12)) 
  v2=as.vector(matrix(0,nrow=12,ncol=12)) 
  
  for(k in 1:182)
  {
    if(k>=start && k<=end)
    {
      v1=v1+im[k,1:144]
    }
    else
    {
      v2=v2+im[k,1:144]
    }
  }
  m1=v1/7#calc m1
  m2=v2/175#calc m2
  sw=as.vector(matrix(0,nrow=12,ncol=12))
  for(k in 1:182)
  {
    if(k>=start && k<=end)
    {
      sw=sw+((im[k,1:144]-m1)%*%t(im[k,1:144]-m1))
      
    }
    else
    {
      sw=sw+((im[k,1:144]-m2)%*%t(im[k,1:144]-m2))
    }
  }
  
  w=ginv(sw)%*%(m1-m2)
  w_list[i,1:144]=w  #list of w 
  
  w0=(-t(w))%*%((m1+m2)/2)
  w0_list[i, 1] = w0
  
  start=start+7
  end=end+7
  
  
}

#Test Part

setwd("D:\\Nile University\\Machine Learning - Assignment 1\\Assignment 1 Dataset\\Assignment 1 Dataset\\Test")
images = list.files()
im = matrix(ncol=144, nrow = 52)
i=1
for(image in images)
{
  im[i, 1:144] = as.vector(readJPEG(image))
  i = i+1
}

y=matrix(0,nrow = 26,ncol=1)
c=1 
count=0 
i=1
correct_classification=list()
for(n in 1:52)
{
  for(j in 1:26)
  {
    y[j,1]=(t(w_list[j,1:144])%*%im[n,1:144])+w0_list[j]
    
  }
  if(which.max(y)==i)
  {
    count=count+1
  }
  
  if(c==2)
  {
    correct_classification=c(correct_classification,count)
    c=0
    count=0
    i=i+1
    
  }
  
  c=c+1
  
  
}

barplot(t(correct_classification),names.arg =LETTERS,xlab="Letters",ylab = "Count",main="Fisher's Linear Discriminant",col="blue")
