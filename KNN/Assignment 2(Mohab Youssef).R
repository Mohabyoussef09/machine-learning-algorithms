require("jpeg")
#since cross validation is 80% and 20% so i will take 20% from each chracter in validation so it will be 2 images foe each chracter with a total of 52 images in validation
#note: i did not use functions for making it easy to trace for you without going up and down in the code

train_dir="D:\\Nile University\\Machine Learning Course\\Machine Learning - Assignment 2\\Problem 2 Dataset\\Noise Train"
test_dir="D:\\Nile University\\Machine Learning Course\\Machine Learning - Assignment 2\\Problem 2 Dataset\\Noise Test"

im_class=matrix(0,nrow = 182,ncol = 1)
im = matrix(ncol=144, nrow = 182)

setwd(train_dir)
images = list.files()
i=1
start=97
c=1
for(image in images)
{
  im[i, 1:144]= as.vector(readJPEG(image)) #load all images
  im_class[i,1]=intToUtf8(start)#load class of each image
  i = i+1
  c=c+1
  if(c==8)
  {
    start=start+1
    c=1
  }
}


k_matrix=matrix(0,nrow = 100,ncol=10)
for (i in 1:10)
{
  
  validation_imgs_class=matrix(0,nrow = 52,ncol = 1) #2 images from each character -->total 52 validation images
  validation_imgs=matrix(0,nrow = 52,ncol = 144)
  training_imgs=matrix(0,nrow = 130,ncol = 144)
  training_imgs_class=matrix(0,nrow = 130,ncol = 1)
  distance_matrix=matrix(0,nrow = 52,ncol = 130)
  distance_matrix_class=matrix(0,nrow = 52,ncol = 130)
  
  #generate random numbers for validation to know validation images and get their classes,and fill the training images&classes    
  start=97
  s=1
  s2=1
  for(j in seq(1,182,by=7))
  {
    random_imgs_no=sample.int(7,2)#random 2 numbers from range 1:7
    rand1=random_imgs_no[1]+(j-1)
    rand2=random_imgs_no[2]+(j-1)
    
    validation_imgs[s,1:144]=im[rand1,1:144]
    validation_imgs_class[s,1]=intToUtf8(start)
    validation_imgs[s+1,1:144]=im[rand2,1:144]
    validation_imgs_class[s+1,1]=intToUtf8(start)
    
    
    for(k in j:(j+6))
    {
      
      if(k!=rand1 && k!=rand2)
      {
        training_imgs[s2,1:144]=im[k,1:144]
        training_imgs_class[s2,1]=intToUtf8(start)
        s2=s2+1
      }
    }
    
    s=s+2
    start=start+1
    
  }
  
  
  #calculate distance
  for(j in 1:52)
  {
    d=0
    for(k in 1:130)
    {
      for(m in 1:144)
      {
        d=d+(validation_imgs[j,m]-training_imgs[k,m])^2
      }
      distance_matrix[j,k]=sqrt(d)
      distance_matrix_class[j,k]=training_imgs_class[k]
      d=0
      
    }
  }
  
  #loop with the 100 k , for each one ,52 images were applied to the algorithm
  for (k in 1:100)
  {
    E=0
    c=0
    s=1
    c1=0
    for (j in 1:52)
    {
      
      y=list()
      min_dist=distance_matrix[j,which(distance_matrix[j,1:130] %in% sort(distance_matrix[j,1:130])[1:k])]
      min_dist_index=which(distance_matrix[j,1:130] %in% sort(distance_matrix[j,1:130])[1:k])
      
      for (m in 1:length(min_dist))
      {
        min_dist_class=distance_matrix_class[j,min_dist_index[m]]
        y=c(y,min_dist_class)
      }
      
      l=list()
      
      for(n in 97:122)
      {
        l=c(l,sum(y %in% c(intToUtf8(n))))
      }
      
      
      #check the tie problem if exist
      tie=FALSE
      if(k!=1)
      {
        for (m in 1:length(l))
        {
          if(s!=k)
          {
            if(l[[s]]-l[[m]]==0)
            {
              tie=TRUE
              break
            }  
          }
          
        }
        
      }
      
      flag=0
      if(tie==TRUE)
      {
        
        for(n in 1:k)
        {
          if(flag==0)
          {
            
            
            z=k-1
            y=list()
            min_dist=distance_matrix[j,which(distance_matrix[j,1:130] %in% sort(distance_matrix[j,1:130])[1:z])]
            min_dist_index=which(distance_matrix[j,1:130] %in% sort(distance_matrix[j,1:130])[1:z])
            
            for (m in 1:length(min_dist))
            {
              min_dist_class=distance_matrix_class[j,min_dist_index[m]]
              y=c(y,min_dist_class)
            }
            
            l=list()
            for(n in 97:122)
            {
              l=c(l,sum(y %in% c(intToUtf8(n))))
            }
            
            tie=FALSE
            if(z!=1)
            {
              for (m in 1:length(l))
              {
                if(s!=z)
                {
                  if(l[[s]]-l[[m]]==0)
                  {
                    tie=TRUE
                    flag=1
                    break
                  }  
                }
                
              }
              
            }
            
          }
          
        }
        
      }
      ##end of tie checker
      
      
      #check if error exist or not
      max_p=which.max(l)
      
      if(max_p!=s)
        E=E+1
      
      c1=c1+1
      if(c1==2)
      {
        c1=0
        s=s+1
      }
      
      
    }
    
    k_matrix[k,i]=E
    
  }
  
}

#calculate average matrx k
avg_matrix=matrix(0,nrow = 100,ncol = 1)
for( i in 1:100)
{
  for(k in 1:10)
  {
    avg_matrix[i,1]=avg_matrix[i,1]+k_matrix[i,k]  
  }
  avg_matrix[i,1]=avg_matrix[i,1]/10
}
barplot(t(avg_matrix),names.arg=seq(1,100),xlab="Choice of K",ylab = "Classification Error",main="KNN Cross-validation Classifier Average Error",col="blue")

#get min k to test with it
min_k=which.min(avg_matrix)

#######Start Testing Part
#the same process is done in test with diffrent matrixes of images and some little changes
im_test_class=matrix(0,nrow = 52,ncol = 1)
im_test = matrix(ncol=144, nrow = 52)

setwd(test_dir)
images = list.files()

i=1
start=97
c=1
for(image in images)
{
  im_test[i, 1:144]= as.vector(readJPEG(image))
  im_test_class[i,1]=intToUtf8(start)
  i = i+1
  
  if(c==2)
  {
    start=start+1
    c=0
  }
  c=c+1
}

distance_matrix_test=matrix(0,nrow = 52,ncol = 182)
distance_matrix_class_test=matrix(0,nrow = 52,ncol = 182)
#calculate distance
for(j in 1:52)
{
  d=0
  for(k in 1:182)
  {
    for(m in 1:144)
    {
      d=d+(im_test[j,m]-im[k,m])^2
    }
    distance_matrix_test[j,k]=sqrt(d)
    distance_matrix_class_test[j,k]=im_class[k]
    d=0
    
  }
}



c=0
s=1
c1=0
acc=0
count=list()
for (j in 1:52)
{
  y=list()
  min_dist=distance_matrix_test[j,which(distance_matrix_test[j,1:182] %in% sort(distance_matrix_test[j,1:182])[1:min_k])]
  min_dist_index=which(distance_matrix_test[j,1:182] %in% sort(distance_matrix_test[j,1:182])[1:min_k])
  
  for (m in 1:length(min_dist))
  {
    min_dist_class=distance_matrix_class_test[j,min_dist_index[m]]
    y=c(y,min_dist_class)
  }
  
  l=list()
  for(n in 97:122)
  {
    l=c(l,sum(y %in% c(intToUtf8(n))))
  }
  
  tie=FALSE
  if(min_k!=1)
  {
    for (m in 1:length(l))
    {
      if(s!=min_k)
      {
        if(l[[s]]-l[[m]]==0)
        {
          tie=TRUE
          break
        }  
      }
      
    }
    
  }
  
  flag=0
  if(tie==TRUE)
  {
    
    for(n in 1:min_k)
    {
      if(flag==0)
      {
        z=min_k-1
        y=list()
        min_dist=distance_matrix_test[j,which(distance_matrix_test[j,1:182] %in% sort(distance_matrix_test[j,1:182])[1:z])]
        min_dist_index=which(distance_matrix_test[j,1:182] %in% sort(distance_matrix_test[j,1:182])[1:z])
        
        for (m in 1:length(min_dist))
        {
          min_dist_class=distance_matrix_class_test[j,min_dist_index[m]]
          y=c(y,min_dist_class)
        }
        
        l=list()
        for(n in 97:122)
        {
          l=c(l,sum(y %in% c(intToUtf8(n))))
        }
        
        tie=FALSE
        if(min_k!=1)
        {
          for (m in 1:length(l))
          {
            if(s!=min_k)
            {
              if(l[[s]]-l[[m]]==0)
              {
                tie=TRUE
                flag=1
                break
              }  
            }
            
          }
          
        }
        
      }
      
    }
    
  }
  
  
  max_p=which.max(l)
  
  if(max_p==s)
    c=c+1
  
  c1=c1+1
  if(c1==2)
  {
    count=c(count,c)
    acc=acc+c
    s=s+1
    c1=0
    c=0
  }
  
  
}

#calc the accuracy
acc=(acc/52)*100
acc=format(round(acc, 2)) 
t=paste("KNN Cross-validation 80%-20% Classifier , Accuracy=",acc,"% at k=",min_k)
barplot(t(count),names.arg =LETTERS,xlab="Letters",ylab = "Count",main=t,col="blue")