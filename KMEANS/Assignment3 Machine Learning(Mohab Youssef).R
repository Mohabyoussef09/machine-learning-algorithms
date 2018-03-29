#install.packages("plotrix")
require(plotrix)

setwd("D:\\Nile University\\Machine Learning Course\\Machine Learning - Assignment 4")
k=3

read_data = as.matrix(read.table("Data.txt"))
data_length=length(read_data)/2



for(t in 1:100)
{
  print (paste("Initialization  No. ",toString(t)))
  center_points=matrix(0,nrow = 3,ncol = 2)
  distance_matrix=matrix(0,nrow =data_length,ncol = 3 )
  rnk_matrix=matrix(0,nrow = data_length,ncol = 5)
  #choose three centers
  row_no=sample(nrow(read_data),1)
  center_points[1,1]=read_data[row_no,1]
  center_points[1,2]=read_data[row_no,2]
  #new_avg=1000000
  for (i in 2:k)
  {
    #calc distance between center and all points
    for(j in 1:data_length)
    {
      if(read_data[j,1] !=center_points[i-1,1] && read_data[j,2] !=center_points[i-1,2])
      {
        distance_matrix[j,1]=read_data[j,1]
        distance_matrix[j,2]=read_data[j,2]
        distance_matrix[j,3]=sqrt((read_data[j,1]-center_points[i-1,1])^2+(read_data[j,2]-center_points[i-1,2])^2)
      }
      
      
    }
    sort_distance_matrix=distance_matrix[order(distance_matrix[,3], decreasing=TRUE),]
    center_points[i,1]=sort_distance_matrix[1,1]
    center_points[i,2]=sort_distance_matrix[1,2]
    
  }
  
  
  ######## END OF CALCULATING CENTERS
  old_center=matrix(0,nrow = 3,ncol = 2)
  x=TRUE
  z=0
  while(x==TRUE)
  {
    
    #calc rnk
    
    rnk_min_dist=matrix(0,nrow=3,ncol=1)
    new_center=matrix(0,nrow = 3,ncol = 2)
    sum_k1n=0
    sum_k2n=0
    sum_k3n=0
    
    for(i in 1:data_length)
    {
      
      rnk_matrix[i,1]=read_data[i,1]
      rnk_matrix[i,2]=read_data[i,2]
      for(j in 1:k)
      {
        rnk_min_dist[j,1]=sqrt((read_data[i,1]-center_points[j,1])^2 +((read_data[i,2]-center_points[j,2])^2))
      }
      
      minimum_value_class=which.min(rnk_min_dist)
      if(minimum_value_class==1)
      {
        rnk_matrix[i,3]=1
        rnk_matrix[i,4]=0
        rnk_matrix[i,5]=0
        sum_k1n=sum_k1n+1
        new_center[1,1]=new_center[1,1]+read_data[i,1]
        new_center[1,2]=new_center[1,2]+read_data[i,2]
      }
      
      else if (minimum_value_class==2)
      {
        rnk_matrix[i,4]=1
        rnk_matrix[i,3]=0
        rnk_matrix[i,5]=0
        sum_k2n=sum_k2n+1
        new_center[2,1]=new_center[2,1]+read_data[i,1]
        new_center[2,2]=new_center[2,2]+read_data[i,2]
      }
      
      else
      {
        rnk_matrix[i,5]=1
        rnk_matrix[i,3]=0
        rnk_matrix[i,4]=0
        sum_k3n=sum_k3n+1
        new_center[3,1]=new_center[3,1]+read_data[i,1]
        new_center[3,2]=new_center[3,2]+read_data[i,2]
      }
      
      
      
    }
    
    
    new_center[1,1]=new_center[1,1]/sum_k1n
    new_center[1,2]=new_center[1,2]/sum_k1n
    
    new_center[2,1]=new_center[2,1]/sum_k2n
    new_center[2,2]=new_center[2,2]/sum_k2n
    
    new_center[3,1]=new_center[3,1]/sum_k3n
    new_center[3,2]=new_center[3,2]/sum_k3n
    
    diff_center_val=old_center-new_center
    if((all(diff_center_val == 0) && !is.na(all(diff_center_val == 0))) || NaN %in% diff_center_val)
    {
      x=FALSE
    }
    
    #print(diff_center_val)
    old_center=new_center
    center_points=new_center
    
  }
  
  
  final_dist=0 
  #distance_matrix[1:distance_matrix,1,2,3]=0
  distance_matrix=matrix(0,nrow =data_length,ncol = 3 )
  c1=c2=c3=0
  for (j in 1:data_length)
  {
    if(rnk_matrix[j,3]==1)
    {
      dist=sqrt((new_center[1,1]-rnk_matrix[j,1])^2+(new_center[1,2]-rnk_matrix[j,2])^2)
      final_dist=final_dist+dist
      distance_matrix[j,1]=dist
      c1=c1+1
    }
    
    else if(rnk_matrix[j,4]==1)
    {
      dist=sqrt((new_center[2,1]-rnk_matrix[j,1])^2+(new_center[2,2]-rnk_matrix[j,2])^2)    
      final_dist=final_dist+dist
      distance_matrix[j,2]=dist
      c2=c2+1
      
    }
    
    else if(rnk_matrix[j,5]==1)
    {
      dist=sqrt((new_center[3,1]-rnk_matrix[j,1])^2+(new_center[3,2]-rnk_matrix[j,2])^2)    
      final_dist=final_dist+dist
      distance_matrix[j,3]=dist
      c3=c3+1
    }
    
  }
  
  avg=final_dist/data_length 
  max_dist=matrix(0,nrow = k,ncol = 1)
  for(s in 1:k)
  {
    d=distance_matrix[order(distance_matrix[,s], decreasing=TRUE),]
    max_dist[s,1]=d[1,s]
  }
  
  
  if(t==1 || avg<new_avg)
  {
    f_c1=c1
    f_c2=c2
    f_c3=c3
    new_avg=avg
    final_avg=avg
    final_centers=new_center
    final_max_dist=max_dist
    final_rnk_matrix=rnk_matrix
  }
  
  
  
}




#######
print(paste("Minimum Average Distance =",final_avg))
print(paste("Number of points in cluster 1:",sum_k1n))
print(paste("Number of points in cluster 2:",sum_k2n))
print(paste("Number of points in cluster 3:",sum_k3n))
print(paste("Centers of cluster 1:",final_centers[1,1],",",final_centers[1,2]))
print(paste("Centers of cluster 2:",final_centers[2,1],",",final_centers[2,2]))
print(paste("Centers of cluster 3:",final_centers[3,1],",",final_centers[3,2]))

#color each cluster with specific color
rnk_color_matrix=matrix(0,nrow = data_length,ncol = 1)
for(i in 1:data_length)
{
  if(final_rnk_matrix[i,3]==1)
  {
    rnk_color_matrix[i,1]="red"
  }
  
  if(final_rnk_matrix[i,4]==1)
  {
    rnk_color_matrix[i,1]="green"
  }
  
  if(final_rnk_matrix[i,5]==1)
  {
    rnk_color_matrix[i,1]="blue"
  }
}




plot(read_data, xlim=c(-20,40), ylim=c(-10,30),main = "K Means Clustering Algotithm",col=rnk_color_matrix[,1]) 
for(i in 1:k)
{
  points(final_centers[i,1],  final_centers[i,2], pch='x',col="black")
  draw.circle(final_centers[i,1], final_centers[i,2], final_max_dist[i], nv=100)
}







