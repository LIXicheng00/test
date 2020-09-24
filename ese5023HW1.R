## 1. Flow Chart
Print_values<-function(a,b,c){
  if (a>b){
    if(b>c){
      print(paste(a,',',b,',',c))
    }
    else{
      if(a>c){
        print(paste(a,',',c,',',b))
      }
      else{
        print(paste(c,',',a,',',b))
      }
    }
  }
  else{
    if(b>c){
      if(a>c){
        print(paste(c,',',a,',',b))
      }
    }else{
      print(paste(c,',',b,',',a))
    }
  }
}
A<-runif(3,min=1,max=100)
Print_values(A[1],A[2],A[3])







##2. Matrix multiplication
#2.1 
M1<-matrix(runif(50,min = 0,max = 50),nrow = 5,ncol = 10)
M2<-matrix(runif(50,min = 0,max = 50),nrow = 10,ncol = 5)
#2.2
Matrix_multip<-function(a,b){
  rown<-dim(a)[1]
  coln<-dim(b)[2]

  mat_re<-matrix(nrow=rown,ncol = rown)
  for (k in 1:rown) {
    for (i in 1:rown) {
      mat_cal<-0
      for (j in 1:coln) {
      mat_cal<-mat_cal+a[k,j]*b[j,i]  
      }
      mat_re[k,i]<-mat_cal
      
    }
  }
  print(mat_re)
}
Matrix_multip(M1,M2)

## HHHHHHHH
xhhh<-1





## 3. Pascal Triangle

Pascal_triangle<-function(k){
  tri_met<-array(0,dim = c(k,k))
  if(k==1){
    tri_met[1,1]<-1
  }
  else if(k==2){
    tri_met[1,1]<-1
    tri_met[2,]<-1
  }
  else{
    tri_met[1,1]<-1
    tri_met[2,1]<-1
    tri_met[2,2]<-1
    for (Xth_row in 3:k) {
      tri_met[Xth_row,1]<-1
      tri_met[Xth_row,Xth_row]<-1
      for (Xth_col in 2:k-1) {
        #tri_met[Xth_row,Xth_col]<-tri_met[Xth_row-1,Xth_col-1]
         # tri_met[Xth_row-1,Xth_col]
        tri_met[Xth_row,Xth_col]<-1
      }
      
    }
    
  }
  print(tri_met[k,])
}
