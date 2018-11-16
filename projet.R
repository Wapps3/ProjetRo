  puis_bool = function (A,p)
  {
    result = A
    
    for(i in 1:(p-1))
    {
      print(i)
      result = result %*% A
    }
    
    for(i in 1:sqrt(length(A)))
      for(j in 1:sqrt(length(A)))
        if(result[i,j] >= 1)
          result[i,j]=1
        
    return(result)
  }
  
  ferm_trans = function (A)
  {
    A_ = A
    N = sqrt(length(A))
    
    for(p in 2 )
    {
      A_ = A_ + puis_bool(A,p)
      print(p)
    }
    
    for(i in 1:sqrt(length(A_)))
      for(j in 1:sqrt(length(A_)))
        if(A_[i,j] >= 1)
          A_[i,j]=1
    
    return(A_)
  }
  
  p = 2
  A=rbind(c(0,1,0,0,0),c(1,0,1,0,0),c(0,0,0,1,1),c(0,0,0,0,1),c(1,0,0,0,0))
  
  r=puis_bool(A,2)
  r
  
  A_ = ferm_trans(A)
  A_
