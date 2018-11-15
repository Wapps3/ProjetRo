
X=c(1,2,3,4,5,6,7)
A=cbind(c(0,1,1,0,1,0,1),c(1,0,1,1,0,1,0),c(1,1,0,1,1,0,1),
        c(0,1,1,0,1,1,1),c(1,0,1,1,0,1,1),c(0,1,0,1,1,0,1),c(1,0,1,1,1,1,0))
A


Welsh_Powell2 = function(X,A){
  
  n = length(X)
  d=rowSums(A)
  s=sort(d,decreasing=TRUE,index.return=TRUE)
  k = 0
  B = s$ix
  
  lignenoncoloree = B
  c=list()
  
  while (length(lignenoncoloree)>0){
    k=k+1
    temp=c()
    
    while(length(B)>0){ 
      temp[length(temp)+1]=B[1]
      lignenoncoloree = setdiff(lignenoncoloree,B[1])
      
      if (is.matrix(A[,temp])){
        B=which(rowSums(A[,temp])==0)
      }
      else{
        B=which(A[,temp]==0)
      }
      
      B=intersect(lignenoncoloree,B)
      c[[k]]=temp
    }
    
    B = lignenoncoloree
  }
  return(c)
}

Welsh_Powell2(X,A)