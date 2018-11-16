X=1:7
A=rbind(c(0,5,8,0,0,0,0),c(0,0,0,4,2,0,0),c(0,0,0,0,5,2,0),
          c(0,0,0,0,0,0,7),c(0,0,0,0,0,0,3),c(0,0,0,0,0,0,3),c(0,0,0,0,0,0,0))
B=rbind(c(0,1,0,0,1,0),c(0,0,0,0,0,0),c(0,0,0,0,0,0),c(0,4,4,0,0,4),c(0,5,5,0,0,0),c(6,0,0,0,0,0))

Ford_Fulkerson = function (X,A,s,p)
{
  phi = matrix(0,nrow=length(X),ncol=length(X))
  V_phi = 0 
  m=matrix(0,nrow=length(X),ncol=3)
  
  while(1)
  {
    
    m[s,2] = Inf
    m[s,3] = 1
    
    S=c(s)
    Sb=setdiff(X,S)

    R1 = A-phi > 0
    R2 = t(phi) > 0
    C = R1 | R2
          
    while(length(which(matrix(C[S,Sb]==TRUE,nrow=length(S),ncol=length(Sb)),arr.ind=TRUE)) > 0  )
    {     
          i = S[ which(matrix(C[S,Sb]==TRUE,nrow=length(S),ncol=length(Sb)),arr.ind=TRUE)[1,1] ]
          
          j = Sb[ which(matrix(C[S,Sb]==TRUE,nrow=length(S),ncol=length(Sb)),arr.ind=TRUE)[1,2] ]
          
          if( A[i,j] - phi[i,j] > 0)
          {
            m[j,1] = i
            m[j,2] = min( m[i,2] , A[i,j] - phi[i,j] )
            m[j,3] = 1
          }
          else if( phi[j,i] > 0 )
          {
            
            m[j,1] = i
            m[j,2] = min( m[i,2] , phi[j,i] )
            m[j,3] = -1
            
          }

          S[length(S)+1]=j
         
          Sb=setdiff(X,S)
         
          
          if(j == p)
          {
            V_phi =+ m[p,2]
            break
          }
            
        
      }

      if( is.element(p,S) )
      {
        while( j != s )
        {
          if( m[j,3] == 1 )
            phi[ m[j,1] ,  j] = phi[ m[j,1] ,  j] + m[p,2]
          
          else if( m[j,3] == -1)
            phi[  j , m[j,1] ] =phi[  j , m[j,1] ]- m[p,2]
          
          j = m[j,1]
          
        }
        
      }
      else
        return(phi)
  }
}


phi = Ford_Fulkerson(X,A,1,7)
phi
phi = Ford_Fulkerson(1:6,B,4,2)
phi
