#--------------------------------------------Exercice2-----------------------------------------------------------

Ford_Fulkerson = function (X,A,s,p)
{
  phi = matrix(0,nrow=length(X),ncol=length(X)) #initialisation de la matrice qui va contenir le flot
  V_phi = 0 #initialisation de la valeur du flot
  m=matrix(0,nrow=length(X),ncol=3) #initialisation de la matrice qui va servir pour le marquage
  while(1)
  {
    m[s,2] = Inf  #marquage de la source
    m[s,3] = 1    #marquage de la source
    S=c(s)  #On mets dans S la source
    Sb=setdiff(X,S) # S barre egale a l'ensemble des noeuds sauf ceux contenu dans S
    R1 = A-phi > 0 #Matrice qui contient TRUE si entre i et j il est possible de faire passer le flot sinon FALSE
    R2 = t(phi) > 0 #Matrice qui contient TRUE si le flot est superieure a 0 sinon non
    C = R1 | R2  # Matrice qui contient TRUE si le flot peut aller de i a j ou de j a i(c�d flot[i,j] > 0)
    
    while(length(which(matrix(C[S,Sb]==TRUE,nrow=length(S),ncol=length(Sb)),arr.ind=TRUE)) > 0  )
      #tant qu'on trouve un ou plusieurs arc (i,j) o� le flot peut passer
    {     
      i = S[ which(matrix(C[S,Sb]==TRUE,nrow=length(S),ncol=length(Sb)),arr.ind=TRUE)[1,1] ] #premier i trouv�
      j = Sb[ which(matrix(C[S,Sb]==TRUE,nrow=length(S),ncol=length(Sb)),arr.ind=TRUE)[1,2] ]#premier j trouv�
      
      if( A[i,j] - phi[i,j] > 0) #test pour savoir si le flot passe de i � j 
      {
        m[j,1] = i                                  #On note le pr�d�c�sseur de j 
        m[j,2] = min( m[i,2] , A[i,j] - phi[i,j] )  #On prend la valeur qui limite le plus le passage du flot
        m[j,3] = 1                                  #On note que le flot passe dans le sens de l'arc (i,j)
      }
      else if( phi[j,i] > 0 ) # ou si il passe de j � i
      {
        m[j,1] = i                                  #On note le pr�d�cesseur de j
        m[j,2] = min( m[i,2] , phi[j,i] )           #On prend la valeur qui limite le plus le passage du flot
        m[j,3] = -1                                 #On note que le flot passe dans le sens contraite de (i,j)
      }
      S[length(S)+1]=j # On marque j dans S
      Sb=setdiff(X,S) # On actualise Sb car S a chang�
      if(j == p) # test pour savoir si on est arriv� au puit
      {
        V_phi =+ m[p,2] #si c'est le cas on augmente la valeur du flot et on vas en 47
        break
      }
    }
    if( is.element(p,S) ) # on test si p a �t� marqu� 
    {
      while( j != s ) # si il a �t� marqu� on remonte le chemin parcourue jusqu'a s
      {
        if( m[j,3] == 1 ) # on test si on traverse l'arc dans le bon sens
          phi[ m[j,1] ,  j] = phi[ m[j,1] ,  j] + m[p,2]
        
        else if( m[j,3] == -1) # ou si on traverse l'arc dans le sens contraire
          phi[  j , m[j,1] ] =phi[  j , m[j,1] ]- m[p,2]
        
        j = m[j,1]
      }
    }
    else # si il n'as pas �t� marqu� cela veut dire qu'il n'existe plus de chaine augmentante entre s et p
      return(phi) # donc on renvoie la matrice phi qui contient le flot maximal
  }
}

#--------------------------------------------Exercice3-----------------------------------------------------------
puis_bool = function (A,p)
  {
    result = A
    
    for(i in 1:(p-1))
    {
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
    
    for(p in 2:N ) 
    {
      A_ = A_ + puis_bool(A,p)
    }
    
    for(i in 1:sqrt(length(A_)))
      for(j in 1:sqrt(length(A_)))
        if(A_[i,j] >= 1)
          A_[i,j]=1
    
    return(A_)
  }
  
