rescale_coefs <- function(coefs,centers,scales){
  # We assume that the center and scale are vectors associated to each variable
  # with the response Y in the last place
  n <- length(coefs)
  p <- length(centers)-1
  scaled_coefs <- t(rep(0,n))
  coef_names <- colnames(coefs)
  colnames(scaled_coefs) <- coef_names
  
  # Intercept
  scaled_coefs[1]=coefs[1]
  for (j in 2:n){
    
    variable_indexes_j=as.integer(unlist(strsplit(coef_names[j], ",")))
    
    aux=rep(0,p)
    for (k in 1:p){
      aux[k] = sum(variable_indexes_j==k)
    }
    
    temp = coefs[j]
    
    for(k in 1:p){
      temp = temp*((-centers[k])^aux[k])
      temp = temp/((scales[k])^aux[k])
    }
    
    scaled_coefs[1]=scaled_coefs[1]+temp
  }
  scaled_coefs[1]=scaled_coefs[1]*scales[p+1]+centers[p+1]
  
  
  # Rest of coefficients
  for (i in 2:n){
    #print("Coeficiente-----------------------------------------------")
    #print(coef_names[i])
    
    # i denotes the coefficient that we want to compute now
    variable_indexes_i=as.integer(unlist(strsplit(coef_names[i], ",")))
    
    for (j in 2:n){
      #print("Consideramos el termino:")
      #print(coef_names[j])
      #print("El cual tiene el siguiente aux:")
      
      temp=0
      
      # this loops over all the coefficients, to find which ones contribute to the scaling of i 
      variable_indexes_j=as.integer(unlist(strsplit(coef_names[j], ",")))
      
      # We can check for each unique variable, if the number of times it appears
      # is at least the same.
      aux=rep(0,p)
      for (k in 1:p){
        aux[k] = sum(variable_indexes_j==k) - sum(variable_indexes_i==k)
      }
      
      #print(aux)
      
      # if all are >=0, then we compute the effect
      if(all(aux>=0)){
        #print("aux valido")
        temp = coefs[j]
        #print("sumamos el beta")
        #print(temp)
        for(k in 1:p){
          if(aux[k]>0){
            #print("añadimos productos y divisiones de la variable")
            #print(k)
            #print("en cantidad de:")
            #print(aux[k])
            temp = temp*((-centers[k])^aux[k])
            temp = temp/((scales[k])^aux[k])
          }
        }
        for (k in 1:p){
          if(k %in% variable_indexes_i & aux[k]>0){
            temp = temp*2^aux[k]
          }
        }
      }
      #print("sumamos todo el temp a este i")
      scaled_coefs[i]=scaled_coefs[i] + temp
      #print(scaled_coefs)
    }
    # Finally we add the common factor for all j:
    scaled_coefs[i]=scaled_coefs[i]*scales[p+1]
    temp_2 <- rep(0,p)
    for(k in 1:p){
      temp_2 = sum(variable_indexes_i==k)
      scaled_coefs[i] = scaled_coefs[i]/(scales[k]^temp_2)
    }
  }
  return(scaled_coefs)
}
