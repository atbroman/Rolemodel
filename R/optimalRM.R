optimalRM <- function(xxbig, ysubbig, xx1, xx2, alpha, gamma, p)
{
    ## Check validity of arguments
    if(p <= 0 | p >= 1)
      stop('p should be between 0 and 1')
    if(alpha <= 0 | alpha >= 1)
      stop('alpha should be between 0 and 1')
    if(gamma <= 0 | gamma >= 1)
      stop('gamma should be between 0 and 1')
    if(alpha > gamma)
      stop('alpha should be less than gamma')
      
    c1 <- log(p) - log(1-p)
    c2 <- log(1-gamma) - log(1-alpha)
    c3 <- log(gamma) - log(alpha)
    ind1 <- logical(ncol(xx1))
    ind2 <- logical(ncol(xx1))
    ind3 <- logical(ncol(xx1))
    if(ncol(xx2) == 1)
    {
         for(i in 1:ncol(xx1))
         {
              ind1[i] <- sum(xxbig[,colnames(xx1)[i]]*xxbig[,colnames(xx2)]) == sum(xx1[,i])
              ind2[i] <- c1 + c2*sum(as.numeric(xxbig[,colnames(xx1)[i]] - xxbig[,colnames(xx2)] > 0) - ysubbig > 0) + c3*sum(ysubbig*as.numeric(xxbig[,colnames(xx1)[i]] - xxbig[,colnames(xx2)] > 0)) > 0
         }
    }
    else
    {
         for(i in 1:ncol(xx1))
         {
              ind1[i] <- any(apply(xxbig[,colnames(xx2)], 2, function(x) sum(xxbig[,colnames(xx1)[i]]*x) == sum(xx1[,i])))
              ind2[i] <- any(apply(xxbig[,colnames(xx2)], 2, function(x) 
                   ##comple<-as.numeric(xxbig[,colnames(xx1)[i]]-x>0)
                   c1 + c2*sum(as.numeric(xxbig[,colnames(xx1)[i]] - x > 0) - ysubbig > 0) + c3*sum(ysubbig*as.numeric(xxbig[,colnames(xx1)[i]] - x > 0)) > 0))
         }
    }  
    
    if(sum(ind2)==1)
       for(i in 1:ncol(xx1))
          ind3[i] <- sum(xx1[,i]*xx1[,ind2]) == sum(xx1[,i])
    if(sum(ind2)>1)
       for(i in 1:ncol(xx1))
          ind3[i] <- any(apply(xx1[,ind2], 2, function(x) sum(xx1[,i]*x) == sum(xx1[,i]))) 
    
    
    ind <- (ind1|ind2|ind3)
    return(ind)
}

