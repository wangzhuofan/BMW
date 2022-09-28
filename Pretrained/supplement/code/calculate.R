ls()
rm(list=ls())
result = foreach(i=1:20,.combine=rbind,.packages=c("progress","testarma","gtools")) %dopar%
  {
    data <- generatedata_overlap(i)
    #y <- data[[1]]
    #yna <- data[[2]]
    c1 <- data[[3]]
    c2 <- data[[4]]
    c3 <- data[[5]]
    #if(i<=5){
    # c1na <- ob1[[i]][[1]]
    #c2na <- ob1[[i]][[2]]
    #c3na <- ob1[[i]][[3]]
    #}
    #else{
    c1na <- ob2[[i]][[1]]
    c2na <- ob2[[i]][[2]]
    c3na <- ob2[[i]][[3]]
    #}
    if(ncol(c1na)==1){
      c1na <- cbind(c1na,rep(0,15))
      c1na <- cbind(c1na,rep(0,15))
      c2na <- cbind(c2na,rep(0,200))
      c2na <- cbind(c2na,rep(0,200))
      c3na <- cbind(c3na,rep(0,15))
      c3na <- cbind(c3na,rep(0,15))
    }
    if(ncol(c1na)==2){
      c1na <- cbind(c1na,rep(0,15))
      c2na <- cbind(c2na,rep(0,200))
      c3na <- cbind(c3na,rep(0,15))
    }
    if(ncol(c1na)>3){
      c1 <- cbind(c1,rep(0,15))
      c2 <- cbind(c2,rep(0,200))
      c3 <- cbind(c3,rep(0,15))
    }
    c(min_ham(c1,c1na),min_ham(c2,c2na),min_ham(c3,c3na))
  }
mean(result[,1]/15)
sd(result[,1]/15)/sqrt(20)
mean(result[,2]/200)
sd(result[,2]/200)/sqrt(20)
mean(result[,3]/15)
sd(result[,3]/15)/sqrt(20)
t.test2 <- function(m1,m2,s1,s2,n1,n2,m0=0,equal.variance=FALSE)
{
  if( equal.variance==FALSE ) 
  {
    se <- sqrt( (s1^2/n1) + (s2^2/n2) )
    # welch-satterthwaite df
    df <- ( (s1^2/n1 + s2^2/n2)^2 )/( (s1^2/n1)^2/(n1-1) + (s2^2/n2)^2/(n2-1) )
  } else
  {
    # pooled standard deviation, scaled by the sample sizes
    se <- sqrt( (1/n1 + 1/n2) * ((n1-1)*s1^2 + (n2-1)*s2^2)/(n1+n2-2) ) 
    df <- n1+n2-2
  }      
  t <- (m1-m2-m0)/se 
  dat <- c(m1-m2, se, t, 2*pt(-abs(t),df))    
  names(dat) <- c("Difference of means", "Std Error", "t", "p-value")
  return(dat) 
}
