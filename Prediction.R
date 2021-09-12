prediction <- function(X, Xnew, kernel, Dmax, res, Err){
  Kv.new <- Kv_new(X, Xnew, kernel, Dmax)[[1]]
  minCritTest <-  which(Err == min(Err, na.rm=TRUE),arr.ind = TRUE)
  l <- dim(Err)[1]
  nbr <- l*(minCritTest[2]-1)+minCritTest[1]
  tetavs <- res[[nbr]]$`Meta-Model`$teta 
  fvnew <- Kv.new%*%t(tetavs) 
  fhatnew <- apply(fvnew,1,sum)
  return(fhatnew)
}
