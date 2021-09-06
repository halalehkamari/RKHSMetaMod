prediction <- function(X, Xnew, kernel, Dmax, res, res_nbr){
  Kv.new <- Kv_new(X, Xnew, kernel, Dmax)[[1]]
  tetavs <- res[[nbr]]$`Meta-Model`$teta 
  fvnew <- Kv.new%*%t(tetavs) 
  fhatnew <- apply(fvnew,1,sum)
  return(fhatnew)
}