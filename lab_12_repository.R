generate_data = function(n, p){
  covariates = matrix(1,n,p)
  responses = vector("integer")
  for (i in 1:np){
    covariates[i] = rnorm(1)
  }
  for (i in 1:n){
    responses[i] = rnorm(1)
  }
  return(c(covariates = covariates, responses = responses))
}
hi