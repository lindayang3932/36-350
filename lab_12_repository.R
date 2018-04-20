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


model_select = function(covariates, responses, cutoff){
  pros.lm = lm(responses ~ covariates)
  indeces = which((summary(pros.lm)$coefficients[, 4]) < 0.0001)
  if (length(indeces) == 0){
    return(vector("integer"))
  }
  return(summary(lm(responses ~ covariates[, indeces]))$coefficients[, 4])
}

run_simulation = function(n_trials, n, p, cutoff){
  df = generate_data(n,p)
  graph = vector("integer")
  for (i in 1:n_trials){
    for (j in 1:length(model_select(df[,1], df[, -1]))){
      graph[ij] = model_select(df[,1], df[, -1])[j]
    }
  }
  hist(graph)
}
run_simulation = function(n_trials, n, p, cutoff){
  df = generate_data(n,p)
  graph = vector("integer")
  for (i in 1:n_trials){
    for (j in 1:length(model_select(df[,1], df[, -1]))){
      graph[ij] = model_select(df[,1], df[, -1])[j]
    }
  }
  return(graph)
}
make_plot = function(datapath){
  df = read.csv(datapath)
  
  
}