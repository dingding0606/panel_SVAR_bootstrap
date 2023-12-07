getDelta = function(mat, maxLag = 8)
{
  l = list()
  for (i in 1:maxLag)
  {   
    range = (maxLag+1-i):(nrow(mat)-i) # if maxlag is 8, then the first lag should be positions 8 to n-1, the 8th lag should be positions 1 to n-8
    currentDelta = apply(mat, 2, 
                         function(x) x[range])
    colnames(currentDelta) = paste0("Z",1:ncol(mat), "lag", i)
    l[[i]] = currentDelta 
  }
  do.call(cbind, l)
}

calculateFFromR = function(r, maxQ = 20)
{
  m = nrow(r[[1]])
  p = length(r) - 1 # r's index starts with 0, therefore need to minus 1 
  f = lapply(1:(maxQ), matrix, data = 0, nrow = m, ncol = m)
  appendToR = lapply(1:(maxQ-p), matrix, data = 0, nrow = m, ncol = m)
  r = c(r, appendToR)
  
  f[[1]] = solve(r[[1]]) 
  for (j in (1:(maxQ-1))) # when j=0, f_j is I, so we start from j=1 here. 
  {
    for (k in 0:(j-1))
      f[[j+1]] = f[[j+1]] + f[[k+1]] %*% r[[j-k+1]] # adjusting position only in [[]]
  }
  
  return(f)
}



a_times_eps = function(a, eps, simplify = F) # need to use list epsilons here
{
  #browser()
  deltaz = list()
  T = length(eps)
  Q = length(a)-1
  m = nrow(a[[1]])
  #eps = c(eps, replicate(Q-1, matrix(c(NA,NA), nrow = 2), simplify = F))
  for (t in 1:T)
  {
    #print(t)
    if (t-Q>0)
    {
      temp = (mapply(function(x,y) x %*% y, a, eps[t:(t-Q)]))
      if (is.null(dim(temp)))
        deltaz[[t]] = sum(temp)
      else
        deltaz[[t]] = matrix(apply(temp, 1, sum, na.rm = F))  # possibly contains NA, which is fine
    }
    else
    {
      deltaz[[t]] = matrix(rep(NA, m), nrow = m)
    }
    
  }
  if (simplify)
    return (t(do.call(cbind, deltaz)))
  else
    return(deltaz)
}

filter_na = function(epslist)
{
  return (epslist[sapply(epslist, function(x) all(complete.cases(x)))])
}

extractAPosition = function(panel_a, pos1, pos2, j) # for nonlinear step 5, gathers the (pos1, pos2) at step j for all n countries
{
  sapply(panel_a, function(x) x[[j]][pos1,pos2])
}

extractPanelRow = function(panel_data, t)
{
  x = as.matrix(t(sapply(panel_data, function(x) x[t,])))
  if(nrow(x) == 1)
    return(t(x))
  return(x)
}

demeanPanel = function(panel_data)
{
  lapply(panel_data, function(x) apply(x, 2, function(y) y - mean(y)))
}

getGammaArrayPosition = function(gammaArray, pos1, pos2)
{
  sapply(gammaArray, function(x) sapply(x, function(y) y[pos1,pos2]))
}

prepareGammaGradient = function(gamma, z1.grid, z2.grid)
{
  
}
