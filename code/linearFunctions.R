##### packages for the main code
### If these packages are not already installed, they will be automatically installed here. 
packages = c("tidyverse", "gridExtra","readxl","mosaic")
lapply(packages, function(p) if(!p %in% rownames(installed.packages())) install.packages(p))
### loading required R packages and source files
library(tidyverse)
library(gridExtra)
library(readxl)
library(mosaic)

getAIC = function(dat, maxlag = 20) # intended to work with panel with no NA
{
  #browser()
  aic = c()
  m = ncol(dat)
  t = nrow(dat)

  for (p in 1:maxlag)
  {
    laggedMatrix = getDelta(dat, maxLag = p) # calculate the lag variables based on the given p
    mu2 = matrix(data = 0, ncol = t-p, nrow = m)
    for(i in 1:m) # i is variable
    {
      y = matrix(dat[(p+1):t,i]) # get the delta Z_t
      lmtemp = lm(y~laggedMatrix)
      coefCurrentM = coef(lmtemp)
      residCurrentM = y - laggedMatrix %*% coefCurrentM[-1] - coefCurrentM[1] # a vector of length equal to time period minus lag 
      residCurrentM = lmtemp$residuals
      #print(c(p,m, length(residCurrentM), dim(mu2)))
      mu2[i,] = residCurrentM
    }
    aic[p] = log(det((mu2 %*% t(mu2))/(t-p+1))) + 2/(t-p+1)*p*m^2 # seems correct
    #print(aic[p])
  }
  return(which.min(aic))
}

calculateSVARFromData = function(dat, vars = names(dat), p, maxQ=20)
{
  m = ncol(dat)
  t = nrow(dat)
  laggedMatrix = getDelta(dat, maxLag = p) # calculate the lag variables based on the given p
  r = lapply(0:p, diag, x = 1, nrow = m, ncol = m)

  constants = c()
  mu = matrix(data = 0, ncol = t, nrow = m) # changed to have length t (with first p being NA) as of 7/7
  mu2 = matrix(data = 0, ncol = t-p, nrow = m)
  for(i in 1:m) # i is variable
  {
    y = matrix(dat[(p+1):t,i]) # get the delta Z_t
    lmtemp = lm(y~laggedMatrix)
    coefCurrentM = coef(lmtemp)
    residCurrentM = y - laggedMatrix %*% coefCurrentM[-1] - coefCurrentM[1] # a vector of length equal to time period minus lag 
    mu[i,] = c(rep(NA, p), residCurrentM)
    #mu2[i,] = residCurrentM
    constants = c(constants, coefCurrentM[1])
    coefCurrentM = matrix(coefCurrentM[-1], ncol = m, byrow = T) 
    # organized in the colnames of the delta matrix
    
    for (j in 1:p) # j is time period that starts from 0
    {
      r[[j+1]][i,] = coefCurrentM[j,] # the i-th row of R_j is the coef on the i-th variable at time period j  
    }
  }
  
  #aic = log(det((mu2 %*% t(mu2))/t)) + 2/t*p*m^2 # seems correct
  f = calculateFFromR(r, maxQ = maxQ) # Check with rats on beyond j=8 (after list index 9)
  return(list(r = r, f = f, mu = mu, constants = constants, p = p))
}


makestructural = function(svarobj, type = "shortrun") 
  # this gives a_l, i.e., the relationship between the shocks and the outcome 
{
  mu = svarobj$mu
  r = svarobj$r
  f = svarobj$f
  m = nrow(r[[1]])
  
  mu = t(mu)
  sigma = cov(as.matrix(mu[complete.cases(mu),]))  # checks out; added as matrix to handle the m = 1 case
  mu = t(mu)
  mulist = as.list(as.data.frame(mu))
  
  if (type == "shortrun")
  {
    a_0 = t(chol(sigma)) # this is A(0); but need a transposition?? 
    epsilon_t = solve(a_0) %*% mu # is this correct? 
    a_l = lapply(f, function(x) x %*% a_0) # checks out 
    return(list(epsilon_t = epsilon_t, a_l = a_l, a_0 = a_0, sigma = sigma))
  }
  
  if (type == "longrun")
  {
    r_1 = diag(m) - Reduce("+", r[-1])
    lr_cov = solve(r_1) %*% sigma %*% t(solve(r_1)) # long run covariance matrix; omega(1)mu
    a_1_lr = t(chol(lr_cov)) #a_1
    a_0_lr = r_1 %*% a_1_lr
    a_l_lr = lapply(f, function(x) x %*% a_0_lr)
    epsilon_t_lr = solve(a_0_lr) %*% mu # same as in shortrun, except a0 is different; changed to have length t (with first p being NA) as of 7/7
    epslist = lapply(mulist, function(mu) solve(a_0_lr) %*% mu)
    return(list(p = svarobj$p,epsilon_t = epsilon_t_lr, a_l = a_l_lr, a_0 = a_0_lr, a_1 = a_1_lr, sigma = sigma, epslist = epslist))
  }
}

plot_al = function(a_l, pos1 = 1, pos2 = 1, cum = F)
{
  if (cum)
    qplot(x = 0:(length(a_l)-1), 
          y = cumsum(sapply(a_l, function(x) x[pos1,pos2])), geom = "line") + 
    geom_hline(yintercept = 0) +
    labs(x = "time", y = paste0("cumulative shock of epsilon", pos2, " on z", pos1))
  
  else
  qplot(x = 0:(length(a_l)-1), 
        y = sapply(a_l, function(x) x[pos1,pos2]), geom = "line") + 
    geom_hline(yintercept = 0) +
    labs(x = "time", y = paste0("shock of epsilon", pos2, " on z", pos1))
}

get_shocks = function(a, pos1 = 1, pos2 = 1, cum = F, qt = 0.5) # a should be a list of length n, each is a list of maxQ of m*m matrices
{
  compositeShocks = sapply(a, function(x) sapply(x, function(y) y[pos1,pos2]))  # maxQ by n; each row is all the countries at time j
  if(cum)
    compositeShocks = apply(compositeShocks, 2, cumsum)
  if (!is.null(qt))
  {
    qtframe = apply(compositeShocks, 1, quantile, qt)
    return(qtframe)
  }
  return(compositeShocks)
}

get_al = function(a_l, pos1, pos2)
{
  sapply(a_l, function(x) x[pos1, pos2])
}

plot_panel_response = function(a, pos1 = 1, pos2 = 1, cum = F, value = F) #for panel data quantile; a should be a list of length n, each is a list of maxQ of m*m matrices
{
  compositeShocks = sapply(a, function(x) sapply(x, function(y) y[pos1,pos2]))  # maxQ by n; each row is all the countries at time j
  if(cum)
    compositeShocks = apply(compositeShocks, 2, cumsum)
  compositeQt = as.data.frame(t(apply(compositeShocks, 1, quantile, c(0.25,0.5,0.75))))
  compositeQt$period = 0:(length(a[[1]])-1)
  if (value)
    return(compositeQt %>% gather(-period, key = "Quantile", value = "ImpulseResponse"))
  else
    return(compositeQt %>% gather(-period, key = "Quantile", value = "ImpulseResponse") %>% ggplot(aes(x = period, y = ImpulseResponse, color = Quantile))+geom_line())
}


### NOTE: the epsilons should all be the same length, because the code assumes that each panel have the same length (incl. NA around both ends, for unbalanced panel)
### First p epsilons should be missing, plus the missing ones due to unbalancedness. 
panelsvar = function(panel_list, type = "longrun", maxlag, autolag = T, lagschosen = NA, cslagschosen = NA, maxQ)
{
  completeCasesPanel = lapply(panel_list, function(x) x[complete.cases(x),]) # only for calculating AIC
  #browser()
  n = length(panel_list)
  m = ncol(panel_list[[1]])
  t = nrow(panel_list[[1]]) # assumes that the data have same length, potentially with NAs for unbalancedness
  print("now choosing lag for panel")
  if (autolag)
  {
    lagschosen = sapply(completeCasesPanel, getAIC, maxlag)
  }
  
  # estimate svar for individual panels
  panelsvars = mapply(function(dat, lag) calculateSVARFromData(dat, p=lag, maxQ = maxQ), panel_list, lagschosen, SIMPLIFY = F)
  panelstructures = lapply(panelsvars, makestructural, type = type)
  indctryconstants = lapply(panelsvars, function(x) x$constants)
  
  # cross sectional
  csavg = matrix(nrow = t, ncol = m)
  colnames(csavg) = colnames(panel_list[[1]])
  for (i in 1:m)
  {
    currentVarPanel = sapply(panel_list, function(x) x[,i])
    csavg[,i] = apply(currentVarPanel, 1, function(x) if (sum(complete.cases(x))>n*0.6) mean(x, na.rm = T) else NA)
  }
  print("now choosing lag for csavg")
  
  if(autolag)
  {
    cslagschosen = getAIC(as.data.frame(csavg[complete.cases(csavg),]), maxlag = maxlag)
  }
  cssvar = calculateSVARFromData(as.data.frame(csavg), maxQ = maxQ, p = cslagschosen)
  #cssvar = calculateSVARFromData(as.data.frame(csavg), maxQ = maxQ, p = 1)
  csstructural = makestructural(cssvar, type = type)
  csconstants = cssvar$constants
  
  # estimate loadings (need to use matrix epsilon here)
  epsilon_bar = csstructural$epsilon_t
  index_epsbar = which(complete.cases(t(epsilon_bar)))
  lambda_i = lapply(1:n, diag, nrow = m, ncol = m, x = 0)
  for(i in 1:n)
  {
    epsilon_it = panelstructures[[i]]$epsilon_t
    # if (sum(complete.cases(t(epsilon_bar))) > sum(complete.cases(t(epsilon_it)))) # trim bar according to it if bar is longer
    # {
    #   epsilon_bar = epsilon_bar[,complete.cases(t(epsilon_it))]
    #   epsilon_it = epsilon_it[,complete.cases(t(epsilon_it))]
    # }
    # 
    # else # trim it according to bar if it is longer 
    # {
    #   epsilon_it = epsilon_it[,complete.cases(t(epsilon_bar))]
    #   epsilon_bar = epsilon_bar[,complete.cases(t(epsilon_bar))]
    # }
    index_epsit = which(complete.cases(t(epsilon_it)))  # robustified the correlation process, as of 22/10/18
    index_intersect = intersect(index_epsit, index_epsbar)
    if (is.null(dim(epsilon_it)))
      lambda_i[[i]] = cor(epsilon_it[index_intersect], epsilon_bar[index_intersect])
    else
      lambda_i[[i]] = diag(diag(cor(t(epsilon_it)[index_intersect,], t(epsilon_bar)[index_intersect,])))
    epsilon_bar = csstructural$epsilon_t
  }
  
  # epsilon tilde for each country
  indctryepstilde = list()
  for (i in 1:n)
  {
    #indctryepstilde[[i]] = panelstructures[[i]]$epsilon_t - lambda_i[[i]] %*% epsilon_bar  # non list version
    indctryepstilde[[i]] = as.list(as.data.frame(panelstructures[[i]]$epsilon_t - lambda_i[[i]] %*% epsilon_bar))
  }
  
  # estimate the three response
  a_i_bar = list()
  a_i_tilde = list()
  
  for(i in 1:n)
  {
    a_i_bar[[i]] = lapply(panelstructures[[i]]$a_l, function(x) x %*% lambda_i[[i]] )
    a_i_tilde[[i]] = lapply(panelstructures[[i]]$a_l, function(x)  x %*% chol(diag(m) - lambda_i[[i]] %*% t(lambda_i[[i]])))
  }
  
  a_i_composite = lapply(panelstructures, function(x) x$a_l)
  
  return(list(composite = a_i_composite, 
              common = a_i_bar, 
              idio = a_i_tilde, 
              lambda_i = lambda_i,
              epsilon_bar_ls = as.list(as.data.frame(epsilon_bar)), 
              indctryepstilde_ls = indctryepstilde, 
              indctrystructures = panelstructures,
              indctryconstants = indctryconstants, 
              csconstants = csconstants, 
              csstructural = csstructural, 
              lagschosen = lagschosen, 
              cslagschosen = cslagschosen))
}


panelsvarbtsp = function(panel_list, type = "longrun", quantile, lagschosen = NA, cslagschosen = NA, maxQ, levels = display_response_in_levels)
{
  #browser()
  n = length(panel_list)
  m = ncol(panel_list[[1]])
  t = nrow(panel_list[[1]]) # assumes that the data have same length, potentially with NAs for unbalancedness
  
  # estimate svar for individual panels
  panelsvars = mapply(function(dat, lag) calculateSVARFromData(dat, p=lag, maxQ = maxQ), panel_list, lagschosen, SIMPLIFY = F)
  panelstructures = lapply(panelsvars, makestructural, type = type)
  indctryconstants = lapply(panelsvars, function(x) x$constants)
  
  # cross sectional
  csavg = matrix(nrow = t, ncol = m)
  colnames(csavg) = colnames(panel_list[[1]])
  for (i in 1:m)
  {
    currentVarPanel = sapply(panel_list, function(x) x[,i])
    csavg[,i] = apply(currentVarPanel, 1, function(x) if (sum(complete.cases(x))>n*0.6) mean(x, na.rm = T) else NA)
  }
  cssvar = calculateSVARFromData(as.data.frame(csavg), maxQ = maxQ, p = cslagschosen)
  csstructural = makestructural(cssvar, type = type)
  csconstants = cssvar$constants
  
  # estimate loadings (need to use matrix epsilon here)
  epsilon_bar = csstructural$epsilon_t
  index_epsbar = which(complete.cases(t(epsilon_bar)))
  lambda_i = lapply(1:n, diag, nrow = m, ncol = m, x = 0)
  for(i in 1:n)
  {
    epsilon_it = panelstructures[[i]]$epsilon_t
    index_epsit = which(complete.cases(t(epsilon_it)))  # robustified the correlation process, as of 22/10/18
    index_intersect = intersect(index_epsit, index_epsbar)
    if (is.null(dim(epsilon_it)))
      lambda_i[[i]] = cor(epsilon_it[index_intersect], epsilon_bar[index_intersect])
    else
      lambda_i[[i]] = diag(diag(cor(t(epsilon_it)[index_intersect,], t(epsilon_bar)[index_intersect,])))
    epsilon_bar = csstructural$epsilon_t
  }
  
  # estimate the three response
  a_i_bar = list()
  a_i_tilde = list()
  
  for(i in 1:n)
  {
    a_i_bar[[i]] = lapply(panelstructures[[i]]$a_l, function(x) x %*% lambda_i[[i]] )
    a_i_tilde[[i]] = lapply(panelstructures[[i]]$a_l, function(x)  x %*% chol(diag(m) - lambda_i[[i]] %*% t(lambda_i[[i]])))
  }
  a_i_composite = lapply(panelstructures, function(x) x$a_l)
  
  commQuantiles = lapply(1:maxQ, matrix, data = NA, nrow = m, ncol = m)
  compQuantiles = lapply(1:maxQ, matrix, data = NA, nrow = m, ncol = m)
  idioQuantiles = lapply(1:maxQ, matrix, data = NA, nrow = m, ncol = m)
  for(i in 1:m)
  {
    for(j in 1:m)
    {
      compPosQt = get_shocks(a_i_composite, pos1 = i, pos2 = j, qt = quantile, cum = levels) # length Q vector of quantile (median) at each period
      commPosQt = get_shocks(a_i_bar, pos1 = i, pos2 = j, qt = quantile, cum = levels)
      idioPosQt = get_shocks(a_i_tilde, pos1 = i, pos2 = j, qt = quantile, cum = levels)
      for(k in 1:maxQ)
      {
        commQuantiles[[k]][i,j] = commPosQt[k] # only the medians at a period
        compQuantiles[[k]][i,j] = compPosQt[k]
        idioQuantiles[[k]][i,j] = idioPosQt[k]
      }
    }
  }
  return(list(commQuantiles = commQuantiles, compQuantiles = compQuantiles, idioQuantiles = idioQuantiles))
}

