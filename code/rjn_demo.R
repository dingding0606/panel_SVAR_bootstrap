# R CODE FOR PANEL STRUCTURAL VAR ESTIMATION   
#
# REFERENCE:                              
# Pedroni, Peter (2013) "Structural Panel VARs", Econometrics, 1 (2), 180-206.              
#
#contact: ppedroni@williams.edu, Special thanks to Jianing Ren
#
## user input section

working_directory =  "/Users/rjn/OneDrive/Williams/pedroni/r_code_demo" # directory of the source code files and the dataset; outputs will also be stored here
file_name = "pedroni_ppp.xls" # name of the dataset; intended for xls
var_variables = c("lne","lnae") # read variables in order of recursivity if using s-r or l-r id option
second_stage_variables = c() # column name of secondary regressor(s), if any. Leave as empty string, c(), if no secondary regression 
panel_identifier = "country" # name of the unique identifier of panel members
maxVARlag = 10 # the maximum number of lags to be considered in VAR estimation
maxIRsteps = 20 # the value of Q+1, i.e., maximum impulse response horizon
Ione = T # use T if var_variables are in I(1) unit root form (i.e., need to difference); F otherwise
display_response_in_levels = Ione # default value is same as Ione; can overwrite with T/F
structural_id_form = "longrun" # either "longrun" or "shortrun", for Cholesky decomposition of the long run or short run covariance matrix
variable_label = c("lne", "lnae") # variable labels for the graphs. if no input, default as "variable1, variable2, ..."
shock_label = c("Real","Nominal") # shock labels for the graphs. if no input, default as "shock1, shock2, ..."
bootstrap = T # whether bootstrap intervals should be estimated
  nreps = 100 # number of bootstrap iterations
  bootstrap_quantile = 0.5 # which quantile point estimate would you like to see confidence bands around?
  conflevel = c(0.1, 0.9) # desired confidence level for bootstrap
  burnin = 100 # how many iterations before bootstrap kicks in


### perform any data manipulation here, such as taking the log or calculating the nominal exchange rate
setwd(working_directory)
source("linearFunctions.R")
source("genericHelpers.R")
dat = read_xls(file_name)
#dat = dat[complete.cases(dat),] # remove the three NAs in the end
dat = dat %>% mutate(lne = log(ae*cpi/uswpi),
                     lnae = log(ae))

#########################################################################################################

## main source code; do not change
### subsetting data
id_string = unique(dat[[panel_identifier]])
balancedpanel = suppressWarnings(dat %>% 
                                   select(all_of(c(var_variables, panel_identifier))) %>% 
                                   mutate(across(!panel_identifier,as.numeric)))
balancedpanel = split(balancedpanel, balancedpanel[,panel_identifier])
if (Ione)
{
  balancedpanel = lapply(balancedpanel, 
                         function(x) as.data.frame(diff(as.matrix(select(x, -panel_identifier)))))
}

### key variable declaration
n = length(balancedpanel)
m = ncol(balancedpanel[[1]])
bigt = nrow(balancedpanel[[1]]) # will change for unbalanced panel
if(!length(variable_label))
{
  variable_label = paste0("variable", 1:m)
}

if(!length(shock_label))
{
  shock_label = paste0("shock", 1:m)
}

### calculating structural panel svar
#lagschosenbyrats = rev(c(5,1,13,12,12,13,1,1,13,12,1,3,1,12,1,1,3,6,1,1))
#cslagchosenbyrats = 1
realrun = panelsvar(balancedpanel,
                           maxQ = maxIRsteps,
                           maxlag = maxVARlag,
                           type = structural_id_form,
                           autolag = T)

realruncommon = realrun$common
realrunidio = realrun$idio
realruncomp = realrun$composite

### writing the three responses
spreadsheetcomp = c()
spreadsheetcomm = c()
spreadsheetidio = c()
scndary_analysis_comp = c()
scndary_analysis_comm = c()
scndary_analysis_idio = data.frame()

for (i in 1:m)
{
  for (j in 1:m)
  {
    name = paste0("IR",i,j,"_",1:maxIRsteps)
    tempcomp = t(sapply(realruncomp, function(x) sapply(x, function(y) y[i,j]))) # all countries' ijth position for all Q
    tempcomm = t(sapply(realruncommon, function(x) sapply(x, function(y) y[i,j])))
    tempidio = t(sapply(realrunidio, function(x) sapply(x, function(y) y[i,j])))
    
    if (display_response_in_levels)
    {
      tempcomp = t(apply(tempcomp, 1, cumsum))
      tempcomm = t(apply(tempcomm, 1, cumsum))
      tempidio = t(apply(tempidio, 1, cumsum))
    }
   
    colnames(tempcomp) = name
    colnames(tempcomm) = name
    colnames(tempidio) = name
    
    spreadsheetcomp = cbind(spreadsheetcomp, tempcomp)
    spreadsheetcomm = cbind(spreadsheetcomm, tempcomm)
    spreadsheetidio = cbind(spreadsheetidio, tempidio)
    
    rownames(spreadsheetcomm) = id_string
    rownames(spreadsheetcomm) = id_string
    rownames(spreadsheetidio) = id_string
  }
}

write.csv(spreadsheetcomp, "IR_to_composite_shock_R.csv")
write.csv(spreadsheetcomm, "IR_to_common_shock_R.csv")
write.csv(spreadsheetidio, "IR_to_idiosyncratic_shock_R.csv")

### secondary analysis

if (length(second_stage_variables))
{
  # common set of rhs variables: the mean for each country across the entire time period
  rhs = dat %>% group_by_at(vars(panel_identifier)) %>% summarize_all(mean) %>% select(all_of(c(second_stage_variables))) # what to do with NA's in taking the mean?
  lmcomp_kl = lapply(as.data.frame(comp), 
                     function(x){
                       a_kl = data.frame(y = x, rhs)
                       lm(y~.,data = a_kl)
                     })
  lmcomm_kl = lapply(as.data.frame(comm),
                     function(x){
                       a_kl_bar = data.frame(y = x, rhs)
                       lm(y~.,data = a_kl_bar)
                     })
  lmidio_kl = lapply(as.data.frame(idio),
                     function(x){
                       a_kl_tilde = data.frame(y = x, rhs)
                       lm(y~.,data = a_kl_tilde)
                     })
  scndary_analysis_comp = data.frame(t(sapply(lmcomp_kl, coef)), r_sq = sapply(lmcomp_kl, function(x) summary(x)$r.squared))
  scndary_analysis_comm = data.frame(t(sapply(lmcomm_kl, coef)), r_sq = sapply(lmcomm_kl, function(x) summary(x)$r.squared))
  scndary_analysis_idio = data.frame(t(sapply(lmidio_kl, coef)), r_sq = sapply(lmidio_kl, function(x) summary(x)$r.squared))
  
  write.csv(scndary_analysis_comp,"scndary_analysis_comp.csv")
  write.csv(scndary_analysis_comm,"scndary_analysis_comm.csv")
  write.csv(scndary_analysis_idio,"scndary_analysis_idio.csv")
}


### plotting the quantile responses
counter = 1
compplot = list()
commplot = list()
idioplot = list()
for (i in 1:m)
{
  for (j in 1:m)
  {
    compplot[[counter]] = 
      plot_panel_response(realruncomp, pos1 = i, pos2 = j, cum = display_response_in_levels)+
      geom_hline(yintercept = 0)+
      labs(title = paste0("Response of ", variable_label[i]," to Composite ", shock_label[j], " Shock"))
    commplot[[counter]] = 
      plot_panel_response(realruncommon, pos1 = i, pos2 = j, cum = display_response_in_levels)+
      geom_hline(yintercept = 0)+
      labs(title = paste0("Response of ", variable_label[i]," to Common ", shock_label[j], " Shock"))
    idioplot[[counter]] = 
      plot_panel_response(realrunidio, pos1 = i, pos2 = j, cum = display_response_in_levels)+
      geom_hline(yintercept = 0)+
      labs(title = paste0("Response of ", variable_label[i]," to Idiosyncratic ", shock_label[j], " Shock"))
    counter = counter + 1
  }
}

comppane = do.call("arrangeGrob", c(compplot, nrow = m))
ggsave("comppane.png", comppane, width = 7*m, height = 7*(m-1))

commpane = do.call("arrangeGrob", c(commplot, nrow = m))
ggsave("commpane.png", commpane, width = 7*m, height = 7*(m-1))

idiopane = do.call("arrangeGrob", c(idioplot, nrow = m))
ggsave("idiopane.png", idiopane, width = 7*m, height = 7*(m-1))

## bootstrap
if (bootstrap)
{
  # pools from which to resample
  epsbarpool = realrun$epsilon_bar_ls
  epstildepool = realrun$indctryepstilde_ls # specific to country
  
  # constants shared across bootstraps
  csconstants = realrun$csconstants
  indctryconstants = realrun$indctryconstants
  
  # start running
  comm_list_of_qt = list()
  comp_list_of_qt = list()
  idio_list_of_qt = list()
  
  for (j in 1:nreps)
  {
    print(paste0("now running bootstrap iter: ", j))
    # things fixed in each nrep
    epsbar_resample = resample(filter_na(epsbarpool), bigt + burnin)
    fakepanel = list()
    # generate the fake panel
    for (i in 1:n)
    {
      constant_i = lapply(realrun$indctrystructures[[i]]$a_l,
                          function(x) {x %*% solve(realrun$indctrystructures[[i]]$a_0) %*% indctryconstants[[1]]}) # constant from composite
      constant_i = Reduce("+", constant_i)
      epstilde_resample = resample(filter_na(epstildepool[[i]]), bigt + burnin)
      indctrydeltaz = a_times_eps(a = realruncommon[[i]], eps = epsbar_resample, simplify = T) +
        a_times_eps(a = realruncomp[[i]], eps = epstilde_resample, simplify = T) +
        matrix(constant_i, nrow = bigt + burnin, ncol = m, byrow = T)# first q will be missing
      fakepanel[[i]] = as.data.frame(tail(indctrydeltaz, n = bigt)) # take the last 245 of this
    }
    # estimate on the fake panel
    temppanelsvar = panelsvarbtsp(panel_list = fakepanel,
                                  maxQ = maxIRsteps,
                                  quantile = bootstrap_quantile,
                                  lagschosen = realrun$lagschosen,
                                  cslagschosen = realrun$cslagschosen)
    #print(fakepanel[[1]][1,1])
    #print(temppanelsvar$composite[[1]][[1]])
    comm_list_of_qt[[j]] = temppanelsvar$commQuantiles # these are length-nrep lists, each is a list of Q m*m matrix of the median (or desired quantiles) 
    comp_list_of_qt[[j]] = temppanelsvar$compQuantiles
    idio_list_of_qt[[j]] = temppanelsvar$idioQuantiles
  }
  
  # retrieving the results
  
  commConfBandLwr = lapply(1:maxIRsteps, matrix, data = NA, nrow = m, ncol = m)
  commConfBandUpr = lapply(1:maxIRsteps, matrix, data = NA, nrow = m, ncol = m)
  compConfBandLwr = lapply(1:maxIRsteps, matrix, data = NA, nrow = m, ncol = m)
  compConfBandUpr = lapply(1:maxIRsteps, matrix, data = NA, nrow = m, ncol = m)
  idioConfBandLwr = lapply(1:maxIRsteps, matrix, data = NA, nrow = m, ncol = m)
  idioConfBandUpr = lapply(1:maxIRsteps, matrix, data = NA, nrow = m, ncol = m)
  
  for(i in 1:m)
  {
    for(j in 1:m)
    {
      idio_conf_band_pos = get_shocks(idio_list_of_qt, i, j, cum = F, qt = conflevel)
      comp_conf_band_pos = get_shocks(comp_list_of_qt, i, j, cum = F, qt = conflevel)
      comm_conf_band_pos = get_shocks(comm_list_of_qt, i, j, cum = F, qt = conflevel)
      for(k in 1:maxIRsteps)
      {
        commConfBandLwr[[k]][i,j] = comm_conf_band_pos[1,k]
        commConfBandUpr[[k]][i,j] = comm_conf_band_pos[2,k]
        compConfBandLwr[[k]][i,j] = comp_conf_band_pos[1,k]
        compConfBandUpr[[k]][i,j] = comp_conf_band_pos[2,k]
        idioConfBandLwr[[k]][i,j] = idio_conf_band_pos[1,k]
        idioConfBandUpr[[k]][i,j] = idio_conf_band_pos[2,k]
      }
    }
  }
  
  # plotting the bootstrap
  commbootstrappane = list()
  compbootstrappane = list()
  idiobootstrappane = list()
  counter = 1
  
  plotcommbootstrap = function(i,j)
  {
    ggplot()+
      geom_line(aes(x = 1:maxIRsteps, y = get_shocks(realruncommon, pos1 = i, pos2 = j, cum = T), color = paste0("Point Estimate of ", bootstrap_quantile*100, "-th Percentile")))+
      geom_line(aes(x = 1:maxIRsteps, y = get_al(commConfBandLwr, i, j), color = paste0(conflevel[1]*100, "-th Confidence Band")))+
      geom_line(aes(x = 1:maxIRsteps, y = get_al(commConfBandUpr, i, j), color = paste0(conflevel[2]*100, "-th Confidence Band")))+
      scale_color_manual(name = "type", values = c("red","red","black"))+
      labs(x = "Period", y = "Impulse Response",
           title = paste0("Bootstrap of Response of ", variable_label[i], " to Common ", shock_label[j], " Shock"))
  }
  
  plotcompbootstrap = function(i,j)
  {
    ggplot()+
      geom_line(aes(x = 1:maxIRsteps, y = get_shocks(realruncomp, pos1 = i, pos2 = j, cum = T), color = paste0("Point Estimate of ", bootstrap_quantile*100, "-th Percentile")))+
      geom_line(aes(x = 1:maxIRsteps, y = get_al(compConfBandLwr, i, j), color = paste0(conflevel[1]*100, "-th Confidence Band")))+
      geom_line(aes(x = 1:maxIRsteps, y = get_al(compConfBandUpr, i, j), color = paste0(conflevel[2]*100, "-th Confidence Band")))+
      scale_color_manual(name = "type", values = c("red","red","black"))+
      labs(x = "Period", y = "Impulse Response", title = paste0("Bootstrap of Response of ", variable_label[i], " to Composite ", shock_label[j], " Shock"))
  }
  
  plotidiobootstrap = function(i,j)
  {
    ggplot()+
      geom_line(aes(x = 1:maxIRsteps, y = get_shocks(realrunidio, pos1 = i, pos2 = j, cum = T), color = paste0("Point Estimate of ", bootstrap_quantile*100, "-th Percentile")))+
      geom_line(aes(x = 1:maxIRsteps, y = get_al(idioConfBandLwr, i, j), color = paste0(conflevel[1]*100, "-th Confidence Band")))+
      geom_line(aes(x = 1:maxIRsteps, y = get_al(idioConfBandUpr, i, j), color = paste0(conflevel[2]*100, "-th Confidence Band")))+
      scale_color_manual(name = "type", values = c("red","red","black"))+
      labs(x = "Period", y = "Impulse Response", title = paste0("Bootstrap of Response of ", variable_label[i], " to Idiosyncratic ", shock_label[j], " Shock"))
  }
  for (i in 1:m)
  {
    for (j in 1:m)
    {
      commbootstrappane[[counter]] = plotcommbootstrap(i,j)
      compbootstrappane[[counter]] = plotcompbootstrap(i,j)
      idiobootstrappane[[counter]] = plotidiobootstrap(i,j)
      counter = counter+1
    }
  }
  
  ggsave("commbootstrappane.png", do.call("arrangeGrob", c(commbootstrappane, nrow = m)), width = 7*m, height = 7*(m-1))
  ggsave("compbootstrappane.png", do.call("arrangeGrob", c(compbootstrappane, nrow = m)), width = 7*m, height = 7*(m-1))
  ggsave("idiobootstrappane.png", do.call("arrangeGrob", c(idiobootstrappane, nrow = m)), width = 7*m, height = 7*(m-1))
}
