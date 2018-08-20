df_bygrpvar <- function(
  #Generate data frame by grouping variable of a level of analysis By Park/Season, Both Park/Season
  df = NULL, #Data frame. E.g.: env.bp
  gv = NULL, #Grouping variable or "by" variable, e.g. 'Season' (optional)
  gv2 = NULL, #Grouping variable number 2, e.g. 'Park' (optional)
  gv2l = NULL, #Level value of grouping variable number 2, e.g. 'NBG' (optional)
  nother = NULL,
  #Name of the other variable from all of which levels will be included.
  #E.g.: 'both Seasons' or 'JBN only'
  dv = NULL, #Data variables. If transect variables, then select apropriate lf (see below)
  #E.g.:
  # trandatavars <- c('Air Temperature (°C)', 'Relative Humidity (%)', 'Wind Speed (m/s)')
  # quadatavars <- c('Soil Temperature (°C)', 'Soil pH', 'Leaf-litter Depth (cm)')
  lf = NULL #Level field for analysis. E.g.: c('Transect Code', 'Quadrat')
){
  require(dplyr)
  require(tidyr)
  if(!is.null(gv2)) df <- df[df[,gv2]==gv2l,]
  df %>% 
    dplyr::select(lf, x = one_of(gv), one_of(dv)) %>% 
    group_by() %>% 
    unique() %>% 
    dplyr::select(-contains(lf)) %>% 
    gather(variable, value, -x) %>% 
    mutate(title = paste0('By ', gv, ',\n', nother))
}
