get_series = function(pef, nm = "DistributionRate", clean = T, na.rm = T, end = NULL){
  if(is.null(end)){
    ss = rep(T, nrow(pef@PeriodData))
  } else {
    ss = as.Date.character(row.names(pef@PeriodData)) <= end
  }

  res= subset(pef@PeriodData, ss, select = c(nm))

  if(na.rm){
    res = na.omit(res)
  }
  if(clean){
    res = PerformanceAnalytics::clean.boudt(res)[[1]]
  }
  return(as.data.frame(res))
}
