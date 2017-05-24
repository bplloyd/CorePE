#' @include executeSP.R
#' @include abbToStrategy.R
#' @include make_paramString.R

get_privateValuations = function(id=NA, strategy=NA, vintage=NA, active = NA, freq = 'm', multiplier = 1)
{
  # paramString = make_paramString(id, strategy, vintage, active)
  # procString = "usp_get_Valuations"
  # val = executeSP(procString, paramString, schema = "Core")
  # val$Month = as.Date.factor(val$Month)
  # val$Month = lubridate::rollback(lubridate::add_with_rollback(val$Month, months(1)))

  if(!is.na(id)) {
    val = valuations[valuations$Holding_ID == id, , drop = FALSE]
  } else {
    holdings = get_privateHoldings(id = id, strategy = strategy, vintage = vintage, active = active)
    val = valuations[valuations$Holding_ID %in% holdings$Holding_ID, ]
    # if(!is.na(strategy)){
    #   holdings = holdings[(!is.na(holdings$Strategy)) & (holdings$Strategy == strategy), ,drop = FALSE]
    # }
    # if(!is.na(vintage)) {
    #   holdings = holdings[(!is.na(holdings$Vintage)) & (holdings$Vintage == vintage), ,drop = FALSE]
    # }
    # if(!is.na(active)) {
    #   holdings = holdings[(!is.na(holdings$Active)) & (holdings$Active == active), ,drop = FALSE]
    # }
  }


  if(nrow(val)>0)
  {
    val = tidyquant::as_xts_(val, "Month")
    names(val) = c("FMV")

    if(freq == "q")
      val = val[xts::endpoints(val, on = "quarters")]
    else if(freq == "y")
      val = val[xts::endpoints(val, on = "years")]
    # else
    #   val = val[zoo::index(val)]

    #zoo::index(val) = lubridate::ceiling_date(zoo::index(val), "month")-1

    return(as.data.frame(val[zoo::index(val) >= start(val[val != 0])]/multiplier))
  }
  else
    return(NULL)


}

