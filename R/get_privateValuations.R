#' @include get_privateHoldings.R

get_privateValuations = function(id=NA, strategy=NA, vintage=NA, active = NA, freq = 'm', multiplier = 1){

  if(!is.na(id)) {
    val = valuations[valuations$Holding_ID == id, , drop = FALSE]
  } else {
    holdings = get_privateHoldings(id = id, strategy = strategy, vintage = vintage, active = active)
    val = valuations[valuations$Holding_ID %in% holdings$Holding_ID, ]
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

