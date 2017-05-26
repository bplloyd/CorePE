#' @include get_privateHoldings.R

get_privateCashFlows = function(id=NA, strategy=NA, vintage=NA, active = NA, freq = 'm', distIsPositive = T, multiplier = 1){
 
  if(!is.na(id)) {
    cf = cash_flows[cash_flows$Holding_ID == id, , drop = FALSE]
  } else {
    holdings = get_privateHoldings(id = id, strategy = strategy, vintage = vintage, active = active)
    cf = cash_flows[cash_flows$Holding_ID %in% holdings$Holding_ID, ]
  }
  names(cf)[names(cf) == "Effective_Date"] = "Date"


  if(nrow(cf)>0){

    #cf =  dplyr::summarise(dplyr::group_by(cf, Date, Transaction_Description), Amount = sum(Amount))
    cf = data.table::dcast(cf, Date ~  Transaction_Description, fun.aggregate = sum, value.var = "Amount")
    cf_names = names(cf)[2:ncol(cf)]
    cf = tidyquant::as_xts_(cf, date_col = "Date")
    names(cf) = cf_names
    callCols = grep("CapitalCall", names(cf))
    distCols = grep("Distribution", names(cf))
    feeCol = grep("FeesAndExpenses", names(cf))

    if(length(callCols) > 0){
      for(i in 1:length(callCols)){
        if(i==1){
          calls = cf[, callCols[i]]
        } else {
          calls = calls + cf[, callCols[i]]
        }


        #calls = xts::xts(apply(cf[, callCols], 1, sum), order.by = zoo::index(cf))
      }
    } else {
      calls = cf[, 1]
      calls[, ] = 0
    }
    if(length(distCols) > 0){
      #dists = xts::xts(apply(cf[, distCols], 1, sum), order.by = zoo::index(cf))
      for(i in 1:length(distCols)){
        if(i==1){
          dists = cf[, distCols[i]]
        } else {
          dists = dists + cf[, distCols[i]]
        }
      }
    } else {
      dists = cf[, 1]
      dists[, ] = 0
    }

    if(length(feeCol) > 0){
      fees = cf[, feeCol]
    } else {
      fees = cf[, 1]
      fees[, ] = 0
    }

    cf$Calls_Total_Net = calls
    cf$Calls_Total_Gross = calls - fees
    cf$Distributions_Total = dists
    cf$CashFlows_Net = calls - dists
    cf$CashFlows_Gross = calls - dists - fees

    if(distIsPositive)
      cf[, c("CashFlows_Net", "CashFlows_Gross")] = -1*cf[, c("CashFlows_Net", "CashFlows_Gross")]

    if(freq != "d"){
      zoo::index(cf) = lubridate::ceiling_date(zoo::index(cf), "month") - 1
      epts = xts::endpoints(cf, "months")
      num_months = switch(freq, 'm' = 1, 'q' = 3, 'y' = 12)
      if(length(epts)>2){
        cf = xts::xts(apply(cf, 2, function(c)xts::period.sum(c, epts)), order.by = zoo::index(cf)[epts])
        #fillDates = lubridate::ceiling_date(start(cf) %m+% months(0:(lubridate::interval(start(cf), end(cf))/months(1))), "months") - 1

        fillDates = lubridate::ceiling_date(lubridate::add_with_rollback(e1 = start(cf), months(0:(lubridate::interval(start(cf), end(cf))/months(1)))), unit = "m")-1
        temp_cf = xts::xts(replicate(ncol(cf), rep(0, length(fillDates))), order.by = fillDates)
        names(temp_cf) = names(cf)
        temp_cf[zoo::index(cf),] = cf
      }
      else{
        temp_cf = cf
      }

      if(freq != "m"){
        per = switch(freq,
                     "q" = "quarters",
                     "y" = "years"
        )
        epts = xts::endpoints(temp_cf, per)
        if(length(epts)>2)
          cf = xts::xts(apply(temp_cf, 2, function(c) xts::period.sum(c, epts)), order.by = zoo::index(temp_cf)[epts])

        zoo::index(cf) = lubridate::ceiling_date(zoo::index(cf), per) - 1
      }
      else{
        cf = temp_cf
      }
    }
    return(as.data.frame(cf/multiplier))
  } else {
    return(NULL)
  }

}
