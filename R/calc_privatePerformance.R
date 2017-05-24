calc_irr = function(cf,fmv, asOfDate = NULL, per = NULL, useFirstValuation = F)
{
  #library(tvm)
  if(is.null(fmv))
    fmv = xts::xts(rep(0, nrow(cf)), order.by = zoo::index(cf))

  data = cbind(cf, fmv)
  data[, 2] =  zoo::na.locf(data[, 2])

  if(is.null(asOfDate)){
    asOfDate = end(cf)
    if(length(fmv[fmv!=0]) > 0)
      asOfDate = max(end(fmv[fmv!=0]), asOfDate)
  }

  data = data[zoo::index(data) <= asOfDate, ]
  if(!is.null(per))
  {
    perf_start = end(data) %m-% per
    data = data[zoo::index(data) >= perf_start]
  }
  data[nrow(data), 1] = zoo::na.fill(data[nrow(data), 1],0) + zoo::na.fill(data[nrow(data), 2],0)
  if(useFirstValuation){
    data[1, 1] = zoo::na.fill(data[1, 1],0) - zoo::na.fill(data[1, 2], 0)
  }
  cf_unrealized = na.omit(data[, 1])

  result = data.frame(IRR = c(tvm::xirr(cf = cf_unrealized,
                                   d = zoo::index(cf_unrealized),
                                   interval = c(-0.999999, .01))),
                      row.names = asOfDate)

  return(result)
}

calc_multiple = function(dists, calls, fmv, asOfDate = NULL)
{
  library(tvm)

  data =  zoo::na.locf(cbind(cumsum(dists), cumsum(calls), fmv))

  if(is.null(asOfDate)){
    result = (data[, 1] + data[, 3])/data[, 2]
  }
  else{
    result = (data[asOfDate, 1] + data[asOfDate, 3])/data[asOfDate, 2]
  }

  names(result) = "Multiple"

  return(result)
}



calc_privatePerformance = function(cf, fmv, asOfDate = NULL)
{
  if(!is.null(cf)){
    cf = data.frame(Date = as.Date(row.names(cf)), cf)
    cf = tidyquant::as_xts(cf, date_col = Date)
    if(is.null(fmv)){
      fmv = xts::xts(rep(0, nrow(cf)), order.by = zoo::index(cf))
    } else {
      fmv = data.frame(Date = as.Date(row.names(fmv)), fmv)
      fmv = tidyquant::as_xts(fmv, date_col = Date)
    }
    if(is.null(asOfDate)){
      asOfDate = max(end(cf), end(fmv))
    }

    irr_net = as.numeric(calc_irr(cf[, "CashFlows_Net"], fmv, asOfDate))
    irr_gross = as.numeric(calc_irr(cf[, "CashFlows_Gross"], fmv, asOfDate))

    multiple_net = as.numeric(calc_multiple(dists = cf[, "Distributions_Total"],
                                              calls = cf[, "Calls_Total_Net"],
                                              fmv = fmv,
                                              asOfDate))
    multiple_gross = as.numeric(calc_multiple(dists = cf[, "Distributions_Total"],
                                                calls = cf[, "Calls_Total_Gross"],
                                                fmv = fmv,
                                                asOfDate))
    # nav = cf$Valuation[nrow(cf)]
    # dists = cumsum(zoo::na.fill(cf$Distributions_Total, 0))[nrow(cf)]
    #
    # #rollingMultiple_realized_net = as.numeric(dists/(-cumsum(zoo::na.fill(cf$Calls_NetFees, 0))[nrow(cf)]))
    # rollingMultiple_unrealized_net = as.numeric((dists + nav)/(-cumsum(zoo::na.fill(cf$Calls_NetFees, 0))[nrow(cf)]))
    #
    # #rollingMultiple_realized_gross = as.numeric(dists/(-cumsum(zoo::na.fill(cf$Calls_GrossFees, 0))[nrow(cf)]))
    # rollingMultiple_unrealized_gross = as.numeric((dists + nav)/(-cumsum(zoo::na.fill(cf$Calls_GrossFees, 0))[nrow(cf)]))
    #
    df = data.frame(IRR = round(100*c(irr_gross, irr_net), 2),
                    RollingMultiple = round(c(multiple_gross, multiple_net), 2))
    row.names(df) = c("GrossFees", "NetFees")
    #result = list(df)
    #names(result) = asOfDate
    #return(result)
    return(df)
  }
  else{
    return(NULL)
  }
}



