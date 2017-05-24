simulateCashRate = function(pef, rate="DistributionRate", n=100, fit = NULL, clean = T, t = 5, lambda_bc = F) {

  #dates = as.Date(row.names(pef@PeriodData)[which(!is.na(pef@PeriodData[, rate]))])
  df_rate = na.omit(subset(pef@PeriodData, rep(T, nrow(pef@PeriodData)), rate))

  if(clean){
    df_rate =  PerformanceAnalytics::clean.boudt(df_rate)[[1]]
  }

  ts_rate = as_ts(df_rate)
  delta = 1/tsp(ts_rate)[3]


  if(is.null(fit)){
    if(lambda_bc){
      lambda = forecast::BoxCox.lambda(ts_rate)
    } else {
      lambda = NULL
    }
    fit = forecast::auto.arima(ts_rate,
                               max.p = 5,
                               max.q = 5,
                               max.Q = 5,
                               max.P = 5,
                               max.d = 3,
                               max.D = 3,
                               trace = F,
                               ic = "aicc",
                               stepwise = F, parallel = T, lambda = lambda)
  }
  sim1 = forecast::simulate.Arima(object = fit, nsim = t / delta, future = T)
  date1 = time(sim1)[1]
  year1 = round(date1, 0)
  month1 = (date1 %% 1) / delta
  date1 = Sys.Date()
  lubridate::year(date1) = year1
  lubridate::month(date1) = month1 + 1
  lubridate::day(date1) = 1

  dates = lubridate::add_with_rollback(e1 = date1, months(0:(length(sim1)-1)))

  lubridate::day(dates) = lubridate::days_in_month(dates)

    # lubridate::ceiling_date(as.Date(zoo::index(xts::as.xts(sim1))),
    #                               unit = "months") - 1
  sims = sapply(X = 1:n, function(i) {fcast = forecast::simulate.Arima(object = fit, nsim = t / delta, future = T);
                                      return(ifelse(fcast>=0, fcast, 0))})
  sims = data.frame(sims, row.names = dates)

  actual = data.frame(Actual = xts_rate, row.names = zoo::index(xts_rate))
  colnames(actual)[1] = "Actual"

  list(Actual = actual, Simulations =sims)
}

