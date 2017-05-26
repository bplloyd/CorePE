cull_data = function(pef, freq)
{
  if(!is.null(pef@CashFlows)){
    commits = data.frame(Date = as.Date(row.names(pef@Commitments)), pef@Commitments)
    cf = data.frame(Date = as.Date(row.names(pef@CashFlows)), pef@CashFlows)
    fmv = data.frame(Date = as.Date(row.names(pef@FMV)), pef@FMV)
    data = merge(x = commits, y = cf, on = "Date", all = T)
    data = merge(x = data, y = fmv, on = "Date", all = T)
    #data$Date = as.Date(data$Date)
    #data = tidyquant::as_xts(data, date_col = Date)

    freq = tolower(freq)

    if(freq != "d")
    {
      per = switch(freq, 'm' = "months", 'q' = "quarters", 'y' = "years")
      num_months = switch(freq, 'm' = 1, 'q' = 3, 'y' = 12)
      #zoo::index(data) = lubridate::ceiling_date(zoo::index(data), per) - 1
      #lubridate::day(zoo::index(data)) = lubridate::days_in_month(zoo::index(data))
      lubridate::day(data$Date) = lubridate::days_in_month(data$Date)
      epts = xts::endpoints(data$Date, per)

      if(length(epts)>2)
      {
        #cf = xts::xts(apply(zoo::na.fill(data[,names(pef@CashFlows)],0), 2, function(c)xts::period.sum(c, epts)), order.by = zoo::index(data)[epts])
        flows = data.frame(Date = data$Date, zoo::na.fill(data[, -which(names(data) %in% c("Date", "FMV"))], 0))
        flows = aggregate(flows[,-which(names(flows)=="Date")], by = list(as.factor(flows$Date)), sum)
        names(flows)[1] = "Date"
        flows$Date = as.Date.factor(flows$Date)

        # commits = xts::period.sum(zoo::na.fill(data[,names(pef@Commitments)],0), epts)
        #
        # names(commits) = names(pef@Commitments)

        fmv = data[epts, c("Date", names(pef@FMV))]
        data = merge(x=flows, y=fmv, by = "Date", all = T)

        fillDates = lubridate::add_with_rollback(e1 = min(data$Date), months(0:(lubridate::interval(min(data$Date), max(data$Date))/months(num_months))))
        lubridate::day(fillDates) = lubridate::days_in_month(fillDates)

        #fillDates = lubridate::ceiling_date(start(data) %m+% months(0:(lubridate::interval(start(data), end(data))/months(num_months))), per) - 1

        if(length(fillDates) != nrow(data)){
          #temp_data = xts::xts(replicate(ncol(data), rep(NA_real_, length(fillDates))), order.by = fillDates)
          data = merge(x=data, y = data.frame(Date = fillDates), by = "Date", all = T)
        }
      }
    }
    data[, c(names(pef@Commitments), names(pef@CashFlows))] = zoo::na.fill(data[, c(names(pef@Commitments), names(pef@CashFlows))],0)
    data[, names(pef@FMV)] = zoo::na.locf(data[, names(pef@FMV)], na.rm = F)

    data$Undrawn = cumsum(data$Commitment - data$Calls_Total_Gross)
    data$DrawdownRate = calc_drawdownRate(calls = data[, "Calls_Total_Gross"],
                                          commits = data[, "Commitment"])
    data$DistributionRate = calc_distributionRate(dists = data[, "Distributions_Total"],
                                                  fmv = data[, "FMV"])

    return(data.frame(data[, -which(names(data)=="Date")], row.names = data$Date))
  } else {
    return(NULL)
  }
}
