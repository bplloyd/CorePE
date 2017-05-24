calc_PME = function(cf, fmv, bm, distIsPositive = T){
  if(is.null(fmv)){
    fmv = data.frame(rep(0, nrow(cf)), row.names = row.names(cf))
  }

  if(!is.null(cf)){
    cf = data.frame(Date = as.Date(row.names(cf)), CashFlows_Net = cf$CashFlows_Net)
    #cf = tidyquant::as_xts(cf, date_col = "Date")
    fmv = data.frame(Date = as.Date(row.names(fmv)), FMV = fmv$FMV)
    #fmv = tidyquant::as_xts(fmv, date_col = "Date")
    bm_name = names(bm)
    bm = data.frame(Date = as.Date(row.names(bm)), bm[, 1])
    #names(bm)[2] = bm_name
    #bm = tidyquant::as_xts(bm, date_col = "Date")
    #data = cbind(cf, fmv, bm)
    data = merge(x = cf, y = fmv, by = "Date", all = T)
    data = merge(x = data, y = bm, by = "Date", all = T)
    names(data) = c("Date", "cf", "fmv", "bm")
    data[, "fmv"] = zoo::na.locf(data[, "fmv"], na.rm = F)
    data[, "bm"] = zoo::na.locf(data[, "bm"], na.rm = F)
    data[, "cf"] = zoo::na.fill(data[, "cf"], 0)
    #data = data[(zoo::index(data) >= min(start(cf), start(na.omit(fmv)))) & (zoo::index(data) <= max(end(cf), end(fmv))), ]

    data = data[which((data$Date >= min(min(cf[, "Date"]), min(fmv[!is.na(fmv$FMV), "Date"]))) & (data$Date <= max(max(cf[, "Date"]), max(fmv$Date)))), ]

    #subset(data, Date >= min(min(cf[, "Date"]), min(fmv[!is.na(fmv$FMV), "Date"])) & Date <= max(max(cf[, "Date"]), max(fmv$Date)))


    if(distIsPositive){
      data[data$Date == min(cf$Date), "cf"] =   data[data$Date == min(cf$Date), "cf"] - zoo::na.fill(data[data$Date == min(cf$Date), "fmv"], 0)
      pme = data.frame(Date = data$Date, PME = -data[, "bm"] * cumsum(data[, "cf"] / data[, "bm"]))
    } else {
      data[data$Date == min(cf$Date), "cf"] =  data[data$Date == min(cf$Date), "cf"] + zoo::na.fill(data[data$Date == min(cf$Date), "fmv"], 0)
      pme = data.frame(Date = data$Date, PME = data[, "bm"] * cumsum(data[, "cf"] / data[, "bm"]))
    }
    names(pme)[2] = paste0(bm_name, "_PME")
  } else {
    pme = NULL
  }
  return(data.frame(pme[, 2], row.names = pme$Date))
}

# directAlpha = function(cf, fmv, bm, distIsPositive = T, net=T){
#   if(net){
#     flow = "Net"
#   } else {
#     flow = "Gross"
#   }
#   flow = paste0("CashFlows_", flow)
#   bm_name = names(bm)
#   cf = data.frame(Date = as.Date.character(row.names(cf)), cf)
#   fmv = data.frame(Date = as.Date.character(row.names(fmv)), fmv)
#   bm = data.frame(Date = as.Date.character(row.names(bm)), bm)
#
#   data = merge(x = cf, y = fmv, by = "Date", all = T)
#   data = merge(x = data, y = bm, by = "Date", all = T)
#   data = data[, c("Date", flow, "FMV", bm_name)]
#   data = data[min(min(which(!is.na(data[, flow]))), min(which(!is.na(data[, "FMV"])))):nrow(data),]
#   data[, "FMV"] = zoo::na.locf(data[, "FMV"], na.rm = F)
#   data[, bm_name] = zoo::na.locf(data[, bm_name], na.rm = F)
#   data[, flow] = zoo::na.fill(data[, flow], 0)
# }


