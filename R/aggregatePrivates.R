aggregatePrivates = function(underlying, stats, net = T){
  i = 1
  while(is.null(underlying[[i]]@Stats[[stats]])){
    i = i + 1
  }
  numCols = ncol(underlying[[i]]@Stats[[stats]])
  colNames = names(underlying[[i]]@Stats[[stats]])

  if(stats == "CashFlows"){
    cf_agg = matrix(unlist(lapply(underlying,
                                  function(v){if(class(v) == "PrivateFund"){v@Stats[[stats]]} else {rep(0, numCols)}}
    )),
    ncol = numCols, byrow = T)
  } else {
    if(net)
      fees = "NetFees"
    else
      fees = "GrossFees"
    cf_agg = matrix(unlist(lapply(underlying,
                                  function(v){if(class(v) == "PrivateFund"){v@Stats[[stats]][fees, ]} else {rep(0, numCols)}}
    )),
    ncol = numCols, byrow = T)
  }


  #row.names(cf_agg) = names(underlying)
  row.names(cf_agg) = names(which(unlist(lapply(underlying, function(u)!is.null(u@Stats[[stats]])))))
  colnames(cf_agg) = colNames

  if(any(as.numeric(row.names(cf_agg)[2:nrow(cf_agg)]) - as.numeric(row.names(cf_agg))[1:(nrow(cf_agg)-1)] > 1)){
    allYears = as.character(as.numeric(row.names(cf_agg)[1]):as.numeric(row.names(cf_agg)[nrow(cf_agg)]))
    missingYears = allYears[which(!(allYears %in% row.names(cf_agg)))]
    for(y in as.numeric(missingYears)){
      yearInsert = matrix(rep(0, numCols), ncol = numCols)
      cf_agg_temp = cf_agg[which(as.numeric(row.names(cf_agg)) > y),]
      if(!is.matrix(cf_agg_temp)){
        cf_agg_temp = matrix(cf_agg_temp, ncol = numCols)
        row.names(cf_agg_temp) = row.names(cf_agg)[nrow(cf_agg)]
      }
      cf_agg = rbind(cf_agg[which(as.numeric(row.names(cf_agg)) < y),], yearInsert)
      row.names(cf_agg)[nrow(cf_agg)] = y
      cf_agg = rbind(cf_agg, cf_agg_temp)
    }
  }

  cf_agg_total = apply(cf_agg, 2, sum, na.rm = T)

  rbind(cf_agg, TOTAL = cf_agg_total)
}
