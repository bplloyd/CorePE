as_ts = function(data, freq=12, dates = NULL, start=NULL, end=NULL){
  # if(is.null(freq)){
  #   freq = switch(xts::periodicity(data)$scale,
  #                 'monthly' = 12,
  #                 'quarterly' = 4,
  #                 'yearly' = 1)
  # }
  if(is.null(dates)){
    dates = as.Date(row.names(data))
  }
  if(is.null(start)){
    start = min(dates)
    start = c(lubridate::year(start), lubridate::month(start))
  }
  if(is.null(end)){
    end = max(dates)
    end = c(lubridate::year(end), lubridate::month(end))
  }
  data = data[1:nrow(data), 1]
  return(ts(data = data,
            start = start,
            end = end,
            frequency = freq))
}

