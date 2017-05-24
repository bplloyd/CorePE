calc_drawdownRate = function(calls, commits, lag = T)
{
  data = cbind(calls, commits)
  undrawn = cumsum(data[, 2] - data[, 1])
  if(lag)
    res = data[, 1]/lag(undrawn)
  else
    res = data[, 1]/undrawn

  names(res)[1] = "DrawdownRate"
  return(res)
}
