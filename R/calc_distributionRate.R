calc_distributionRate = function(dists, fmv)
{
  data = cbind(dists, fmv)
  res = data[, 1]/lag(data[, 2])
  return(res)
}
