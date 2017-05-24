cashFlowSimulation = function(I0, L0, D0, R0, C0, sig_S, sig_F, mu_S, mu_F, dist_rate=NULL, dd_rate=NULL, rho=0.5, lambda=0.2, v=0.01, n=1000, delta=1/12, t=3) {
  if(is.matrix(dist_rate)) {
    t = nrow(dist_rate) * delta
    n = ncol(dist_rate)
  }
  #pef = coreActive
  #------------------parameter setup for simulation
  sig_S = 0.08
  mu_S = 0.04
  sig_F = 0.16
  mu_F = 0.10
  rho = 0.4
  lambda = 0.2
  v = 0.025
  n = 10000
  delta=1/12
  t=8
  delta_mu = mu_S - mu_F
  delta_var_S = sig_S^2 - sig_S * sig_F * rho
  delta_var_F = sig_F^2 - sig_S * sig_F * rho
  var_cor = delta_var_F + delta_var_S
  noises = c(S = "S", F = "F")
  cormat = matrix(c(1, rho, rho, 1), nrow=2)



  #------------------historical parameters
  y = 2017
  lastDate = as.Date.character(paste0(y, "-03-31"))

  dists = as_ts(get_series(coreFund, "Distributions_Total", clean  = F, end = lastDate))
  calls =  as_ts(get_series(coreFund, "Calls_Total_Gross", clean = F, end = lastDate))
  undrawn = as_ts(get_series(coreActive, "Undrawn", clean = F, end = lastDate))
  allocations = allocation_dollars[lubridate::year(allocation_dollars$Date) <= y, ]
  hist_weights = as.matrix(allocation_weights[(allocation_weights$Date %in% allocations$Date), c("ILLIQUID", "LIQUID")])
  row.names(hist_weights) = as.character(zoo::as.yearmon(allocations$Date))


  I0 = as.vector((allocations[(nrow(allocations) - 1):nrow(allocations), "ILLIQUID"]/1000000)[, 1])

  L0 = as.vector((allocations[(nrow(allocations) - 1):nrow(allocations), "LIQUID"]/1000000)[, 1])

  C0 = as.vector(undrawn)[(length(undrawn)-1):length(undrawn)]
  #C0 = as.vector(coreActive@PeriodData[(nrow(coreActive@PeriodData) - 1):nrow(coreActive@PeriodData), "Undrawn"])
  #Cm1 = as.numeric(coreFund@PeriodData[nrow(coreFund@PeriodData) - 1, "Undrawn"])

  D0 = as.vector(cumsum(calls)[(length(calls)-1):length(calls)])
  #Dm1 = as.numeric(cumsum(calls)[nrow(calls) - 1,])

  R0 = as.vector(cumsum(dists)[(length(dists)-1):length(dists)])
  #Rm1 = as.numeric(cumsum(dists)[nrow(dists) - 1,])



  #Pm1 = Im1 + Lm1
  P0 = I0 + L0

  #wLm1= Lm1 / Pm1
  wL0 = L0 / P0

  #wIm1 = Im1 / Pm1
  wI0 = I0 / P0

  #wCm1 = Cm1 / Pm1
  wC0 = C0 / P0

  date0 = allocations$Date[nrow(allocations)]
  month0 = lubridate::month(date0)


  #-------------------data, fit, and simulations for distribution rate
  dist_rate = as_ts(get_series(coreActive, "DistributionRate", end = lastDate))
  lam = forecast::BoxCox.lambda(dist_rate)

  #hybrid
  # fit_list = list(arima = forecast::Arima(y = dist_rate, order = c(0, 1, 1), seasonal = c(3, 0, 0), lambda = lam),
  #                 stlm = forecast::stlm(y = dist_rate, s.window = 11, method = "ets", etsmodel = "AAN", lambda = lam),
  #                 net = forecast::nnetar(y = dist_rate, p = 1, P = 3, lambda = lam))

  #single arima
  lam = BoxCox.lambda(dist_rate)
  #lam = NULL

  fit_stlm = stlm(y = dist_rate, s.window = 13, method = "ets", etsmodel = "ZAN", lambda = lam, biasadj = T)
  fit_arima = Arima(y = dist_rate, order = c(0, 1, 1), seasonal = c(3, 0, 0), include.drift = T, lambda = lam, biasadj =T)

  sim_hyb = simulate.hybrid(fit_list = list(arima = fit_arima, stlm = fit_stlm), nahead = t*12, N = n, bootErrs = T)



  # roll_sims = lapply(X = 2008:2014,function(y){
  #                                               rate=window(dist_rate, end = c(y, 12))
  #                                               lam=NULL
  #                                               fit = forecast::Arima(y = rate, order = c(0, 1, 1), seasonal = c(3, 0, 0), lambda = lam)
  #                                               sim1 = forecast::simulate.Arima(fit, nsim = 24, future = T)
  #                                               sims = sapply(1:1000, function(j)forecast::simulate.Arima(fit, nsim = 24, future = T))
  #                                               ts(sims, start = tsp(sim1)[1], end = tsp(sim1)[2], frequency = tsp(sim1)[3])
  #                                            })
  #
  #
  # mean_sims = lapply(roll_sims, function(r)ts(rowMeans(r), start=tsp(r)[1], frequency = tsp(r)[3]))
  #
  # lam=NULL
  #


  sim_dist_rate = as.matrix((1/delta) *  sim_hyb$hybrid$simulations)
  #}


  #-------------------data, fit, and simulations for drawdown rate
  dd_rate = as_ts(get_series(coreActive, "DrawdownRate", end = lastDate))

  fit_dd = forecast::ets(y = window(dd_rate, start = 2006)
                         #lambda = forecast::BoxCox.lambda(window(dd_rate, start= 2006))
                         )
  sim_dd_rate = (1/delta)*sapply(1:n,
                   function(i) forecast::simulate.ets(object = fit_dd, nsim = t*12, future = T, bootstrap = T))

########################
  set.seed(123)
  W = lapply(1:n, function(i) mvtnorm::rmvnorm(n = (t / delta), mean = c(0, 0), sigma = cormat))

  Z = lapply(noises, function(z) matrix(data = NA, nrow = t / delta, ncol = n))

  for(i in 1:length(W)) {
    Z$S[, i] = W[[i]][, 1]
    Z$F[, i] = W[[i]][, 2]
  }



  #L = liquid allocation
  L = matrix(data = 0, nrow = nrow(Z$S) + 2, ncol = ncol(Z$S))
  row.names(L) = as.character(c(zoo::as.yearmon(allocations$Date[(nrow(allocations)-1):nrow(allocations)]),
                                zoo::as.yearmon(time(sim_dist_rate))))
  redemptions = which(lubridate::month(zoo::as.Date(zoo::as.yearmon(row.names(L)))) %in% c(5, 8, 11, 2))
  # wL = matrix(data = 0, nrow = nrow(Z$S) + 1, ncol = ncol(Z$S))
  # row.names(wL) = c(row.names(dist_rate$Actual)[nrow(dist_rate$Actual)], row.names(sim_dist_rate))
  # redemptions = which(lubridate::month(as.Date(row.names(wL))) %in% c(5, 8, 11, 2))

  simulations = array(data = NA_real_,
                      dim = c(nrow(Z$S) + 2, ncol(Z$S), 6),
                      dimnames = list(NULL, NULL, c("I", "L", "C", "P", "D", "R")))

  #I = liquid allocation
  #C = undrawn commitment
  #P = total fund nav
  #L = illiquid weight
  #L = liquid weight
  #C = undrawn commitment weight
  #D = drawdowns
  #R = distributions

  simulations[1:2, , "L"] = replicate(expr = L0, n = ncol(simulations))
  simulations[1:2, , "I"] = replicate(expr = I0, n = ncol(simulations))
  simulations[1:2, , "C"] = replicate(expr = C0, n = ncol(simulations))
  simulations[1:2, , "P"] = replicate(expr = P0, n = ncol(simulations))
  simulations[1:2, , "D"] = replicate(expr = D0, n = ncol(simulations))
  simulations[1:2, , "R"] = replicate(expr = R0, n = ncol(simulations))


  for(h in 1:nrow(sim_dd_rate)) {
    i = h + 2

    #Liquid return
    R_L = (mu_S * delta + sig_S * sqrt(delta) * Z$S[h, ])

    #Illiquid return
    R_I = (mu_F * delta + sig_F * sqrt(delta) * Z$F[h, ])

    #Distributions
    simulations[i, , "R"]  =  simulations[i-1, , "R"]  +  delta * sim_dist_rate[h, ] * simulations[i-1, , "I"]

    #Drawdowns
    simulations[i, , "D"]  =  simulations[i-1, , "D"]  +  delta * sim_dd_rate[h, ] * simulations[i-1, , "C"]

    #Undrawn Capital Commitment
    simulations[i, , "C"]  =  simulations[i-1, , "C"]  +  simulations[i-1, , "P"] * v * delta  -  (simulations[i, , "D"] - simulations[i-1, , "D"])

    #Liquid Allocation
    simulations[i, , "L"]  =  simulations[i-1, , "L"] * (1 + R_L)  -  (simulations[i, , "D"] - simulations[i-1, , "D"])  +  (simulations[i, , "R"] - simulations[i-1, , "R"])

    #Subtract redemptions from liquid allocation
    if(i %in% redemptions){
      simulations[i, , "L"]  =  simulations[i, , "L"]  -  0.25 * lambda * simulations[i-2, , "P"]
    }

    #Illiquid Allocation
    simulations[i, , "I"]  =  simulations[i-1, , "I"] * (1 + R_I)  -   (simulations[i, , "R"] - simulations[i-1, , "R"])  +  (simulations[i, , "D"] - simulations[i-1, , "D"])

    #Total fund NAV
    simulations[i, , "P"]  =  simulations[i, , "L"]  +  simulations[i, , "I"]


    #simulations[i, , "C"] = simulations[i-1, , "C"] + (simulations[i-1, , "P"] * v  - sim_dd_rate[h, ] * simulations[i-1, , "C"]) * delta
    # simulations[i, , "L"] = simulations[i-1, , "L"] + simulations[i-1, , "L"] * (mu_S * delta + sig_S * sqrt(delta) * Z$S[h, ]) -
    #                     sim_dd_rate[h, ] * simulations[i-1, , "C"] * delta +
    #                     sim_dist_rate[h, ] * I[i-1, ] * delta
    # simulations[i, , "L"] = simulations[i-1, , "L"] + simulations[i-1, , "L"] * R_L -
    #                     sim_dd_rate[h, ] * simulations[i-1, , "C"] * delta +
    #                     sim_dist_rate[h, ] * I[i-1, ] * delta
    # I[i, ] = I[i-1, ] + I[i-1, ] * (R_I - sim_dist_rate[h, ] * delta) +
    #   sim_dd_rate[h, ] * simulations[i-1, , "C"] * delta

    # I[i, ] = I[i-1, ] + I[i-1, ] * ((mu_F - sim_dist_rate[h, ]) * delta + sig_F * sqrt(delta) * Z$F[h, ]) +
    #   sim_dd_rate[h, ] * simulations[i-1, , "C"] * delta
  }

  sim_dists = ts(diff(simulations[ , , "R"])[2:(nrow(sim_dist_rate) + 1),],
                 start = tsp(sim_dist_rate)[1], frequency = tsp(sim_dist_rate)[3])
  sim_dist_mean = ts(rowMeans(sim_dists), start = tsp(sim_dists)[1], frequency = tsp(sim_dists)[3])

  sim_illiq = ts(simulations[3:nrow(simulations),,"I"]/simulations[3:nrow(simulations),,"P"], start = tsp(sim_dist_rate)[1], frequency = tsp(sim_dist_rate)[3])
  act_illiq = ts(allocations$ILLIQUID/allocations$TOTAL, start = c(2004, 1), frequency = 12)

  sim_liq = ts(simulations[3:nrow(simulations),,"L"]/simulations[3:nrow(simulations),,"P"], start = tsp(sim_dist_rate)[1], frequency = tsp(sim_dist_rate)[3])
  act_liq = ts(allocations$LIQUID/allocations$TOTAL, start = c(2004, 1), frequency = 12)

  sim_illiq_NAV = ts(simulations[3:nrow(simulations),,"I"], start = tsp(sim_dist_rate)[1], frequency = tsp(sim_dist_rate)[3])
  act_illiq_NAV = ts(allocations$ILLIQUID/1e+06, start = c(2004, 1), frequency = 12)

  sim_liq_NAV = ts(simulations[3:nrow(simulations),,"L"], start = tsp(sim_dist_rate)[1], frequency = tsp(sim_dist_rate)[3])
  act_liq_NAV = ts(allocations$LIQUID/1e+06, start = c(2004, 1), frequency = 12)

  sim_tot_NAV =  ts(simulations[3:nrow(simulations),,"P"], start = tsp(sim_dist_rate)[1], frequency = tsp(sim_dist_rate)[3])
  act_tot_NAV = ts(allocations$TOTAL/1e+06, start = c(2004, 1), frequency = 12)

  sim_dists_cum = ts(simulations[3:nrow(simulations),,"R"], start = tsp(sim_dist_rate)[1], frequency = tsp(sim_dist_rate)[3])
  act_dists_cum = ts(cumsum(dists), start = c(2004, 1), frequency = 12)

  sim_calls_cum = ts(simulations[3:nrow(simulations),,"D"], start = tsp(sim_dist_rate)[1], frequency = tsp(sim_dist_rate)[3])
  act_calls_cum = ts(cumsum(calls), start = c(2004, 1), frequency = 12)

  sim_net_cum = sim_dists_cum - sim_calls_cum
  act_net_cum = act_dists_cum - act_calls_cum



  # plot.sim(sim_illiq_NAV, actual = act_illiq_NAV, levels = c(80, 95), main = "Private Equity NAV Simulation", ylab = "NAV (millions)", xlab="year")
  # plot.sim(sim_liq_NAV, actual = act_liq_NAV, ylim = c(0, 1200),
  #          levels = c(80, 95),
  #          main = "Hedge Fund NAV Simulation",
  #          ylab = "NAV (millions)", xlab="year")
  #
  # plot.sim(sim_dists_cum, actual = act_dists_cum, levels = c(80, 95), main = "Private Equity Distributions Simulation")
  # plot.sim(sim_calls_cum, actual = act_calls_cum, levels = c(80, 95), main = "Private Equity Calls Simulation")
  # plot.sim(sim_net_cum, actual = act_net_cum, levels = c(80, 95), main = "Private Equity Net CF Simulation", ylim = c(-300, 350))
  #
  # plot.sim(sim = sim_dist_rate/12, actual = dist_rate,  levels = c(80, 95))
  #
  # plot.sim(sim_liq, actual = act_liq, levels = c(80, 95), main = "Hedge Fund Weight Simulation", ylab = "weight", xlab="year", ylim = c(0, 1))
  plot.sim(sim_illiq, actual = act_illiq, levels = c(80, 95), main = "Private Equity Weight Simulation", ylab = "weight", xlab="year", ylim = c(0, 1))


  # wI_std = wI
  # wL_std = wL
  #
  # for(i in 2:nrow(wC)) {
  #   wC[i, ] = wC[i-1, ] + (v - wC[i-1, ] * (sim_dist_rate[i-1, ] + wL[i-1, ] * mu_S + (1 - wL[i-1, ]) * mu_F)) * delta -
  #             wC[i-1, ] * sqrt(delta) * (wL[i-1, ] * sig_S * Z$S[i-1, ] + (1 - wL[i-1, ]) * sig_F * Z$F[i-1, ])
  #
  #   wL[i, ] = wL[i-1, ] + (sim_dist_rate[i-1, ] * (1 - wL[i-1, ]) - sim_dd_rate[i-1, ] * wC[i-1, ] + wL[i-1, ] * (1 - wL[i-1, ]) * (delta_mu - wL[i-1, ] * delta_var_S + (1 - wL[i-1, ]) * delta_var_F)) * delta -
  #             wL[i-1, ] * (1 - wL[i-1, ]) * (sig_F * Z$F[i-1, ] - sig_S * Z$S[i-1, ]) * sqrt(delta)
  #
  #
  #   # if(i %in% redemptions){
  #   #   wL[i, ] = wL[i, ] - 0.05 * wL[i - 2, ]
  #   #
  #   # }
  #
  # }
  # wI = 1-wL

  # row.names(wL) = c(row.names(dist_rate$Actual)[nrow(dist_rate$Actual)], row.names(sim_dist_rate))
  #

  # sim_dist_tests = vector(mode = "list", length = 6)
  # names(sim_dist_tests) = 2010:2015

  # sim_dist_tests[[as.character(y)]] = list(mean = sim_dist_mean, fullsim = sim_dists)
  #
  #
  # matplot(simulations[, , "I"]/simulations[, , "P"], type = "l", main = "Illiquid Weight")
  # matplot(simulations[, , "L"]/simulations[, , "P"], type = "l", main = "Liquid Weight")
  # matplot(simulations[, , "C"]/simulations[, , "P"], type = "l", main = "Undrawn Committed Capital Weight")
  #
  # matplot(simulations[, , "I"], type = "l", main = "Illiquid NAV")
  # matplot(simulations[, , "L"], type = "l", main = "Liquid NAV")
  # matplot(simulations[, , "C"], type = "l", main = "Undrawn Commitment")
  # matplot(simulations[, , "P"], type = "l", main = "Total Fund Nav")
  #



 ########################

}
