
#' @include calc_drawdownRate.R
#' @include calc_distributionRate.R
#' @include get_privateHoldings.R
#' @include get_privateCommitments.R
#' @include get_privateCashFlows.R
#' @include get_privateValuations.R
#' @include calc_privatePerformance.R
#' @include cull_data.R
#' @include calc_PME.R
#' @include captureErrors.R
#' @include aggregatePrivates.R

setOldClass(c("xts"))

#----------------------PrivateFund class definition---------------------------------------------------------
setClass("PrivateFund",
         representation = list(Name = "character",
                                Stats = "list",
                                CashFlows = "ANY",
                                FMV = "ANY",
                                Commitments = "ANY",
                                PeriodData = "ANY",
                                Holdings = "data.frame",
                                PME_Benchmark = "ANY",
                                UnderlyingVintages = "list",
                                UnderlyingStrategies = "list",
                                UnderlyingFunds = "list",
                                InitialCommitment = "numeric",
                                TotalCommitment = "numeric",
                                Vintage =  "numeric",
                                Strategy = "character",
                                Active = "integer",
                                ID = "numeric",
                                Freq = "character",
                                Multiplier = "numeric",
                                ErrorList = "list"
                                ))



#----------------------PrivateFund init function---------------------------------------------------------
setMethod("initialize",
          "PrivateFund",
          function(.Object,
                   name = NA_character_,
                   id = NA_integer_,
                   strategy=NA_character_,
                   vintage=NA_integer_,
                   initialCommitment = NA_real_,
                   totalCommitment = NA_real_,
                   commitments = NULL,
                   cashFlows=NULL,
                   fmv = NULL,
                   periodData = NULL,
                   holdings = NULL,
                   pme_bm = NULL,
                   underlyingVintages = list(),
                   underlyingStrategies = list(),
                   underlyingFunds = list(),
                   freq = "d",
                   dataFreq = "m",
                   multiplier = 1e+06,
                   active = NA_integer_,
                   loadVintages = F,
                   loadStrategies = F,
                   loadFunds = F
          ){

            i = 1

            errorListName = paste0("errorList_", i)
            while(exists(errorListName, envir = globalenv())){
              i = i + 1
              errorListName = paste0("errorList_", i)
            }
            assign(x = errorListName, value = list(), envir = globalenv())

            if(is.na(id)){
              .Object@Vintage = vintage
              .Object@Strategy = strategy
              .Object@ID = id
              .Object@Active = active


              if(is.na(name)){
                if(is.na(vintage) & is.na(strategy))
                  name = "Core"
                else if(!is.na(vintage) & is.na(strategy))
                  name = paste0("Core - ", vintage, " Vintage")
                else if(is.na(vintage) & !is.na(strategy))
                  name = paste0("Core - ", strategy)
                else
                  name = paste0("Core - ", vintage, " ", strategy)
              }

              if(!is.na(active)){
                if(active == 1){
                  name = paste0(name, " (Active Funds Only")
                }
                if(active == 0){
                  name = paste0(name, " (Inactive Funds Only")
                }
              }
            } else {
              .Object@ID = id


              #holdings = get_privateHoldings(id = id, strategy = strategy, vintage = vintage, active = active)
              holdings = captureErrors(func = get_privateHoldings,
                                       captureList = errorListName,
                                       default = NULL,
                                       id = id,
                                       strategy = strategy,
                                       vintage = vintage,
                                       active = active)


              .Object@Vintage = as.integer(holdings$Vintage)
              .Object@Strategy = as.character(holdings$Strategy)
              .Object@Active = as.integer(holdings$Active)
              .Object@InitialCommitment = as.numeric(holdings$Initial_Commitment_USD)/multiplier


              if(is.na(name))
                name = as.character(holdings$Holding_Name[1])
            }

            if(is.null(holdings))
            {
              #holdings = get_privateHoldings(id = id, strategy = strategy, vintage = vintage, active = active)
              holdings = captureErrors(func = get_privateHoldings,
                                       captureList = errorListName,
                                       default = NULL,
                                       id = id,
                                       strategy = strategy,
                                       vintage = vintage,
                                       active = active)

            }

            if(is.null(commitments))
            {
              #commitments = get_privateCommitments(id = id, strategy = strategy, vintage = vintage, active = active, freq = freq, multiplier = multiplier)
              commitments = captureErrors(func = get_privateCommitments,
                                          captureList = errorListName,
                                          default = NULL,
                                          id = id,
                                          strategy = strategy,
                                          vintage = vintage,
                                          active = active,
                                          freq = freq,
                                          multiplier = multiplier)

            }

            if(is.na(totalCommitment)){
              if(!is.null(commitments))
                totalCommitment = sum(commitments, na.rm = T)
              else
                totalCommitment = NA_real_
            }

            if(is.na(initialCommitment)){
              if((!is.null(holdings)) & (nrow(holdings) > 0))
                initialCommitment = sum(holdings$Initial_Commitment_USD, na.rm = T)/multiplier
              else
                initialCommitment = NA_real_
            }

            if(is.null(cashFlows)){
              #cashFlows = get_privateCashFlows(id = id, strategy = strategy, vintage = vintage, active = active, freq = freq, distIsPositive = T, multiplier = multiplier)
              cashFlows = captureErrors(func = get_privateCashFlows,
                                        captureList = errorListName,
                                        default = NULL,
                                        id = id,
                                        strategy = strategy,
                                        vintage = vintage,
                                        active = active,
                                        freq = freq,
                                        multiplier = multiplier,
                                        distIsPositive = T)

            }

            if(is.null(fmv)){
              #fmv = get_privateValuations(id = id, strategy = strategy, vintage = vintage, active = active, freq = freq, multiplier = multiplier)
              fmv = captureErrors(func = get_privateValuations,
                                  captureList = errorListName,
                                  default = NULL,
                                  id = id,
                                  strategy = strategy,
                                  vintage = vintage,
                                  active = active,
                                  freq = freq,
                                  multiplier = multiplier)


            }
            if(is.null(fmv)){
              fmv_end = 0
            } else {
              fmv_end = fmv[nrow(fmv),]
            }

            if(is.null(pme_bm)){
              pme_bm = as.data.frame(loadIndices("SPTR"))
            }

            # pme = calc_PME(cashFlows[, "CashFlows_Net"], fmv = fmv, bm = publicBM)
            # commit = sum(commitments)
            # called_net = sum(cashFlows[, "Calls_Total_Net"])
            # called_gross = sum(cashFlows[, "Calls_Total_Gross"])
            # dists = sum(cashFlows[, "Distributions_Total"])
            # fmv_end = fmv[nrow(fmv)]
            #
            # perf = try(calc_privatePerformance(cashFlows, fmv))
            # pme_perf = try(calc_privatePerformance(cashFlows, pme))
            #
            # cf_stats = data.frame(Net = c(commit, called_net)
            #                       )
            #
            #
            # colnames(cf_stats)[2:3] = c("Called_w_Fees", "Called_wo_Fees")
            #
            # stats = list(Performance = try(calc_privatePerformance(cashFlows, fmv)),
            #              PME_Performance = try(calc_privatePerformance(cashFlows, pme)),
            #              CashFlows = try(cf_stats))

            if(!is.na(totalCommitment)){
              if(is.null(cashFlows)){
                called_net = 0
                called_gross = 0
                dists = 0
                pme = NULL
                pme_perf = NULL
                perf = NULL
              } else {
                #pme = calc_PME(cashFlows[, "CashFlows_Net"], fmv = fmv, bm = publicBM)


                pme =  captureErrors(func = calc_PME,
                                     captureList = errorListName,
                                     default = NULL,
                                     cf = cashFlows,
                                     fmv = fmv,
                                     bm = pme_bm)


                #pme_perf = try(calc_privatePerformance(cashFlows, pme))
                pme_perf = captureErrors(func = calc_privatePerformance,
                                         captureList = errorListName,
                                         default = NULL,
                                         cf =  cashFlows,
                                         fmv = pme)



                #perf =  try(calc_privatePerformance(cashFlows, fmv))
                perf =  captureErrors(func = calc_privatePerformance,
                                      captureList = errorListName,
                                      default = NULL,
                                      cf =  cashFlows,
                                      fmv = fmv)



                called_net = sum(cashFlows[, "Calls_Total_Net"])


                called_gross = sum(cashFlows[, "Calls_Total_Gross"])


                dists = sum(cashFlows[, "Distributions_Total"])
              }

              cf_stats = data.frame(Committed = totalCommitment,
                                    Called_w_Fees = called_net,
                                    Called_wo_Fees = called_gross,
                                    Distributed = dists,
                                    CashFlows_Net = dists - called_net,
                                    CashFlows_Gross = dists - called_gross,
                                    FMV = fmv_end)

              colnames(cf_stats)[2:3] = c("Called_w_Fees", "Called_wo_Fees")

              stats = list(Performance = perf,
                       PME_Performance = pme_perf,
                           CashFlows = cf_stats)

            } else {
              stats = list(Performance = NULL,
                           PME_Performance = NULL,
                           CashFlows = NULL)
            }


            .Object@Commitments = commitments
            .Object@TotalCommitment = totalCommitment
            .Object@InitialCommitment = initialCommitment
            .Object@Holdings = holdings
            .Object@CashFlows = cashFlows
            .Object@FMV = fmv
            .Object@Freq = freq
            .Object@Multiplier = multiplier
            .Object@Name = name
            .Object@UnderlyingFunds = underlyingFunds
            .Object@UnderlyingVintages = underlyingVintages
            .Object@UnderlyingStrategies = underlyingStrategies
            .Object@Stats = stats
            .Object@PME_Benchmark = pme_bm

            #.Object@PeriodData = cull_Data(.Object, dataFreq)
            .Object@PeriodData = captureErrors(func = cull_data,
                                               captureList = errorListName,
                                               default = NULL,
                                               pef =  .Object,
                                               freq =  dataFreq)


            if(loadVintages){
              .Object@UnderlyingVintages = loadUnderlying(.Object, "v", errorListName)
            }
            if(loadStrategies){
              # underlyingStrategies = list(Aggregated = list(), Underlying = loadUnderlying(coreFund, mode = "s"))
              .Object@UnderlyingStrategies = loadUnderlying(.Object, mode = "s", errorListName)
            }
            if(loadFunds){
              #underlyingFunds = loadUnderlying(.Object, mode = "f")
              .Object@UnderlyingFunds = loadUnderlying(.Object, mode = "f", errorListName)
            }

            .Object@ErrorList = globalenv()[[errorListName]]
             rm(list = c(errorListName), envir = globalenv())

             .Object

          })
#----------------------PrivateFund get_data_ggplot definition---------------------------------------------------------
# setGeneric(name = "get_data_ggplot", function(pef, cols,  melt_cols = cols, cumulative_cols = NA_character_, scaledBy =NA_character_, underlying = NA_character_, date_filter=NA_character_) standardGeneric("get_data_ggplot"))
# setMethod(f = "get_data_ggplot",
#           c("PrivateFund", "character", "character", "character", "character", "character", "character" ),
#           function(pef, cols, melt_cols = cols, cumulative_cols = NA_character_, scaledBy = NA_character_, underlying = NA_character_, date_filter=NA_character_) {
#             melt_cols = cols
#             if(is.na(underlying)) {
#               if(is.na(date_filter)) {
#                 xts_data = pef@PeriodData[ , cols]
#               } else {
#                 xts_data = pef@PeriodData[date_filter, cols]
#               }
#
#               if(!is.na(cumulative_cols)) {
#                 xts_data[, cumulative_cols] = cumsum(zoo::na.fill(xts_data[, cumulative_cols], 0))
#               }
#
#               if(!is.na(scaledBy)) {
#                 xts_data = xts_data/abs(sum(na.omit(pef@PeriodData[, scaledBy])))
#               }
#
#
#               df = data.frame(date = zoo::index(xts_data), xts_data)
#               df_melt = data.table::melt(data = df,
#                                          #id.vars = names(df)[which(!(names(df) %in% melt_cols))],
#                                          measure.vars = melt_cols,
#                                          variable.name = "cashflow")
#             } else if(tolower(substr(underlying,1,1)) == "v"){
#               df_list = lapply(pef@UnderlyingVintages$Underlying,
#                                function(v) get_data_ggplot(v, cols, melt_cols=cols, cumulative_cols, scaledBy, underlying = NA_character_,date_filter))
#               names(df_list) = names(pef@UnderlyingVintages$Underlying)
#
#               for(i in 1:length(df_list)) {
#                 v = names(df_list)[[i]]
#                 if(i == 1) {
#                   df_melt = cbind(df_list[[i]], vintage = rep(v, nrow(df_list[[i]])))
#                 } else {
#                   df_melt = rbind(df_melt, cbind(df_list[[i]], vintage = rep(v, nrow(df_list[[i]]))))
#                 }
#               }
#             }
#             return(df_melt)
#         }
# )

#----------------------PrivateFund constructor definition---------------------------------------------------------
PrivateFund = function(name = NA_character_,
                        id = NA_integer_,
                        strategy=NA_character_,
                        vintage=NA_integer_,
                        initialCommitment = NA_real_,
                        totalCommitment = NA_real_,
                        commitments = NULL,
                        cashFlows=NULL,
                        fmv = NULL,
                        periodData = NULL,
                        holdings = NULL,
                        pme_bm = NULL,
                        underlyingFunds = list(),
                        underlyingVintages = list(),
                        underlyingStrategies = list(),
                        freq = "d",
                        dataFreq = "m",
                        multiplier = 1e+06,
                        active = NA_integer_,
                        loadVintages = F,
                        loadStrategies = F,
                        loadFunds = F)
{
  new("PrivateFund",
      name = name,
      id = id,
      strategy = strategy,
      vintage = vintage,
      initialCommitment = initialCommitment,
      totalCommitment = totalCommitment,
      commitments = commitments,
      cashFlows = cashFlows,
      fmv = fmv,
      periodData = periodData,
      holdings = holdings,
      pme_bm = pme_bm,
      underlyingFunds = underlyingFunds,
      underlyingStrategies = underlyingStrategies,
      underlyingVintages = underlyingVintages,
      freq = freq,
      dataFreq = dataFreq,
      multiplier = multiplier,
      active = as.integer(active),
      loadVintages = loadVintages,
      loadStrategies = loadStrategies,
      loadFunds = loadFunds
      )
}




#----------------------PrivateFund privateIRR definition---------------------------------------------------------
# setMethod(f = "privateIRR",
#           c("PrivateFund", "Period"),
#           function(pef, per)
#           {
#             cf_net = pef@CashFlows$CashFlows_Net
#             cf_gross = pef@CashFlows$CashFlows_Gross
#             fmv = pef@FMV
#
#             perf_start = max(end(cf_net), end(cf_gross), end(fmv)) %m-% per
#             cf_net = cf_net[which(zoo::index(cf_net) >= perf_start)]
#             cf_gross = cf_gross[which(zoo::index(cf_gross) >= perf_start)]
#             fmv = fmv[which(zoo::index(fmv)>=perf_start)]
#
#             cf_net = rbind(-fmv[1], cf_net, fmv[nrow(fmv)])
#             cf_gross = rbind(-fmv[1], cf_gross, fmv[nrow(fmv)])
#
#
#             return(UnderlyingFunds)
#           }
# )
#----------------------PrivateFund loadUnderlying definition---------------------------------------------------------
setGeneric(name = "loadUnderlying", function(pef, mode, errorListName) standardGeneric("loadUnderlying"))
setMethod(f = "loadUnderlying",
        c("PrivateFund", "character", "character"),
        function(pef, mode, errorListName){
          mode = tolower(strtrim(mode, 1))
          freq = tolower(pef@Freq)
          active = pef@Active
          pme_bm = pef@PME_Benchmark
          if(mode == "f"){
            ids = pef@Holdings$Holding_ID
            names(ids) = as.character(pef@Holdings$Holding_Name)
            #underlying = lapply(ids, function(id)try(PrivateFund(id = id, freq = freq)))
            underlying = lapply(ids,
                                function(id) captureErrors(func = PrivateFund,
                                                                captureList = errorListName,
                                                                default = NULL,
                                                                id  =  id,
                                                                freq = freq,
                                                                pme_bm = pme_bm))


          } else if (mode == "v"){
            vintages = sort(unique(pef@Holdings$Vintage))
            strategy = pef@Strategy
            names(vintages) = vintages

            #underlying = lapply(vintages, function(v)try(PrivateFund(vintage = v, strategy = strategy, freq = freq)))
            underlying = lapply(vintages,
                                function(v) captureErrors(func = PrivateFund,
                                                                captureList = errorListName,
                                                                default = NULL,
                                                                strategy = strategy,
                                                                vintage = v,
                                                                freq = freq,
                                                                active = active,
                                                          pme_bm = pme_bm))

            # cf_agg = matrix(unlist(lapply(underlying,
            #                               function(v){if(class(v) == "PrivateFund"){v@Stats$CashFlows}else{rep(0, 5)}})), ncol = 5, byrow = T)
            # row.names(cf_agg) = names(underlying)
            # colnames(cf_agg) = names(underlying[[1]]@Stats$CashFlows)
            # if(any(as.numeric(row.names(cf_agg)[2:nrow(cf_agg)]) - as.numeric(row.names(cf_agg))[1:(nrow(cf_agg)-1)] > 1)){
            #   allYears = as.character(as.numeric(row.names(cf_agg)[1]):as.numeric(row.names(cf_agg)[nrow(cf_agg)]))
            #   missingYears = allYears[which(!(allYears %in% row.names(cf_agg)))]
            #   for(y in as.numeric(missingYears)){
            #     yearInsert = matrix(rep(0, 5), ncol = 5)
            #     cf_agg_temp = cf_agg[which(as.numeric(row.names(cf_agg)) > y),]
            #     if(!is.matrix(cf_agg_temp)){
            #       cf_agg_temp = matrix(cf_agg_temp, ncol = 5)
            #       row.names(cf_agg_temp) = row.names(cf_agg)[nrow(cf_agg)]
            #     }
            #     cf_agg = rbind(cf_agg[which(as.numeric(row.names(cf_agg)) < y),], yearInsert)
            #     row.names(cf_agg)[nrow(cf_agg)] = y
            #     cf_agg = rbind(cf_agg, cf_agg_temp)
            #   }
            # }
            #
            # cf_agg_total = apply(cf_agg, 2, sum, na.rm = T)
            #
            # cf_agg = rbind(cf_agg, TOTAL = cf_agg_total)
            cf_agg = captureErrors(func = aggregatePrivates,
                                   captureList = errorListName,
                                   default = NULL,
                                   underlying = underlying,
                                   stats = "CashFlows")
            perf_net_agg = captureErrors(func = aggregatePrivates,
                                     captureList = errorListName,
                                     default = NULL,
                                     underlying = underlying,
                                     stats = "Performance",
                                     net=T)
            perf_gross_agg = captureErrors(func = aggregatePrivates,
                                         captureList = errorListName,
                                         default = NULL,
                                         underlying = underlying,
                                         stats = "Performance",
                                         net=F)

            pme_net_agg = captureErrors(func = aggregatePrivates,
                                         captureList = errorListName,
                                         default = NULL,
                                         underlying = underlying,
                                         stats = "PME_Performance",
                                         net=T)
            pme_gross_agg = captureErrors(func = aggregatePrivates,
                                           captureList = errorListName,
                                           default = NULL,
                                           underlying = underlying,
                                           stats = "PME_Performance",
                                           net=F)

            underlying = list(Underlying = underlying,
                              Aggregated = list(CashFlows = cf_agg,
                                                Performance = list(Net = perf_net_agg,
                                                                   Gross = perf_gross_agg),
                                                PME = list(Net = pme_net_agg,
                                                           Gross = pme_gross_agg)))
          } else if (mode == "s"){
            vintages = sort(unique(pef@Holdings$Vintage))
            names(vintages) = vintages
            strategies = sort(unique(pef@Holdings$Strategy))
            names(strategies) = strategies
            # underlying = lapply(strategies,
            #                     function(s) lapply(vintages,
            #                                        function(v) try(PrivateFund(vintage = v, strategy = s, freq = freq))))
            underlying = lapply(strategies,
                                function(s) lapply(vintages,
                                                   function(v) captureErrors(func = PrivateFund,
                                                                    captureList = errorListName,
                                                                    default = NULL,
                                                                    strategy = s,
                                                                    vintage = v,
                                                                    freq = freq,
                                                                    active = active,
                                                                    pme_bm = pme_bm)))

            cf_agg = lapply(strategies,
                            function(s) captureErrors(func = aggregatePrivates,
                                                      captureList = errorListName,
                                                      default = NULL,
                                                      underlying = underlying[[s]],
                                                      stats = "CashFlows"))
            perf_net_agg = lapply(strategies,
                            function(s) captureErrors(func = aggregatePrivates,
                                                      captureList = errorListName,
                                                      default = NULL,
                                                      underlying = underlying[[s]],
                                                      stats = "Performance",
                                                      net = T))
            perf_gross_agg = lapply(strategies,
                                    function(s) captureErrors(func = aggregatePrivates,
                                                              captureList = errorListName,
                                                              default = NULL,
                                                              underlying = underlying[[s]],
                                                              stats = "Performance",
                                                              net = F))
            pme_net_agg = lapply(strategies,
                                  function(s) captureErrors(func = aggregatePrivates,
                                                            captureList = errorListName,
                                                            default = NULL,
                                                            underlying = underlying[[s]],
                                                            stats = "PME_Performance",
                                                            net = T))
            pme_gross_agg = lapply(strategies,
                                    function(s) captureErrors(func = aggregatePrivates,
                                                              captureList = errorListName,
                                                              default = NULL,
                                                              underlying = underlying[[s]],
                                                              stats = "PME_Performance",
                                                              net = F))

            underlying = list(Underlying = underlying,
                              Aggregated = list(CashFlows = cf_agg,
                                                Performance = list(Net = perf_net_agg,
                                                                   Gross = perf_gross_agg),
                                                PME = list(Net = pme_net_agg,
                                                           Gross = pme_gross_agg)))
          }


          return(underlying)
        }
)
#----------------------PrivateFund compareHoldings definition---------------------------------------------------------
setGeneric(name = "compareHoldings", function(pef) standardGeneric("compareHoldings"))
setMethod(f = "compareHoldings",
          c("PrivateFund"),

          function(pef)
          {
            ids = pef@Holdings$Holding_ID
            if(length(ids)>1)
            {
              if(length(pef@UnderlyingFunds) == 0){
                names(ids) = as.character(pef@Holdings$Holding_Name)
                freq = tolower(pef@Freq)
                UnderlyingFunds = lapply(ids, function(id)new("PrivateFund", ID = id, Freq = freq))
              }
              else{
                UnderlyingFunds = pef@UnderlyingFunds
              }

              comparisons = vector(mode = "list", length = 2)
              names(comparisons) = c("Performance", "CashFlows")

              comparisons$Performance = vector(mode = "list", length = ncol(pef@Stats$Performance))
              names(comparisons$Performance) = colnames(pef@Stats$Performance)
              comparisons$CashFlows = vector(mode = "list", length = ncol(pef@Stats$CashFlows))
              names(comparisons$CashFlows) = colnames(pef@Stats$CashFlows)


              for(comp in names(comparisons$Performance))
              {
                comparisons$Performance[[comp]] = vector(mode = "list", length = 2)
                names(comparisons$Performance[[comp]]) = c("Total", "Underlying")
                comparisons$Performance[[comp]]$Total = pef@Stats$Performance["NetFees", comp]

                for(fund in UnderlyingFunds)
                {
                  df = data.frame(fund@Stats$Performance["NetFees", comp], row.names = fund@Name)
                  colnames(df) = comp
                  if(is.null(comparisons$Performance[[comp]]$Underlying))
                    comparisons$Performance[[comp]]$Underlying = df
                  else
                    comparisons$Performance[[comp]]$Underlying = rbind(comparisons$Performance[[comp]]$Underlying, df)

                }
                ord = order(comparisons$Performance[[comp]]$Underlying, decreasing = T)
                comparisons$Performance[[comp]]$Underlying = data.frame(comparisons$Performance[[comp]]$Underlying[ord, comp],
                                                                        row.names = row.names(comparisons$Performance[[comp]]$Underlying)[ord])
                colnames(comparisons$Performance[[comp]]$Underlying) = comp
              }

              for(comp in names(comparisons$CashFlows))
              {
                comparisons$CashFlows[[comp]] = vector(mode = "list", length = 2)
                names(comparisons$CashFlows[[comp]]) = c("Total", "Underlying")
                comparisons$CashFlows[[comp]]$Total = pef@Stats$CashFlows[, comp]

                for(fund in UnderlyingFunds)
                {
                  df = data.frame(fund@Stats$CashFlows[ , comp], row.names = fund@Name)
                  colnames(df) = comp
                  if(is.null(comparisons$CashFlows[[comp]]$Underlying))
                    comparisons$CashFlows[[comp]]$Underlying = df
                  else
                    comparisons$CashFlows[[comp]]$Underlying = rbind(comparisons$CashFlows[[comp]]$Underlying, df)

                }
                ord = order(comparisons$CashFlows[[comp]]$Underlying, decreasing = T)
                comparisons$CashFlows[[comp]]$Underlying = data.frame(comparisons$CashFlows[[comp]]$Underlying[ord, comp],
                                                                        row.names = row.names(comparisons$CashFlows[[comp]]$Underlying)[ord])
                colnames(comparisons$CashFlows[[comp]]$Underlying) = comp
              }


              #
              # if(tolower(substr(comparison, 1, 1)) == "i")
              # {
              #   comp = "IRR"
              #   df = data.frame(IRR = pef@Performance$`2016-12-31`["NetFees", comp], row.names = c("Total"))
              #
              # }
              # else
              # {
              #   comp = "RollingMultiple"
              #   df = data.frame(IRR = pef@Performance[[1]]["NetFees", comp], row.names = c("Total"))
              # }
              # for(fund in UnderlyingFunds){
              #   newdf = data.frame(fund@Performance[[1]]["NetFees", comp], row.names = c(fund@Name))
              #   colnames(newdf) = comp
              #   rownames(newdf) = fund@Name
              #   df = rbind(df, newdf)
              # }


              return(comparisons)
            }
            # return(df)

          }
)

#----------------------PrivateFund print definition---------------------------------------------------------
setMethod("print",
          signature(c(x="PrivateFund")),
          function(x)
            {
              pef = x
              #perf = pef@Stats$Performance
              # commit = sum(pef@Commitments)
              # called_net = sum(pef@CashFlows[, "Calls_Total_Net"])
              # called_gross = sum(pef@CashFlows[, "Calls_Total_Gross"])
              # dists = sum(pef@CashFlows[, "Distributions_Total"])

              # cf_stats = data.frame(Committed = commit,
              #                       Called_Net = called_net,
              #                       Called_Gross = called_gross,
              #                       Distributed = dists)



              cat('\nCash Flow Stats-------------------------------------------\n')
              #print(cf_stats)
              print(pef@Stats$CashFlows)
              cat('\n\n')
              cat('Performance Stats-------------------------------------------\n')
              #print(perf)
              print(pef@Stats$Performance)
            })




#----------------------PrivateFund modelDistributions definition---------------------------------------------------------
# setMethod(f = "modelDistributions",
#           c("PrivateFund"),
#
#           function(pef)
#           {
#             require(yuima)
#             dist_rate = data[, "DistributionRate"]
#             fmv = data[zoo::index(dist_rate), "FMV"]
#             dist_start = zoo::index(fmv[fmv!=0])[2]
#             dist_rate = zoo::na.fill(dist_rate[zoo::index(dist_rate) >= dist_start], 0)
#
#             param.init_shift = list(a = 1.1, b = -2, sigma = 1)
#
#             low.par = list(a = 0, b = -1e+09, sigma = 0)
#             upp.par = list(a = 1e+09, b = 0, sigma = 100)
#             prior <-
#               list(
#                 a=list(measure.type="code",df="dnorm(z,0,1)"),
#                 b=list(measure.type="code",df="dnorm(z,0,1)"),
#                 sigma=list(measure.type="code",df="dnorm(z,0,1)")
#               )
#
#             mod = setModel(drift = "(a + b*x)", diffusion = "sigma*sqrt(x)", solve.variable = "x")
#           }
# )
