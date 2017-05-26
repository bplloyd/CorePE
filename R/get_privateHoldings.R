#' @include abbToStrategy.R

get_privateHoldings = function(id=NA, strategy=NA, vintage=NA, active = NA){
  if(!is.na(id)) {
    holdings = private_holdings[private_holdings$Holding_ID == id, , drop = FALSE]
  } else {
    holdings = private_holdings
    if(!is.na(strategy)){
      if(nchar(strategy) <= 3){
        strategy = abbToStrategy(strategy)
      }
      holdings = holdings[(!is.na(holdings$Strategy)) & (holdings$Strategy == strategy), ,drop = FALSE]
    }
    if(!is.na(vintage)) {
      holdings = holdings[(!is.na(holdings$Vintage)) & (holdings$Vintage == vintage), ,drop = FALSE]
    }
    if(!is.na(active)) {
      holdings = holdings[(!is.na(holdings$Active)) & (holdings$Active == active), ,drop = FALSE]
    }
  }
  holdings
}

