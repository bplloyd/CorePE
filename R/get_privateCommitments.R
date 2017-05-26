#' @include get_privateHoldings.R

get_privateCommitments = function(id=NA, strategy=NA, vintage=NA, active = NA, freq = 'd', multiplier = 1){

  if(!is.na(id)) {
    commits = commitments[commitments$Holding_ID == id, , drop = FALSE]
  } else {
    holdings = get_privateHoldings(id = id, strategy = strategy, vintage = vintage, active = active)
    commits = commitments[commitments$Holding_ID %in% holdings$Holding_ID, c("Effective_Date", "Amount")]
  }
  names(commits)[names(commits) == "Effective_Date"] = "Date"
  if(nrow(commits)>0)
  {
    commits = tidyquant::as_xts_(commits, "Date")
    names(commits) = "Commitment"
    commits = apply.daily(commits, sum, na.rm = TRUE)
    if(freq != "d"){
      zoo::index(commits) = lubridate::ceiling_date(zoo::index(commits), "months") - 1
      commits = xts::apply.monthly(commits, sum)
      fillDates =  lubridate::ceiling_date(lubridate::add_with_rollback(e1 = start(commits), months(0:(lubridate::interval(start(commits), end(commits))/months(1)))), unit = "m")-1
      temp_commits =  xts::xts(rep(0, length(fillDates)), order.by = fillDates)
      temp_commits[zoo::index(commits)] = commits

      if(freq == 'm'){
        commits = temp_commits
      }
      if(freq == 'q'){
        commits = xts::apply.quarterly(temp_commits, sum)
        zoo::index(commits) = lubridate::ceiling_date(zoo::index(commits), "quarters") - 1
      }
      if(freq=='y'){
        commits = xts::apply.yearly(temp_commits, sum)
        zoo::index(commits) = lubridate::ceiling_date(zoo::index(commits), "years") - 1
      }
    }
    return(as.data.frame(commits/multiplier))
  } else {
    return(NULL)
  }

}

