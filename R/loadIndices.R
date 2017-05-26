loadIndices = function(code = "YAHOO/INDEX_GSPC") {
  ind = Quandl::Quandl(code = "YAHOO/INDEX_GSPC", type = "xts")
  ind = ind[, "Adjusted Close"]
  names(ind) = "SPTR"
  ind
}