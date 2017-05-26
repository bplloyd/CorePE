captureErrors = function(func, captureList, default = NULL, ...)
{
  res = try(wrapr::DebugFnW(as.name('lastError'), func)(...), silent = T)
  if(exists("lastError", envir = globalenv())){
    assign(x=captureList,
           value = do.call(append,
                           args = list(x=as.name(captureList),
                                       values = list(list(#calling_function = funcName,
                                                          wrapr_data = lastError,
                                                          error_msg = res)))),
           inherits = T)
    do.call(what = rm, args = list(as.name('lastError')), envir = globalenv())
    res = default
  }
  return(res)
}
