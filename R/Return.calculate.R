#' @export
Return.calculate <- function(mAssetObject, method = c("discrete", "log", "difference")){
  UseMethod(generic = "Return.calculate",object = mAssetObject)
}

#' @export
Return.calculate.mAsset <- function(mAssetObject, method = c("discrete", "log", "difference")){
  
  if(identical(method, c("discrete", "log", "difference"))){method <- method[1]}
  
  result <- PerformanceAnalytics::Return.calculate(prices = mAssetObject$assetTimeSeries, method = method)
  mAssetObject$assetTimeSeries <- result
  mAssetObject$assetTimeSeriesType <- method
  return(mAssetObject)
}