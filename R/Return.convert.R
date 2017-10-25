Return.convert <- function(mAssetObject, destinationType = c("discrete", "log", "difference", "level"), seedValue, initial){
  UseMethod(generic = "Return.convert",object = mAssetObject)
}

Return.convert.mAsset <- function(mAssetObject, destinationType = c("discrete", "log", "difference", "level"), seedValue = NULL, initial = TRUE){
  
  if(identical(destinationType,c("discrete", "log", "difference", "level"))){destinationType <- destinationType[1]}
  
  if(is.null(seedValue) & destinationType == "level"){
    seedValue <- mAssetObject$seedValue
  }
  
  result <- PerformanceAnalytics::Return.convert(R = mAssetObject$assetTimeSeries, destinationType = destinationType, seedValue = seedValue, initial = initial)
  mAssetObject$assetTimeSeries <- result
  mAssetObject$assetTimeSeriesType <- destinationType
  return(mAssetObject)
}