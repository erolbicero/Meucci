#' Create a mAsset (Meucci Asset) object containing time series and asset-specific meta-data
#' 
#' This function effectively adds extensive metadata to xts objects, linking asset-specific details to the object itself, simplifying 
#' Meucci-method calculations by using data defined at object-creation.  It requires a name, time series, 
#' time series type, asset class, and invariant information.  By providing this information, 
#' executing functions is simplified, since each function is executed according to Meucci's methods.
#' The mAsset objects are the building blocks for mPortfolio objects.
#' 
#' @param assetName a string naming the object
#' @param assetTimeSeries an xts object of the raw series; prices/levels are preferred, but returns can be passed
#' @param assetTimeSeriesType one of "level", "difference", "discrete", "log", so that the package knows how to handle the xts object
#' @param assetClass one of "equity", "debt", "currency", "option", "future"
#' @param pricingFunction a function, currently not used
#' @param seedValue the price or level associated with the first time-stamp in the assetTimeSeries object, if levels/prices are provided it will automatically be extracted
#' @param invariant a list of invariant information - this is experimental and may likely change as development progresses
#' @param invariantFunction a function used to calculate the assetTimeSeries argument to the asset-specific invariant - again this is experimental and may likely change as development progresses
#' @param marginalDistribution a fitted distribution function - not yet used, may move to "invariant"
#' 
#' @author Erol Biceroglu
#' @seealso \code{\link{createPortfolio}}
#' 
#' @importFrom zoo index
#' 
#' @examples
#' 
#' TBD... this isn't related
#' data(managers)
#' managers <- managers[,1:6] #equities only
#' xtsAttributes(managers) <- list(coredata_content = "discreteReturn") #append attribute
#' chart.CumReturns(managers) #here is baseline
#' managersL <- Level.calculate(R = managers)
#' plot(managersL-1)
#' 
#' #Here they are equal
#' Return.cumulative(managers)  #Baseline
#' last(managersL-1) #This function
#' 
#' @export
createAsset <- function( assetName = NULL
                          , assetTimeSeries = NULL #pass an xts object with return calc, or user specify price/etc, check if it's a VECTOR (can't be more than that, unless user specifies *model*)
                          , assetTimeSeriesType = c("level", "difference", "discrete", "log") #the function should check if the xts attribute is populated, if not, user *must* pass something
                          , assetClass = c("equity", "debt", "currency", "option", "future")
                          , pricingFunction = NULL #make this text and do methods to call fns
                          , seedValue = NULL
                          , invariant = list(type = c("log","difference","residual")
                                            ,model = list(data = NULL
                                                          , model = NULL
                                                      )
                                            )
                         , invariantFunction = quote(Return.calculate.mAsset)
                          #PerformanceAnalytics::Level.calculate
                          , marginalDistribution = NULL #make this text and do methods to call fns
                                #stats::pnorm
                          ){
  
  #make this *compatible* with FinancialInstrument for later
    
  #####################################################
  #This needs error checking, streamlining, and good defaults
  #select default args
  
  #####assetClass <- c(assetClass, "mAsset")
  # invariant <- unlist(invariant[1])
  
  #
  if(identical(assetClass,c("equity", "debt", "currency", "option", "future"))){assetClass <- assetClass[1]}
  if(identical(invariant$type,c("log","difference","residual"))){invariant$type <- invariant$type[1]}
  
  assetTimeSeriesType <- assetTimeSeriesType[1] #check xts object attribute first
  
  if(assetTimeSeriesType == "level"){seedValue <- as.vector(assetTimeSeries[1,1])}
  
  if(is.null(pricingFunction)){
    
    xts::xtsAttributes(assetTimeSeries) <- list(coredata_content = ifelse(assetTimeSeriesType=="discrete","discreteReturn",ifelse(assetTimeSeriesType=="log","logReturn",assetTimeSeriesType))
                                           )
    if(assetTimeSeriesType == "level"){
    pricingFunction <- bquote(PerformanceAnalytics::Level.calculate(seedValue = .(seedValue), initial = TRUE)
                                                                    #= .(as.vector(assetTimeSeries[1,1])), initial = TRUE)
                             )
    }
  }
  #####################################################
  
  
  #create output object
  assetObject <- structure(
    list(
      assetName = assetName
    , assetTimeSeries = assetTimeSeries
    , assetTimeSeriesType = assetTimeSeriesType
    , pricingFunction = {if(class(pricingFunction) %in% c("name","call")){enquote(pricingFunction)} else {quote(pricingFunction)}} #ifelse(class(pricingFunction) %in% c("name","call"),enquote(pricingFunction),quote(pricingFunction))
    , seedValue = seedValue
    , invariant = invariant
    , invariantFunction = invariantFunction
    , marginalDistribution = quote(marginalDistribution)
    )
    , assetClass = assetClass
    , class = c(paste0("m",tools::toTitleCase(assetClass)), "mAsset") #Meucci Asset
  )
  
  #declare attributes
  # attributes(assetObject) <- list( assetClass = assetClass
  #                                 # , invariant = invariant
  #                                 # , pricingFunction = pricingFunction
  #                                 # , marginalDistribution = marginalDistribution
  #                                 )
if(is.null(assetName)){
  stop("assetName requires an argument")
} 
else {
  return(assetObject)  
  }

}

#' @export
print.mAsset <- function(mAssetObject){
  cat(
    "Asset Class Object\n"  
    , "---------------------\n"
    , sprintf("Asset Name: %s\n", mAssetObject$assetName)
    , sprintf("Asset Class: %s\n", attr(mAssetObject, "assetClass")) 
    , sprintf("Asset Invariant: %s\n", mAssetObject$invariant)
    , sprintf("Time Series: ")
    , sep = ""
  )
  tmpXtsOjb <- mAssetObject$assetTimeSeries
  names(tmpXtsOjb) <- "" 
  
  
  tailRows <- max(c(nrow(tmpXtsOjb)-2,1))
  
  
  
  if(tailRows<=7){
    print(tmpXtsOjb)
  } else {
          headRows <- min(c(nrow(mAssetObject$assetTimeSeries),3))
          print(tmpXtsOjb[1:(headRows),])
          cat(
            "...\n"
          , "...\n"
          , "..."
          , sep = ""
          )
          print(tmpXtsOjb[(tailRows):nrow(tmpXtsOjb),])
  }
}

#' @export
head.mAsset <- function(mAssetObject){
  result <- zoo:::head.zoo(mAssetObject$assetTimeSeries)
  mAssetObject$assetTimeSeries <- result
  return(mAssetObject)
}

#' @export
tail.mAsset <- function(mAssetObject){
  result <- zoo:::tail.zoo(mAssetObject$assetTimeSeries)
  mAssetObject$assetTimeSeries <- result
  return(mAssetObject)
}

#need functions for all of these:
# methods(class=xts())
#...e.g. ...
#first
#last

`[.mAsset` <- function(mAssetObject, i, j, drop = FALSE, which.i=FALSE,...){
  result <- xts:::`[.xts`(x = mAssetObject$assetTimeSeries, i = i, j = j, drop = drop, which.i=which.i, ... = ...)
  mAssetObject$assetTimeSeries <- result
  return(mAssetObject)
}


`[<-.mAsset` <- function(mAssetObject, i, j, value){
  
  # result <- xts:::`[<-.xts`(x = mAssetObject$assetTimeSeries, i = i, j = j, value = value)
  
  # result <- mAssetObject$assetTimeSeries[i,j]
  # result <- value
  # 
  # mAssetObject$assetTimeSeries[i,j] <- result
  
  
  # if(missing(j)){j <- 1:ncol(mAssetObject$assetTimeSeries)}
  # if(is.character(i)){iValue <- 1:nrow(mAssetObject$assetTimeSeries[i,j]) }
  # if(missing(i)){iValue <- 1:nrow(mAssetObject$assetTimeSeries)}
  # 
  # if(is.atomic(value)){
  #     if(!((length(value) == length(iValue) & length(j) == 1) | (  length(value) == length(j) & length(iValue) == 1 )  )){stop("Dimension lengths need to agree")} 
  # } else {
  #   if(!(dim(value)[1] == length(iValue) & dim(value)[2] == length(j))){stop("Dimension lengths need to agree")}
  # }
  
  
  if(missing(i) & !missing(j)){
    mAssetObject$assetTimeSeries[,j] <- value
  }
  
  if(!missing(i) & missing(j)){
    mAssetObject$assetTimeSeries[i,] <- value
  }
  
  if(missing(i) & missing(j)){
    mAssetObject$assetTimeSeries[,] <- value
  }
  
  if(!missing(i) & !missing(j)){
  mAssetObject$assetTimeSeries[i,j] <- value
  }

  return(mAssetObject)
  
}