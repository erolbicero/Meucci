#' Create a mPortfolio (Meucci Portfolio) object linking mAsset objects into one
#' 
#' This function combines mAsset objects into an mPortfolio, and uses additional user-defined portfolio-level metadata,
#' thereby simplifying Meucci-method calculations by using data defined at object-creation.
#' It requires at least 1 mAsset object, defaults to an outer join, and potentially a multivariate distribution for the invariants.
#' Note for advanced users, each mAsset object is stored on its own within a list, and so the xts matrix is constructed on-demand.
#'  
#' Product of all the individual period returns
#' 
#' For arithmetic returns:
#' \deqn{(1+r_{1})(1+r_{2})(1+r_{3})\ldots(1+r_{n})=cumprod(1+R)}{cumprod(1+R)}
#' 
#' For log returns:
#' \deqn{exp(r_{1}+r_{2}+r_{3} + \ldots + r_{n})=exp(cumsum(R))}{exp(cumsum(R))}
#' 
#' @param ... mAsset objects
#' @param join defaults to "fullouter"
#' @param multivariateDistribution an optional multivariate distribution for the invariants - this may be removed as development progresses
#' @author Erol Biceroglu
#' @seealso \code{\link{createAsset}}
#' 
#' @examples
#' 
#' #' TBD... this isn't related
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
createPortfolio <- function( # portfolioName = NULL
                         # , 
                          #use list or dots for assets?
                          ...
                         #, assetList = NULL #pass an xts object with return calc, or user specify price/etc, check if it's a VECTOR (can't be more than that, unless user specifies *model*)
                         , join = "fullouter" #inner, or vector, like merge xts
                         , multivariateDistribution = NULL #mvtnorm::dmvnorm #list(copulaMarginal = , multivariateDistribution = list())
                         #issue a warning if the data types all differ when constructing the portfolio
                         #object, i.e. if you have invariant and levels, should be all the same
){
  
  #potentially make this *compatible* with FinancialInstrument for later
  #objective/output of this package is to create simulated timeseries for portfolioAnalytics
  
  #####################################################
  #This needs error checking, streamlining, and good defaults
  #select default args
  
  #portfolioName <- #if null provide a default, check current environment, IS THIS EVEN NEEDED?
  assetList <- list(...) #check that these are mAssets
  names(assetList) <- sapply(assetList,function(asset){asset$assetName})
  assetListCheck <- sapply(assetList,function(x){ifelse("mAsset" %in% class(x),TRUE,FALSE)})
  assetListCheck <- ifelse(prod(as.numeric(assetListCheck))==1,TRUE,FALSE)
  if(!assetListCheck){stop("objects must be class mAsset")}
  multivariateDistribution <- #check/warn that one of copula+marginal or multivariate distribution
      #where copula+marginal will take priority over a multivariate distribution

  #create output object
  portfolioObject <- structure(
    list(
      #portfolioName = portfolioName
      #, 
      assetList = assetList
      , multivariateDistribution = quote(multivariateDistribution)
    )
    , class = c("mPortfolio") #Meucci Portfolio
  )
  
  # if(is.null(assetName)){
  #   stop("assetName requires an argument")
  # } 
  # else {
    return(portfolioObject)  
  # }
  
}

##############################
#' @export
extractAssetNames <- function(mPortfolioObject){
  
  
  #mAssetObject
  result <-
  sapply(mPortfolioObject$assetList, function(mAssetObject){
      return(mAssetObject$assetName)
    })
  
  return(result)
  
}

#' @export
extractAssetClasses <- function(mPortfolioObject){
  
  
  result <- 
  sapply(mPortfolioObject$assetList, function(mAssetObject){
    return(attributes(mAssetObject)$assetClass)
  })
  
  return(result)
}

#' @export
extractAssetTimeSeries <- function(mPortfolioObject){

  result <- 
  lapply(mPortfolioObject$assetList, function(mAssetObject){
    return(mAssetObject$assetTimeSeries)
  })
  
  return(result)
  
  
}

##############################

#you want an lapply method
# sapply method
# roll apply method

#' @export
print.mPortfolio <- function(mPortfolioObject){
  
  assetNames <- extractAssetNames(mPortfolioObject)
  assetClasses <- extractAssetClasses(mPortfolioObject)
  assetTimeSeriesList <- extractAssetTimeSeries(mPortfolioObject)
  
  cat(
    "Portfolio Object\n"  
    , "---------------------\n"
    , sprintf("Asset Names: %s\n", do.call(paste,as.list(assetNames)))
    , sprintf("Asset Classes: %s\n", do.call(paste,as.list(assetClasses)))
    #, sprintf("Asset Invariant: %s\n", mAssetObject$invariant)
    , sprintf("Time Series: \n")
    , sep = ""
  )
  
  #format the time series for head/tail
  #tmpXtsOjb <- list()
  
  
  tmpXtsOjb <- do.call(merge,assetTimeSeriesList) #mAssetObject$assetTimeSeries
  names(tmpXtsOjb) <- rep("",ncol(tmpXtsOjb))
  
  tailRows <- max(c(nrow(tmpXtsOjb)-2,1))
  
  if(tailRows<=7){
    print(tmpXtsOjb)
  } else {
    headRows <- min(c(nrow(tmpXtsOjb),3))
    print(tmpXtsOjb[1:(headRows),])
    cat(
      "...\n"
      , "...\n"
      , "..."
      , sep = ""
    )
    print(tmpXtsOjb[(tailRows):nrow(tmpXtsOjb),])
  }
  
  # tmpXtsOjb$head <- lapply(assetTimeSeriesList,zoo:::head.zoo)
  # tmpXtsOjb$tail <- lapply(assetTimeSeriesList,zoo:::tail.zoo)
  # 
  # tmpXtsOjb$head <- do.call(xts:::merge.xts, tmpXtsOjb$head)
  # colnames(tmpXtsOjb$head) <- assetNames
  # tmpXtsOjb$tail <- do.call(xts:::merge.xts, tmpXtsOjb$tail)
  # colnames(tmpXtsOjb$tail) <-rep("",ncol(tmpXtsOjb$tail))
  # 
  # print(tmpXtsOjb$head)
  # cat(
  #   "...\n"
  #   , "...\n"
  #   , "..."
  #   , sep = ""
  # )
  # 
  # print(tmpXtsOjb$tail)
  # 
  # #clean up
  # rm(assetNames, assetClasses, assetTimeSeriesList)
}


#' @export
head.mPortfolio <- function(mPortfolioObject){
  
  mPortfolioObject$assetList <- 
    lapply(mPortfolioObject$assetList
           ,function(mAssetObject){
             mAssetObject$assetTimeSeries <- zoo:::head.zoo(mAssetObject$assetTimeSeries)
             return(mAssetObject)
             }
           )
  
  return(mPortfolioObject)
  
  
}

#' @export
tail.mPortfolio <- function(mPortfolioObject){
  
  mPortfolioObject$assetList <- 
    lapply(mPortfolioObject$assetList
           ,function(mAssetObject){
             mAssetObject$assetTimeSeries <- zoo:::tail.zoo(mAssetObject$assetTimeSeries)
             return(mAssetObject)
           }
    )
  
  return(mPortfolioObject)
}

`[.mPortfolio` <- function(mPortfolioObject, i, j, drop = FALSE, which.i=FALSE,...){
  
#filter j's
  #****Need to check for variable names
  if(!missing(j)){mPortfolioObject$assetList <- mPortfolioObject$assetList[j]}


#filter i's
  if(!missing(i)){
  mPortfolioObject$assetList <- 
    lapply(mPortfolioObject$assetList, function(mAssetObject, i){
      mAssetObject$assetTimeSeries <- mAssetObject$assetTimeSeries[i,]
      return(mAssetObject)
    }
    , i = i
    )
  }

  return(mPortfolioObject)

}

`[<-.mPortfolio` <- function(mPortfolioObject, i, j, value){
  timeSeriesObj <- extractAssetTimeSeries(mPortfolioObject)
  
  #need to build xts matrix
  timeSeriesObj <- do.call(xts:::merge.xts, timeSeriesObj)
  
  if(missing(i) & !missing(j)){
    timeSeriesObj[,j] <- value
  }
  
  if(!missing(i) & missing(j)){
    timeSeriesObj[i,] <- value
  }
  
  if(missing(i) & missing(j)){
    timeSeriesObj[,] <- value
  }
  
  if(!missing(i) & !missing(j)){
    timeSeriesObj[i,j] <- value
  }
  
  
  
  return(timeSeriesObj)
  
}

#extractTimeSeriesMatrix  (this is a do.call(merge,...) for the assets)
