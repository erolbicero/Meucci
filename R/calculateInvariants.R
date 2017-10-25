#invariant calculation
calculateInvariants <- function(mAssetObject){
  UseMethod(generic = "calculateInvariants",object = mAssetObject)
}

calculateInvariants.mAsset <- function(mAssetObject){
  #make this more generic, like this might only be for equities, and even
  #then it has to handle whatever the user passes
  eval(
    call(name = as.character(mAssetObject$invariantFunction)
         , mAssetObject = mAssetObject
         , method = mAssetObject$invariant$type
    )
  )
}

calculateInvariants.mPortfolio <- function(mPortfolioObject){
  mPortfolioObject$assetList <- lapply(mPortfolioObject$assetList, calculateInvariants.mAsset)
  return(mPortfolioObject)
}