installModel <- function(from, target){
  if(inherits(from, 'error') || is.character(from)){
    stop(from)
  }
  if(class(from)[1] == 'Strategy'){
    target$backtest <- from$backtest
    target$paramset <- from$paramset
  }else if(class(from)[1] == 'list'){
    for(i in seq_along(from)){
      target[[i]]$backtest <- from[[i]]$backtest
      target[[i]]$paramset <- from[[i]]$paramset
    }
  }
}
