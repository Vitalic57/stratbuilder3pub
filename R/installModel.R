installModel <- function(from, target){
  if(class(from)[1] == 'Strategy'){
    target$backtest <- from$backtest
  }else if(class(from)[1] == 'list'){
    for(i in seq_along(from)){
      target[[i]]$backtest <- from[[i]]$backtest
    }
  }
}
