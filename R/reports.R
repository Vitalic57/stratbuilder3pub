#' Calculate statistic
#'
#' Statistic and all dependent statistics will be calculated where possible
#'
#' @param this Strategy
#' @param s
#' @param start
#' @param end
#' @param recalcl
#' @rdname precalcStat
#' @export
#' @method precalcStat Strategy
precalcStat.Strategy <- function(this, s, start, end, recalc){
  if(is.null(.env[['session']])){
    stop('Please, use ssh_connect function before proceeding')
  }
  session <- .env[['session']]
  args <- rlang::enexprs(s=s, start=start, end=end, recalc=recalc)
  res <- list()
  for(name in names(args)){
    tryCatch({
      res[[name]] <- eval(args[[name]], env = parent.frame())
    }, error = function(e){})
  }
  send_to_server <- FALSE

  if(!is.null(res$s$func)){
    period <- paste('per', start, end, sep = '_')
    if(!is.null(s[['depends']])){
      nms <- sapply(this$report_stats, "[[", 'name')
      for(x in s[['depends']]){
        ind <- which(nms == x)[1]
        if(length(ind) == 0){
          s1 <- acceptable_stats[[x]]
        }else{
          s1 <- this$report_stats[[ind]]
        }
        if(is.null(s1)){
          stop(paste('No such stat', x))
        }
        if(!is.null(s1$func)){
          next
        }
        if(!s1$general && (is.null(this$backtest$stats[[period]][[x]]) || res$recalc)){
          send_to_server <- TRUE
        }else if(s1$general && is.null(this$backtest$results[[x]])){
          send_to_server <- TRUE
        }
      }
    }
  }else{
    send_to_server <- TRUE
  }
  if(send_to_server){
    l <- list(this=this,
              request= rlang::call2('calcStat', quote(this), !!!res))
    send_rdata(session, l, verbose=PARAMS('verbose'))
    res <- get_results(session, verbose=PARAMS('verbose'))
    installModel(res, this)
  }
  return(invisible(this))
}


#' Get report of Strategy
#'
#' @param this Strategy
#' @param start numeric / Date / character, start of the period
#' @param end numeric / Date / character, end of the period
#' @param recalc logical, whether recalculate report or not
#'
#' @export
#' @method getReport Strategy
#' @rdname getReport
getReport.Strategy <- function(this, start, end, returns = 'tibble', recalc=FALSE){
  if(is.null(this$backtest)){
    stop('Please, perform your strategy before calling this method')
  }
  s <- Stat(func = function(...){}, depends = sapply(this$report_stats, '[[', 'name'))
  start <- get_backtest_start_index(this, start)
  end <- get_backtest_end_index(this, end)
  precalcStat(this, s, start, end, recalc)
  res <- list()
  for(i in seq_along(this$report_stats)){
    s <- this$report_stats[[i]]
    name <- names(this$report_stats[i])
    value <- calcStat(this, s, start, end)
    res[[name]] <- value
  }
  if(returns == 'tibble'){
    return(res %>% {tibble::as_tibble(.)})
  }
  return(res)
}


# #' Get variable acceptable_stats
# #'
# #' @export
# #' @method getAcceptableStats Strategy
# #' @rdname getAcceptableStats
# getAcceptableStats.Strategy <- function(this){
#   return(acceptable_stats)
# }
