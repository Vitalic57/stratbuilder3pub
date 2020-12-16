#' Apply defined distributions
#'
#' @export
#' @rdname applyParamset
#' @method applyParamset Strategy
applyParamset.Strategy <- function(this,  ...){
  if(is.null(.env[['session']])){
    stop('Please, use ssh_connect function before proceeding')
  }
  if(is.null(this$paramset) || length(this$paramset$distributions) == 0){
    stop('Please, add distributions')
  }
  if(is.null(this$data)){
    stop('Please, set Data first')
  }
  session <- .env[['session']]
  l <- list(this=this,
            request= rlang::call2('applyParamset', quote(this), parallel = FALSE, !!!list(...)))
  send_rdata(session, l, verbose=PARAMS('verbose'))
  res <- get_results(session, verbose=PARAMS('verbose'))
  installModel(res, this)
  return(invisible(this))
}
