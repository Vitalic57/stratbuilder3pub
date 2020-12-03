#' Apply defined distributions
#'
#' @export
#' @rdname applyParamset
#' @method applyParamset Strategy
applyParamset.Strategy <- function(this,  ...){
  if(is.null(.env[['session']])){
    stop('Please, use ssh_connect function before proceeding')
  }
  session <- .env[['session']]
  l <- list(this=this,
            request= rlang::call2('applyParamset', quote(this), parallel = FALSE, !!!rlang::enexprs(...)))
  send_rdata(session, l, verbose=PARAMS('verbose'))
  res <- get_results(session, verbose=PARAMS('verbose'))
  installModel(res, this)
  return(invisible(this))
}
