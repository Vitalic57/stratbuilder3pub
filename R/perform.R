#' Simulate strategy
#'
#' @export
#' @rdname perform
#' @method perform Strategy
perform.Strategy <- function(this, calc_reports=TRUE, ...){
  if(is.null(.env[['session']])){
    stop('Please, use ssh_connect function before proceeding')
  }
  if(is.null(this$data)){
    stop('Please, set Data first')
  }
  session <- .env[['session']]
  l <- list(this=this,
            request= rlang::call2('perform', quote(this), calc_reports=calc_reports, !!!list(...)))
  send_rdata(session, l, verbose=PARAMS('verbose'))
  res <- get_results(session, verbose=PARAMS('verbose'))
  installModel(res, this)
  return(invisible(this))
}
