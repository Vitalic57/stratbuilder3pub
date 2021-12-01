send_rdata <- function(session, obj, verbose=FALSE){
  file_path <- file.path(tempdir(), 'file.RData')
  saveRDS(obj, file_path)
  tryCatch({
    capture.output(ssh::scp_upload(session, file_path))
  }, error = function(e){
    #print(e)
  })
  if(verbose){
    cat('data uploaded\n')
  }

  capture.output(ssh::ssh_exec_wait(session, 'cat file.RData > .RData'))
  if(verbose){
    cat('File renamed on server\n')
  }
  file.remove(file_path)
  if(verbose){
    cat('Temporary file removed\n')
  }
}



#' Get info about the session
#'
#' @param session ssh_session
#'
#' @export
ssh_info <- function(session){
  info <- ssh::ssh_info(session)
  info$keyfile <- .env[['keyfile']]
  return(info)
}


#' Interrupt simulation on the server
#'
#' @param session ssh_session
#' @param verbose logical, print message or not
#'
#' @export
interruptSimulation <- function(session, verbose=TRUE){
  if(missing(session)){
    session <- .env[['session']]
  }
  file_path <- file.path(tempdir(), 'keyboardInterrupt.txt')
  file.create(file_path)
  tryCatch({
    x <- capture.output(ssh::scp_upload(session, file_path))
    if(verbose){
      cat("Simulation interrupted")
    }
  }, error = function(e){
    print(e)
  })
}


.env <- new.env()

#' Connect to server
#'
#' @param host character, name of host
#'
#' @param keyfile character, path to key
#' @param passwd askpass::askpass
#' @param verbose logical
#'
#' @export
ssh_connect <- function(host, keyfile = NULL, passwd = askpass::askpass, verbose = FALSE){
  .env[['keyfile']] <- keyfile
  suppressWarnings(suppressMessages({
    tryCatch({
      ssh::ssh_disconnect(.env[['session']])
    }, error = function(e){})
  }))
  .env[['session']] <- ssh::ssh_connect(host=host, keyfile = keyfile, passwd = passwd, verbose = verbose)
  return(.env[['session']])
}


get_results <- function(session, verbose=FALSE){
  reports <- 'strategy'
  # wait for results--------------------------
  files_path <- file.path(tempdir())

  vec_cond <- logical(0)
  vec_names <- c(strategy = 'report.RDS')
  vec_cond['strategy'] <-  TRUE
  pwd <- capture.output(ssh::ssh_exec_wait(session, 'pwd'))[1]
  tryCatch({
    Sys.sleep(0.5)

    t <- Sys.time()
    if(verbose){
      cat('Before cycle of getting results\n')
    }

    while(TRUE){
      x <- capture.output(ssh::ssh_exec_wait(session, paste0('ls ', pwd, '/last_results')))
      res <- any(vec_names[vec_cond] %in% x)
      Sys.sleep(1)
      if(verbose && Sys.time() - t > 10){
        cat('simulation in progress\n')
        cat('current data in last results:')
        cat(capture.output(ssh::ssh_exec_wait(session, paste0('ls ', pwd, '/last_results'))))
        cat('\n')
        t <- Sys.time()
      }
      if(res){
        break
      }
    }
  },
  finally = {
    x <- capture.output(ssh::ssh_exec_wait(session, paste0('ls ', pwd, '/last_results')))
    res <- any(vec_names[vec_cond] %in% x)
    if(!res){
      interruptSimulation(session, verbose = FALSE)
      stop('Simulation interrupted')
    }
  }
  )

  if(verbose){
    cat('Results have been gotten\n')
  }
  # available results
  x <- capture.output(ssh::ssh_exec_wait(session, paste0('ls ', pwd, '/last_results')))
  vec_avail <- vec_names[vec_cond] %in% x
  names(vec_avail) <- names(vec_names[vec_cond])

  if(verbose){
    cat('Before downloading results\n')
  }
  # download results -------------------
  if(verbose){
    print('vec_names:')
    print(vec_names)
    print('vec_cond:')
    print(vec_cond)
    print('vec_avail:')
    print(vec_avail)
    print('files_path:')
    print(files_path)
  }

  x <- sapply(seq_along(vec_names), function(i){
    x <- names(vec_names)[i]
    if(vec_cond[x] && vec_avail[x]){
      if(verbose){
        print(paste0('try to download: ', paste0('last_results/', vec_names[x])))
      }
      ssh::scp_download(session, paste0(pwd, '/last_results/', vec_names[x]), files_path, verbose = FALSE)
      if(verbose){
        print('downloaded')
      }
    }
  })
  if(verbose){
    cat('Files of results downloaded\n')
  }

  # show results ----------------------------------------
  txt_path <- file.path(files_path, 'report.RDS')
  res <- readRDS(txt_path)
  file.remove(txt_path)
  if(verbose){
    cat('Reports downloaded\n')
  }

  if(res == 'OK'){
    ssh::scp_download(session, paste0(pwd, '/last_results/model.RData'), files_path, verbose = FALSE)
    Sys.sleep(1)
    model <- suppressWarnings(readRDS(file.path(files_path, 'model.RData')))
    return(model)
  }else{
    return(res)
  }
}
