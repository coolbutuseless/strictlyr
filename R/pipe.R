

# STRICTLYR_LOG = 'stop', 'warning', 'message', 'quiet'

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#' Unified function for log output
#'
#' By default, all log output is via \code{stop()}
#'
#' Set global environment variables to control the output behavour. Values must
#' be one of c('stop', 'warning', 'message', 'quiet').
#'
#' e.g. \code{options(STRICTLYR_PIPE = 'message')}
#'
#' @param text the text to output
#' @param message_type message type
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
log_output <- function(text, message_type) {
  global_log_level <- getOption('STRICTLYR_LOG', 'stop')
  func_log_level  <- getOption(message_type, global_log_level)

  if (func_log_level == 'quiet') {

  } else if (func_log_level == 'message') {
    message(text)
  } else if (func_log_level == 'warning') {
    warning(text, call. = FALSE)
  } else {
    stop(text, call. = FALSE)
  }

}


#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#' A stricter version of the pipe
#'
#' The drop-in replacement for the pipe operator will give a warning if they
#' input dataset is grouped, or the output dataset is grouped.
#'
#' @param lhs,rhs the two sides of the pipe
#'
#' @importFrom dplyr groups
#' @export
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
'%>%' <- function (lhs, rhs) {
  parent <- parent.frame()
  env <- new.env(parent = parent)
  chain_parts <- split_chain(match.call(), env = env)
  pipes <- chain_parts[["pipes"]]
  rhss <- chain_parts[["rhss"]]
  lhs <- chain_parts[["lhs"]]
  env[["_function_list"]] <- lapply(
    1:length(rhss),
    function(i) wrap_function(rhss[[i]], pipes[[i]], parent)
  )
  env[["_fseq"]] <- `class<-`(eval(quote(
    function(value) freduce(value, `_function_list`)),
    env, env), c("fseq", "function")
  )
  env[["freduce"]] <- freduce
  if (is_placeholder(lhs)) {
    env[["_fseq"]]
  } else {
    env[["_lhs"]] <- eval(lhs, parent, parent)
    if (!is.null(groups(env[["_lhs"]])) || inherits(env[['_lhs']], 'rowwise_df')) {
      log_output("This data.frame already has groups - did you forget to call `ungroup()` earlier?", 'STRICTLYR_PIPE')
    }
    result <- withVisible(eval(quote(`_fseq`(`_lhs`)), env, env))
    if (is_compound_pipe(pipes[[1L]])) {
      eval(call("<-", lhs, result[["value"]]), parent, parent)
    }  else {
      if (!is.null(groups(result[["value"]])) || inherits(result[["value"]], 'rowwise_df')) {
        log_output("The end result of this operation still has groups - did you mean to call `ungroup()` as well?", 'STRICTLYR_PIPE')
      }
      if (result[["visible"]])
        result[["value"]]
      else
        invisible(result[["value"]])
    }
  }
}