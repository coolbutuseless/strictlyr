
freduce <- function (value, function_list) {
  k <- length(function_list)
  if (k > 1) {
    for (i in 1:(k - 1L)) {
      value <- function_list[[i]](value)
    }
  }
  value <- withVisible(function_list[[k]](value))
  if (value[["visible"]])
    value[["value"]]
  else invisible(value[["value"]])
}


is_compound_pipe <- function (pipe) {
  identical(pipe, quote(`%<>%`))
}

is_dollar <- function (pipe) {
  identical(pipe, quote(`%$%`))
}


# these are functions copied from magrittr that are not exported from that package


is_first <- function (expr) {
  !any(vapply(expr[-1], identical, logical(1), quote(.)))
}

is_function <- function (expr) {
  is.symbol(expr) || is.function(expr)
}

is_funexpr <- function (expr) {
  is.call(expr) && identical(expr[[1]], quote(`{`))
}

is_parenthesized <- function (expr) {
  is.call(expr) && identical(expr[[1]], quote(`(`))
}

is_pipe <- function (pipe) {
  identical(pipe, quote(`%>%`)) || identical(pipe, quote(`%T>%`)) ||
    identical(pipe, quote(`%<>%`)) || identical(pipe, quote(`%$%`))
}

is_placeholder <- function (symbol) {
  identical(symbol, quote(.))
}

is_tee <- function (pipe) {
  identical(pipe, quote(`%T>%`))
}

prepare_first <- function (expr) {
  as.call(c(expr[[1L]], quote(.), as.list(expr[-1L])))
}

prepare_function <- function (f) {
  as.call(list(f, quote(.)))
}

split_chain <- function (expr, env) {
  rhss <- list()
  pipes <- list()
  i <- 1L
  while (is.call(expr) && is_pipe(expr[[1L]])) {
    pipes[[i]] <- expr[[1L]]
    rhs <- expr[[3L]]
    if (is_parenthesized(rhs))
      rhs <- eval(rhs, env, env)
    rhss[[i]] <- if (is_dollar(pipes[[i]]) || is_funexpr(rhs))
      rhs
    else if (is_function(rhs))
      prepare_function(rhs)
    else if (is_first(rhs))
      prepare_first(rhs)
    else rhs
    if (is.call(rhss[[i]]) && identical(rhss[[i]][[1L]],
                                        quote(`function`)))
      stop("Anonymous functions myst be parenthesized",
           call. = FALSE)
    expr <- expr[[2L]]
    i <- i + 1L
  }
  list(rhss = rev(rhss), pipes = rev(pipes), lhs = expr)
}

wrap_function <- function (body, pipe, env) {
  if (is_tee(pipe)) {
    body <- call("{", body, quote(.))
  }
  else if (is_dollar(pipe)) {
    body <- substitute(with(., b), list(b = body))
  }
  eval(call("function", as.pairlist(alist(. = )), body), env,
       env)
}
