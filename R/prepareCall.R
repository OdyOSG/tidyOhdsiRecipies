#' Prepare Call for Function in Environment
#'
#' This function prepares a call for a specified function with the given arguments.
#'
#' @param fn_name A character string specifying the name of the function.
#' @param args A named list of arguments to match with the function's parameters.
#'
#' @return A call object for the specified function.
#' @export
#'
#' @examples
#' \dontrun{
#' con <- eval(prepareCall(
#'   "DatabaseConnector::connect", list(
#'     connectionDetails = connectionDetails
#'   )
#' ))
#' }
prepareCall <- function(fn_name, args) {
  rlang::call2(
    eval(str2lang(fn_name)), !!!purrr::keep(
      args, names(args) %in%
        rlang::fn_fmls_names(
          eval(str2lang(fn_name))
        )
    )
  )
}
