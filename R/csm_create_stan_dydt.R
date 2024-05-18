#' Create a Stan function for rate of change of state variables
#'
#' @md
#'
#' @param name name of the resulting Stan function
#' @param states a list vector containing CSM state
#'   variables defined using [csmdeveloper::csm_create_state()]
#'   in the intended order
#' @param arg_names a named character vector whose elements
#'   correspond to the argument name in the function to be
#'   generated and whose names correspond to either `states`
#'   or the name of the argument in `...`
#' @param ... optional arguments of list vectors containing
#'   CSM parameters defined using [csmdeveloper::csm_create_parameter()]
#'   or CSM variables defined using [csmdeveloper::csm_create_variable()]
#'
#' @export
csm_create_stan_dydt <- function(name, state, arg_names = c(state = "y"),
                                 ...){

  dots_list <- list(...)

  state_type <- get_v_type(state)

  state_arg <- paste0(state_type, " ", arg_names["state"])

  if(length(dots_list) > 0){
    dots_args <- get_dots_args(dots_list, arg_names)
  }else{
    dots_args <- NULL
  }

  dydt_args <- paste0(c(state_arg, dots_args),
                      collapse = ", ")

  dydt_signature <- paste0(state_type, " ", name, "(", dydt_args, ")")

  dydt_state <-
    c("// State variables:",
      mapply(dydt_declaration,
             .i = 1:length(state),
             .name = names(state),
             .var = state,
             .in = arg_names["state"],
             .in_type = state_type),
      ""
      )

  if(length(dots_list) == 0){
    dydt_dots <- NULL
  }else{
    dydt_dots <- get_dydt_dots(dots_list, arg_names)
  }

  dydt_rates <-
    c(
      "// Calculation of rate of change for state variables:",
      mapply(dydt_rate_eqs,
             .i = 1:length(state),
             .state = state,
             .state_name = arg_names["state"]),
      ""
    )

  dydt_body <- c(
    dydt_state,
    dydt_dots,
    dydt_rates
  )

  dydt_function <- c(
    paste0(dydt_signature, "{"),
    "",
    paste0("  ", dydt_body),
    paste0("  return d", arg_names["state"], "_dt;"),
    "}",
    ""
  )

  return(dydt_function)
}

dydt_declaration <- function(.i, .name, .var, .in, .in_type = "vector"){
  if(.in_type == "vector"){
    .in_ind <- paste0("[", .i, "]")
  }else{
    .in_ind <- ""
  }
  c(
    paste0("// ", .name, ": ",
           .var,
           " (", attr(.var, "units"), ")"),
    paste0("real ", .name, " = ", .in, .in_ind, ";")
  )
}

dydt_rate_eqs <- function(.i, .state, .state_name){
  paste0("d", .state_name, "_dt[", .i, "] = ",
         attr(.state, "equation")[2],";")
}

get_dots_args <- function(dots_list, arg_names){

  dots_type <-
    dots_list |>
    lapply(get_v_type) |>
    unlist()

  dots_name <-
    1:length(dots_list) |>
    lapply(get_arg_name,
           .v_list = dots_list,
           arg_names = arg_names) |>
    unlist()

  dots_args <-
    paste0(dots_type, " ", dots_name)

  return(dots_args)
}

get_v_type <- function(.v){
  if(length(.v) == 1){
    return("real")
  }else{
    return("vector")
  }
}

get_arg_name <- function(.i, .v_list, arg_names){
  if(names(.v_list)[.i] %in% arg_names){
    .arg_name <- arg_names[names(.v_list)[.i]]
  }else{
    .arg_name <- paste0("arg_", .i)
  }
  return(.arg_name)
}

get_declaration <- function(.d, dots_list, arg_names){

  .d_name <- get_arg_name(.d, dots_list, arg_names)

  .d_type <- get_v_type(dots_list[[.d]])

  mapply(dydt_declaration,
         .i = 1:length(dots_list[[.d]]),
         .name = names(dots_list[[.d]]),
         .var = dots_list[[.d]],
         .in = .d_name,
         .in_type = .d_type,
         SIMPLIFY = FALSE) |>
    lapply(paste0, collapse = "\n")
}

is_csm_parameter <- function(.v){
  "csm_parameter" %in% class(.v)
}

get_dydt_dots <- function(dots_list, arg_names){

  dots_declaration <-
    1:length(dots_list) |>
    lapply(get_declaration,
           dots_list = dots_list,
           arg_names = arg_names) |>
    unlist()

  dots_is_parameter <-
    dots_list |>
    lapply(\(.x) sapply(.x, is_csm_parameter)) |>
    unlist()

  if(!any(dots_is_parameter)){
    dydt_parameters <- NULL
  }else{
    dydt_parameters <-
      dots_is_parameter |>
      (\(.x) dots_declaration[.x])() |>
      (\(.x) c("// Parameters:", .x))() |>
      strsplit(split = "\n") |>
      unlist()
  }

  if(all(dots_is_parameter)){
    dydt_other_variables <- NULL
  }else{
    dydt_other_variables <-
      dots_is_parameter |>
      (\(.x) dots_declaration[!.x])() |>
      (\(.x) c("// Other Variables:", .x))() |>
      strsplit(split = "\n") |>
      unlist()
  }

  dydt_dots <- c(dydt_parameters,
                 "",
                 dydt_other_variables,
                 "")

  return(dydt_dots)
}
