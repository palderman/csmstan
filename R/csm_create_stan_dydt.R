#' Create a Stan function for rate of change of state variables
#'
#' @md
#'
#' @param name name of the resulting Stan function
#' @param states a list vector containing CSM state
#'   variables defined using [csmdeveloper::csm_create_state()]
#'   in the intended order
#' @param parameters an optional list vector containing
#'    CSM parameters defined using [csmdeveloper::csm_create_parameter()]
#'    the names of all parameters
#' @param forcings an optional character vector
#'    containing the names of all forcing variables
#'
#' @export
csm_create_stan_dydt <- function(name, states,
                            parameters = NULL,
                            forcings = NULL,
                            state_name = "y",
                            parameter_name = "theta",
                            forcing_name = "forcings"){

  state_arg <- paste0("vector ", state_name)

  if(!is.null(parameters)){
    parameter_arg <- paste0("vector ", parameter_name)
  }else{
    parameter_arg <- NULL
  }

  if(!is.null(forcings)){
    forcings_arg <- paste0("row_vector ", forcings_name)
  }else{
    forcings_arg <- NULL
  }

  dydt_args <- paste0(c(state_arg, parameter_arg, forcings_arg),
                      collapse = ", ")

  dydt_signature <- paste0("vector ", name, "(", dydt_args, ")")

  dydt_states <-
    c("// State variables:",
      mapply(dydt_declaration,
             .i = 1:length(states),
             .name = names(states),
             .var = states,
             .vec_in = state_name),
      ""
      )

  if(is.null(parameters)){
    dydt_parameters <- NULL
  }else{
    dydt_parameters <-
      c("// Parameters:",
        mapply(dydt_declaration,
               .i = 1:length(parameters),
               .name = names(parameters),
               .var = parameters,
               .vec_in = parameter_name),
        ""
      )
  }

  dydt_rates <-
    c(
      "// Calculation of rate of change for state variables:",
      mapply(dydt_rate_eqs,
             .i = 1:length(states),
             .state = states,
             .state_name = state_name),
      ""
    )

  dydt_body <- c(
    dydt_states,
    dydt_parameters,
    dydt_rates
  )

  dydt_function <- c(
    paste0(dydt_signature, "{"),
    paste0("  ", dydt_body),
    paste0("  return d", state_name, "_dt;"),
    "}"
  )

  return(dydt_function)
}

dydt_declaration <- function(.i, .name, .var, .vec_in){
  c(
    paste0("// ", .name, ": ",
           .var,
           " (", attr(.var, "units"), ")"),
    paste0("real ", .name, " = ", .vec_in, "[", .i, "];")
  )
}

dydt_rate_eqs <- function(.i, .state, .state_name){
  paste0("d", .state_name, "_dt[", .i, "] = ",
         attr(.state, "equation")[2],";")
}
