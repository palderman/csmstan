orig_stan_to_functions_stan <- function(stan_file){

  file_dir <- tempdir() |>
    file.path("functions") |>
    make_dir("stan")

  if(!dir.exists(file_dir)){
    dir.create(file_dir, recursive = TRUE)
  }

  file_name <- stan_file |>
    basename() |>
    (\(.x) gsub("\\.stan",
                "_functions.stan",
                .x))() |>
    (\(.x) file.path(file_dir, .x))()

  readLines(stan_file) |>
    (\(.x) c("functions{",
             paste0("  ", .x),
             "}",
             ""))() |>
    write(file_name)

  return(file_name)
}

make_dir <- function(orig_path, ...){

  new_dir <- do.call(file.path,
                     c(orig_path, list(...)))

  if(!dir.exists(new_dir)){
    dir.create(new_dir, recursive = TRUE)
  }

  return(new_dir)
}

make_file_name <- function(orig_name, new_dir, replace, regex = "\\.stan$"){
  orig_name |>
    basename() |>
    (\(.x) gsub(regex,
                replace,
                .x))() |>
    (\(.x) file.path(new_dir, .x))()
}

#' Compile functions defined in a Stan source file
#'
#' @param stan_file a length-one character vector
#'    with the path to a Stan source file
#'
#' @export
#'
csm_compile_stan_functions <- function(stan_file){

  functions_stan <-
    orig_stan_to_functions_stan(stan_file)

  functions_root_dir <-
    stan_file |>
    dirname() |>
    make_dir("functions") |>
    normalizePath()

  # create executable directory
  exe_dir <-
    functions_root_dir |>
    make_dir("exe")

  # Create shared library directory
  lib_dir <-
    functions_root_dir |>
    make_dir("lib")

  # Create R script directory
  R_dir <-
    functions_root_dir |>
    make_dir("R")

  # Create executable name
  exe_file_name <-
    functions_stan |>
    make_file_name(new_dir = exe_dir,
                   replace = "")

  # Create shared library name
  lib_file_name <-
    functions_stan |>
    make_file_name(new_dir = lib_dir,
                   regex = "stan$",
                   replace = get_lib_ext())

  # Create R file name
  R_file_name <-
    functions_stan |>
    make_file_name(new_dir = R_dir,
                   regex = "stan$",
                   replace = "R")

  fun_mod <- cmdstanr::cmdstan_model(
    stan_file = functions_stan,
    exe_file = exe_file_name)

  fun_mod$compile(force_recompile = TRUE, dry_run = TRUE)
  fun_mod$expose_functions()
  src_lib <- find_source_cpp_lib()
  file.copy(src_lib$lib_file, lib_file_name,
            overwrite = TRUE)
  src_lib$R_file |>
    readLines() |>
    (\(.x) gsub("[^ `]+_DLLInfo",
                "new",
                .x))() |>
    (\(.x) gsub("dyn\\.load\\([^)]+\\)",
                paste0("dyn.load('", lib_file_name, "')"),
                .x))() |>
    write(R_file_name)

  return(c(exe_file_name,
           lib_file_name,
           R_file_name))
}

get_lib_ext <- function(){
  if(Sys.info()["sysname"] == "Windows"){
    lib_ext <- "dll"
  }else{
    lib_ext <- "so"
  }
  return(lib_ext)
}

find_source_cpp_lib <- function(){
  lib_ext <- get_lib_ext()
  lib_file <-
    tempdir() |>
    list.files(pattern = "sourceCpp",
               full.names = TRUE) |>
    list.files(pattern = "sourcecpp",
               full.names = TRUE) |>
    list.files(full.names = TRUE,
               pattern = lib_ext) |>
    file.info() |>
    (\(.x) row.names(.x[which.max(.x$mtime),]))()
  R_file <-
    lib_file |>
    dirname() |>
    list.files(full.names = TRUE,
               pattern = ".R$")
  return(list(lib_file = lib_file, R_file = R_file))
}
