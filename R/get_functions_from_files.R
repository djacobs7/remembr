
#======  Get Functions From a File ( rmd or R)

#' Get functions from a file
#'
#' @examples
#' env = getFunctionsFromFile('~/git/leitnr/R/get_functions.R' )
#'
#' TODO: should output a list of missing pacakges
#'
#' @importFrom tools file_ext
#' @importFrom rlang parse_exprs
#' @importFrom purrr safely
#' @importFrom purrr map
getFunctionsFromFiles = function(paths, output_env = NULL, libraries = NULL){
  if( is.null(output_env)){
    output_env= loadOrCreateEnv()
  }


  calling_environment = parent.frame()

  # if it's a directory, walk the directory
  if( dir.exists( paths )){

    # get all R or rmd files in directory
    pathst = dir(paths, "\\.R$",recursive=T, full.names = TRUE)
    #TODO: fix the capitalization
    paths = c(pathst, dir(paths, "\\.Rmd$",recursive=T, full.names = TRUE))

  } else {
    paths = paths
  }

  if ( is.null(libraries)){
    libraries = loadOrCreateEnv()
  }

  errors = purrr::map( paths,
                       .getFromFile,
                       call_counts_hash_table = output_env,
                       calling_environment = calling_environment,
                       libraries = libraries )
  list(
    libraries = libraries,
    cards = output_env,
    errors = errors )
}


#' @importFrom purrr safely
#' @importFrom purrr walk
.getFromFile = function(path, call_counts_hash_table, calling_environment, libraries  ){

  print(path)
  get_functions_safely = purrr::safely(get_functions)

  if ( is.null(calling_environment)){
    calling_environment = parent.frame()
  }

  errors = c()

  parse_error = tryCatch({
    if ( tools::file_ext(path) == 'Rmd'){
      parseable = knitr::purl(text = readr::read_file(path))
    } else {
      parseable = file(path)
    }

    exprs = rlang::parse_exprs( parseable )
    FALSE
  },
  error = function(e){
    message(paste0("parse failure for ", path))
    TRUE
  })

  if (parse_error){
    return(errors)
  }
  #TODO: fix windows style line breaks


  result = purrr::map( exprs,
                       get_functions_safely,
                       call_counts_hash_table = call_counts_hash_table,
                       calling_environment = calling_environment,
                       needs_substitute = FALSE,
                       libraries = libraries,
                       throw_errors = TRUE,
                       documentation_url = path )

  errors = result %>% purrr::map( ~.x$errors )


  errors
}

#
#sample_rmd ='repos/tidytext/vignettes/tidytext.Rmd'
#
#path_name = sample_rmd
#if ( tools::file_ext(path_name) == 'Rmd'){
#  r_code = knitr::purl(path_name)
#}


