#https://www.ninds.nih.gov/News-Events/News-and-Press-Releases/Press-Releases/Want-learn-new-skill-Take-some-short-breaks




#https://stat.ethz.ch/R-manual/R-devel/library/base/html/taskCallback.html

#' This just echos the input expressoion
#'
#' @param param s-language expression
#' @param value result of the expression evaluation
#' @param status logical indicating success or not
#' @param visible was the output printed
#'
#' @param a data object that is accessible to the callback ( passed in from addTaskCallback)
#'
#' @return I guess this indicates if the callback succeeded?
echoExpressionCallback = function(expr, value, status, visible, data){
  print( paste0( str( expr ) ) )

  TRUE
}


#' This is the main callback.  It can get called on any expression.
#'  If you call it, it will modify the your state
#'
#' @param param s-language expression
#' @param value result of the expression evaluation
#' @param status logical indicating success or not
#' @param visible was the output printed
#'
#' @param a data object that is accessible to the callback ( passed in from addTaskCallback)
#'
#' @return I guess this indicates if the callback succeeded?
addCallCountsCallback = function(expr, value, status, visible, data){

  get_functions( expr, getCallCountsHashTable(), needs_substitute = FALSE )


  if ( getOption("remembr.should_persist") ){
    saveCallCountsHashTable()


  }
  TRUE
}


clearCallCounts = function(){
  env = getCallCountsHashTable()
  rm( list= ls( env ), envir = env)
}



showCallCounts = function(){
  ls( getCallCountsHashTable() )
}


loadOrCreateEnv = function(path = NULL, default_env_path = NULL ){
  if ( is.null(path)){
      return( new.env( hash = TRUE, parent = emptyenv()))
  }

  if (file.exists(path)){
    readRDS( path )
  } else {
    if (!is.null( default_env_path)){
      readRDS(default_env_path)
    } else {
      #http://adv-r.had.co.nz/Environments.html
      #this is a trick for creating private variables.. otherwise packages are not allowed to modify state.
      #also this is what we wanted to do anyway, a new.env is the same as a hash table in R
      new.env( hash = TRUE, parent = emptyenv())
    }


  }
}


#can also use .first and .last
#https://www.statmethods.net/interface/customizing.html
#https://github.com/HenrikBengtsson/startup/blob/master/R/startup.R

#https://github.com/HenrikBengtsson/startup/blob/master/R/install.R

#opts = new.env( hash = TRUE, parent = emptyenv())
#opts[["should_persist"]] = TRUE
storage_file_directory = "~/.rRemembr/"

#if ( opts[["should_persist"]]){
#  dir.create(storage_file_directory,showWarnings = FALSE)
#}


call_counts_hash_table_path = file.path( storage_file_directory, "call_counts_hash_table.Rds" )

# we want only one R process modifying the call_counts_hash_table at a time
# here, if a second process launches, it will take control of the writes
# previously, whichever process _wrote_ last got to save this state.
# with this patch, it will be whoever launched last
last_known_modified_time = Sys.time()

storage_hash_table_path =file.path( storage_file_directory,"storage_hash_table_path.Rds" )
storage_hash_table = loadOrCreateEnv( storage_hash_table_path )

reloadCallCountsHashTable = function(){


  options( 'remembr.call_counts_hash_table' = loadOrCreateEnv(call_counts_hash_table_path) )
}

saveCallCountsHashTable = function(call_counts_hash_table = NULL){

  if ( is.null(call_counts_hash_table )){
    call_counts_hash_table = getCallCountsHashTable()
  }
  if( !file.exists(dirname(call_counts_hash_table_path))){
    stop(paste0( "Could not save call counts hash table, because ", dirname(call_counts_hash_table_path), " does not exist.  Please run remembr::install_remembr() to clear this message.") )
  }

  #if ( !file.exists(call_counts_hash_table_path)){
  #  last_modified_time = as.POSIXct(0, origin ='1970-01-01')
  #} else {
  #  last_modified_time = file.mtime(call_counts_hash_table_path )
  #}
  #if ( last_modified_time > last_known_modified_time ){
    #Then don't write anything!
  #} else {
    saveRDS(  call_counts_hash_table, call_counts_hash_table_path, compress = TRUE )
  #  last_known_modified_time = file.mtime( call_counts_hash_table_path )
  #}

}


#'
#'
#' Initializes the options
#'
#' This is private method and is called either in onLoad or in initRemembr
#'
initOptions = function(){
  #call_counts_hash_table = loadOrCreateEnv( call_counts_hash_table_path, "data/default_call_counts_hash_table.Rds" ) #new.env( hash = TRUE, parent = emptyenv())

  call_counts_hash_table = loadOrCreateEnv( call_counts_hash_table_path, NULL )


  op = options()
  op.remembr= list(
    remembr.should_persist = TRUE,
    remembr.call_counts_hash_table_path = call_counts_hash_table_path,
    remembr.call_counts_hash_table = call_counts_hash_table
  )
  options( op.remembr )
  toset <- !(names(op.remembr) %in% names(op))
  if(any(toset)) options(op.remembr[toset])

}


getCallCountsHashTable = function(){
  #call_counts_hash_table
  getOption("remembr.call_counts_hash_table")
}

#' @export
initRemembr = function(){
  initOptions()
  removeTaskCallback("addCallCounts")
  addTaskCallback(  addCallCountsCallback, name = "addCallCounts", data = getCallCountsHashTable())
  invisible(TRUE)
}

stopRemembr = function(){
  removeTaskCallback("addCallCounts")
}

.initializeLibraries = function(libraries){
  if ( is.null(libraries)){
    libraries = loadOrCreateEnv()
  }
  if ( is.character(libraries)){
    l = libraries
    libraries = loadOrCreateEnv()
    for ( lib in l ){
      libraries[[lib]] = TRUE
    }
  }
  libraries
}


.safely_get_namespace = purrr::safely(getNamespace)

# No side effects from call to library!
# initRemembr()

#'
#' get_functions
#'
#' @description
#' If call_counts_hash_table is NULL, then it will use a default value.
#' Otherwise it needs an environment which is created like this new.env( hash = TRUE, parent = emptyenv())
#'
#' @importFrom rlang quo_set_expr
#' @importFrom rlang quo_set_env
#' @importFrom rlang quo_get_expr
#' @importFrom rlang eval_tidy
#' @importFrom rlang is_quosure
#' @importFrom rlang get_expr
#' @importFrom pryr standardise_call
#' @importFrom pryr where
#' @importFrom globals walkAST
#' @importFrom lubridate now
#' @importFrom lubridate days
get_functions= function( expression,
                         call_counts_hash_table = NULL,
                         needs_substitute = TRUE,
                         libraries = NULL ,
                         calling_environment = NULL,
                         throw_errors = FALSE,
                         documentation_url = NULL){
  #print("analyzing functions")

  #TODO: not sure if this is necessary or helpful or doing the right hting
  if ( is.null( calling_environment )){
    calling_environment = parent.frame()
  }

  if ( needs_substitute ){
    sub = substitute(expression )#, calling_environment )
  } else {
    sub = expression
  }
  #print( sub)


  libraries = .initializeLibraries(libraries)




  ## internal handler for calls in get_functions
  .handleCall = function( call ){
  #  print("HANDLING CALL")
  #  print( call )

    could_not_get_keyname = tryCatch({
      keyname = .get_keyname_from_call( call, calling_environment, libraries )
      FALSE
    },
    error = function(e) {
      message(paste0("Message from remembr: ", e))
      cat("\n")
      if ( throw_errors){
        stop(e)
      } else {
        TRUE
      }

    })

    if( could_not_get_keyname ){
      return(call)
    }
    #keyname = .get_keyname_from_call( call )


    if( keyname == 'base::library' | keyname == 'base::require'   ){
      library_name = as.character(call[[2]])
      ns = .safely_get_namespace(library_name)
      if ( is.null(ns$error)){
        libraries[[ library_name  ]] = TRUE
      } else {
        message( ns$error )
      }

    }

    updateCard(keyname, call_counts_hash_table = call_counts_hash_table)
    #addDocumentationURL(keyname, documentation_url, call_counts_hash_table = call_counts_hash_table)

    call
  }

  #printCall = function( call ) {}
  .printAtomic = function( atomic ){
    #print( paste0( "atomic: ", atomic) );
    atomic
  }
  .printName = function( name ) {
    #print( paste0( "name: ", name) );
    #print(str(name))
    name
  }

  globals::walkAST( expr = sub, atomic = .printAtomic, name =  .printName, call = .handleCall)
}


# make sure to pass in a standardized call from pryr::Standardize_call
.get_keyname_from_call = function( call, calling_environment, libraries ){
  for ( library in ls(libraries)){  # TRY THE LIBRARY LIST FIRST
    function_name  = call[[1]] %>% as.character()
    if ( exists( as.character(function_name) , getNamespace(library), inherits = FALSE ) ){
      keyname = paste0( library , "::", function_name)
      return(keyname)
    }
  }

  err = NULL
  could_not_standardise_call = tryCatch({
    standardised_call = pryr::standardise_call( call, env = calling_environment )
    FALSE
  },
  error = function(e) {
    err <<- e
    TRUE
  })

  if (!is.null(err)){
    stop(err)
  }



  function_name = standardised_call[[1]]

  #https://stackoverflow.com/questions/592322/php-expects-t-paamayim-nekudotayim/592326
  contains_nekudotayim = grepl("::", deparse(function_name))

  ##TODO:
  # IF IT IS 'library' THEN PRINT IT OUT!

  if(contains_nekudotayim){
    keyname = deparse(function_name)
    #make me wonder if we should handle all of this in the name checker instead
  } else{

    error = NULL
    tryCatch(
      {

        qq = rlang::quo(function_name)

        #FIXME:
        # so - we are basically assuming that if we are in a nested series of functions, that no function is ever redefined.
        # this is not the safest of assumptions, and will often be incorrect
        # however, the only way I can think to handle this is by actually executing the code.
        # Presently this is written like a parser or a compile-time thing.
        # In order to properly evaluate the environments of things, we would actually need to do this at runtime; or somehow hook into every method that could potentially modify a calling environment.  Maybe these is a way?
        qq = rlang::quo_set_expr(qq, function_name)
        qq = rlang::quo_set_env(qq, calling_environment) #TODO: verify this

        expression_string = deparse( rlang::quo_get_expr(qq) )

        environment = pryr::where( expression_string, env = calling_environment )
      },
      error = function(e){
        cat(paste0("remembr:: failed on ", expression_string))

        error <<- e

      }
    )


    if ( !is.null(error) ){ stop(error )}

    environmentName = environmentName( environment )
    environmentName = gsub( pattern = "package:", replacement =  "", x =  environmentName)
    keyname = paste0( environmentName , "::", function_name)



  }
}



.print_ast= function( expression ){

  sub = substitute(expression)
  print( sub)

  printName = function( name ) {print( paste0( "name: ", name) ); name}
  printCall = function( call ) {print( paste0( "call: ", paste0(call, collapse = " ::: ")) ); call}
  printAtomic = function( atomic ){ print( paste0( "atomic: ", atomic) ); atomic}
  globals::walkAST( expr = sub, atomic = printAtomic, name =  printName, call =printCall)
}

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


