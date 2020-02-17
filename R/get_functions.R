

#https://www.ninds.nih.gov/News-Events/News-and-Press-Releases/Press-Releases/Want-learn-new-skill-Take-some-short-breaks



clearCallCounts = function(){
  env = getCallCountsHashTable()
  rm( list= ls( env ), envir = env)
}



showCallCounts = function(){
  ls( getCallCountsHashTable() )
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
#' @param expression an R expression.  This will be not be evaluated; but it will be parsed
#' and the methods found will be returned in the result
#' @param call_counts_hash_table A call_counts_hash_table environment; this is where results get written to.
#' @param needs_substitute ( TODO NOT SURE)
#' @param libraries ( TODO NOT SURE )
#' @param calling_environment The calling environment for the expression.
#'  get_functions will try to use the calling_environment to figure out which package is being referenced. Defaults to parent.frame()
#' @param throw_errors If TRUE this will throw an error if it cannot parse the expression; otherwise it will print a message
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
                         throw_errors = FALSE){
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
