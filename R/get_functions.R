#https://www.ninds.nih.gov/News-Events/News-and-Press-Releases/Press-Releases/Want-learn-new-skill-Take-some-short-breaks

print_ast= function( expression ){

  sub = substitute(expression)
  print( sub)

  printName = function( name ) {print( paste0( "name: ", name) ); name}
  printCall = function( call ) {print( paste0( "call: ", paste0(call, collapse = " ::: ")) ); call}
  printAtomic = function( atomic ){ print( paste0( "atomic: ", atomic) ); atomic}
  globals::walkAST( expr = sub, atomic = printAtomic, name =  printName, call =printCall)
}


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


  if ( opts$should_persist ){
    saveRDS(  getCallCountsHashTable(), call_counts_hash_table_path, compress = TRUE )

  }
  TRUE
}


clearCallCounts = function(){
  rm( list= ls( call_counts_hash_table ), envir = call_counts_hash_table)
}



showCallCounts = function(){
  ls( call_counts_hash_table )
}


loadOrCreateEnv = function(path = NULL){
  if ( is.null(path)){
    return( new.env( hash = TRUE, parent = emptyenv()))
  }

  if (file.exists(path)){
    readRDS( path )
  } else {
    #http://adv-r.had.co.nz/Environments.html
    #this is a trick for creating private variables.. otherwise packages are not allowed to modify state.
    #also this is what we wanted to do anyway, a new.env is the same as a hash table in R
    new.env( hash = TRUE, parent = emptyenv())
  }
}


#can also use .first and .last
#https://www.statmethods.net/interface/customizing.html
#https://github.com/HenrikBengtsson/startup/blob/master/R/startup.R

#https://github.com/HenrikBengtsson/startup/blob/master/R/install.R

opts = new.env( hash = TRUE, parent = emptyenv())
opts[["should_persist"]] = TRUE
storage_file_directory = "~/.rRemembr/"

if ( opts[["should_persist"]]){
  dir.create(storage_file_directory,showWarnings = FALSE)
}

call_counts_hash_table_path = file.path( storage_file_directory, "call_counts_hash_table.Rds" )
call_counts_hash_table = loadOrCreateEnv( call_counts_hash_table_path ) #new.env( hash = TRUE, parent = emptyenv())

storage_hash_table_path =file.path( storage_file_directory,"storage_hash_table_path.Rds" )
storage_hash_table = loadOrCreateEnv( storage_hash_table_path )


getCallCountsHashTable = function(){
  call_counts_hash_table
}

#' @export
initRemembr = function(){
  removeTaskCallback("addCallCounts")
  addTaskCallback(  addCallCountsCallback, name = "addCallCounts", data = getCallCountsHashTable())
  invisible(TRUE)
}

initRemembr()

#'
#'
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
get_functions= function( expression, call_counts_hash_table = NULL, needs_substitute = TRUE ){
  #print("analyzing functions")

  #TODO: not sure if this is necessary or helpful or doing the right hting
  calling_enviroment = parent.frame()

  if ( needs_substitute ){
    sub = substitute(expression )#, calling_enviroment )
  } else {
    sub = expression
  }
  #print( sub)

  printName = function( name ) {
    #print( paste0( "name: ", name) );
    #print(str(name))
    name
  }

  ## internal handler for calls in get_functions
  handleCall = function( call ){
    #print("HANDLING CALL")
    #print( call )
#    shouldStop = FALSE
#  shouldStop =  tryCatch({
    standardised_call = pryr::standardise_call( call )
#    return(FALSE)
#  },
#  finally = function(e){
#    return(FALSE)
#  }
#  ), error =  function(e){
#    print(e)
#    print("BANANA")
#    return( TRUE )

#  })
#  print("BANANA")
#  print(shouldStop)
#    shouldStop = FALSE
#    if (shouldStop == TRUE){ print("APPLE"); return(call); }


    #print( standardised_call )
    #print(str(standardised_call))

    function_name = standardised_call[[1]]
    #print(str(function_name))
    #print(paste0('function name: ', deparse(function_name)))


    #https://stackoverflow.com/questions/592322/php-expects-t-paamayim-nekudotayim/592326
    contains_nekudotayim = grepl("::", deparse(function_name))
    #print (paste0( "has double colon:  ", (contains_nekudotayim)))
    #if (function_name == "::"){  #handle specific cases where the called function is `::`.  Like when we call  addTargetFunctions( dplyr::summarise )
    # print( "WE ARE HERE")

    #  keyname = paste0( standardised_call$pkg, "::", standardised_call$name)
    #}

    if(contains_nekudotayim){
      #print("has doube colon")
      #we already got it, so do nothing
      keyname = deparse(function_name)
      #make me wonder if we should handle all of this in the name checker instead
    } else{

      #print( "NOW WE ARE HERE")
      qq = rlang::quo(function_name)


      #FIXME:
      # so - we are basically assuming that if we are in a nested series of functions, that no function is ever redefined.
      # this is not the safest of assumptions, and will often be incorrect
      # however, the only way I can think to handle this is by actually executing the code.
      # Presently this is written like a parser or a compile-time thing.
      # In order to properly evaluate the environments of things, we would actually need to do this at runtime; or somehow hook into every method that could potentially modify a calling environment.  Maybe these is a way?
      qq = rlang::quo_set_expr(qq, function_name)
      qq = rlang::quo_set_env(qq, calling_enviroment) #TODO: verify this

      expression_string = deparse( rlang::quo_get_expr(qq) )
      #print(paste0("expression_string", expression_string))
      tryCatch({
        environment = pryr::where( expression_string, env = calling_enviroment )


      },

      error = function(e){

        stop(e)
      })

      #print(qq)
      #environment = environment( rlang::eval_tidy( qq ))
      environmentName = environmentName( environment )
      environmentName = gsub( pattern = "package:", replacement =  "", x =  environmentName)
      keyname = paste0( environmentName , "::", function_name)

    }

    #print( keyname )

    if (is.null(call_counts_hash_table)){
      call_counts_hash_table = getCallCountsHashTable()
    }
    prev_record = call_counts_hash_table[[keyname]]

    if ( is.null (prev_record )){
      prev_record = list( first_use =   lubridate::now(tzone = 'UTC'),
                          most_recent_use = lubridate::now(tzone = 'UTC'),
                          total_uses = 1,
                          bucket_id = nextBucket(NULL) )
    } else {
      if ( is.null( prev_record$bucket_id )){
        prev_record$bucket_id = nextBucket(c())
      }
      bucket_timer = as.numeric( getDurationFromBucketId( prev_record$bucket_id ) )
      now_t = lubridate::now(tzone = 'UTC')
      dt = difftime(prev_record$most_recent_use + bucket_timer,now_t  )

       if(  dt < 0 ){
        next_bucket = nextBucket( prev_record$bucket_id )
      } else {
        next_bucket = prev_record$bucket_id
      }

      prev_record  = list(
        first_use= prev_record$first_use,
        most_recent_use = lubridate::now(tzone= "UTC"),
        total_uses = prev_record$total_uses + 1,
        bucket_id = next_bucket
        )
    }
    #names( prev_record ) = c( 'first_use', 'most_recent_use', 'total_uses')

    call_counts_hash_table[[keyname]] = prev_record

    #print( paste0( "call: ", paste0(call, collapse = " ::: ")) );
    call
  }
  #printCall = function( call ) {}
  printAtomic = function( atomic ){
    #print( paste0( "atomic: ", atomic) );
    atomic
  }
  globals::walkAST( expr = sub, atomic = printAtomic, name =  printName, call =handleCall)
}


getObjectFromName = function( name ){
  get( name )
}


#'
#' Given an id, get the id for the next bucket
#'
#' If bucket_id is null, then start from 1
#'
#'
nextBucket = function(bucket_id ){
  if ( is.null(bucket_id) | length( bucket_id) == 0){
    1
  }else if ( bucket_id < 6 ){
    bucket_id + 1
  } else {
    bucket_id
  }
}


#' Given a bucket id, return a timer for that bucket
#'
#'
#' @importFrom lubridate minutes
#' @importFrom lubridate hours
#' @importFrom lubridate days
getDurationFromBucketId = function(bucket_id){
  sapply( bucket_id, function(a){
  switch( as.character(a) ,
          '1' = as.numeric( lubridate::minutes(10 )),
          '2' = as.numeric( lubridate::minutes(60 )),
          '3' = as.numeric( lubridate::hours(6 )),
          '4' = as.numeric( lubridate::hours(24 )),
          '5' = as.numeric( lubridate::days(7 )),
          '6' = as.numeric( lubridate::days(30 ))
          )})
}




