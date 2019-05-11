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

#'
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


#'
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

  get_functions( expr, needs_substitute = FALSE )


  if ( opts$should_persist ){
    saveRDS(  call_counts_hash_table, call_counts_hash_table_path, compress = TRUE )

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
  if (!is.null(path) & file.exists(path)){
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



#' @export
initRemembr = function(){
  removeTaskCallback("addCallCounts")
  addTaskCallback(  addCallCountsCallback, name = "addCallCounts", data = call_counts_hash_table)
  invisible(TRUE)
}

initRemembr()
#' @import rlang
#' @importFrom pryr standardise_call
#' @importFrom pryr where
#' @importFrom globals walkAST
#' @import lubridate
get_functions= function( expression, needs_substitute = TRUE ){
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





addTargetFunctions = function( ... ){
  arguments = rlang::enquos(...)


  lapply( arguments, function(qq){
    ll = getFunctionPropertiesFromQuosure(qq )
    storage_hash_table[[ ll$keyname ]] = ll
  })
  storage_hash_table
}


getFunctionPropertiesFromQuosure = function(qq ){
  ##make sure you pass in a quosure
  if( rlang::is_quosure(qq)){
    keyname = NULL
    substituted =  rlang::get_expr(qq)
    if( is.call(substituted  )){ #handle function calls
      function_name = substituted[[1]]
      if (function_name == "::"){  #handle specific cases where the called function is `::`.  Like when we call  addTargetFunctions( dplyr::summarise )
        substituted = pryr::standardise_call( substituted )
        keyname = paste0( substituted$pkg, "::", substituted$name)
      }
    }

    ref = rlang::eval_tidy( qq )
    environment = environment( rlang::eval_tidy( qq ))
    environmentName = environmentName( environment )

    if ( is.null(keyname)){
      keyname = paste0( environmentName, '::' , deparse( rlang::quo_get_expr ( qq )  ) )
    }

    result = list(
      ref = ref,
      environment = environment,
      environmentName = environmentName,
      keyname = keyname
    )
  } else{
    throw("This function expects a quosure")
  }
  result
}

#' @export
addDocumentationURL = function( targetFunction, urls ){
  #addTargetFunction( targetFunction )  #not sure how to do this part!

  qq = rlang::enquo( targetFunction )

  props = getFunctionPropertiesFromQuosure( qq )
  keyname = props$keyname
  present_list = storage_hash_table[[keyname]]
  if ( is.null(present_list)){
    throw("make sure your target function is already added. FIXME!! add addTargetFunction atop")
  }
  present_list$urls = unique( c( present_list$urls, urls ) )
  storage_hash_table[[keyname]] = present_list
  #but the plan here is you can type in a url and this will persist it along with your function name.  easy breezy.
}

#' @export
getDocumentationURL = function( targetFunction ){
  qq = rlang::enquo( targetFunction )

  props = getFunctionPropertiesFromQuosure( qq )
  keyname = props$keyname
  storage_hash_table[[keyname]]$urls
}
showTargetFunctions = function(){
  ls ( storage_hash_table)
}

nextBucket = function(bucket_id ){
  if ( is.null(bucket_id) | length( bucket_id) == 0){
    1
  }else if ( bucket_id < 6 ){
    bucket_id + 1
  } else {
    bucket_id
  }
}

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



#'
#'
#' @import lubridate
#' @import dplyr
convertCallCountsToHashTable = function( call_counts_hash_table ){
  convertEnvToDataFrame  = function( call_counts_hash_table ){
    result = eapply(call_counts_hash_table, function(a){
      a$bucket_id = if(  'bucket_id' %in%  names(a) ) { a$bucket_id } else { nextBucket(c()); }
      a
    })

    df =do.call( rbind, result) %>%
      dplyr::as_tibble(rownames = 'function_name') %>%
      dplyr::mutate_all(unlist) %>%
      mutate( first_use = lubridate::as_datetime(first_use ),
              most_recent_use = lubridate::as_datetime(most_recent_use ),

      )
    df
  }

  df = convertEnvToDataFrame( call_counts_hash_table )

  #this becomes a default actually!
  df$bucket_timer = getDurationFromBucketId(df$bucket_id )


  df = df %>%
    tidyr::separate(function_name, c('package','name'), sep = '::', remove = FALSE) %>%
    mutate(package = ifelse( function_name =="::", 'base', package),
           name = ifelse( function_name =="::", '::', name)
           ) %>%
    mutate(
      review_timer = most_recent_use + bucket_timer,
      needs_review = ifelse(
        difftime(review_timer, lubridate::now() ) > 0 ,
        FALSE,
        TRUE
      ) )

  df
}

#'
reminder = function( num_functions = 5 ){


  df = convertCallCountsToHashTable(call_counts_hash_table )




  df %>%
    filter(needs_review) %>%
    filter( package != "R_GlobalEnv") %>%
    top_n( num_functions, desc(review_timer ))

  #how to do the algorithm?

  #1 ) Compute a bucket for each thing
  #2 ) Default bucket is immediate.

  #buckets: 10 minutes; 1 hour; 1 day; 1 week; 1 month

  #3 ) compute bucket
  #4 )

  #call_counts_hash_table

}

timeStampToIntervalString = function(times){
  timeDiffs = (difftime( lubridate::now(), times, units = 'mins'))

  getDiff = function( d ){
    if ( d < 1 ){
      return ('less than a minute ago')
    } else if ( d < 60 ){
      return(paste0( floor(d), ' minutes ago') )
    } else if ( d < 60*24 ){
      if ( floor( d/60) == 1){
        return(paste0( floor(d/60), ' hour ago') )
      } else{
        return(paste0( floor(d/60), ' hours ago') )
      }

    }else if ( d < 60*24*30 ){
      if ( floor( d/60) == 1){
        return(paste0( floor(d/(24*60)), ' day ago') )
      } else{
        return(paste0( floor(d/(24*60)), ' days ago') )
      }

    } else if ( d < 60*24*30*12 ) {
      return(paste0( floor(d/(24*60*30)), ' months ago') )
    } else {
      return( 'more than a year ago')
    }
  }
  sapply( timeDiffs, getDiff)
}


timeStampToIntervalString = function(times){
  timeDiffs = (difftime( lubridate::now(), times, units = 'mins'))

  getDiff = function( d ){
    if ( d < 1 ){
      return ('less than a minute ago')
    } else if ( d < 60 ){
      return(paste0( floor(d), ' minutes ago') )
    } else if ( d < 60*24 ){
      if ( floor( d/60) == 1){
        return(paste0( floor(d/60), ' hour ago') )
      } else{
        return(paste0( floor(d/60), ' hours ago') )
      }

    }else if ( d < 60*24*30 ){
      if ( floor( d/60) == 1){
        return(paste0( floor(d/(24*60)), ' day ago') )
      } else{
        return(paste0( floor(d/(24*60)), ' days ago') )
      }

    } else if ( d < 60*24*30*12 ) {
      return(paste0( floor(d/(24*60*30)), ' months ago') )
    } else {
      return( 'more than a year ago')
    }
  }
  sapply( timeDiffs, getDiff)
}
timeStampToIntervalStringFuture = function(times){
  timeDiffs = (difftime( times, lubridate::now(), units = 'mins'))

  getDiff = function( d ){
    if ( d < 1 ){
      return ('less than a minute from now')
    } else if ( d < 60 ){
      return(paste0( floor(d), ' minutes from now') )
    } else if ( d < 60*24 ){
      if ( floor( d/60) == 1){
        return(paste0( floor(d/60), ' hour from now') )
      } else{
        return(paste0( floor(d/60), ' hours from now') )
      }

    }else if ( d < 60*24*30 ){
      if ( floor( d/60) == 1){
        return(paste0( floor(d/(24*60)), ' day from now') )
      } else{
        return(paste0( floor(d/(24*60)), ' days from now') )
      }

    } else if ( d < 60*24*30*12 ) {
      return(paste0( floor(d/(24*60*30)), ' months from now') )
    } else {
      return( 'more than a year from now')
    }
  }
  sapply( timeDiffs, getDiff)
}


#' @export
remindPackage = function( packageName ){
  df = convertCallCountsToHashTable(call_counts_hash_table ) %>%
    filter( package == !!(packageName )) %>%
    arrange( ( review_timer ))

  package_string = df %>%
    mutate( str = paste0( "(", row_number(), ") " , crayon::bold(name) ,
                          " used ", crayon::bgWhite(total_uses), " times.  ",
                          "Last used ",
                          timeStampToIntervalString( most_recent_use ),
                          ".",
                          ifelse( needs_review, " Needs review.",
                                  paste0( "Needs review in ", timeStampToIntervalStringFuture( review_timer)) )
                          ) ) %>%
    select( str )

  cat(paste0( "\nHere is everything to review from the ",crayon::bgWhite(packageName) , " package\n"))
  cat(paste(package_string$str, collapse = "\n"))

  invisible(df )
}

#' @export
upcomingReminders = function(){
  df = convertCallCountsToHashTable(call_counts_hash_table ) %>%
    arrange( ( review_timer )) %>%
    filter( row_number() < 10 )

  package_string = df %>%
    mutate( str = paste0( "(", row_number(), ") " , crayon::bold(name) ,
                          " used ", crayon::bgWhite(total_uses), " times.  ",
                          "Last used ",
                          timeStampToIntervalString( most_recent_use ),
                          ".",
                          ifelse( needs_review, " Needs review. ",
                                  paste0( " Needs review ", timeStampToIntervalStringFuture( review_timer)) )
    ) ) %>%
    select( str )

  cat(paste0( "\nHere is everything to review\n"))
  cat(paste(package_string$str, collapse = "\n"))

  invisible(df )
}

#' @import crayon
#' @export
remindMe = function(){
  df = convertCallCountsToHashTable(call_counts_hash_table )

  package_reminders = df %>%
    filter( package != "R_GlobalEnv") %>%
    filter(needs_review) %>%
    group_by( package) %>%
    arrange( ( review_timer)) %>%
    filter( row_number() == 1) %>%
    select( package, function_name, review_timer)%>%
    ungroup() %>%
    top_n( 3, desc(review_timer ))

  top_5 = reminder( 5 )

  if ( nrow( top_5) == 0 ){
    cat("You have no methods to review at this time.  You can use remindPackage( packageName ) to see upcoming review items for a specific package.")
    return( invisible( df ))
  }

  result =top_5 %>%
    mutate( str = paste0( "(", row_number(), ") " , crayon::bold(name) , " from the ", crayon::bgWhite(package), " package") ) %>%
    select( str )

  package_string =package_reminders %>%
    mutate( str = paste0( "(", row_number(), ") " , crayon::bgWhite(package)) ) %>%
    select( str )

  cat("Based on your previous R usage, we recommend that you review the following methods\n")
  cat(paste(result$str, collapse = "\n"))

#  cat("\n\nYou will have upcoming review in the following packages\n")
#  cat(paste(package_string$str, collapse = "\n"))


  cat(paste0( "\n To review a method, just call it in the console or call ", crayon::bgWhite( 'flashCards()' ) ,"."))


  invisible(result)
}

#' @export
flashCards = function(num_flashcards = 5){

  df = convertCallCountsToHashTable(call_counts_hash_table )

  stack = df %>%
  #  filter(needs_review) %>%
    filter( package != "R_GlobalEnv") %>%
    top_n( num_flashcards, desc(review_timer ))

  if ( nrow( stack )== 0){
    cat("You have nothing to review")
    return(invisible(NULL))
  }


  cat("Get ready to start your flashcards.\n")
  cat("Look for help in the browser window.\n")
  readline( "Press any key to start\n")
  for ( i in 1:nrow(stack)){
    row = stack[i,]
    with(data = row, expr = {
      str = paste0(  crayon::bold(name) , " from the ", crayon::bgWhite(package), " package")
      prompt = paste0( "(", i, ") ", "Do you feel comfortable with ", str ,"? (y/n) " )

      if (is.na( package  )){
        package = 'base'
      }
      h = help( name, package = (package), help_type = "html")

        print(h)
      cat(prompt)
      cat("\n")
      yesNo = readline()

      keyname = paste0( package, "::", name )
      prev_record = call_counts_hash_table[[keyname]]

      if( is.null(prev_record)){
        error("record not found")
      }


      if( yesNo == 'y'| yesNo == 'Yes' | yesNo == 'YES'){
        prev_record$bucket_id = nextBucket( prev_record$bucket_id )
      } else if ( yesNo == 'n'| yesNo == 'No' | yesNo == 'NO') {

      } else if ( yesNo == 'q' | yesNo == 'Q' | yesNo == 'QUIT'| yesNo == 'quit' ){
        break
      }

      call_counts_hash_table[[keyname]] = prev_record
      #utils::askYesNo( prompt = "" )
    })




  }
}

#'
#'
#' Adds a line to your .Rprofile so that remembr runs by default.
#' Can be uninstalled at any time with uninstall_remembr
#' @export
install_remembr = function(){
  remembrinstall::install()
}

#'
#' Removes remembr from your rprofile
#'
#' @export
uninstall_remembr = function(){
  remembrinstall::uninstall()
}


extract_help <- function(pkg, fn = NULL, to = c("txt", "html", "latex", "ex")){
  to <- match.arg(to)
  rdbfile <- file.path(find.package(pkg), "help", pkg)
  rdb <- tools:::fetchRdDB(rdbfile, key = fn)
  convertor <- switch(to,
                      txt   = tools::Rd2txt,
                      html  = tools::Rd2HTML,
                      latex = tools::Rd2latex,
                      ex    = tools::Rd2ex
  )
  f <- function(x) capture.output(convertor(x))
  if(is.null(fn)) lapply(rdb, f) else f(rdb)
}
