

#unusued?
addTargetFunctions = function( ... ){
  arguments = rlang::enquos(...)


  lapply( arguments, function(qq){
    ll = getFunctionPropertiesFromQuosure(qq )
    storage_hash_table[[ ll$keyname ]] = ll
  })
  storage_hash_table
}

#unused?
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
    stop("This function expects a quosure")
  }
  result
}

#'
#'
#' Add documentation for a function
#'
#' Enter a link that you think documents this function well. IT will show up when you do flashcards
#' ( not supported yet! )
#'
#' @param targetFunction the target function that documentation is being added for
#' @param urls the urls that are added as documentation
#' @param call_counts_hash_table the call count hash table ( or defaults to the remembr one)
#'
#' @export
addDocumentationURL = function( targetFunction, urls , call_counts_hash_table = NULL ){

  if ( is.null(call_counts_hash_table)){
    call_counts_hash_table = getCallCountsHashTable()
  }
  #addTargetFunction( targetFunction )  #not sure how to do this part!

  if ( is.character(targetFunction)){
    keyname = targetFunction
  } else {
    qq = rlang::enquo( targetFunction )
    props = getFunctionPropertiesFromQuosure( qq )
    keyname = props$keyname
  }

  if (is.null(urls) | length(urls) == 0){
    return()
  }

  present_card = call_counts_hash_table[[keyname]]

  if ( is.null( present_card)){
    stop(paste0( "card ",keyname,  " does not exist, could not add documentation "))
  }

  present_card$urls = unique( c( present_card$urls, urls ) )
  call_counts_hash_table[[keyname]] = present_card

  call_counts_hash_table
}


#' Get documentation for a function
#'
#' Pass in a function and get any urls associated with it
#'
#' @param targetFunction a function reference that you want to get documentation urls for
#' @param call_counts_hash_table the call counts hash table ( uses remebmr one by default )
#'
#' @export
getDocumentationURLs = function( targetFunction , call_counts_hash_table = NULL ){

  if ( is.null(call_counts_hash_table)){
    call_counts_hash_table = getCallCountsHashTable()
  }
  if ( is.character(targetFunction)){
    keyname = targetFunction
  } else{
    qq = rlang::enquo( targetFunction )

    props = getFunctionPropertiesFromQuosure( qq )
    keyname = props$keyname
  }

  if ( rlang::env_has(call_counts_hash_table, keyname)){
    call_counts_hash_table[[keyname]]$urls
  } else {
    return( character()  )
  }

}


#showDocumentationUrls = function( storage_env ){
#  ls ( storage_env )
#}
