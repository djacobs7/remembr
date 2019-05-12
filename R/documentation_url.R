


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
