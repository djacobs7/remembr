#https://www.ninds.nih.gov/News-Events/News-and-Press-Releases/Press-Releases/Want-learn-new-skill-Take-some-short-breaks

get_functions= function( expression ){

  sub = substitute(expression)
  print( sub)

  printName = function( name ) {print( paste0( "name: ", name) ); name}
  printCall = function( call ) {print( paste0( "call: ", paste0(call, collapse = " ::: ")) ); call}
  printAtomic = function( atomic ){ print( paste0( "atomic: ", atomic) ); atomic}
  globals::walkAST( expr = sub, atomic = printAtomic, name =  printName, call =printCall)
}


getObjectFromName = function( name ){
  get( name )
}



#http://adv-r.had.co.nz/Environments.html
#this is a trick for creating private variables.. otherwise packages are not allowed to modify state.
#also this is what we wanted to do anyway, a new.env is the same as a hash table in R
storage_hash_table = new.env( hash = TRUE, parent = emptyenv())

addTargetFunctions = function( ... ){
  arguments = rlang::enquos(...)


  lapply( arguments, function(qq){

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

    # need some specific solution when the argument passed in already contains a ::
    keyname = paste0( environmentName, '::' , deparse( quo_get_expr ( qq )  ) )
    storage_hash_table[[ keyname ]] = list(
      ref = ref,
      environment = environment,
      environmentName = environmentName,
      expr = keyname
    )
  })
  storage_hash_table
}

showTargetFunctions = function(){
  ls ( storage_hash_table)
}
