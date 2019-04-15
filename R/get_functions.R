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

createTargetFunctions = function( ... ){
  arguments = rlang::enquos(...)

  storage_hash_table = new.env( hash = TRUE)
  lapply( arguments, function(qq){
    ref = rlang::eval_tidy( qq )
    environment = environment( rlang::eval_tidy( qq ))
    environmentName = environmentName( environment )

    keyname = as_string( quo_get_expr ( qq )  )
    storage_hash_table[[ keyname ]] = list(
      ref = ref,
      environment = environment,
      environmentName = environmentName
    )
  })
  storage_hash_table
}
