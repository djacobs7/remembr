
loadOrCreateEnv = function(path = NULL ){
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

#Deprecated:
# this is some code for storing documentation; probably needs to be rewritten anyuway
#storage_hash_table_path =file.path( storage_file_directory,"storage_hash_table_path.Rds" )
#storage_hash_table = loadOrCreateEnv( storage_hash_table_path )

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
  #FIXME: This is currently unsupported ( and may  need to move to the inst directory
  #call_counts_hash_table = loadOrCreateEnv( call_counts_hash_table_path, "data/default_call_counts_hash_table.Rds" )

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


#' Initialize remembr
#'
#' This tells R to record your R code, as your are coding, in order to build
#' your personal flashcards and list of function calls.  It will remain on for
#' the duration of your R session.
#' This adds a callback handler to your REPL.
#' It will track what you do in a local file; and only track function and
#' package names.
#'
#' @export
initRemembr = function(){
  initOptions()
  removeTaskCallback("addCallCounts")
  addTaskCallback(  addCallCountsCallback, name = "addCallCounts", data = getCallCountsHashTable())
  invisible(TRUE)
}

#' Stops remembr
#'
#' Stop remembr from recording what you do in the REPL.
#'
#' @export
stopRemembr = function(){
  removeTaskCallback("addCallCounts")
}



#'
#' Add Call Counts Callback
#'
#' This is the main callback.  It can get called on any expression.
#'  If you call it, it will modify the your state.
#'
#'  This follows the standard API of any R callback ( @see addTaskHandlers)
#'
#'
#' @param expr an expression
#' @param value result of the expression evaluation
#' @param status logical indicating success or not
#' @param visible was the output printed
#' @param data data object that is accessible to the callback ( passed in from addTaskCallback)
#'
#' @return I guess this indicates if the callback succeeded?
addCallCountsCallback = function(expr, value, status, visible, data){
  get_functions( expr, getCallCountsHashTable(), needs_substitute = FALSE )

  if ( getOption("remembr.should_persist") ){
    saveCallCountsHashTable()
  }
  TRUE
}





#https://stat.ethz.ch/R-manual/R-devel/library/base/html/taskCallback.html

#' This just echos the input expression.  It is here for testing out the
#' taskCallback code
#'
#' @param expr s-language expression
#' @param value result of the expression evaluation
#' @param status logical indicating success or not
#' @param visible was the output printed
#' @param data data object that is accessible to the callback ( passed in from addTaskCallback)
#'
#' @return I guess this indicates if the callback succeeded?
echoExpressionCallback = function(expr, value, status, visible, data){
  print( paste0( str( expr ) ) )

  TRUE
}
