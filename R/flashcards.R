

#'
#' Flashcards for Code
#'
#' @description
#' Shows flashcards for the methods that will help you become more fluent at R.
#' You will gave a short practice session to review each method.
#' If you say you are comfortable with the method, you will not see it for a while.
#' If you are not comfortable, it will return to your flashcards so you have another chance to review.
#'
#' @param num_flashcards how many flashcards to do at once.
#' @param time_since_last_use time interval since the last time the function was used in console.  For example: lubridate::hours(1)
#' @param pack If you would like to use a custom card pack, then you can choose on here ( TODO: figure this out and test it )
#'
#' @export
flashCards = function(num_flashcards = 10, time_since_last_use = NULL,  pack = NULL){
  start_time = lubridate::now(tzone = 'UTC')
  result_counts = list( comfortable = 0, not_comfortable = 0 )

  if ( is.null(pack)){
    pack = getCallCountsHashTable()
  }

  pack_name = NULL

  df = .getFilteredFlashcardsDataFrame(time_since_last_use, pack_name , pack )
  stack = df %>%
    #top_n( num_flashcards, desc(bucket, review_timer )) %>%
    arrange(   .data$review_timer )

  .num_flashcards = min( num_flashcards, nrow(df ))

  if ( nrow( df )== 0){
    cat("You have nothing to review")
    return(invisible(NULL))
  }

  num_needs_review = sum( df$needs_review )

  cat("Get ready to start your flashcards.\n")
  cat("Look for help in the browser window.\n")
  if( num_needs_review > num_flashcards ){
    cat(paste0( "You currently have at least ", num_needs_review, " functions that need review.\n"))
  }

  cat(paste0( "Don't worry, we will do them in chunks of ", num_flashcards, " at a time"))
  readline( "Press any key to start\n")
  for ( i in 1:.num_flashcards){
    row = stack[i,]
    with(data = row, expr = {

      #print(paste0("needs review in ", timeStampToIntervalStringFuture( row$review_timer )))
      str = paste0(  crayon::bold(name) , " from the ",  .background_highlight(package), " package")
      prompt = paste0( "(", i, ") ", "Do you feel comfortable with ", str ,"? (y/n/q) " )

      if (is.na( package  ) | package == ""){
        package = 'base'
      }

      tryCatch({
        h = help( name, package = (package), help_type = "html")
        print(h)
      }, error = function(e){
        print("could not find help")
      })



      cat(prompt)
      cat("\n")
      yesNo = readline()

      if ( name == '::' ){
        keyname = name
      }else if ( name == ':'){
        keyname ="base:::"
      } else{
        keyname = paste0( package, "::", name )
      }

      should_skip = FALSE
      if( yesNo == 'y'| yesNo == 'Yes' | yesNo == 'YES'){
        should_update_bucket = TRUE

        result_counts$comfortable = result_counts$comfortable + 1
      } else if ( yesNo == 'n'| yesNo == 'No' | yesNo == 'NO') {
        result_counts$not_comfortable = result_counts$not_comfortable + 1
        should_update_bucket = FALSE
      } else if ( yesNo == 'q' | yesNo == 'Q' | yesNo == 'QUIT'| yesNo == 'quit' ){
        break
      } else {
        should_skip = TRUE
      }
      if (!should_skip){
        time = lubridate::now(tzone = 'UTC')
        reviewCard( keyname, time, should_update_bucket, pack )
      }

    })
  }


  present_time = lubridate::now(tzone = 'UTC')

  time_spent_in_review = difftime( present_time ,  start_time, units = 'secs' )

  cat(paste0("You spent ", time_spent_in_review, " seconds in review. "))

  #TODO: reapply the filters
  df = convertCallCountsToHashTable( pack )%>%
    filter( package != "R_GlobalEnv")

  num_needs_review = sum( df$needs_review )

  if( num_needs_review == 0 ){
    cat(paste0( "You currently are currently up to date with your review.\n"))
  } else {
    cat(paste0( "You currently have ", num_needs_review, " functions that need review.\n"))
  }
  if( num_needs_review > 0 ){
    cat("You can run flashcards(num_flashcards) to study more.\n")
  }
  #saveCallCountsHashTable( pack )
  invisible(NULL)


}



.getFilteredFlashcardsDataFrame = function( time_since_last_use = NULL,
                                            pack_name = NULL,
                                            call_counts_hash_table = NULL ){
  df = convertCallCountsToHashTable( call_counts_hash_table )%>%
    filter( .data$package != "R_GlobalEnv")

  if ( !is.null(pack_name)){
    pack_keys = listPackKeys(pack_name)
    if ( length(pack_keys) ==0 ){
      stop(paste0("Could not find anything to study for ", pack_name))
    }

    df = df %>%
      right_join( data_frame( function_name = pack_keys ),
                  by = 'function_name')
  }
  if(!is.null(time_since_last_use) ){
    df = df %>% filter(
      .data$most_recent_use > lubridate::now() - time_since_last_use
    )
  }

  df = df %>% arrange( .data$review_timer )
  df
}
