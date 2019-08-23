
#'
remindMeDataFrame = function( num_functions = 5 ){
  df = convertCallCountsToHashTable(getCallCountsHashTable() )
  df %>%
    filter( package != "R_GlobalEnv") %>%
    arrange((review_timer ))
}


#' Shows upcoming reminders for a specific package
#'
#' @param  packageNames a list of package names
#' @param all if all is true then shows all packages
#' @return  a data frame with the upcoming reminder.
#'
#' @export
remindPackage = function( packageNames = 'base', all = FALSE ){
  array = packageNames
  df = convertCallCountsToHashTable(getCallCountsHashTable() )
  if ( all == TRUE ){
    df = df %>%
      filter( package != "R_GlobalEnv") %>%
      arrange( ( review_timer ))
  } else{
    df = df %>%
      filter( package %in% !!(array) ) %>%
      arrange( ( review_timer ))
  }




  package_string = df %>%
    group_by( package )%>%
    mutate( str = paste0( "(", dense_rank(review_timer), ") " , crayon::bold(name) , " from the ", crayon::bgWhite(package), " package",
                          " used ", crayon::bgWhite(total_uses), " times.  ",
                          "Last used ",
                          timeStampToIntervalString( most_recent_use ),
                          ".",
                          ifelse( needs_review, " Needs review.",
                                  paste0( "Needs review in ", timeStampToIntervalStringFuture( review_timer)) )
    ) ) %>%
    select( package, str )

  #  cat(paste0( "\nHere is everything to review from the ",crayon::bgWhite(packageNames) , " package\n"))
  #  cat(paste(package_string$str, collapse = "\n"))

  per_package_string= package_string %>% group_by( package) %>% summarize( str =
                                                                             paste0(str, collapse = "\n")
  )

  s = paste0(
    paste0( "\nHere is everything to review from the ",crayon::bgWhite(per_package_string$package) , " package\n\n"),
    per_package_string$str
  )


  cat( paste( s, collapse = "\n") )

  invisible(df )
}


#'
#' Upcomping reminders
#'
#' Prints out the methods that will need to be reviewed next time you do a review
#' @export
upcomingReminders = function(num_methods = 10 ){
  df = convertCallCountsToHashTable(getCallCountsHashTable() ) %>%
    arrange( ( review_timer )) %>%
    filter( row_number() < num_methods )

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

#' Simple plot of your flashCards
#'
plotFlashCards = function(){
  df = convertCallCountsToHashTable(getCallCountsHashTable() )

  ggplot2::ggplot(df %>% group_by(package) %>% summarize(n = n()), ggplot2::aes( x= package, y = n) ) +
    ggplot2::geom_bar(stat='identity') +
    ylab("Unique methods to study")


  ggplot2::ggplot(df %>% group_by(package ) %>%
          arrange(package, total_uses) %>%
          mutate( uses_rank = row_number()),
          ggplot2::aes( x= package, y = uses_rank, label = name) ) +
    ggplot2::geom_text() #+
   #geom_bar(stat='identity')



  ggplot2::ggplot(df %>%
                    mutate(usage_bkt =round( log( total_uses/ 10 ))) %>%
                    group_by(usage_bkt) %>%
                    arrange(usage_bkt, most_recent_use) %>%
                    mutate( use_rank = row_number()),
                  ggplot2::aes( x= usage_bkt, y = use_rank, label = name) ) +
    ggplot2::geom_label()


}

#' @import crayon
#' @export
remindMe = function( num_rows = 5) {
  df = convertCallCountsToHashTable(getCallCountsHashTable() )

  package_reminders = df %>%
    filter( package != "R_GlobalEnv") %>%
    filter(needs_review) %>%
    group_by( package) %>%
    arrange( ( review_timer)) %>%
    filter( row_number() == 1) %>%
    select( package, function_name, review_timer)%>%
    ungroup() %>%
    top_n( 3, desc(review_timer ))


  top_5 = df %>%
    arrange( ( review_timer )) %>%
    filter( row_number() < num_rows )

  if ( nrow( top_5) == 0 ){
    cat(paste0( "You have no methods to review at this time.  Your next review is ",  timeStampToIntervalStringFuture( min( df$review_timer) ) , ". You can use remindPackage( packageName ) to see upcoming review items for a specific package.") )
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

getFilteredFlashcardsDataFrame = function( time_since_last_use = NULL, pack_name = NULL, call_counts_hash_table = NULL ){
  df = convertCallCountsToHashTable( call_counts_hash_table )%>%
    filter( package != "R_GlobalEnv")

  if ( !is.null(pack_name)){
    pack_keys = listPackKeys(pack_name)
    if ( length(pack_keys) ==0 ){
      stop(paste0("Could not find anything to study for ", pack_name))
    }

    df = df %>% right_join( data_frame( function_name = pack_keys ), by = 'function_name')
  }
  if(!is.null(time_since_last_use) ){
    df = df %>% filter(
      most_recent_use > lubridate::now() - time_since_last_use
    )
  }

  df = df %>% arrange( review_timer )
  df
}

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
#'
#' @export
flashCards = function(num_flashcards = 10, time_since_last_use = NULL,  pack = NULL){
  start_time = lubridate::now(tzone = 'UTC')
  result_counts = list( comfortable = 0, not_comfortable = 0 )

  if ( is.null(pack)){
    pack = getCallCountsHashTable()
  }

  pack_name = NULL

  df = getFilteredFlashcardsDataFrame(time_since_last_use, pack_name , pack )
  stack = df %>%
    #top_n( num_flashcards, desc(bucket, review_timer )) %>%
    arrange(   review_timer )

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
      str = paste0(  crayon::bold(name) , " from the ", crayon::bgWhite(package), " package")
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
