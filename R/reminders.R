

#' Helper method for parsing the call_counts_hash_table environment and presenting it as a data frame
#'
#' @importFrom  lubridate as_datetime
#' @import dplyr
convertCallCountsToHashTable = function( call_counts_hash_table ){
  convertEnvToDataFrame  = function( call_counts_hash_table ){
    result = eapply(call_counts_hash_table, function(a){
      a$bucket_id = if(  'bucket_id' %in%  names(a) ) { a$bucket_id } else { nextBucket(c()); }
      a$most_recent_review = if(  'most_recent_review' %in%  names(a) ) { a$most_recent_review } else { NA }
      a
    })

    df =do.call( rbind, result) %>%
      dplyr::as_tibble(rownames = 'function_name') %>%
      dplyr::mutate_all(unlist) %>%
      mutate( first_use = lubridate::as_datetime(first_use ),
              most_recent_use = lubridate::as_datetime(most_recent_use )

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
      review_timer = most_recent_use + bucket_timer , #TODO: change to most_recent_review
      review_timer = ifelse( is.na( most_recent_review), review_timer, most_recent_review + bucket_timer )   %>% lubridate::as_datetime())   %>%
   mutate(
     needs_review = ifelse(
       difftime(review_timer, lubridate::now(tzone = 'UTC') ) > 0 ,
       FALSE,
       TRUE
     ))

  df
}


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


#'
#' Flashcards for Code
#'
#' @description
#' Shows flashcards for the methods that will help you become more fluent at R.
#' You will gave a short practice session to review each method.
#' If you say you are comfortable with the method, you will not see it for a while.
#' If you are not comfortable, it will return to your flashcards so you have another chance to review.
#'
#' @param how many flashcards to do at once.
#'
#' @export
flashCards = function(num_flashcards = 10){

  df = convertCallCountsToHashTable(getCallCountsHashTable() )%>%
    filter( package != "R_GlobalEnv")

  stack = df %>%
    top_n( num_flashcards, desc(review_timer )) %>%
    arrange( ( review_timer ))

  if ( nrow( stack )== 0){
    cat("You have nothing to review")
    return(invisible(NULL))
  }

  num_needs_review = sum( df$needs_review )

  cat("Get ready to start your flashcards.\n")
  cat("Look for help in the browser window.\n")
  cat(paste0( "You currently have ", num_needs_review, " functions that need review.\n"))
  cat(paste0( "Don't worry, we will do them in chunks of ", num_flashcards, " at a time"))
  readline( "Press any key to start\n")
  for ( i in 1:nrow(stack)){
    row = stack[i,]
    with(data = row, expr = {

      #print(paste0("needs review in ", timeStampToIntervalStringFuture( row $review_timer )))
      str = paste0(  crayon::bold(name) , " from the ", crayon::bgWhite(package), " package")
      prompt = paste0( "(", i, ") ", "Do you feel comfortable with ", str ,"? (y/n/q) " )

      if (is.na( package  ) | package == ""){
        package = 'base'
      }

      tryCatch({
        h = help( name, package = (package), help_type = "html")
        print(h)
      })



      cat(prompt)
      cat("\n")
      yesNo = readline()

      if ( name == '::' ){
        keyname = name
      }else if ( name == ':'){
        keyname =":::"
      } else{
        keyname = paste0( package, "::", name )
      }

      call_counts_hash_table = getCallCountsHashTable()
      prev_record = call_counts_hash_table[[keyname]]

      if( is.null(prev_record)){
        stop("record not found")
      }


      if( yesNo == 'y'| yesNo == 'Yes' | yesNo == 'YES'){
        prev_record$bucket_id = nextBucket( prev_record$bucket_id )
      } else if ( yesNo == 'n'| yesNo == 'No' | yesNo == 'NO') {

      } else if ( yesNo == 'q' | yesNo == 'Q' | yesNo == 'QUIT'| yesNo == 'quit' ){
        break
      }

      #TODO: implement
      prev_record$most_recent_review = lubridate::now(tzone = 'UTC')
      call_counts_hash_table[[keyname]] = prev_record
      #utils::askYesNo( prompt = "" )
    })




  }
  df = convertCallCountsToHashTable(getCallCountsHashTable() )%>%
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
  invisible(NULL)


}
