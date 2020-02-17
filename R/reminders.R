
# Choosing to remove this method, in favor of getMyCalls
# @importFrom rlang .data
#remindMeDataFrame = function( num_functions = 5 ){
#  df = convertCallCountsToHashTable(getCallCountsHashTable() )
#  df %>%
#    filter( .data$package != "R_GlobalEnv") %>%
#    arrange((.data$review_timer ))
#}


#' Shows upcoming reminders for a specific package
#'
#' @param  packageNames a list of package names
#' @param all if all is true then shows all packages
#' @return  a data frame with the upcoming reminder.
#'
#'
#' @importFrom rlang .data
#' @export
remindPackage = function( packageNames = 'base', all = FALSE ){
  array = packageNames
  df = convertCallCountsToHashTable(getCallCountsHashTable() )
  if ( all == TRUE ){
    df = df %>%
      filter( .data$package != "R_GlobalEnv") %>%
      arrange( ( .data$review_timer ))
  } else{
    df = df %>%
      filter( .data$package %in% !!(array) ) %>%
      arrange( ( .data$review_timer ))
  }



  package_string = df %>%
    group_by( .data$package )%>%
    mutate( str = paste0( "(", dense_rank(.data$review_timer), ") ",
                          crayon::bold( .data$name),
                          " from the ", .background_highlight(.data$package),
                          " package",
                          " used ",
                          .background_highlight(.data$total_uses),
                          " times.  ",
                          "Last used ",
                          timeStampToIntervalString( .data$most_recent_use ),
                          ".",
                          ifelse( .data$needs_review,
                                  " Needs review.",
                                  paste0( "Needs review in ",
                                          timeStampToIntervalStringFuture(
                                            .data$review_timer)
                                          )
                                )
    ) ) %>%
    select( .data$package, .data$str )

  per_package_string= package_string %>%
    group_by( .data$package) %>%
    summarize( str = paste0( .data$str, collapse = "\n")
  )

  s = paste0(
    paste0( "\nHere is everything to review from the ",
            .background_highlight(per_package_string$package),
            " package\n\n"),
    per_package_string$str
  )


  cat( paste( s, collapse = "\n") )

  invisible(df )
}

.background_highlight = function(x){
  crayon::bold(x)
}


#'
#' Upcomping reminders
#'
#' Prints out the methods that will need to be reviewed next time you do a review
#' @export
#'
#' @param  num_methods How many methods to show
#'
#' @importFrom rlang .data
upcomingReminders = function(num_methods = 10 ){
  df = convertCallCountsToHashTable(getCallCountsHashTable() ) %>%
    arrange( ( .data$review_timer )) %>%
    filter( row_number() < num_methods )

  package_string = df %>%
    mutate( str = paste0( "(", row_number(), ") " ,
                          crayon::bold(.data$name) ,
                          " used ",
                          .background_highlight(.data$total_uses),
                          " times.  ",
                          "Last used ",
                          timeStampToIntervalString( .data$most_recent_use ),
                          ".",
                          ifelse( .data$needs_review,
                                  " Needs review. ",
                                  paste0( " Needs review ",
                                          timeStampToIntervalStringFuture(
                                            .data$review_timer
                                  )) )
    ) ) %>%
    select( .data$str )

  cat(paste0( "\nHere is everything to review\n"))
  cat(paste(package_string$str, collapse = "\n"))

  invisible(df )
}

#' Simple plot of your flashCards
#'
#'
#' @importFrom rlang .data
plotFlashCards = function(){
  df = convertCallCountsToHashTable(getCallCountsHashTable() )

  df %>%
    group_by(.data$package) %>%
    summarize(n = n()) %>%
  ggplot2::ggplot( ggplot2::aes( x= .data$package, y = n) ) +
    ggplot2::geom_bar(stat='identity') +
    ggplot2::ylab("Unique methods to study")



  df %>% group_by(.data$package ) %>%
    arrange(.data$package, .data$total_uses) %>%
    mutate( uses_rank = row_number()) %>%
  ggplot2::ggplot(
          ggplot2::aes( x= package, y = uses_rank, label = name) ) +
    ggplot2::geom_text() #+
   #geom_bar(stat='identity')



  ggplot2::ggplot(df %>%
                    mutate(usage_bkt =round( log( .data$total_uses/ 10 ))) %>%
                    group_by(.data$usage_bkt) %>%
                    arrange(.data$usage_bkt, .data$most_recent_use) %>%
                    mutate( use_rank = row_number()),
                  ggplot2::aes( x= usage_bkt, y = use_rank, label = name) ) +
    ggplot2::geom_label()


}

#' Remind Me
#'
#' Shows a list of functions that are upcoming for you to study.  This is based
#' on your personal R history, and any flashcard decks you may use.
#'
#' @import crayon
#' @export
#'
#' @param num_rows how many functions to show
#'
#' @importFrom rlang .data
remindMe = function( num_rows = 5) {
  df = convertCallCountsToHashTable(getCallCountsHashTable() )

  package_reminders = df %>%
    filter( .data$package != "R_GlobalEnv") %>%
    filter(.data$needs_review) %>%
    group_by( .data$package) %>%
    arrange( ( .data$review_timer)) %>%
    filter( row_number() == 1) %>%
    select( .data$package, .data$function_name, .data$review_timer)%>%
    ungroup() %>%
    top_n( num_rows, desc(.data$review_timer ))


  top_5 = df %>%
    arrange( ( .data$review_timer )) %>%
    filter( row_number() < num_rows )

  if ( nrow( top_5) == 0 ){
    cat(paste0( "You have no methods to review at this time.  Your next review is ",
                timeStampToIntervalStringFuture( min( df$review_timer) ) ,
                ". You can use remindPackage( packageName ) to see upcoming review items for a specific package.") )
    return( invisible( df ))
  }

  result =top_5 %>%
    mutate( str = paste0( "(", row_number(), ") " , crayon::bold(.data$name) , " from the ",  .background_highlight(.data$package), " package") ) %>%
    select( .data$str )

  package_string =package_reminders %>%
    mutate( str = paste0( "(", row_number(), ") " ,  .background_highlight(.data$package)) ) %>%
    select( .data$str )

  cat("Based on your previous R usage, we recommend that you review the following methods\n")
  cat(paste(result$str, collapse = "\n"))

  #  cat("\n\nYou will have upcoming review in the following packages\n")
  #  cat(paste(package_string$str, collapse = "\n"))


  cat(paste0( "\n To review a method, just call it in the console or call ",  .background_highlight( 'flashCards()' ) ,"."))


  invisible(result)
}
