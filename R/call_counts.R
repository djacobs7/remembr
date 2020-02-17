#'
#'
#'
#' @importFrom lubridate now
updateCard = function(keyname, time = NULL, call_counts_hash_table = NULL){
  if ( is.null(time)){
    time = lubridate::now(tzone='UTC')
  }

  if (is.null(call_counts_hash_table)){
    call_counts_hash_table = getCallCountsHashTable()
  }
  prev_record = call_counts_hash_table[[keyname]]

  if ( is.null(prev_record)){
    prev_record =.createCard( time )
  } else {
    prev_record =.updateCard(prev_record, time )
  }

  if (!is.null(getCurrentPackName())){
    addKeynameToPack( keyname )
  }

  call_counts_hash_table[[keyname]] = prev_record
}



.createCard = function(time){
  prev_record = list( first_use =   time,
                      most_recent_use = time,
                      total_uses = 1,
                      bucket_id = nextBucket(NULL) )
  prev_record

}

.updateCard = function(prev_record,time){
  if ( is.null( prev_record$bucket_id )){
    prev_record$bucket_id = nextBucket(c())
  }
  bucket_timer = as.numeric( getDurationFromBucketId( prev_record$bucket_id ) )
  now_t = time
  dt = difftime(prev_record$most_recent_use + bucket_timer,now_t  )

  if(  dt < 0 ){
    next_bucket = nextBucket( prev_record$bucket_id )
  } else {
    next_bucket = prev_record$bucket_id
  }

  prev_record$most_recent_use = time
  prev_record$total_uses = prev_record$total_uses + 1
  prev_record$bucket_id = next_bucket

  prev_record
}

#'
#' Next bucket
#'
#' Given an id, get the id for the next bucket
#' If bucket_id is null, then start from 1
#'
#' @param bucket_id the id of the current bucket
#'
nextBucket = function(bucket_id ){
  if ( is.null(bucket_id) | length( bucket_id) == 0){
    1
  }else if ( bucket_id < 6 ){
    bucket_id + 1
  } else {
    bucket_id
  }
}


#' Given a bucket id, return a timer for that bucket
#'
#'
#' @importFrom lubridate minutes
#' @importFrom lubridate hours
#' @importFrom lubridate days
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


#---------- CODE FOR REVIEWING A CARD ( does a write )

reviewCard = function( keyname, time, should_update_bucket, call_counts_hash_table = NULL ){
  if ( is.null( call_counts_hash_table ) ){
    call_counts_hash_table = getCallCountsHashTable()
  }
  prev_record = call_counts_hash_table[[keyname]]
  if( is.null(prev_record)){
    stop("record not found")
  }

  if ( should_update_bucket){
    prev_record$bucket_id = nextBucket( prev_record$bucket_id )
  }

  prev_record$most_recent_review = time
  call_counts_hash_table[[keyname]] = prev_record
  prev_record
}

#========



#' Add a deck
#'
#' Downloads a card deck from www.dontyouremember.com ( see TODOTODO for a list
#' of all card decks )
#'
#' @param  deck_name Name of the deck
#'
#' @examples
#' addCardDeck( "advanced-r-object-oriented-part-1" )
#'
#' @export
addCardDeck = function( deck_name ){
  storage_name = tempfile()
  downloaded_file = utils::download.file(paste0("https://www.dontyouremember.com/packs/", deck_name, ".Rds"), storage_name )
  deck = readRDS( storage_name )
  message(paste0("Installing ",  length(ls(deck$cards)) , " cards from ", deck_name) )
  mergeCallCountHashTables( getCallCountsHashTable(), deck$cards, in_place = TRUE, zero_uses = FALSE )
}

#'  Code for merging two hash tables together
#'
#'  The order shouldn't matter?
#'
#' @param call_counts_hash_table1 env1
#' @param call_counts_hash_table2 env2
#' @param in_place If this is true, then results will merge into call_counts_hash_table1; otherwise the result in a new environment
#' @param zero_uses zero out uses from cc2.  Used when cc1 are results from actual your coding work, and cc2 is a merged in deck from a book ( for example )
#' This way we dont break the anything.
#'
#' @importFrom rlang env_has
#' @importFrom rlang env_get
#' @importFrom rlang env_names
#' @importFrom rlang env_clone
mergeCallCountHashTables = function(call_counts_hash_table1,
                                    call_counts_hash_table2,
                                    in_place = FALSE,
                                    zero_uses = FALSE){
  c1 = call_counts_hash_table1
  c2 = call_counts_hash_table2

  if ( in_place ){
    out = call_counts_hash_table1
  } else {
    out = env_clone( call_counts_hash_table1 )

  }

  for( key in rlang::env_names(c2)){
    v2 = rlang::env_get( c2, key )

    if ( zero_uses ){
      v2$total_uses = 0
    }
    if (  rlang::env_has( c1, key ) ){
      v1 = rlang::env_get( c1, key )
      out[[key]] = .mergeCards( v1, v2 )
    } else {
      out[[key]] = v2
    }
  }

  out
}



.mergeCards = function(a,b){
  out = list()

  if ( !is.null( a$first_use) & !is.null( b$first_use ) ) {
    out$first_use = min(a$first_use, b$first_use, na.rm  = TRUE)
  }
  if ( !is.null( a$most_recent_use) & !is.null( b$most_recent_use ) ) {
    out$most_recent_use = max(a$most_recent_use, b$most_recent_use, na.rm  = TRUE)
  }
  if ( !is.null( a$total_uses) & !is.null( b$total_uses ) ) {
    out$total_uses = sum(a$total_uses, b$total_uses, na.rm  = TRUE)
  }
  if ( !is.null( a$bucket_id) & !is.null( b$bucket_id ) ) {
    out$bucket_id = max(a$bucket_id, b$bucket_id, na.rm  = TRUE)
  }
  if ( !is.null( a$most_recent_review) & !is.null( b$most_recent_review ) ) {
    out$most_recent_review = max(a$most_recent_review, b$most_recent_review, na.rm  = TRUE)
  }
  out
}

#'  Gets my call history as a data frame
#'
#' @export
getMyCalls = function(){
  convertCallCountsToHashTable( getCallCountsHashTable( ))
}

#--------  CODE FOR CONVERTING TO DATAFRAME

#' convert call counts to hash table
#'
#' Helper method for parsing the call_counts_hash_table environment and presenting it as a data frame
#'
#' @param call_counts_hash_table A call counts hash table ( like the one you would
#' get from getCallCountsHashTable() )
#' @param time The current time.  So that the hash table can have the corret time
#' since you last reviewed.
#'
#' @importFrom  lubridate as_datetime
#' @importFrom rlang .data
#' @import dplyr
convertCallCountsToHashTable = function( call_counts_hash_table , time = NULL){
  if ( is.null(time)){
    time = lubridate::now(tzone = 'UTC')
  }

  df = .convertEnvToDataFrame( call_counts_hash_table )



  #this becomes a default actually!
  df$bucket_timer = getDurationFromBucketId(df$bucket_id )


  if ( nrow( df ) == 0 ){
    return(df)
  }

  df = df %>%
    tidyr::separate(.data$function_name, c('package','name'), sep = '::', remove = FALSE) %>%
    mutate(package = ifelse( .data$function_name =="::", 'base', .data$package),
           name = ifelse( .data$function_name =="::", '::', .data$name)
    ) %>%
    mutate(
      # THE CORRECT ORDER IS:
      # MOST RECENT ITEMS IN THE MOST RECENT BUCKET

      review_timer = .data$most_recent_use + .data$bucket_timer , #TODO: change to most_recent_review
      review_timer = if_else(
                        is.na( .data$most_recent_review),
                        .data$review_timer,
                        .data$most_recent_review + .data$bucket_timer ) %>%
        lubridate::as_datetime()
    )   %>%
    mutate(
      needs_review = if_else(
        difftime(.data$review_timer, time ) > 0 ,
        FALSE,
        TRUE
      ))

  df
}


#' helper function for convertCallCountsToHashTable
#'
#' @param call_counts_hash_table A Call counts hash table ( like from
#' getCallCountsHashTable )
.convertEnvToDataFrame  = function( call_counts_hash_table ){
  result = eapply(call_counts_hash_table, function(a){
    a$bucket_id = if(  'bucket_id' %in%  names(a)  ) {   a$bucket_id  } else {  nextBucket(c() ) }
    a$most_recent_review = if(  'most_recent_review' %in%  names(a) ) { a$most_recent_review } else { NA_integer_ }
    a
  })

  if ( length(result) == 0 ){
    df = .createEmptyCallCountsDataFrame()
  } else {
    df =do.call( rbind, result) %>%
      dplyr::as_tibble(rownames = 'function_name') %>%
      dplyr::mutate_all(unlist) %>%
      mutate( first_use = lubridate::as_datetime(.data$first_use ),
              most_recent_use = lubridate::as_datetime(.data$most_recent_use ),
              most_recent_review = lubridate::as_datetime( .data$most_recent_review )
      )
  }


  df
}

.createEmptyCallCountsDataFrame = function(){
  tibble::tibble(
    function_name = character(0),
    package = character(0),
    name = character(0),
    first_use = lubridate::as_datetime(numeric(0)),
    most_recent_use = lubridate::as_datetime(numeric(0)),
    total_uses = double(0),
    bucket_id = double(0),
    review_timer = lubridate::as_datetime(numeric(0)),
    needs_review = logical(0)
  )
}

#==== call counts views:

viewCallCountsReview = function(){

}
