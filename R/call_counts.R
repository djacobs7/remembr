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
#' Given an id, get the id for the next bucket
#'
#' If bucket_id is null, then start from 1
#'
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


#'  Code for merging two hash tables together
#'
#' @importFrom rlang env_has
#' @importFrom rlang env_names
#' @importFrom rlang env_clone
mergeCallCountHashTables = function(call_counts_hash_table1, call_counts_hash_table2){
  c1 = call_counts_hash_table1
  c2 = call_counts_hash_table2

  out = env_clone( call_counts_hash_table1 )
  for( key in rlang::env_names(c2)){
    v2 = call_counts_hash_table2[[key]]
    if (  rlang::env_has( c1, key ) ){
      v1 = rlang::env_has( c1, key )
      out[[key]] = .mergeCards( v1, v2 )
    } else {
      out[[key]] = v2
    }
  }
  out
}

.mergeCards = function(a,b){
  list(
    first_use = min(a$first_use, b$first_use, na.rm  = TRUE),
    most_recent_use = max( a$most_recent_use, b$most_recent_use, na.rm  = TRUE),
    total_uses = sum( a$total_uses, b$total_uses ),
    bucket_id = max( a$bucket_id, b$bucket_id, na.rm = TRUE ),
    most_recent_review = max( a$most_recent_review, b$most_recent_review, na.rm = TRUE )
  )
}

#--------  CODE FOR CONVERTING TO DATAFRAME

#' convert call counts to hash table
#'
#' Helper method for parsing the call_counts_hash_table environment and presenting it as a data frame
#'
#' @importFrom  lubridate as_datetime
#' @import dplyr
convertCallCountsToHashTable = function( call_counts_hash_table , time = NULL){
  if ( is.null(time)){
    time = lubridate::now(tzone = 'UTC')
  }

  convertEnvToDataFrame  = function( call_counts_hash_table ){
    result = eapply(call_counts_hash_table, function(a){
      a$bucket_id = if(  'bucket_id' %in%  names(a)  ) {   a$bucket_id  } else {  nextBucket(c() ) }
      a$most_recent_review = if(  'most_recent_review' %in%  names(a) ) { a$most_recent_review } else { NA_integer_ }
      a
    })

    df =do.call( rbind, result) %>%
      dplyr::as_tibble(rownames = 'function_name') %>%
      dplyr::mutate_all(unlist) %>%
      mutate( first_use = lubridate::as_datetime(first_use ),
              most_recent_use = lubridate::as_datetime(most_recent_use ),
              most_recent_review = lubridate::as_datetime( most_recent_review )
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
      # THE CORRECT ORDER IS:
      # MOST RECENT ITEMS IN THE MOST RECENT BUCKET

      review_timer = most_recent_use + bucket_timer , #TODO: change to most_recent_review
      review_timer = if_else( is.na( most_recent_review), review_timer, most_recent_review + bucket_timer )   %>% lubridate::as_datetime())   %>%
    mutate(
      needs_review = if_else(
        difftime(review_timer, time ) > 0 ,
        FALSE,
        TRUE
      ))

  df
}


#==== call counts views:

viewCallCountsReview = function(){

}
