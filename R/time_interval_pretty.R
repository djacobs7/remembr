


timeStampToIntervalString = function(times){
  timeDiffs = (difftime( lubridate::now(tzone  ='UTC'), times, units = 'mins'))

  getDiff = function( d ){
    if ( d < 1 ){
      return ('less than a minute ago')
    } else if ( d < 60 ){
      return(paste0( floor(d), ' minutes ago') )
    } else if ( d < 60*24 ){
      if ( floor( d/60) == 1){
        return(paste0( floor(d/60), ' hour ago') )
      } else{
        return(paste0( floor(d/60), ' hours ago') )
      }

    }else if ( d < 60*24*30 ){
      if ( floor( d/60) == 1){
        return(paste0( floor(d/(24*60)), ' day ago') )
      } else{
        return(paste0( floor(d/(24*60)), ' days ago') )
      }

    } else if ( d < 60*24*30*12 ) {
      return(paste0( floor(d/(24*60*30)), ' months ago') )
    } else {
      return( 'more than a year ago')
    }
  }
  sapply( timeDiffs, getDiff)
}
timeStampToIntervalStringFuture = function(times){
  timeDiffs = (difftime( times, lubridate::now(tzone = 'UTC'), units = 'mins'))

  getDiff = function( d ){
    if ( d < 1 ){
      return ('less than a minute from now')
    } else if ( d < 60 ){
      return(paste0( floor(d), ' minutes from now') )
    } else if ( d < 60*24 ){
      if ( floor( d/60) == 1){
        return(paste0( floor(d/60), ' hour from now') )
      } else{
        return(paste0( floor(d/60), ' hours from now') )
      }

    }else if ( d < 60*24*30 ){
      if ( floor( d/60) == 1){
        return(paste0( floor(d/(24*60)), ' day from now') )
      } else{
        return(paste0( floor(d/(24*60)), ' days from now') )
      }

    } else if ( d < 60*24*30*12 ) {
      return(paste0( floor(d/(24*60*30)), ' months from now') )
    } else {
      return( 'more than a year from now')
    }
  }
  sapply( timeDiffs, getDiff)
}
