
getPackState = function(){
  getOption("remembr.pack_state")
}

getPacks = function(){
  packState = getPackState()
  if(is.null(packState[["packs"]])){
    packState[["packs"]] =loadOrCreateEnv(pack_path)
  }
  packState[["packs"]]
}

getCurrentPackName = function(){
  packState = getPackState()
  packState[[ "currentPackName"]]
}

#'
#' Start Pack
#'
#' Allows for creation of a "pack" of flashcards.  The idea is that you
#' startPack; then code as normal; and then endPack.  Any code entered in between
#' will become part of a custom card pack
#'
#' @param pack_name the name of the pack
#'
#' @export
startPack = function(pack_name){
  packState= getPackState()
  packState[[ "currentPackName"]] = pack_name

  packs = getPacks()

  pack = packs[[pack_name]]
  if ( is.null(pack) ){
    packs[[pack_name]] = loadOrCreateEnv(NULL)
    pack = packs[[pack_name]]
  }
  pack
}


#'
#' Finish Pack
#'
#' Allows for creation of a "pack" of flashcards.  The idea is that you
#' startPack; then code as normal; and then endPack.  Any code entered in between
#' will become part of a custom card pack
#'
#' @param persist Do you want to save the pack to your file system? TRUE or FALSE
#'
#' @export
finishPack = function(persist = TRUE){
  pack_name_ = NULL
  if( persist){
    saveRDS(getPacks(), pack_path)
  }

}

addKeynameToPack = function( keyname ){
  if( !is.null( getCurrentPackName())){
    packs = getPacks()
    arr = packs[[getCurrentPackName()]]
    arr[[ keyname ]] = TRUE
  }

}

listPacks = function(){
  ls(getPacks())
}

listPackKeys = function(pack_name){
  packs = getPacks()
  pack = packs[[pack_name]]
  if ( !is.null(pack)){
    ls(pack)
  } else {
    vector(mode = 'character', length = 0)
  }

}

#'
#'  Create a deck of cards from files
#'
#'  Creates a new card deck and stores it in a local folder called 'packs'
#'  (this is an alternative method (  and perhaps better ) than the method above!
#'
#'  @param name name of the deck
#'  @files a vector of file names
#'
#'  @export
createCardDeck = function(name, files ){
  out2 = getFunctionsFromFiles( files )
  saveRDS(out2, paste0( "packs/", name) )
  out2
}





