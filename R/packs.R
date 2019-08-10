

storage_file_directory = "~/.rRemembr/"
pack_path = paste0( storage_file_directory, "packs.rds")


packState = loadOrCreateEnv(NULL)

getPacks = function(){
  if(is.null(packState[["packs"]])){
    packState[["packs"]] =loadOrCreateEnv(pack_path)
  }
  packState[["packs"]]
}

getCurrentPackName = function(){
  packState[[ "currentPackName"]]
}

#' @export
startPack = function(pack_name){
  packState[[ "currentPackName"]] = pack_name

  packs = getPacks()

  pack = packs[[pack_name]]
  if ( is.null(pack) ){
    packs[[pack_name]] = loadOrCreateEnv(NULL)
    pack = packs[[pack_name]]
  }
  pack
}

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


createPack = function(){

  startPack("advanced-r-functional-programming")

  finishPack()
}
