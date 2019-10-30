

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
  out = getFunctionsFromFiles(
    c("repos/adv-r/Functionals.Rmd",
      "repos/adv-r/Function-factories.Rmd",
      "repos/adv-r/Function-operators.Rmd"
    )
  )
  finishPack()


  createCardDeck( 'advanced-r-names-values', c(
    "repos/adv-r/Names-values.Rmd"
  ))

  createCardDeck( 'advanced-r-vectors', c(
    "repos/adv-r/Vectors.Rmd"
  ))

  createCardDeck( 'advanced-r-subsetting', c(
    "repos/adv-r/Subsetting.Rmd"
  ))

  createCardDeck( 'advanced-r-control-flow', c(
    "repos/adv-r/Control-flow.Rmd"
  ))

  createCardDeck( 'advanced-r-functions', c(
    "repos/adv-r/Functions.Rmd"
  ))

  createCardDeck( 'advanced-r-environments', c(
    "repos/adv-r/Environments.Rmd"
  ))

  createCardDeck( 'advanced-r-conditions', c(
    "repos/adv-r/Conditions.Rmd"
  ))

  createCardDeck( 'advanced-r-oo-2', c(
    "repos/adv-r/S4.Rmd",
    "repos/adv-r/R6.Rmd",
    "repos/adv-r/OO-tradeoffs.Rmd"
  ))

  startPack("advanced-r-object-oriented")



  out2 = remembr:::getFunctionsFromFiles(
    c(
      "repos/adv-r/S4.Rmd",
      "repos/adv-r/R6.Rmd",
      "repos/adv-r/OO-tradeoffs.Rmd"
    )
  )



  createCardDeck = function(name, files ){
    out2 = remembr:::getFunctionsFromFiles( files )
    saveRDS(out2, paste0( "packs/", name) )
    out2
  }

  finishPack()


}




