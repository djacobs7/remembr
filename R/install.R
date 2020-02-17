
#'
#'
#' Adds a line to your .Rprofile so that remembr runs by default.
#' Can be uninstalled at any time with uninstall_remembr
#' @export
install_remembr = function(){

  dir.create(storage_file_directory)



  remembr::initRemembr()


  if ( utils::askYesNo("Would you like remembr to launch automatically on startup?")){

    remembrinstall::install()

    cat(paste0(
      "\n",
      "remembr will launch automatically on startup - you can change this in your ~/.rProfile file\n"
    ))

  } else {
    cat("\n remembr will not launch on startup.")
  }
  cat(paste0(
    "\n",
    "Launching remembr. This will automatically create flashcards for you as you code interactively.  You can uninstall at any time with ",
    crayon::bgWhite("uninstall_remembr()"), "\n"
  ))




}



#'
#' Removes remembr from your rprofile
#'
#' @export
uninstall_remembr = function(){
  remembrinstall::uninstall()
  stopRemembr()
  cat(paste0("remembr has been uninstalled from your .rProfile. You can manually delete ", storage_file_directory, " to permanently delete your flashcards."))
}

