
#'
#'
#' Adds a line to your .Rprofile so that remembr runs by default.
#' Can be uninstalled at any time with uninstall_remembr
#' @export
install_remembr = function(){

  dir.create(storage_file_directory)

  remembrinstall::install()
  remembr::initRemembr()

  cat(paste0(
    "\n",
    "Installing remembr. This will automatically create flashcards for you as you code interactively.  You can uninstall at any time with ",
    crayon::bgWhite("uninstall_remembr()"), "\n"
  ))

  cat(paste0(
    "\n",
    "remembr will launch automatically on startup - you can change this in your ~/.rProfile file\n"
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

