
#'
#'
#' Adds a line to your .Rprofile so that remembr runs by default.
#' Can be uninstalled at any time with uninstall_remembr
#' @export
install_remembr = function(){
  remembr::initRemembr()
  remembrinstall::install()
}

#'
#' Removes remembr from your rprofile
#'
#' @export
uninstall_remembr = function(){
  remembrinstall::uninstall()
}

