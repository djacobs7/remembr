#'
#'
#' @importFrom utils capture.output
#' @importFrom tools Rd2txt
#' @importFrom tools Rd2HTML
#' @importFrom tools Rd2latex
#' @importFrom tools Rd2ex
#'
extract_help <- function(pkg, fn = NULL, to = c("txt", "html", "latex", "ex")){
  to <- match.arg(to)
  rdbfile <- file.path(find.package(pkg), "help", pkg)
  rdb <- fetchRdDB(rdbfile, key = fn)
  convertor <- switch(to,
                      txt   = tools::Rd2txt,
                      html  = tools::Rd2HTML,
                      latex = tools::Rd2latex,
                      ex    = tools::Rd2ex
  )
  f <- function(x) utils::capture.output(convertor(x))
  if(is.null(fn)) lapply(rdb, f) else f(rdb)
}

#'
#'
#' @importFrom utils browseURL
generateCustomHelp = function(pkg, fn = NULL){
  #https://www.rdocumentation.org/packages/utils/versions/3.6.0/source
  path <- file.path(tempdir(), ".R/doc/html")
  dir.create(path, recursive = TRUE, showWarnings = FALSE)

  help_str = extract_help(pkg, fn, to = 'html')

  help_str = paste0( help_str, collapse ='' )
  help_str = paste0( '<h2>help_str</h2>', help_str, collapse ='' )
  help_str = paste0( '<li><a href="http://www.google.com">The Goog</a></li>', help_str, collapse ='' )
  writeLines(help_str, file.path(path, "customhelp.html"))


  #grabbed from utils::help
  port = tools::startDynamicHelp(NA)
  if (port <= 0L){
    stop("could not open help")
    lib.loc = NULL
    return(library(help = package, lib.loc = lib.loc,
                   character.only = TRUE))
  }


  utils::browseURL(paste0("http://127.0.0.1:", port,
                   "/doc/html/customhelp.html"),
            getOption("browser"))
}


# this code is copied from fetchRdDB
# it's pasted here directly in case something changes in the tools package
fetchRdDB = function (filebase, key = NULL)
{
  fun <- function(db) {
    vals <- db$vals
    vars <- db$vars
    datafile <- db$datafile
    compressed <- db$compressed
    envhook <- db$envhook
    fetch <- function(key) lazyLoadDBfetch(vals[key][[1L]],
                                           datafile, compressed, envhook)
    if (length(key)) {
      if (key %notin% vars)
        stop(gettextf("No help on %s found in RdDB %s",
                      sQuote(key), sQuote(filebase)), domain = NA)
      fetch(key)
    }
    else {
      res <- lapply(vars, fetch)
      names(res) <- vars
      res
    }
  }
  res <- lazyLoadDBexec(filebase, fun)
  if (length(key))
    res
  else invisible(res)
}
