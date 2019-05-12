
extract_help <- function(pkg, fn = NULL, to = c("txt", "html", "latex", "ex")){
  to <- match.arg(to)
  rdbfile <- file.path(find.package(pkg), "help", pkg)
  rdb <- tools:::fetchRdDB(rdbfile, key = fn)
  convertor <- switch(to,
                      txt   = tools::Rd2txt,
                      html  = tools::Rd2HTML,
                      latex = tools::Rd2latex,
                      ex    = tools::Rd2ex
  )
  f <- function(x) capture.output(convertor(x))
  if(is.null(fn)) lapply(rdb, f) else f(rdb)
}


generateCustomHelp = function(pkg, fn = NULL){
  #https://www.rdocumentation.org/packages/utils/versions/3.6.0/source
  path <- file.path(tempdir(), ".R/doc/html")
  dir.create(path, recursive = TRUE, showWarnings = FALSE)

  help_str = extract_help(pkg, fn, to = 'html')

  help_str = paste0( help_str, collapse ='' )
  help_str = paste0( '<h2>help_str</h2>', help_str, collapse ='' )
  writeLines(help_str, file.path(path, "customhelp.html"))
  browseURL(paste0("http://127.0.0.1:", port,
                   "/doc/html/customhelp.html"),
            getOption("browser"))
}
