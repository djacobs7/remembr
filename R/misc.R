debug_build_ignore = function(){


  #https://github.com/wch/r-source/blob/1d4f7aa1dac427ea2213d1f7cd7b5c16e896af22/src/library/tools/R/build.R
  get_exclude_patterns <- function()
    c("^\\.Rbuildignore$",
      "(^|/)\\.DS_Store$",
      "^\\.(RData|Rhistory)$",
      "~$", "\\.bak$", "\\.swp$",
      "(^|/)\\.#[^/]*$", "(^|/)#[^/]*#$",
      ## Outdated ...
      "^TITLE$", "^data/00Index$",
      "^inst/doc/00Index\\.dcf$",
      ## Autoconf
      "^config\\.(cache|log|status)$",
      "(^|/)autom4te\\.cache$", # ncdf4 had this in subdirectory 'tools'
      ## Windows dependency files
      "^src/.*\\.d$", "^src/Makedeps$",
      ## IRIX, of some vintage
      "^src/so_locations$",
      ## Sweave detrius
      "^inst/doc/Rplots\\.(ps|pdf)$"
    )



  inRbuildignore <- function(files, pkgdir) {
    exclude <- rep.int(FALSE, length(files))
    ignore <- get_exclude_patterns()
    ## handle .Rbuildignore:
    ## 'These patterns should be Perl regexps, one per line,
    ##  to be matched against the file names relative to
    ##  the top-level source directory.'
    ignore_file <- file.path(pkgdir, ".Rbuildignore")
    if (file.exists(ignore_file))
      ignore <- c(ignore, readLines(ignore_file, warn = FALSE))
    for(e in ignore[nzchar(ignore)])
      exclude <- exclude | grepl(e, files, perl = TRUE,
                                 ignore.case = TRUE)
    exclude
  }

  df =tibble::enframe( list.files("."))
  df$ignore = inRbuildignore( list.files("."), "." )
  df
}
