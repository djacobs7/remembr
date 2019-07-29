.onAttach <- function(libname, pkgname) {
  if( !dir.exists( storage_file_directory )){
    base::packageStartupMessage(
      paste0("If you would like remembr to automatically create flash cards as you code, please run ",
                    crayon::bgWhite("install_remember()"),
             "   \n Then run ", crayon::bgWhite("flashCards()"), "to review your flashcards.",
             "Visit www.dontyouremember.com/documentation.html to learn more"
      )
    )
  } else {

  }
}
