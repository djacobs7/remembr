
#'
#'
#'  When a new user first launches remembr, we want them to first learn how to use remembr!
#'
#'
create_initial_db = function(){
  env = loadOrCreateEnv()
  get_functions({
    remembr::flashCards(5)
  }, env )
  Sys.sleep(2)

  get_functions({
    remembr::remindPackage()
    remembr::upcomingReminders()
    remembr::addDocumentationURL()
  }, env )


  rm( list = c('::', 'base::{'),envir = env)

  saveRDS(env, "data/default_call_counts_hash_table.Rds")
  env
}
