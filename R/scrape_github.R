

scrape_github_tidy_tuesday = function(){

  url =   "https://github.com/topics/r?l=r"

  all_repos = tibble()
  for ( page in seq_len(20 )){
    url =paste0("https://api.github.com/search/repositories?q=tidytuesday&page=", page)
    list_page = xml2::read_html(url)
    nodes= list_page %>% rvest::html_nodes("p") %>% rvest::html_text() %>% jsonlite::parse_json()
    repos = nodes$items %>% purrr::map_dfr( function(e) { list( size = e$size, name = e$name, url = e$clone_url, owner= e$owner$login, fullname = e$full_name, updated_at = e$updated_at)})
  Sys.sleep( 5 )
    all_repos = rbind( all_repos, repos )
  }
  #curl https://api.github.com/search/repositories?q=tetris+language:assembly&sort=stars&order=desc




  saveRDS(all_repos, 'data-local/tidy_tuesday_repos')
  clone_commands = "mkdir -p {full_name} && git clone {url} {full_name} --depth 1 " %>% glue::glue( full_name = all_repos$fullname, url = all_repos$url)
  tmpfilename = tempfile()
  write( clone_commands, tmpfilename )
}



scrape_github = function(){

  url =   "https://github.com/topics/r?l=r"
#curl -H 'Accept: application/vnd.github.mercy-preview+json' 'https://api.github.com/search/repositories?q=language:r'
  list_page = xml2::read_html(url)

  links = list_page %>%
    html_nodes("h3.f3 a") %>%
    html_attr("href")

  clone_command = "git clone https://github.com/"
  clone_commands =paste0( clone_command, links, " --depth 1")
 #   tidyverse/ggplot2.git

  tmpfilename = tempfile()

  write( clone_commands, tmpfilename )
}

getReposSummary = function(){


  library(testthat)
  library(dplyr)
  library(devtools)
  library(rlang)
  library(pryr)
  library(readr)
  repo_path =  "~/git/leitnr/repos/"
  repos = dir(repo_path)

 # repos = intersect( repos, c("adv-r"))

  errors = c()


  summary_df = purrr::map_df( paste0(repo_path, repos, "/" ),
                           function(a){
                             print(a)
                             tryCatch({
                               result = getFunctionsFromFiles(a)
                               df = convertCallCountsToHashTable(result$cards)
                               df$source = a
                               errors <<- c(errors, result$errors)
                               df})
                           }
  )
#  all_dfs = sapply( paste0(repo_path, repos, "/" ), function(a){
#    print(a)
#    tryCatch({
#      result = remembr:::getFunctionsFromFile(a)
#      df = remembr:::convertCallCountsToHashTable(result)
#      df$source = a
#      df})
#  })
#
#  summary_df = data_frame(
#
#    function_name =   unlist(all_dfs['function_name',]),
#    package =   unlist(all_dfs['package',]),
#    name =   unlist(all_dfs['name',]),
#    total_uses =   unlist(all_dfs['total_uses',]),
#    source =   unlist(all_dfs['source',])
#  )

  list(
    summary_df = summary_df,
    errors = errors )
}

errorCounts = function(){
  a = grep(  "there is no", (summary_obj$errors), value = TRUE )



 missing_packages =  tibble(error = a) %>% dplyr::count(error) %>% arrange(desc(n)) %>%
    mutate( error = gsub(pattern = "there is no package called", replacement = "", x = error)) %>%
    mutate( error = gsub(pattern = "’|‘", replacement = "", x = error)) %>%
    mutate( error = gsub(pattern = " ", replacement = "", x = error))
}


getFeatureVectors = function(summary_df){
  rrepos = summary_df %>% select( source, function_name, total_uses) %>% group_by( source ) %>% tidyr::spread( function_name, total_uses, fill =0 )
}


plotDendros = function(){
  mtx = as.matrix( rrepos %>% ungroup() %>% select(-source) )
  rownames(mtx) = rrepos$source
  plot(hclust(dist(mtx)))



  tfidfmat = tfidf %>% ungroup() %>% select(function_name, tfidf, source) %>%
    tidyr::spread( function_name, tfidf, fill =0 ) %>%
    mutate( source = gsub( "~/git/leitnr/repos/", "", source  ) )
  mtx = as.matrix( tfidfmat %>% select(-source) )
  rownames(mtx) = tfidfmat$source

  hca = hclust(dist(mtx))
  plot(hca)
  rect.hclust(hca, k = 12, border = "red")


}



compileResults = function(summary_df){

  summary_df %>% dplyr::count(package) %>% arrange(desc(n)) %>% View()

#summary_df = summary
#package ='dplyr'


  package_key = 'base'
  idf = summary_df  %>%
    dplyr::mutate(num_sources = dplyr::n_distinct( source )) %>%
    dplyr::filter(package == !!(package_key)) %>%
    dplyr::group_by(name ) %>%
    dplyr::summarize(  idf =log( first(num_sources) / dplyr::n_distinct(  source ) ))


  idf_package = summary_df  %>%
    dplyr::mutate(num_sources = n_distinct( source )) %>%
    dplyr::group_by(package ) %>%
    dplyr::summarize(  idf =log( first(num_sources) / n_distinct(  source ) ))


  tfidf = summary_df  %>%
    dplyr::filter(package == !!(package_key)) %>%
    dplyr::left_join( idf, by = 'name') %>%
    dplyr::mutate(tfidf = total_uses * idf )


  #result = dist(t(as.matrix( uses %>% select(-name) )) )
  #heatmap(as.matrix(result))
}
#
#ggplot(
#    summary_df %>%
#      filter(
#        name == 'R6Class' |
#          name == '@' | name == 'setClass' | name == 'setGeneric' | name == 'setMethod' | name == 'new' | name == 'is' | name == 'slot' |
#        name =='class' |name =='UseMethod' | name == 'unclass' | name == 'NextMethod') %>%
#      mutate(
#       class_type =  ifelse( name == 'R6Class', 'r6',
#                ifelse( name =='class' |name =='UseMethod' | name == 'unclass' | name == 'NextMethod', 's3', 's4' )
#                )
#      ) %>%
#      select(source, name,total_uses, class_type) %>%
#      group_by(source, class_type) %>%
#      summarize(
#        total_uses = sum(total_uses)
#      ) ,
#aes( x = source, y = total_uses, fill = class_type) ) + geom_bar(stat='identity') + coord_flip()
#
#
#summary_df %>%
#  filter(
#    name == 'R6Class' |
#      name == '@' | name == 'setClass' | name == 'setGeneric' | name == 'setMethod' | name == 'new' | name == 'is' | name == 'slot' |      name =='class' |name =='UseMethod' | name == 'unclass' | name == 'NextMethod') %>%
#  mutate(
#    class_type =  ifelse( name == 'R6Class', 'r6',
#                          ifelse( name =='class' |name =='UseMethod' | name == 'unclass' | name == 'NextMethod', 's3', 's4' )
#    )
#  ) %>%
#  select(source, name,total_uses, class_type) %>%
#  group_by( class_type, name ) %>%
#  summarize(
#    total_uses = sum(total_uses)
#  )
#
#summary_df %>%
#  filter(
#    name == 'R6Class' |
#      name == '@' | name == 'setClass' | name == 'setGeneric' | name == 'setMethod' | name == 'new' | name == 'is' | name == 'slot' |      name =='class' |name =='UseMethod' | name == 'unclass' | name == 'NextMethod') %>%
#  mutate(
#    class_type =  ifelse( name == 'R6Class', 'r6',
#                          ifelse( name =='class' |name =='UseMethod' | name == 'unclass' | name == 'NextMethod', 's3', 's4' )
#    )
#  ) %>%
#  select(source, name,total_uses, class_type) %>%
#  filter( name == 'UseMethod')  %>% select(source, total_uses)
#
