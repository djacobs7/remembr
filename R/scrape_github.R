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


  all_dfs = sapply( paste0(repo_path, repos, "/" ), function(a){
    print(a)
    tryCatch({
      result = remembr:::getFunctionsFromFile(a)
      df = remembr:::convertCallCountsToHashTable(result)
      df$source = a
      df})
  })

  summary_df = data_frame(

    function_name =   unlist(all_dfs['function_name',]),
    package =   unlist(all_dfs['package',]),
    name =   unlist(all_dfs['name',]),
    total_uses =   unlist(all_dfs['total_uses',]),
    source =   unlist(all_dfs['source',])
  )

  summary_df
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

  summary_df %>% count(package) %>% arrange(desc(n)) %>% View()

summary_df = summary
#package ='dplyr'


  package_key = 'base'
  idf = summary_df  %>%
    mutate(num_sources = n_distinct( source )) %>%
    filter(package == !!(package_key)) %>%
    group_by(name ) %>%
    summarize(  idf =log( first(num_sources) / n_distinct(  source ) ))


  idf_package = summary_df  %>%
    mutate(num_sources = n_distinct( source )) %>%
    group_by(package ) %>%
    summarize(  idf =log( first(num_sources) / n_distinct(  source ) ))


  tfidf = summary_df  %>%
    filter(package == !!(package_key)) %>%
    left_join( idf, by = 'name') %>%
    mutate(tfidf = total_uses * idf )


  #result = dist(t(as.matrix( uses %>% select(-name) )) )
  #heatmap(as.matrix(result))
}

