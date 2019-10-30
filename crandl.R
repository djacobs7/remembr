contrib = xml2::read_html("https://cran.r-project.org/src/contrib/")

table = contrib %>% rvest::html_table()


df =table[[1]] %>%
  as_tibble(.name_repair = 'unique') %>%
  mutate( unit = stringr::str_extract(Size, "([A-Z])" )) %>%
  mutate( num = readr::parse_number(Size)) %>%
  mutate( size_in_bytes = ifelse( unit == 'K',
                                  num * 1000,
                                  ifelse( unit =='M', num * 1000 * 1000, NA ) ) )


all_r_packages = df %>%
  filter( !is.na(size_in_bytes)) %>%
  select(Name) %>%
  mutate( pkname = gsub(".tar.gz", "", Name) ) %>%
  tidyr::separate( pkname, sep ="_", into = c('pkname', 'v')) %>%
  filter(!is.na(v))

top_downloads = cranlogs::cran_top_downloads(count = 100)


cranlogs::cran_downloads(when = "last-week", packages =all_r_packages$pkname[1:20])

cranlogs::cran_downloads(when = "last-week", packages =top_downloads$package)

#IDEA:

# NO GIT

# AN R PACKAGE TO JUST SAVE YOUR CODE TO A REMOTE GIT REPO  .  IT AUTO COMMITS FOR YOU AND HAS BETTER SYNTAX THAN GIT.
# ITS FREE LOCAL; YOU PAY FOR THE SHARED STORAGE
# AND YOU CAN GET GET BACKUPS... PAY FOR CODE REVIEWS...  GET AUTOMATIC CODE ANNOTATIONS


packages_path = "/Users/djacobs7/git/leitnr/packages/"
packages_zipped_path = "/Users/djacobs7/git/leitnr/packages_zipped/"


download_package = function(package_name ){
  if (!dir.exists( paste0( packages_path, package_name) )){
    zipped = download.packages(pkgs = package_name,
                               destdir = packages_zipped_path,
                               type = "source")[,2]


    untar( zipped, exdir =paste0( packages_path) )
  }
}


top_downloads$package %>% purrr::walk(download_package)




getCardsFromTidyTuesday = function(package_name ){


  package_name= as.character(package_name)

  print("=======")
  print(package_name)
  package_path= paste0("~/git/leitnr/external_sources/tidy_tuesday/", package_name )
  #namespace_info = parseNamespaceFile( package = package_name, package.lib = "~/git/leitnr/packages/")
  #imports =namespace_info$imports %>% purrr::map_chr( ~.x[[1]])
  imports = c()

  subpaths = c("/" )

  out = subpaths%>% purrr::map( function(subpath_dir){
    output_env = remembr:::loadOrCreateEnv()

    if( subpath_dir == '/tests/'){
      my_imports = c(imports, 'testthat')
    } else{
      my_imports = imports
    }
    subpath = paste0(  package_path, subpath_dir )
    if ( dir.exists(subpath)){
      result =remembr:::getFunctionsFromFiles(subpath,output_env = output_env, libraries = my_imports )
    } else {
      result = list()
    }
    result$package_name = package_name
    result$subpath = subpath
    result
  })


  out$imports = imports

  out$package_name = package_name
  out

}

t = getCardsFromTidyTuesday("MarcelvonReth/tidytuesday")

tidy_tuesday_repos = readRDS("data-local/tidy_tuesday_repos")

tidy_tue_fns =   tidy_tuesday_repos$fullname%>%
  purrr::map(  ~getCardsFromTidyTuesday(.x ), .id = 'fullname' )

saveRDS( tidy_tue_fns, "data-local/tidy_tue_fns.Rds")



getCardsFromPackage = function(package_name ){


  package_name= as.character(package_name)

  print("=======")
  print(package_name)
  package_path= paste0("~/git/leitnr/packages/", package_name )
  namespace_info = parseNamespaceFile( package = package_name, package.lib = "~/git/leitnr/packages/")
    imports =namespace_info$imports %>% purrr::map_chr( ~.x[[1]])
  imports = c(imports, package_name)

  subpaths = c("/R/", "/vignettes/", "/tests/" )

 # out = subpaths%>% purrr::map( function(subpath_dir){
 #   output_env = remembr:::loadOrCreateEnv()
#
 #   if( subpath_dir == '/tests/'){
 #     my_imports = c(imports, 'testthat')
 #   } else{
 #     my_imports = imports
 #   }
 #   subpath = paste0(  package_path, subpath_dir )
 #   if ( dir.exists(subpath)){
 #     result =remembr:::getFunctionsFromFiles(subpath,output_env = output_env, libraries = my_imports )
 #   } else {
 #     result = list()
 #   }
 #   result$package_name = package_name
 #   result$subpath = subpath
 #   result
 # })

  out = list()
   out$imports = imports

  out$package_name = package_name
  out

}

all_imports = top_downloads$package%>%
  purrr::map( ~list(.x = .x )) %>%
  purrr::map(  ~getCardsFromPackage(.x ), .id = 'package' )

package_fns2 = top_downloads$package%>%
  purrr::map( ~list(.x = .x )) %>%
  purrr::map(  ~getCardsFromPackage(.x ), .id = 'package' )

saveRDS( package_fns2, "package_fns2.Rds")




dfs = unlist(package_fns2, recursive = FALSE ) %>%
  purrr::keep(  function(.x){
    if(is.character(.x)){ FALSE}
                else if (!( 'cards' %in% names(.x) )){FALSE}
               else
              {  ls(.x$cards)%>%length() != 0 }

  }) %>%
  purrr::map_dfr(
                    function(.x){
                      out = remembr:::convertCallCountsToHashTable( .x$cards)
                      out$subpath = .x$subpath
                      out$package_name = .x$package_name
                      out
                    }
                    , .id = 'source' )



dfs = package_fns %>%
  purrr::keep( ~ ls(.x$cards)%>%length() != 0 ) %>%
  purrr::map_dfr( ~remembr:::convertCallCountsToHashTable( .x$cards), .id = 'source' )

class_types =bind_rows(
dfs %>% filter(package =='methods') %>% count(source) %>% mutate( class = "S4"),
dfs %>% filter(name == 'setClass' | name == 'setGeneric') %>% count(source) %>% mutate( class = "S4Sure"),
dfs %>% filter(package =='R6') %>% count(source) %>% mutate( class = "R6"),
dfs %>% filter(name =='UseMethod') %>% count(source) %>% mutate( class = "S3")
)

dfs %>%filter( package =='methods') %>% filter( name == 'setClass')%>% select(source)
dfs %>%filter( package =='methods') %>% filter( name == 'setGeneric') %>% select(source)


#####
pkg_downloads = vroom::vroom("~/Downloads/2019-09-11.csv", delim = ',')

ranked_package_downloads = pkg_downloads %>% count( package ) %>% arrange( desc(n)) %>%
  mutate( all_rank = row_number())

my_uses = remembr:::convertCallCountsToHashTable(  call_counts_hash_table_daniel ) %>%
  group_by(package) %>%
  summarize( total_uses = sum(total_uses )) %>%
  arrange( desc(total_uses) ) %>%
  mutate( my_rank = row_number()) %>%
  filter( !is.na(all_rank))

joint_use = my_uses %>%
  left_join( ranked_package_downloads, by = 'package' ) %>%
  filter(!is.na(all_rank))

underused_packages = ranked_package_downloads %>%
  left_join( my_uses, by = 'package' ) %>%
  filter( all_rank < 100) %>%
  filter( is.na(total_uses) )

ggplot( joint_use, aes( y =my_rank, x =all_rank, label = package)) + geom_text()
