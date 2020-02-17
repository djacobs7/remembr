
#devtools::test(filter = "*functions_from_file*")
test_that("we can handle errors sensibly in a file", {
  skip("this isn't possible yet")


})

test_that("we can handle a file with an error in it ", {

  result = getFunctionsFromFiles( system.file("test-files/file_with_error.R", package ='remembr') )
  expect_setequal(ls(result$libraries), character(0))
  expect_setequal(c("base::=", "base::~", "base::$"), ls( result$cards) )


})

test_that("we can get functions from a simple file", {

  result = getFunctionsFromFiles( system.file("test-files/simple_test.R", package ='remembr') )
  expect_setequal(ls(result$libraries), character(0))
  expect_setequal(c("base::=", "base::~", "base::$", "stats::lm"), ls( result$cards) )

  expect_setequal(ls(result$errors), character(0))
})

  test_that("we can get functions from a file", {
skip("need to debug this")
    result = getFunctionsFromFiles( system.file("test-files/tidytext.R", package ='remembr') )

    expect_setequal(

      c( "dplyr",
         "ggplot2",
         "janeaustenr",
         "knitr",
         "reshape2",
         "stringr",
         "tidyr",
         "tidytext",
         "wordcloud"
      ),
      ls( result$libraries ) )

    expect_setequal(
    c("base::-", "base::!=", "base::[", "base::/", "base::%/%", "base::+",
      "base::<-", "base::==", "base::>", "base::~", "base::$", "base::c",
      "base::cumsum", "base::ifelse", "base::library", "base::requireNamespace",
      "base::with", "dplyr::anti_join", "dplyr::count", "dplyr::filter",
      "dplyr::group_by", "dplyr::inner_join", "dplyr::left_join", "dplyr::mutate",
      "dplyr::n", "dplyr::row_number", "dplyr::semi_join", "dplyr::summarise",
      "dplyr::summarize", "dplyr::top_n", "dplyr::ungroup", "ggplot2::aes",
      "ggplot2::coord_flip", "ggplot2::facet_wrap", "ggplot2::geom_bar",
      "ggplot2::geom_col", "ggplot2::ggplot", "ggplot2::labs", "ggplot2::theme_light",
      "ggplot2::theme_set", "janeaustenr::austen_books", "remembr::%>%",
      "remembr::tibble", "reshape2::acast", "stats::reorder", "stringr::regex",
      "stringr::str_detect", "tidyr::spread", "tidytext::get_sentiments",
      "tidytext::get_stopwords", "tidytext::unnest_tokens", "wordcloud::comparison.cloud",
      "wordcloud::wordcloud"),
    ls( result$cards ) )

    expect(!is.null( result) )
  })
