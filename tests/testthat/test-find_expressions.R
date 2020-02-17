context("test-find_expressions")

test_that( "Simple call to get_functions", {



  call_counts_hash_table = loadOrCreateEnv(NULL)
  testthat::expect_null(  call_counts_hash_table[['stats::lm']] )
  get_functions({ a = iris;
  model = lm( iris$Sepal.Length ~ iris$Petal.Length )
  model = stats::lm( iris$Sepal.Length ~ iris$Petal.Length )} ,
    call_counts_hash_table
  )



  testthat::expect_equal(  call_counts_hash_table[['stats::lm']]$total_uses, 2 )
})

test_that("multiple calls to get_functions", {
  call_counts_hash_table = loadOrCreateEnv(NULL)
#  expression  =

#  createTargetFunctions(lm, dplyr::summarize)

  testthat::expect_null(  call_counts_hash_table[['stats::lm']] )
  get_functions({ a = iris;
  model = lm( iris$Sepal.Length ~ iris$Petal.Length )}, call_counts_hash_table  )


  testthat::expect_equal(  call_counts_hash_table[['stats::lm']]$total_uses, 1 )
  get_functions({ summary( data.frame( a = 2, b = 1))} , call_counts_hash_table )

  testthat::expect_equal(  call_counts_hash_table[['stats::lm']]$total_uses, 1 )
  testthat::expect_equal(  call_counts_hash_table[['base::summary']]$total_uses, 1 )

  get_functions( {lm( iris$Sepal.Length ~ iris$Sepal.Width ) }, call_counts_hash_table )

  testthat::expect_equal(  call_counts_hash_table[['stats::lm']]$total_uses, 2 )



  #showCallCounts()
})

test_that("can create a function within an expression", {

  call_counts_hash_table = loadOrCreateEnv(NULL)
  get_functions({
    runLm = function(){ lm( iris$Sepal.Width ~ iris$Petal.Length ) }
    runLm()
    ls()
  },
  call_counts_hash_table
  )
  testthat::expect_equal(  call_counts_hash_table[['stats::lm']]$total_uses, 1 )
  testthat::expect_equal(  call_counts_hash_table[['base::ls']]$total_uses, 1 )

})

#'
test_that("can interpet library", {

  #TODO: IMPLEMENT ( right now this test has a side effect I'd rather not have.)
  call_counts_hash_table = loadOrCreateEnv(NULL)
  get_functions({

    library(lubridate)
    date()
  },
  call_counts_hash_table
  )

  testthat::expect_equal(  call_counts_hash_table[['lubridate::date']]$total_uses, 1 )
})


test_that("can accept a list of libraries", {

  #TODO: IMPLEMENT ( right now this test has a side effect I'd rather not have.)
  call_counts_hash_table = loadOrCreateEnv(NULL)
  get_functions({
    date()
  },
  libraries = 'lubridate',
  call_counts_hash_table =call_counts_hash_table
  )

  testthat::expect_equal(  call_counts_hash_table[['lubridate::date']]$total_uses, 1 )
})


test_that("can deal with a non-existant library", {

  call_counts_hash_table = loadOrCreateEnv(NULL)
  expect_error({
    get_functions({
      library(bananaface)
    },
    call_counts_hash_table
    )
  },"there is no package called .bananaface.")

  #testthat::expect_equal(  call_counts_hash_table[['library']]$total_uses, 1 )

})




testthat::test_that("we handle namespaces properly", {


  call_counts_hash_table = loadOrCreateEnv(NULL)
  get_functions({
    stats::lm( iris$Sepal.Width ~ iris$Petal.Length )
  }, call_counts_hash_table)
  testthat::expect_equal(  call_counts_hash_table[['stats::lm']]$total_uses, 1 )

})
