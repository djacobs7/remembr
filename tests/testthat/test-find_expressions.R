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




testthat::test_that("we handle namespaces properly", {


  call_counts_hash_table = loadOrCreateEnv(NULL)
  get_functions({
    stats::lm( iris$Sepal.Width ~ iris$Petal.Length )
  }, call_counts_hash_table)
  testthat::expect_equal(  call_counts_hash_table[['stats::lm']]$total_uses, 1 )

})

test_that("can_add_documentation", {
  addTargetFunctions(pryr::standardise_call)
  addDocumentationURL(pryr::standardise_call, "http://adv-r.had.co.nz/Expressions.html#calls")

  addTargetFunctions(parent.frame)
  addDocumentationURL(parent.frame, "http://adv-r.had.co.nz/Environments.html")

  addTargetFunctions(addTaskCallback)
  addDocumentationURL(addTaskCallback , "http://developer.r-project.org/TaskHandlers.pdf")

})

test_that("can add a target function",{
  addTargetFunctions( lm, dplyr::summarise )


  addTargetFunctions(lm)
  addTargetFunctions(new.env)
  addTargetFunctions(ls)
  addTargetFunctions(parent.frame)

  currentTargets = showTargetFunctions()

  expect_true( grep( "stats::lm", currentTargets) != 0 )
  expect_true( grep( "dplyr::summarise", currentTargets) != 0 )
  expect_true( grep( "base::new.env", currentTargets) != 0 )

  # FIND OUT WHAT THESE DO AND TEST THEM
  #createTargetFunctions( 1 )
  #createTargetFunctions( NULL )
  #createTargetFunctions( a + 1 )
  #createTargetFunctions( 1 + 2 )
})
