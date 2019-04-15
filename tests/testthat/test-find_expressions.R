context("test-find_expressions")

test_that("multiplication works", {


#  expression  =

#  createTargetFunctions(lm, dplyr::summarize)

  get_functions({ a = iris;
  model = lm( iris$Sepal.Length ~ iris$Petal.Length )} )
  expect_equal(2 * 2, 4)
})

test_that("can_add_documentation", {
  addTargetFunctions(pryr::standardise_call)
  addDocumentationURL(pryr::standardise_call, "http://adv-r.had.co.nz/Expressions.html#calls")

})

test_that("can add a target function",{
  addTargetFunctions( lm, dplyr::summarise )


  addTargetFunctions(lm)
  addTargetFunctions(new.env)
  addTargetFunctions(ls)

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
