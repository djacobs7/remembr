context("test-find_expressions")

test_that("multiplication works", {


#  expression  =

  createTargetFunctions(lm, dplyr::summarize)

  get_functions({ a = iris;
  model = lm( iris$Sepal.Length ~ iris$Petal.Length )} )
  expect_equal(2 * 2, 4)
})


