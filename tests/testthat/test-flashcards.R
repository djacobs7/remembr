
context("test-flashcards")

createTestTable = function(){
  call_counts_hash_table = loadOrCreateEnv(NULL)
  testthat::expect_null(  call_counts_hash_table[['stats::lm']] )
  get_functions({ a = iris;
  model = lm( iris$Sepal.Length ~ iris$Petal.Length )
  model = stats::lm( iris$Sepal.Length ~ iris$Petal.Length )} ,
  call_counts_hash_table
  )
  call_counts_hash_table
}


test_that("we can do some flashcards",{
  cards = createTestTable()
  expect_output({
    .flashCards( num_flashcards = 2, pack  =  cards, inputs = c("y", "n"))
  }, "*Get ready to start*")

})


test_that("we can handle an empty pack ok",{
  expect_output( {
    .flashCards( num_flashcards = 2, pack  = loadOrCreateEnv(NULL) , inputs = c("y", "n"))
  }, "You have no flashcards*")

})
