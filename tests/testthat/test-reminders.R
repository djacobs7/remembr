context("test-reminders")

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


test_that("remind me works", {
 options("remembr.call_counts_hash_table" = createTestTable())

 expect_output(remindMe(),regexp = "Based on your previous R usage")
})


test_that("remind package works", {
  options("remembr.call_counts_hash_table" = createTestTable())
  expect_output( remindPackage('dplyr'), regexp = "Here is everything to review from the" )
})


test_that("upcoming reminders works", {
  options("remembr.call_counts_hash_table" = createTestTable())
  expect_output( upcomingReminders(10), regexp = "Here is everything to review" )
})

test_that("upcoming reminders works", {
  options("remembr.call_counts_hash_table" = createTestTable())
  expect_output( upcomingReminders(10), regexp = "Here is everything to review" )
})

teardown({
  options("remembr.call_counts_hash_table" = remembr:::loadOrCreateEnv(
                                                  remembr::getOptions("remembr.call_counts_hash_table_path")
                                              ))
})


