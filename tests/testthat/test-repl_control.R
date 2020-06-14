
#clean up code for when things go wrong!
resetOptions = function(){
  clearOptions()
  initOptions()
}


test_that("you can initialize and clear remembr options", {

  clearOptions()

  expect_null( getOption("remembr.should_persist") )
  expect_null( getOption("remembr.call_counts_hash_table_path") )
  expect_null( getOption("remembr.call_counts_hash_table") )
  expect_null( getOption("remembr.pack_state") )


  initOptions()

  expect_equal( getOption("remembr.should_persist"), TRUE )
  expect_type( getOption("remembr.call_counts_hash_table") , "environment")
  expect_type( getOption("remembr.pack_state") , "environment")
  expect_type( getOption("remembr.call_counts_hash_table_path") , "character")

})

test_that("you can start and stop the callbackhandler", {
  initRemembr()
  expect_true( "addCallCounts" %in%  getTaskCallbackNames() )
  stopRemembr()
  expect_false( "addCallCounts" %in%  getTaskCallbackNames() )
})

test_that( "you can create a custom hash table path", {
  resetOptions()

  #reset tempfile
  td = tempdir()
  tmppath = paste0(td, "call_counts_hash_table.Rds")
  unlink(tmppath)
  expect_false( file.exists( tmppath ))

  #actually set the tmp file path
  # TODO: perhaps this should be one single method :
  #   reloadCallCountsHashTable( which can be renamed )
  options(list( "remembr.call_counts_hash_table_path" = tmppath  ) )
  initOptions()

  #test out the callback!
  addCallCountsCallback(
    quote({
      lm( iris$Sepal.Length ~ iris$Sepal.Width )
    })
    , value = FALSE,
    status = FALSE,
    visible = FALSE,
    data = FALSE
  )

  cc = getCallCountsHashTable()
  print(ls(cc))
  testthat::expect_equal(cc[['stats::lm']]$total_uses, 1 )
  expect_true( file.exists( tmppath ))

  #cleanup
  resetOptions()
  unlink(tmppath)
})

