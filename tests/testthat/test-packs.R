test_that("pack creation api", {

  pack_name = "samplePack"
  startPack(pack_name)

  addKeynameToPack("banana")
  addKeynameToPack("apple")

  finishPack(persist = FALSE)

  expect_setequal(c('banana', 'apple'), listPackKeys(pack_name))
})


test_that("methods get added ok", {
  startPack('testpack2')

  get_functions({
    ls()
    lm( iris$Sepal.Length ~ iris$Sepal.Width )
  })

  finishPack(persist =  FALSE)

  pack_keys = listPackKeys('testpack2')

  expect_true("base::ls" %in%  pack_keys  )
  expect_true("stats::lm" %in%  pack_keys  )
})
