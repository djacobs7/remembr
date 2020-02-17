
test_that("can_add_documentation", {
  skip("adding documentation urls is not currently supported")

  addTargetFunctions(pryr::standardise_call)
  addDocumentationURL(pryr::standardise_call, "http://adv-r.had.co.nz/Expressions.html#calls")

  addTargetFunctions(parent.frame)
  addDocumentationURL(parent.frame, "http://adv-r.had.co.nz/Environments.html")

  addTargetFunctions(addTaskCallback)
  addDocumentationURL(addTaskCallback , "http://developer.r-project.org/TaskHandlers.pdf")

})

test_that("can add a target function",{

  skip("this is not supported currently (show target functions method ) ")
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
