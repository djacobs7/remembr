test_that("can add a card ok", {
  cc = loadOrCreateEnv()

  updateCard( "base::ls", call_counts_hash_table = cc)
  updateCard( "stats::lm", call_counts_hash_table =  cc)

  expect_setequal( ls( cc), c( "base::ls", "stats::lm"))

})


test_that("can track usage counts", {
  cc = loadOrCreateEnv()

  updateCard( "base::ls", call_counts_hash_table = cc)
  updateCard( "base::ls", call_counts_hash_table =  cc)

  updateCard( "stats::lm", call_counts_hash_table =  cc)


  card = cc[["base::ls"]]
  expect_equal(card$total_uses,2)
  expect_equal(card$bucket,1)


  card = cc[["stats::lm"]]
  expect_equal(card$total_uses,1)
  expect_equal(card$bucket,1)
})


test_that("timing is reasonable", {
  cc = loadOrCreateEnv()

  updateCard( "base::ls", call_counts_hash_table = cc)

  updateCard( "stats::lm", call_counts_hash_table =  cc)
  updateCard( "base::ls", call_counts_hash_table =  cc)

  cardls = cc[["base::ls"]]
  cardlm = cc[["stats::lm"]]

  expect_lt(cardls$first_use,cardlm$first_use)
  expect_gt(cardlm$first_use,cardls$first_use)
})


#'
#'
#'  I am not sure this makes any sense
#'  Currently, the longer break you have, the later bucket you get into.
#'
#'  I guess that makes sense - the bucket can increase if you have not used in a long time.
#'
#'
#' TODO: There should be ar ule that the bucket can also jump if you use the function a lot in a short period of time.
#'
test_that("buckets work ok", {
  cc = loadOrCreateEnv()

  fname = "base::ls"
  start_time = lubridate::ymd_hms("2019-01-24 19:39:07")
  updateCard(fname , time = start_time, call_counts_hash_table = cc)

  expect_equal(cc[[fname]]$bucket_id, 1 )
  updateCard( fname,
              time = start_time + lubridate::minutes(5),
              call_counts_hash_table = cc)
  expect_equal(cc[[fname]]$bucket_id, 1 )

  updateCard( fname,
              time = start_time + lubridate::minutes(16),
              call_counts_hash_table = cc)
  expect_equal(cc[[fname]]$bucket_id, 2 )

  updateCard( fname,
              time = start_time + lubridate::minutes(26),
              call_counts_hash_table = cc)
  expect_equal(cc[[fname]]$bucket_id, 2 )

  updateCard( fname,
              time = start_time + lubridate::minutes(88),
              call_counts_hash_table = cc)
  expect_equal(cc[[fname]]$bucket_id, 3 )
})




#-----------  GET HASH TABLE TESTS

test_that("can get hashtable ok", {
  cc = loadOrCreateEnv()

  updateCard( "base::ls", call_counts_hash_table = cc)
  updateCard( "stats::lm", call_counts_hash_table =  cc)

  cc_df = convertCallCountsToHashTable(cc)

  expect_setequal(
    names( cc_df),c('function_name','package','name','first_use','most_recent_use','total_uses','bucket_id','most_recent_review','bucket_timer','review_timer','needs_review' )

  )
})

test_that("can get basic fields in data frame", {
  cc = loadOrCreateEnv()

  start_time = lubridate::ymd_hms("2019-01-24 19:39:07")

  updateCard( "base::ls", time =start_time, call_counts_hash_table = cc)

  cc_df = convertCallCountsToHashTable(cc)
  expect_equal(cc_df$name, "ls")
  expect_equal(cc_df$package, "base")

  expect_equal(cc_df$first_use, start_time)
  expect_equal(cc_df$most_recent_use, start_time)
  expect_equal(cc_df$total_uses, 1)

  expect_equal(cc_df$bucket_id, 1)
})

test_that("bucket review logic is not bonkers", {
  cc = loadOrCreateEnv()

  start_time = lubridate::ymd_hms("2019-01-24 19:39:07")

  updateCard( "base::ls", time =start_time, call_counts_hash_table = cc)

  cc_df = convertCallCountsToHashTable(cc, time = start_time)
  expect_equal(cc_df$name, "ls")
  expect_equal(cc_df$package, "base")

  expect_equal(cc_df$first_use, start_time)
  expect_equal(cc_df$most_recent_use, start_time)
  expect_equal(cc_df$total_uses, 1)
  expect_equal(cc_df$bucket_id, 1)

  expect_false(cc_df$needs_review)
  review_time_1 =  start_time + lubridate::minutes(5)
  cc_df = convertCallCountsToHashTable(cc, time = review_time_1)
  expect_false(cc_df$needs_review)

  review_time_1 =  start_time + lubridate::minutes(11)
  cc_df = convertCallCountsToHashTable(cc, time = review_time_1)
  expect_true(cc_df$needs_review)

 reviewCard( "base::ls", time = review_time_1, call_counts_hash_table = cc )

 review_time_2 =  start_time + lubridate::minutes(12)
 cc_df = convertCallCountsToHashTable(cc, time = review_time_2)
 expect_equal(cc_df$most_recent_review, review_time_1)
 expect_equal(cc_df$most_recent_use, start_time)

 expect_false(cc_df$needs_review)
 expect_equal(cc_df$bucket_id, 2)
})


test_that("flash cards order properly", {
  cc = loadOrCreateEnv()

  start_time = lubridate::ymd_hms("2019-01-24 19:39:07")

  updateCard( "base::ls", time =start_time, call_counts_hash_table = cc)
  updateCard( "stats::lm", time =start_time, call_counts_hash_table = cc)

  review_time_1 =  start_time + lubridate::minutes(11)
  cc_df = convertCallCountsToHashTable(cc, time = review_time_1)

  updateCard( "stats::lm", time =review_time_1, call_counts_hash_table = cc)

  #reviewCard( "base::ls", time = review_time_1, call_counts_hash_table = cc )


  #FIXME: actually use the proper method here
  #cc_df = convertCallCountsToHashTable(cc, time = review_time_2)
  #TODO: this is not finished
})
