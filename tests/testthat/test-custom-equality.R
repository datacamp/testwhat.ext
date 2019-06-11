context("Custom equality")

test_that("are_classes_equal() works", {
  s <- setup_state(
    sol_code = 'x <- structure(month.name, class = c("months", "character"))',
    stu_code = 'x <- structure(month.abb, class = c("months", "character"))'
  )
  expect_pass(
    s %>%
      check_object("x") %>%
      check_equal(eq_fun = are_classes_equal)
  )
  expect_error(
    s %>%
      override_solution('x <- structure(month.name, class = c("time", "character"))') %>%
      check_object('x') %>%
      check_equal(eq_fun = are_classes_equal),
    class = "sct_failure"
  )
})

test_that("are_lengths_equal() works", {
  s <- setup_state(
    sol_code = "x <- month.name",
    stu_code = "x <- sample(month.name)"
  )
  expect_pass(
    s %>%
      check_object("x") %>%
      check_equal(eq_fun = are_lengths_equal)
  )
  expect_error(
    s %>%
      override_solution("x <- letters") %>%
      check_object('x') %>%
      check_equal(eq_fun = are_lengths_equal),
    class = "sct_failure"
  )
})

test_that("are_dims_equal() works", {
  s <- setup_state(
    sol_code = "x <- matrix(1:12, 4, 3)",
    stu_code = "x <- data.frame(x = 1:4, y = 1:4, z = 1:4)"
  )
  expect_pass(
    s %>%
      check_object("x") %>%
      check_equal(eq_fun = are_dims_equal)
  )
  expect_error(
    s %>%
      override_solution("x <- matrix(1:12, 3, 4)") %>%
      check_object('x') %>%
      check_equal(eq_fun = are_dims_equal),
    class = "sct_failure"
  )
})

test_that("are_igraphs_equal() works", {
  s <- setup_state(
    sol_code = "x <- make_ring(5)",
    stu_code = "x <- make_ring(5)"
  )
  expect_pass(
    s %>%
      check_object("x") %>%
      check_equal(eq_fun = are_igraphs_equal)
  )
  expect_error(
    s %>%
      override_solution("x <- make_ring(5, directed = TRUE)") %>%
      check_object('x') %>%
      check_equal(eq_fun = are_igraphs_equal),
    class = "sct_failure"
  )
})

test_that("are_corpora_equal() works", {
  s <- setup_state(
    sol_code = "x <- tm::VCorpus(tm::VectorSource(month.name))",
    stu_code = "x <- tm::VCorpus(tm::VectorSource(month.name))"
  )
  expect_pass(
    s %>%
      check_object("x") %>%
      check_equal(eq_fun = are_corpora_equal)
  )
  expect_error(
    s %>%
      override_solution("x <- tm::VCorpus(tm::VectorSource(month.abb))") %>%
      check_object('x') %>%
      check_equal(eq_fun = are_corpora_equal),
    class = "sct_failure"
  )
})
