context("Examples")

test_that("check_object2 passes", {
  state <- setup_state("x <- 1; y <- 2", "x <- 1; y <- 2")
  state %>% check_object2(c('x', 'y'))
})

test_that("check_object2 fails", {
  state <- setup_state("", "x <- 1")
  expect_error(
    state %>% check_object2(c('x')),
    'sct_failed_error')
})
