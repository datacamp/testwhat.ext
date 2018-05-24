library(testwhat) # for setup_state

expect_pass <- function(res) {
  expect_true(inherits(res, "State"))
}
