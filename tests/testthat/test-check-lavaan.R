context("lavaan")

test_that("check_lavaan_df", {
  # incorrect use
  state <- setup_state(sol_code = "model1 = ''",
                       stu_code = "model1 <- ''")
  expect_error(state %>% check_lavaan_df(), regexp = "can only be used")
  expect_error(state %>% check_object('model1') %>% check_lavaan_df(), regexp = "does not validly represent")

  # fails
  state <- setup_state(sol_code = "model1 <- 'a ~ x1'",
                       stu_code = "model1 <- ''")
  expect_error(state %>% check_object('model1') %>% check_lavaan_df(),
               class = "sct_failure",
               regexp = "The contents of the variable `model1` aren't correct. Make sure it is a valid lavaan model.",
               fixed = TRUE)

  # passes
  state <- setup_state(sol_code = "model1 <- 'a ~ x1'",
                       stu_code = "model1 <- 'a ~ x1'")
  expect_pass(state %>% check_object('model1') %>% check_lavaan_df())
})

test_that("check_lavaan_uses fails", {
  state <- setup_state(sol_code = "model1 <- 'a ~ x1 + x2'",
                       stu_code = "model1 <- 'a =~ x1'")

  # incorrect use
  expect_error(state %>% check_object('model1') %>% check_lavaan_uses(), regexp = "can only be used")

  # fails
  failing <- list(
    list(lhs = 'a', op = "~"),
    list(rhs = 'x2'),
    list(op = '~', rhs = 'x1'),
    list(lhs = 'a', op = "~", rhs = "x1"),
    list(lhs = 'a', op = "~", rhs = "x2")
  )
  for (args in failing) {
    expect_error({
        x <- state %>%
          check_object('model1') %>%
          check_lavaan_df()
        do.call(check_lavaan_uses, c(args, list(x, incorrect_msg = 'wrong')))
      },
      class = "sct_failure",
      regexp = "The contents of the variable `model1` aren't correct. Check the lavaan model it represents. Wrong",
      fixed = TRUE
    )
  }

  passing <- list(
    list(lhs = 'a'),
    list(rhs = 'x1'),
    list(lhs = 'a', rhs = 'x1')
  )
  for (args in passing) {
    expect_pass({
      x <- state %>%
        check_object('model1') %>%
        check_lavaan_df()
      do.call(check_lavaan_uses, c(args, list(x, incorrect_msg = 'wrong')))
    })
  }
})

# test_that("check_lavaan_match", {
#   state <- setup_state("model1 <- 'a =~ x1 + x2 + x3'", "model1 <- 'a =~ x1 + x2'")
#   expect_error(
#     state %>% check_lavaan_df %>% check_lavaan_match('model1', lhs = 'a', op = '=~', incorrect_msg = 'wrong'),
#     class = "sct_failure"
#   )
# })

test_that("check_lavaan_pattern", {
  # incorrect_use
  state <- setup_state(sol_code = "model1 <- 3",
                       stu_code = "model1 <- 3")
  expect_error(state %>% check_lavaan_pattern(), regexp = "can only be used")
  expect_error(state %>% check_object('model1') %>% check_lavaan_pattern('a'),
               regexp = "targetting is not a string")

  # fail: not a string
  state <- setup_state(sol_code = "model1 <- 'a ~ x1'",
                       stu_code = "model1 <- 3")
  expect_error(state %>% check_object('model1') %>% check_lavaan_pattern('a'),
               regexp = "The contents of the variable `model1` aren't correct. It is not a string so it can't represent a lavaan model.",
               fixed = TRUE,
               class = 'sct_failure')

  # fails and passes
  state <- setup_state(sol_code = "mod <- 'a =~ x1 + x2'",
                       stu_code = "mod <- 'a =~ x1'")
  for (patt in c("x1\\s\\+\\s+x2", "x2", "\\+")) {
    expect_error(state %>% check_object('mod') %>% check_lavaan_pattern(patt, incorrect_msg = 'wrong'),
                 regexp = "The contents of the variable `mod` aren't correct. Wrong",
                 fixed = TRUE,
                 class = "sct_failure")
  }
  for (patt in c("a", "a\\s+=~\\s+x1", "x1")) {
    expect_pass(state %>% check_object('mod') %>% check_lavaan_pattern(patt, incorrect_msg ='wrong'))
  }

})
