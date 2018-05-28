context("rcpp")

pec <- "library(Rcpp)"

cpp_part <- "#include <Rcpp.h>
using namespace Rcpp ;

// [[Rcpp::export]]
int answer(){
return 42 ;
}"

r_part <- "x <- answer()
x"

embedded_r <- sprintf("/*** R\n%s\n*/", r_part)

test_that("simple Rcpp: parsing + object access works", {
  s <- setup_state(
    sol_code = paste(cpp_part, embedded_r, sep = "\n"),
    stu_code = paste(cpp_part, embedded_r, sep = "\n"),
    pec = pec,
    ex_type = "RCppExercise"
  )

  # Can check objects in R environments
  expect_pass(s %>% check_object('x') %>% check_equal())
  expect_error(s %>% override_solution(x = 3) %>% check_object('x') %>% check_equal(),
               class = "sct_failure")

  # Can 'zoom in' on the R code
  expected_pd <- build_pd(r_part)
  sub_state <- s %>% check_embedded_r()
  expect_equal(sub_state$get("student_code"), r_part)
  expect_equal(sub_state$get("solution_code"), r_part)
  expect_equal(sub_state$get("student_pd"), expected_pd, check.attributes = FALSE)
  expect_equal(sub_state$get("student_pd"), expected_pd, check.attributes = FALSE)
  expect_pass(sub_state %>% check_function("answer") %>% check_result())

  # Can 'zoom in' on the cpp code
  sub_state <- s %>% check_cpp()
  expect_equal(sub_state$get("student_code"), cpp_part)
  expect_equal(sub_state$get("solution_code"), cpp_part)
  expect_pass(sub_state %>% check_code("[[Rcpp::export]]", fixed = TRUE))
  expect_error(sub_state %>% check_code("x <- answer()", fixed = TRUE),
               class = "sct_failure")
})

test_that("parse_embedded_r works with two embedded r scripts", {
  s <- setup_state(
    sol_code = paste(cpp_part, embedded_r, embedded_r, sep = "\n"),
    stu_code = paste(cpp_part, embedded_r, embedded_r, sep = "\n"),
    pec = pec,
    ex_type = "RCppExercise"
  )

  # 'merges' the two pieces of R code
  sub_state <- s %>% check_embedded_r()
  expect_equal(sub_state$get("student_code"), paste(r_part, r_part, sep = "\n"))
  expect_equal(sub_state$get("solution_code"), paste(r_part, r_part, sep = "\n"))
  sub_state <- s %>% check_cpp()
  expect_equal(sub_state$get("student_code"), cpp_part)
  expect_equal(sub_state$get("solution_code"), cpp_part)
})

test_that("parse_embedded_r works with no r scripts", {
  s <- setup_state(
    sol_code = cpp_part,
    stu_code = cpp_part,
    pec = pec,
    ex_type = "RCppExercise"
  )

  # 'merges' the two pieces of R code
  sub_state <- s %>% check_embedded_r()
  expect_equal(sub_state$get("student_code"), "")
  expect_equal(sub_state$get("solution_code"), "")
  sub_state <- s %>% check_cpp()
  expect_equal(sub_state$get("student_code"), cpp_part)
  expect_equal(sub_state$get("solution_code"), cpp_part)
})

ok1 <- "
// [[Rcpp::export]]
int answer(){
return 42 ;
}"

ok2 <- "
//  [[Rcpp::export]]
int  answer (){
return 42 ;
}"

ok3 <- "
//  [[Rcpp::export]]



int  answer (){
return 42 ;
}"

nok1 <- "// [[Rcpp::xport]]
double answer(){
return 42 ;
}"

nok2 <- "
// [[Rcpp::export]]
double answer(){
return 42 ;
}"

nok3 <- "
// [[Rcpp::export]]
int test(){
return 42 ;
}"

test_that("check_cpp_exported works", {
  for(ok in c(ok1, ok2, ok3)) {
    expect_pass(RootState$new(student_code = ok) %>%
                  check_cpp_function_exported("int", "answer"))
  }
  for(nok in c(nok1, nok2, nok3)) {
    expect_error(RootState$new(student_code = nok) %>%
                   check_cpp_function_exported("int", "answer"),
                 regexp = "Did you properly", class = "sct_failure")
  }
  expect_error(RootState$new(student_code = nok1) %>%
                 check_cpp_function_exported("int", "answer", not_exported_msg = "Not_exp"),
               regexp = "Not_exp", class = "sct_failure")
})
