Sys.unsetenv("R_TESTS")

library(testthat)
library(testwhat.ext)

test_check("testwhat.ext")
