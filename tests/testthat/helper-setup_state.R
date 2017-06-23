setup_state <- function(stu_code, sol_code, PEC = "", output = "") {
  if (is.character(output)) output <- list(list(type = "output", payload = output))

  tw <<- testwhat:::tw

  sol_env <- new.env()
  stu_env <- new.env()

  eval(parse(text = PEC),      envir=sol_env)
  eval(parse(text = sol_code), envir=sol_env)
  eval(parse(text = PEC),      envir=stu_env)
  eval(parse(text = stu_code), envir=stu_env)

  tw$clear()

  state <- testwhat:::RootState$new(
    pec = PEC,
    student_code = stu_code,
    student_pd = testwhat:::build_pd(stu_code),
    student_env = stu_env,
    solution_code = sol_code,
    solution_pd = testwhat:::build_pd(sol_code),
    solution_env = sol_env,
    output_list = output,
    test_env = new.env(parent=globalenv())
  )

  # testwhat will access the reporter and state from the tw object
  rep <- testwhat:::DC_reporter$new()
  tw$set(state = state, reporter = rep, stack = TRUE)

  state
}
