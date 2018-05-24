BASE_DESC_LINES <- readLines(system.file("DESCRIPTION"))
TESTWHAT_DESC_LINES <- readLines(system.file("DESCRIPTION", package = "testwhat"))

# check_has_desc_element --------------------------------------------------

context("check_has_desc_element")

test_that(
  "test check_has_desc_element() passes on a DESCRIPTION with the required field", {
    state <- setup_state(stu_code = BASE_DESC_LINES, ex_type = "FileExercise")
    expect_pass(
      state %>%
        parse_desc() %>%
        check_has_desc_element('Package')
    )
  }
)

test_that(
  "test check_has_desc_element() fails on a DESCRIPTION without the required field", {
    state <- setup_state(stu_code = BASE_DESC_LINES, ex_type = "FileExercise")
    expect_error(
      state %>%
        parse_desc() %>%
        check_has_desc_element('PACKAGE')
    )
  }
)

# check_desc_element_matches ----------------------------------------------

context("check_desc_element_matches")

test_that(
  "test check_desc_element_matches() passes on a DESCRIPTION with a matching field", {
    state <- setup_state(stu_code = BASE_DESC_LINES, ex_type = "FileExercise")
    expect_pass(
      state %>%
        parse_desc() %>%
        check_desc_element_matches('License', '^Part of R \\d.\\d.\\d$')
    )
  }
)

test_that(
  "test check_desc_element_matches() passes on a DESCRIPTION with a fixed-matching field", {
    state <- setup_state(stu_code = BASE_DESC_LINES, ex_type = "FileExercise")
    expect_pass(
      state %>%
        parse_desc() %>%
        check_desc_element_matches('Title', 'The R Base Package', fixed = TRUE)
    )
  }
)

test_that(
  "test check_desc_element_matches() fails on a DESCRIPTION without the required field", {
    state <- setup_state(stu_code = BASE_DESC_LINES, ex_type = "FileExercise")
    expect_error(
      state %>%
        parse_desc() %>%
        check_desc_element_matches('PACKAGE')
    )
  }
)

test_that(
  "test check_desc_element_matches() fails on a DESCRIPTION with a non-matching field", {
    state <- setup_state(stu_code = BASE_DESC_LINES, ex_type = "FileExercise")
    expect_error(
      state %>%
        parse_desc() %>%
        check_desc_element_matches('Description', 'Army bases, bass fishing, and bass drums', fixed = TRUE)
    )
  }
)

# check_desc_version ------------------------------------------------------

context("check_desc_version")

test_that(
  "test check_desc_version() passes on a DESCRIPTION with a correct Version field", {
    state <- setup_state(stu_code = BASE_DESC_LINES, ex_type = "FileExercise")
    r_version <- as.package_version(R.Version())
    expect_pass(
      state %>%
        parse_desc() %>%
        check_desc_version(r_version)
    )
  }
)

test_that(
  "test check_desc_version() fails on a DESCRIPTION without the Version field", {
    state <- setup_state(
      stu_code = BASE_DESC_LINES[!grepl("^Version", BASE_DESC_LINES)],
      ex_type = "FileExercise"
    )
    r_version <- as.package_version(R.Version())
    expect_error(
      state %>%
        parse_desc() %>%
        check_desc_version(r_version)
    )
  }
)

test_that(
  "test check_has_desc_version() fails on a DESCRIPTION with an incorrect Version field", {
    state <- setup_state(
      stu_code = BASE_DESC_LINES[!grepl("^Version", BASE_DESC_LINES)],
      ex_type = "FileExercise"
    )
    bad_version <- as.package_version('0.0-1')
    expect_error(
      state %>%
        parse_desc() %>%
        check_desc_version(bad_version)
    )
  }
)

# check_desc_date ------------------------------------------------------

context("check_desc_date")

test_that(
  "test check_desc_date() passes on a DESCRIPTION with a correct Date field", {
    state <- setup_state(
      stu_code = c(BASE_DESC_LINES, paste("Date:", Sys.Date())),
      ex_type = "FileExercise"
    )
    expect_pass(
      state %>%
        parse_desc() %>%
        check_desc_date()
    )
  }
)

test_that(
  "test check_desc_date() fails on a DESCRIPTION without the Date field", {
    state <- setup_state(stu_code = BASE_DESC_LINES, ex_type = "FileExercise")
    expect_error(
      state %>%
        parse_desc() %>%
        check_desc_date()
    )
  }
)

test_that(
  "test check_desc_date() fails on a DESCRIPTION with an incorrect Date field", {
    state <- setup_state(
      stu_code = c(BASE_DESC_LINES, "Date: 1915-06-16"),
      ex_type = "FileExercise"
    )
    expect_error(
      state %>%
        parse_desc() %>%
        check_desc_date()
    )
  }
)

# check_desc_authors_at_r -------------------------------------------------

context("check_desc_authors_at_r")

test_that(
  "test check_desc_authors_at_r() passes on a DESCRIPTION with a correct Authors@R field", {
    state <- setup_state(
      stu_code = c(
        BASE_DESC_LINES,
        "Authors@R: as.person('R Core Team <R-core@r-project.org> [aut, cre]')"
      ),
      ex_type = "FileExercise"
    )
    expect_pass(
      state %>%
        parse_desc() %>%
        check_desc_authors_at_r(
          utils::as.person('R Core Team <R-core@r-project.org> [aut, cre]')
        )
    )
  }
)

test_that(
  "test check_desc_authors_at_r() fails on a DESCRIPTION without the Authors@R field", {
    state <- setup_state(stu_code = BASE_DESC_LINES, ex_type = "FileExercise")
    expect_error(
      state %>%
        parse_desc() %>%
        check_desc_authors_at_r(
          utils::as.person('R Core Team <R-core@r-project.org> [aut, cre]')
        )
    )
  }
)

test_that(
  "test check_desc_authors_at_r() fails on a DESCRIPTION with an incorrect Authors@R field", {
    state <- setup_state(
      stu_code = c(
        BASE_DESC_LINES,
        "Authors@R: as.person('Richie Cotton <richie@datacamp.com> [aut, cre])'"
      ),
      ex_type = "FileExercise"
    )
    expect_error(
      state %>%
        parse_desc() %>%
        check_desc_authors_at_r(
          utils::as.person('R Core Team <R-core@r-project.org> [aut, cre]')
        )
    )
  }
)
