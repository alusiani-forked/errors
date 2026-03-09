# tests/testthat/test-print.R
#
# Revised and extended test suite for format.errors / print.errors.
#
# Changes vs. the original:
#  - Each logical concern is its own test_that() block (easier to diagnose)
#  - PDG 950-999 promotion bug is explicitly covered (was absent)
#  - PDG boundary values (354/355, 949/950, 999) are each tested
#  - plus-minus notation gets its own parallel block
#  - scientific notation, decimals flag, digits= integer get dedicated blocks
#  - Edge cases (NA, NaN, Inf, zero value, zero uncertainty) are grouped
#  - Value alignment (value rounded to match uncertainty precision) is checked
#  - Vectorised scalar consistency check retained
# ----------------------------------------------------------------------------

library(errors)

# Shared constant used in plus-minus expected strings
.pm <- "\u00b1"
pm  <- function(v, e) paste(v, .pm, e)   # convenience builder

# ===========================================================================
# 1. Default formatting (digits=1, parenthesis notation)
# ===========================================================================

test_that("default parenthesis formatting spans all uncertainty magnitudes", {
  x <- set_errors(rep(11111.22222, 8),
                  c(12345678, 1234.5678, 12.345678, 1.2345678,
                    .12345678, .012345678, .000012345678, .000000012345678))

  expect_equal(format(x, notation = "parenthesis"),
    c("10000(10000000)", "11000(1000)", "11110(10)", "11111(1)",
      "11111.2(1)", "11111.22(1)", "11111.22222(1)", "11111.22222000(1)"))
})

test_that("default plus-minus formatting spans all uncertainty magnitudes", {
  x <- set_errors(rep(11111.22222, 8),
                  c(12345678, 1234.5678, 12.345678, 1.2345678,
                    .12345678, .012345678, .000012345678, .000000012345678))

  expect_equal(format(x, notation = "plus-minus"), c(
    pm("10000",          "10000000"),
    pm("11000",          "1000"),
    pm("11110",          "10"),
    pm("11111",          "1"),
    pm("11111.2",        "0.1"),
    pm("11111.22",       "0.01"),
    pm("11111.22222",    "0.00001"),
    pm("11111.22222000", "0.00000001")
  ))
})

# ===========================================================================
# 2. Explicit digits= integer
# ===========================================================================

test_that("digits=3 parenthesis formatting is correct", {
  x <- set_errors(rep(11111.22222, 8),
                  c(12345678, 1234.5678, 12.345678, 1.2345678,
                    .12345678, .012345678, .000012345678, .000000012345678))

  expect_equal(format(x, notation = "parenthesis", digits = 3),
    c("10000(12300000)", "11110(1230)", "11111.2(123)", "11111.22(123)",
      "11111.222(123)", "11111.2222(123)", "11111.2222200(123)",
      "11111.2222200000(123)"))
})

test_that("digits=3 plus-minus formatting is correct", {
  x <- set_errors(rep(11111.22222, 8),
                  c(12345678, 1234.5678, 12.345678, 1.2345678,
                    .12345678, .012345678, .000012345678, .000000012345678))

  expect_equal(format(x, notation = "plus-minus", digits = 3), c(
    pm("10000",            "12300000"),
    pm("11110",            "1230"),
    pm("11111.2",          "12.3"),
    pm("11111.22",         "1.23"),
    pm("11111.222",        "0.123"),
    pm("11111.2222",       "0.0123"),
    pm("11111.2222200",    "0.0000123"),
    pm("11111.2222200000", "0.0000000123")
  ))
})

# ===========================================================================
# 3. PDG rounding — general cases
# ===========================================================================

test_that("PDG parenthesis formatting selects correct sig figs by range", {
  x <- set_errors(rep(11111.22222, 8),
                  c(12345678, 1234.5678, 12.345678, 1.2345678,
                    .12345678, .012345678, .000012345678, .000000012345678))

  # Uncertainties here fall in the 100-354 range (2 sig figs) for most entries
  expect_equal(format(x, notation = "parenthesis", digits = "pdg"),
    c("10000(12000000)", "11100(1200)", "11111(12)", "11111.2(12)",
      "11111.22(12)", "11111.222(12)", "11111.222220(12)", "11111.222220000(12)"))
})

test_that("PDG plus-minus formatting selects correct sig figs by range", {
  x <- set_errors(rep(11111.22222, 8),
                  c(12345678, 1234.5678, 12.345678, 1.2345678,
                    .12345678, .012345678, .000012345678, .000000012345678))

  expect_equal(format(x, notation = "plus-minus", digits = "pdg"), c(
    pm("10000",        "12000000"),
    pm("11100",        "1200"),
    pm("11111",        "12"),
    pm("11111.2",      "1.2"),
    pm("11111.22",     "0.12"),
    pm("11111.222",    "0.012"),
    pm("11111.222220", "0.000012"),
    pm("11111.222220000", "0.000000012")
  ))
})

# ===========================================================================
# 4. PDG boundary values: 354/355 and 949/950
# ===========================================================================

test_that("PDG boundary 354 uses 2 sig figs", {
  # unc leading digits = 354 -> still 2 sig figs
  x <- set_errors(1, 0.0354)
  expect_equal(format(x, notation = "plus-minus", digits = "pdg"),
               pm("1.000", "0.035"))
})

test_that("PDG boundary 355 uses 1 sig fig", {
  # unc leading digits = 355 -> 1 sig fig
  x <- set_errors(1, 0.0355)
  expect_equal(format(x, notation = "plus-minus", digits = "pdg"),
               pm("1.00", "0.04"))
})

test_that("PDG boundary 949 uses 1 sig fig", {
  # unc leading digits = 949 -> 1 sig fig
  x <- set_errors(1, 0.0949)
  expect_equal(format(x, notation = "plus-minus", digits = "pdg"),
               pm("1.00", "0.09"))
})

# ===========================================================================
# 5. PDG 950-999: promotion to next power of ten (the bug case)
# ===========================================================================

test_that("PDG 950-999 promotes to next power and shows 2 sig figs", {
  # 0.0951 has leading digits 951 -> rounds UP to 0.10 (not 0.09 or 0.1)
  # value should align: 1.23(10) not 1.2(1)
  x <- set_errors(1.234, 0.0951)
  expect_equal(format(x, notation = "parenthesis", digits = "pdg"), "1.23(10)")
  expect_equal(format(x, notation = "plus-minus",  digits = "pdg"), pm("1.23", "0.10"))
})

test_that("PDG 950 (exact boundary) promotes correctly", {
  x <- set_errors(5.678, 0.0950)
  expect_equal(format(x, notation = "parenthesis", digits = "pdg"), "5.68(10)")
  expect_equal(format(x, notation = "plus-minus",  digits = "pdg"), pm("5.68", "0.10"))
})

test_that("PDG 999 (top of range) promotes correctly", {
  x <- set_errors(5.678, 0.0999)
  expect_equal(format(x, notation = "parenthesis", digits = "pdg"), "5.68(10)")
  expect_equal(format(x, notation = "plus-minus",  digits = "pdg"), pm("5.68", "0.10"))
})

test_that("PDG 950-999 promotion works at different scales", {
  # Large scale: 9500 -> 10000; value 12345 rounds to nearest 10000 -> 10000
  x <- set_errors(12345, 9500)
  expect_equal(format(x, notation = "parenthesis", digits = "pdg"), "10000(10000)")
  # Small scale: 0.000095x -> 0.00010
  x <- set_errors(0.001234, 0.0000951)
  expect_equal(format(x, notation = "plus-minus",  digits = "pdg"), pm("0.00123", "0.00010"))
})

test_that("PDG example from original test: 0.827 +/- 0.962 promotes", {
  # 0.962 has leading digits 962 -> promotes to 1.0
  # value 0.827 rounded to same scale as 1.0 -> 1
  x <- set_errors(0.827, 0.962)
  expect_equal(format(x, notation = "plus-minus", digits = "pdg"), pm("1", "1"))
})

test_that("PDG examples from PDG spec itself", {
  # From original test retained for regression
  x <- set_errors(rep(0.827, 3), c(0.119, 0.367, 0.962))
  expect_equal(format(x, notation = "plus-minus", digits = "pdg"), c(
    pm("0.83", "0.12"),   # 119 -> 2 sig figs
    pm("0.8",  "0.4"),    # 367 -> 1 sig fig
    pm("1",    "1")       # 962 -> promoted
  ))
})

# ===========================================================================
# 6. Scientific notation
# ===========================================================================

test_that("scientific parenthesis notation is correct", {
  x <- set_errors(rep(11111.22222, 8),
                  c(12345678, 1234.5678, 12.345678, 1.2345678,
                    .12345678, .012345678, .000012345678, .000000012345678))

  expect_equal(format(x, notation = "parenthesis", scientific = TRUE),
    c("1(1000)e4", "1.1(1)e4", "1.111(1)e4", "1.1111(1)e4", "1.11112(1)e4",
      "1.111122(1)e4", "1.111122222(1)e4", "1.111122222000(1)e4"))
})

test_that("scientific plus-minus notation is correct", {
  x <- set_errors(rep(11111.22222, 8),
                  c(12345678, 1234.5678, 12.345678, 1.2345678,
                    .12345678, .012345678, .000012345678, .000000012345678))

  expect_equal(format(x, notation = "plus-minus", scientific = TRUE), c(
    pm("(1",              "1000)e4"),
    pm("(1.1",            "0.1)e4"),
    pm("(1.111",          "0.001)e4"),
    pm("(1.1111",         "0.0001)e4"),
    pm("(1.11112",        "0.00001)e4"),
    pm("(1.111122",       "0.000001)e4"),
    pm("(1.111122222",    "0.000000001)e4"),
    pm("(1.111122222000", "0.000000000001)e4")
  ))
})

test_that("scientific notation with common exponent (e12) is correct", {
  x <- set_errors(c(0e10, 1e12), 0.1e12)
  expect_equal(format(x),           c("0.0(1)e12", "1.0(1)e12"))
  expect_equal(format(x, digits=2), c("0.00(10)e12", "1.00(10)e12"))
})

# ===========================================================================
# 7. decimals flag
# ===========================================================================

test_that("decimals=TRUE shows decimal uncertainty in parenthesis mode", {
  x <- set_errors(rep(11111.22222, 8),
                  c(12345678, 1234.5678, 12.345678, 1.2345678,
                    .12345678, .012345678, .000012345678, .000000012345678))

  expect_equal(format(x, notation = "parenthesis", digits = 3, decimals = TRUE),
    c("10000(12300000)", "11110(1230)", "11111.2(12.3)", "11111.22(1.23)",
      "11111.222(123)", "11111.2222(123)", "11111.2222200(123)",
      "11111.2222200000(123)"))
})

# ===========================================================================
# 8. Edge cases: zero, NA, NaN, Inf
# ===========================================================================

test_that("zero value with nonzero uncertainty formats correctly", {
  x <- set_errors(10, 1)
  expect_equal(format(x - set_errors(10)), "0(1)")   # value cancels, unc does not
  expect_equal(format(x - x),             "0(0)")   # both cancel
})

test_that("NA, NaN, Inf format gracefully", {
  x <- set_errors(c(0.4, NA, NaN, Inf, -Inf))
  expect_equal(format(x[1]), "0.4(0)")
  expect_equal(format(x[2]), "NA(NA)")
  expect_equal(format(x[3]), "NaN(NaN)")
  expect_equal(format(x[4]), "Inf(Inf)")
  expect_equal(format(x[5]), "-Inf(Inf)")
})

test_that("zero uncertainty formats with correct trailing zeros", {
  x <- set_errors(3.14159, 0)
  # With zero uncertainty the value is shown at default precision
  expect_match(format(x), "^3\\.14.*\\(0\\)$")
})

# ===========================================================================
# 9. print() output consistency
# ===========================================================================

test_that("print() header line shows raw errors in default numeric format", {
  x <- set_errors(rep(11111.22222, 8),
                  c(12345678, 1234.5678, 12.345678, 1.2345678,
                    .12345678, .012345678, .000012345678, .000000012345678))

  out <- capture.output(print(x))
  expect_match(out[1], "^Errors:")
  expect_match(out[1], "1\\.234568e\\+07")
  expect_match(out[2], "^\\[1\\]")
  expect_match(out[2], "11111\\.22")
})

test_that("print() of each scalar matches its own format()", {
  x <- set_errors(rep(11111.22222, 8),
                  c(12345678, 1234.5678, 12.345678, 1.2345678,
                    .12345678, .012345678, .000012345678, .000000012345678))

  for (i in seq_along(x))
    expect_equal(capture.output(print(x[i])), format(x[i]))
})

# ===========================================================================
# 10. Value alignment: central value rounds to match uncertainty precision
# ===========================================================================

test_that("central value is rounded to match uncertainty decimal places", {
  # unc = 0.12 -> 2 decimal places; value should match
  x <- set_errors(0.82749, 0.119)
  fmt <- format(x, notation = "plus-minus", digits = "pdg")
  # Should be "0.83 +/- 0.12" not "0.82749 +/- 0.12"
  expect_equal(fmt, pm("0.83", "0.12"))
})

test_that("central value alignment works when unc promotes by PDG rule", {
  # unc = 0.0951 promotes to 0.10; value must align to 2 decimal places
  x <- set_errors(1.23456, 0.0951)
  fmt <- format(x, notation = "plus-minus", digits = "pdg")
  expect_equal(fmt, pm("1.23", "0.10"))
})

# ===========================================================================
# 11. Vectorised consistency: format on length-1 slice matches scalar format
# ===========================================================================

test_that("format on length-1 slice is consistent with scalar construction", {
  vals <- c(100, 1, 0.01, 1e-6)
  uncs <- c(12, 0.95, 0.00354, 1.5e-7)
  x <- set_errors(vals, uncs)
  for (i in seq_along(x)) {
    scalar <- set_errors(vals[i], uncs[i])
    expect_equal(format(x[i], digits = "pdg"),
                 format(scalar, digits = "pdg"),
                 info = paste("element", i))
  }
})
