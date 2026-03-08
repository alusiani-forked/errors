#' Encode \code{errors}
#'
#' Format an \code{errors} object for pretty printing.
#'
#' @param x an \code{errors} object.
#' @param digits how many significant digits are to be used for uncertainties.
#' The default, \code{NULL}, uses \code{getOption("errors.digits", 1)}.
#' Use \code{digits="pdg"} to choose an appropriate number of digits for each
#' value according to the Particle Data Group rounding rule (see references).
#' @param extra non-negative integer; extra digits to display beyond the PDG
#' minimum when \code{digits="pdg"}. Default \code{0}. Presently ignored.
#' @param scientific logical specifying whether the elements should be
#' encoded in scientific format.
#' @param notation error notation; \code{"parenthesis"} and \code{"plus-minus"}
#' are supported through the \code{"errors.notation"} option.
#' @param decimals logical specifying whether the uncertainty should be formatted
#' with a decimal point even when the \code{"parenthesis"} notation is used.
#' Otherwise (by default), the \code{"parenthesis"} notation scales the
#' uncertainty to match the least significant digit of the value.
#' @param ... ignored.
#'
#' @references
#' K. Nakamura et al. (Particle Data Group), J. Phys. G 37, 075021 (2010)
#'
#' @examples
#' x <- set_errors(1:3*100, 1:3*100 * 0.05)
#' format(x)
#' format(x, digits=2)
#' format(x, digits=2, decimals=TRUE)
#' format(x, scientific=TRUE)
#' format(x, notation="plus-minus")
#'
#' x <- set_errors(c(0.827, 0.827), c(0.119, 0.367))
#' format(x, notation="plus-minus", digits="pdg")
#'
#' @export
format.errors = function(x,
                         digits = NULL,
                         scientific = FALSE,
                         notation = getOption("errors.notation", "parenthesis"),
                         decimals = getOption("errors.decimals", FALSE),
                         extra = 0L,
                         ...)
{
  stopifnot(notation %in% c("parenthesis", "plus-minus"))

  if (is.null(digits)) digits <- getOption("errors.digits", 1)
  pdg <- identical(digits, "pdg")
  digits <- if (pdg) digits_pdg(.e(x), extra = extra) else rep(digits, length(x))

  scipen <- getOption("scipen", 0)
  prepend <- rep("", length(x))
  append <- rep("", length(x))

  # For PDG 950-999: the three leading digits round up to 1000, promoting the
  # uncertainty by one power of ten. signif() rounds to nearest and gives the
  # wrong result (e.g. signif(0.0951, 2) = 0.095, not 0.10). Detect these
  # cases and replace with an exact ceiling to the next power of ten.
  raw_e <- .e(x)
  if (pdg) {
    eraw     <- get_exponent(raw_e)
    hod      <- round(raw_e / 10^(eraw - 2))    # three leading digits
    promoted <- is.finite(raw_e) & raw_e > 0 & hod >= 950L
    e        <- ifelse(promoted, 10^(eraw + 1L), signif(raw_e, digits))
  } else {
    e <- signif(raw_e, digits)
  }
  nulle <- e == 0 & !is.na(e)
  eexp <- get_exponent(e)
  xexp <- ifelse(.v(x) == 0, eexp + 1, get_exponent(x))
  value_digits <- ifelse(e, digits - eexp, digits)
  value <- ifelse(e, signif(.v(x), xexp + value_digits), .v(x))
  value <- ifelse(is.finite(value), value, .v(x))
  # For promoted elements with eexp >= 0 (uncertainty rounds to >= 1), the
  # formula value_digits-1 gives one too many decimal places (e.g. 0.827±0.962
  # -> e=1, value_digits=2, formatC would give "0.8" or "1.0" not "1").
  # Round value to 0 decimal places and set value_digits=1 for these cases.
  if (pdg && any(promoted)) {
    fix <- promoted & eexp >= 0L
    if (any(fix)) {
      value[fix]        <- round(value[fix], 0L)
      value_digits[fix] <- 1L
    }
  }

  cond <- (scientific | (xexp > 4+scipen | xexp < -3-scipen)) & is.finite(e)
  e[cond] <- e[cond] * 10^(-xexp[cond])
  value[cond] <- value[cond] * 10^(-xexp[cond])
  value_digits[cond] <- digits[cond] - get_exponent(e)[cond]
  value_digits[!is.finite(value_digits)] <- 0
  value_digits[nulle] <- getOption("digits", 7)

  if (notation == "parenthesis") {
    sep <- "("
    append[] <- ")"
    e_scale_flag <- if (!isTRUE(decimals)) is.finite(e) else
      (cond & eexp < xexp) | (!cond & is.finite(e) & eexp < 0)
    e[e_scale_flag] <- (e * 10^(pmax(0, value_digits-1)))[e_scale_flag]
  } else {
    sep <- paste0(" ", .pm, " ")
    prepend[cond] <- "("
    append[cond] <- ")"
  }
  append[cond] <- paste(append[cond], "e", xexp[cond], sep="")

  value <- sapply(seq_along(value), function(i) {
    formatC(value[[i]], format="f",
            digits=max(0, value_digits[[i]]-1),
            decimal.mark=getOption("OutDec"))
  })
  value[nulle] <- prettyNum(value[nulle], drop0trailing=TRUE)

  e <- sapply(seq_along(digits), function(i) {
    # Promoted PDG uncertainties with eexp >= 0 are exact powers of ten (1, 10,
    # 100, ...). Format them as integers to avoid spurious ".0" from "%#.2g".
    # This is distinct from a genuine 1.0 input which is not PDG-promoted.
    if (pdg && promoted[i] && eexp[i] >= 0L)
      return(formatC(round(e[[i]]), format="d",
                     decimal.mark=getOption("OutDec")))
    formatC(e[[i]], format="fg", flag="#",
            digits=digits[[i]], width=max(1, digits[[i]]),
            decimal.mark=getOption("OutDec"))
  })
  e <- sub("\\.$", "", e)

  paste(prepend, value, sep, e, append, sep="")
}

#' Print Values
#'
#' S3 method for \code{errors} objects.
#'
#' @param x an \code{errors} object.
#' @inheritParams base::print
#'
#' @examples
#' x <- set_errors(1:10, 1:10 * 0.05)
#' print(x)
#' print(x[1:3])
#' print(x[1])
#' print(x[1], digits=2)
#' print(x[1], notation="plus-minus")
#'
#' @export
print.errors <- function(x, ...) {
  if (is.array(x) || length(x) > 1L) {
    err <- .e(x)
    e <- paste(format(err[1:min(5, length(err))]), collapse=" ")
    if (length(err) > 5L)
      e <- paste(e, "...")
    cat("Errors: ", e, "\n", sep = "")
    x_next <- drop_errors(x)
    print(x_next, ...)
  } else {
    cat(format(x, ...), "\n", sep="")
  }
  invisible(x)
}
