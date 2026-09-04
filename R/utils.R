# PURPOSE: Internal helpers shared across the package -- factor cleaning, weighted variance, and
#   the base-R string functions that replaced stringr.
# ROLE: Leaf module. Depends on nothing in the package; every other file in R/ may call it.
# KEY CONSTRAINTS:
#   - The string helpers reproduce stringr's semantics, NOT base R's. See their section comment
#     and tests/testthat/test-str-shim.R before changing one.
#   - Vendored code is credited in place (stringr, tidyselect).
# See: dev/dependency-audit.md for why stringr, stringi and ggforce are gone.



#Fonctions and options to work with factors and lists -------------

#' A regex pattern to clean the names of factors.
#' @keywords internal
# @export
cleannames_condition <- function() {
   "^[^- ]+-(?![[:lower:]])|^[^- ]+(?<![[:lower:]])-| *\\(.+\\)"
}



#' Recode Factor Levels with Detected Pattern inside
#' @description Recode factor levels whose name matches a regex.
#' @param factor A factor.
#' @param pattern A character vector of length 1.
#' @param replacement A character vector of length 1.
#' @param negate A factor.
#'
#' @return A factor.
#' @keywords internal
# @export
#'
# @examples
fct_detect_replace <- function(factor, pattern, replacement, negate = FALSE){
  if (is.data.frame(factor)) {stop("must be a vector, not a data.frame")}
  if (!is.factor(factor)) { factor <- as.factor(factor) }
  lvs <- levels(factor)
  levels <- if (negate == FALSE) {
    purrr::set_names(lvs, purrr::map(lvs, ~ dplyr::if_else(str_detect(.x, pattern), replacement, .x)))
  } else {
    purrr::set_names(lvs, purrr::map(lvs, ~ dplyr::if_else(!str_detect(.x, pattern), replacement, .x)))
  }
  return(forcats::fct_recode(factor, !!!levels))
}



#' @keywords internal
levels_to_na <- function(data, vars, excl, levels_to = "NULL") {
  if (length(excl) == 0) return(data)

  vars <- names(tidyselect::eval_select(rlang::enquo(vars), data))
  excl <- paste0(excl, collapse = "|")

  levels_to_excl <- data |>
    dplyr::select(tidyselect::all_of(vars) & where(~ any(str_detect(levels(.), excl)))) |>
    purrr::imap(~ levels(.x)[str_detect(levels(.x), excl)] ) |>
    purrr::flatten_chr()

  # print(levels_to_excl)

  data <- data |>
    dplyr::mutate(dplyr::across(
      tidyselect::all_of(vars) & where(~ any(levels(.) %in% levels_to_excl)),
      ~ suppressWarnings(forcats::fct_recode(., !!!purrr::set_names(levels_to_excl, levels_to))
      )
    ))

  data
}






# === SECTION: String helpers ==================================================

# Base-R stand-ins for the twelve stringr functions this package used, so stringr
# (and through it stringi) is not a dependency. Semantics are stringr's, not base R's:
# each one is parity-tested against the documented stringr behaviour in
# tests/testthat/test-str-shim.R. Do not "simplify" these to bare paste0()/sub()/formatC()
# -- every deviation below is load-bearing and was measured.
#
# Reimplemented from the stringr API (MIT licence: https://stringr.tidyverse.org/LICENSE.html).
# Thanks to Hadley Wickham and the stringr authors.

# DESIGN: str_c() propagates NA where paste0() would render the string "NA", and returns
# character(0) for a zero-length input where paste0() returns a length-1 string. Several
# callers use that NA as a guard (a name later dropped by !is.na(), a replace_na() to "").
#' @keywords internal
str_c <- function(..., sep = "", collapse = NULL) {
  args <- Filter(Negate(is.null), list(...))
  if (length(args) == 0L) return(character(0))
  if (any(lengths(args) == 0L)) {
    return(if (is.null(collapse)) character(0) else paste(character(0), collapse = collapse))
  }
  out <- do.call(paste, c(args, list(sep = sep, collapse = collapse)))
  n   <- max(lengths(args))
  na  <- Reduce(`|`, lapply(args, function(a) rep_len(is.na(a), n)))
  if (is.null(collapse)) { out[na] <- NA_character_ } else if (any(na)) { out <- NA_character_ }
  out
}

# DESIGN: base sub()/gsub()/grepl() take only pattern[1] (with a warning); stringr recycles
# pattern element-wise against string. Used where a per-row prefix is stripped.
str_vapply <- function(fn, string, pattern, replacement = NULL, out_chr = TRUE) {
  n <- max(length(string), length(pattern))
  string <- rep_len(string, n); pattern <- rep_len(pattern, n)
  if (is.null(replacement)) {
    out <- mapply(function(s, p) fn(p, s), string, pattern, USE.NAMES = FALSE)
  } else {
    replacement <- rep_len(replacement, n)
    out <- mapply(function(s, p, r) fn(p, r, s), string, pattern, replacement, USE.NAMES = FALSE)
  }
  if (out_chr) as.character(out) else as.logical(out)
}

# WARNING: perl = TRUE is mandatory throughout, not a preference. cleannames_condition()
# uses lookahead and lookbehind, which base TRE cannot compile at all -- it errors.
#' @keywords internal
str_detect <- function(string, pattern, negate = FALSE) {
  out <- if (length(pattern) > 1L) {
    str_vapply(function(p, s) grepl(p, s, perl = TRUE), string, pattern, out_chr = FALSE)
  } else {
    grepl(pattern, string, perl = TRUE)
  }
  out[is.na(string)] <- NA
  if (negate) !out else out
}

#' @keywords internal
str_replace <- function(string, pattern, replacement) {
  if (length(pattern) > 1L || length(replacement) > 1L) {
    return(str_vapply(function(p, r, s) sub(p, r, s, perl = TRUE), string, pattern, replacement))
  }
  sub(pattern, replacement, string, perl = TRUE)
}

#' @keywords internal
str_replace_all <- function(string, pattern, replacement) {
  if (length(pattern) > 1L || length(replacement) > 1L) {
    return(str_vapply(function(p, r, s) gsub(p, r, s, perl = TRUE), string, pattern, replacement))
  }
  gsub(pattern, replacement, string, perl = TRUE)
}

#' @keywords internal
str_remove <- function(string, pattern) str_replace(string, pattern, "")

#' @keywords internal
str_remove_all <- function(string, pattern) str_replace_all(string, pattern, "")

# DESIGN: regmatches() drops non-matches, returning a vector SHORTER than its input, which
# silently corrupts a mutate() and can error a case_when(). Fill NA and place only the hits.
#' @keywords internal
str_extract <- function(string, pattern) {
  m   <- regexpr(pattern, string, perl = TRUE)
  out <- rep(NA_character_, length(string))
  hit <- !is.na(m) & m != -1L
  out[hit] <- regmatches(string, m)
  out[is.na(string)] <- NA_character_
  out
}

# DESIGN: padding aligns tooltip numbers under a monospace font, so it must be exact.
# formatC() cannot do either thing needed here: it errors on a vector `width` (used with a
# per-row max_length column) and cannot pad with anything but " " or "0" (callers pad "@").
# strrep() is vectorised over both. Like str_pad(), this never truncates.
#' @keywords internal
str_pad <- function(string, width, side = c("left", "right", "both"), pad = " ") {
  side   <- match.arg(side)
  string <- as.character(string)
  n      <- max(length(string), length(width))
  string <- rep_len(string, n); width <- rep_len(width, n)
  short  <- pmax(0L, width - nchar(string, type = "chars"))
  short[is.na(string)] <- 0L
  out <- switch(
    side,
    left  = paste0(strrep(pad, short), string),
    right = paste0(string, strrep(pad, short)),
    both  = paste0(strrep(pad, floor(short / 2)), string, strrep(pad, ceiling(short / 2)))
  )
  out[is.na(string)] <- NA_character_
  out
}

#' @keywords internal
str_length <- function(string) nchar(string, type = "chars")

# DESIGN: substr() cannot take negative indices, and ifelse() returns a result as long as its
# TEST, so a scalar start against a vector string would silently collapse to length 1.
# Recycle everything to a common length first.
#' @keywords internal
str_sub <- function(string, start = 1L, end = -1L) {
  n  <- nchar(string, type = "chars")
  ln <- max(length(string), length(start), length(end))
  n <- rep_len(n, ln); start <- rep_len(start, ln); end <- rep_len(end, ln)
  string <- rep_len(string, ln)
  s <- ifelse(start < 0L, pmax(1L, n + start + 1L), pmax(1L, start))
  e <- ifelse(end   < 0L, n + end + 1L,             pmin(n, end))
  out <- substr(string, s, e)
  out[is.na(string)] <- NA_character_
  out
}

#' @keywords internal
str_squish <- function(string) gsub("\\s+", " ", trimws(string), perl = TRUE)

# NOTE: stringr took locale = "en"; toupper() follows the session locale instead.
#' @keywords internal
str_to_upper <- function(string, locale = "en") toupper(string)


#tidyselect:::where
# MIT + Lience : https://tidyselect.r-lib.org/LICENSE.html
# Thanks to Hadley Wickham and Lionel Henry
#' @keywords internal
where <- function (fn)
{
  predicate <- rlang::as_function(fn)
  function(x, ...) {
    out <- predicate(x, ...)
    if (!rlang::is_bool(out)) {
      rlang::abort("`where()` must be used with functions that return `TRUE` or `FALSE`.")
    }
    out
  }
}


#' @keywords internal
unbrk <- "\u202f" # narrow no-break space




#' @keywords internal
weighted.var <- function(x, wt, na.rm = FALSE) {
  #Nwt_non_zero <- length((wt)[wt != 0])
  round(
    sum(wt * (x - stats::weighted.mean(x, wt, na.rm = na.rm))^2,  na.rm = na.rm) /
      ( sum(wt, na.rm = na.rm) ),
    10)
  #((Nwt_non_zero - 1) / Nwt_non_zero) *
} #Same results as sqrt(Hmisc::wtd.var(!!num_var, !!wt, na.rm = TRUE, method = "ML")


# Soft-deprecated argument names ------------------------------------

# One warning per session per (function, argument), the way lifecycle::deprecate_soft() behaves,
# without taking lifecycle as a dependency.
deprecated_args_warned <- new.env(parent = emptyenv())

# Route a renamed argument to its new name. Callers pass the OLD argument's value and get it back,
# having warned once; the caller does the missing() test, since missing() only works in its own frame.
#' @keywords internal
# The FUNCTION twin of renamed_arg(): one warning per session, naming what to use instead.
#' @keywords internal
#' @noRd
deprecated_fn <- function(old, new) {
  key <- str_c("fn::", old)
  if (!isTRUE(deprecated_args_warned[[key]])) {
    assign(key, TRUE, envir = deprecated_args_warned)
    warning(old, "() is deprecated; use ", new, "() instead.", call. = FALSE)
  }
  invisible(NULL)
}

renamed_arg <- function(value, old, new, fn) {
  key <- str_c(fn, "::", old)
  if (!isTRUE(deprecated_args_warned[[key]])) {
    assign(key, TRUE, envir = deprecated_args_warned)
    warning("The `", old, "` argument of ", fn, "() is deprecated; use `", new, "` instead.",
            call. = FALSE)
  }
  value
}
