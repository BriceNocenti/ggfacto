# PURPOSE: Lock the base-R string helpers in R/utils.R to stringr's semantics.
# ROLE: These twelve functions replaced stringr (and through it stringi) as a dependency. Roughly
#   twenty of the 188 converted call sites relied on behaviour where base R differs from stringr,
#   so the helpers deliberately do NOT delegate straight to paste0()/sub()/formatC().
# KEY CONSTRAINTS:
#   - Expectations are written as literals, never by calling stringr: the package no longer
#     depends on it, and a test that needs the dependency back would defeat the point.
#   - Every case below is one that a naive base-R rewrite gets wrong. Do not prune them as
#     redundant; each documents a measured divergence.
# See: dev/dependency-audit.md, section Tier 3.

cleannames <- "^[^- ]+-(?![[:lower:]])|^[^- ]+(?<![[:lower:]])-| *\\(.+\\)"

# --- str_c: NA propagation and zero length ----------------------------------------------------

test_that("str_c propagates NA instead of rendering the string 'NA'", {
  # paste0("names_", NA) is "names_NA"; callers filter on is.na() and would keep the junk.
  expect_identical(str_c("names_", NA), NA_character_)
  expect_identical(str_c("<b>", c("a", NA), "</b>"), c("<b>a</b>", NA))
  expect_identical(str_c("Axe ", 1:2, " (", c(1.5, NA), "%)"),
                   c("Axe 1 (1.5%)", NA))
})

test_that("str_c returns character(0) for a zero-length input", {
  # paste0(character(0), "x") returns "x", a length-1 vector, which silently lengthens a column.
  expect_identical(str_c(character(0), "x"), character(0))
  expect_length(str_c(character(0)), 0L)
})

test_that("str_c drops NULL arguments but keeps literal ones", {
  expect_identical(str_c("a", NULL, "b"), "ab")
  expect_identical(str_c("a", "b"), "ab")
})

test_that("str_c recycles and honours sep and collapse", {
  expect_identical(str_c(c("a", "b"), "_", 1), c("a_1", "b_1"))
  expect_identical(str_c(c("a", "b"), collapse = ""), "ab")
  expect_identical(str_c("a", "b", sep = "-"), "a-b")
  expect_identical(str_c(c("a", NA), collapse = "-"), NA_character_)
})

# --- perl = TRUE is mandatory -----------------------------------------------------------------

test_that("the cleannames pattern compiles, which needs perl = TRUE", {
  # gsub(cleannames, "", x) without perl = TRUE raises "Invalid regexp": TRE cannot parse the
  # lookahead and lookbehind at all. This is not a preference, it is a hard requirement.
  expect_error(suppressWarnings(gsub(cleannames, "", "ABC-def")), "Invalid regexp")
  expect_identical(str_remove_all("ABC-def (note)", cleannames), "def")
  expect_identical(str_detect(c("ABC-def", "zz"), cleannames), c(TRUE, FALSE))
})

test_that("pattern functions return NA for NA input", {
  expect_identical(str_detect(c("a", NA), "a"), c(TRUE, NA))
})

# --- vectorised patterns ----------------------------------------------------------------------

test_that("a pattern vector is recycled element-wise", {
  # base sub() warns and uses pattern[1] only, which strips the wrong prefix from every later row.
  expect_identical(str_remove(c("tea_black", "coffee_milk"), c("^tea_", "^coffee_")),
                   c("black", "milk"))
  expect_identical(str_detect(c("tea_b", "coffee_m"), c("^tea", "^zz")), c(TRUE, FALSE))
})

# --- str_extract keeps the input length -------------------------------------------------------

test_that("str_extract returns NA for a non-match, never a shorter vector", {
  # regmatches(x, regexpr(p, x)) drops non-matches: length 1 out of 2 here, which corrupts a
  # mutate() and can error a case_when().
  expect_identical(str_extract(c("AB-cd", "zz"), "^[A-Z]+-"), c("AB-", NA))
  expect_length(str_extract(c("a", "b", "c"), "zzz"), 3L)
  expect_identical(str_extract(c("a.b", "nodot"), "\\.[^\\.]+$"), c(".b", NA))
})

# --- str_sub ----------------------------------------------------------------------------------

test_that("str_sub accepts negative indices, which substr() cannot", {
  # substr("hello", -1, -1) is "".
  expect_identical(str_sub(c("hello", "a"), -1, -1), c("o", "a"))
  expect_identical(str_sub("hello", 1, 3), "hel")
  expect_identical(str_sub(c("hello", NA), -1, -1), c("o", NA))
})

test_that("str_sub recycles a scalar index against a vector string", {
  # ifelse() returns a result as long as its TEST, so a scalar start would collapse the output.
  expect_length(str_sub(c("hello", "world", "x"), -1, -1), 3L)
  expect_identical(str_sub(c("hello", "world"), 1, c(2, 4)), c("he", "worl"))
})

# --- the small ones ---------------------------------------------------------------------------

test_that("str_length, str_squish and str_to_upper behave", {
  expect_identical(str_length(c("abc", NA)), c(3L, NA_integer_))
  expect_identical(str_squish("  a   b "), "a b")
  expect_identical(str_to_upper("abc"), "ABC")
})

test_that("str_replace changes the first match and str_replace_all every one", {
  expect_identical(str_replace("a-b-c", "-", "+"), "a+b-c")
  expect_identical(str_replace_all("a-b-c", "-", "+"), "a+b+c")
  expect_identical(str_replace("Dim.1_a", "(^[^\\.]+\\.)", "X\\1"), "XDim.1_a")
})

test_that("the no-break space round-trips through a replacement", {
  # The old code wrote a literal "\\u202f" and unescaped it afterwards with stringi; the
  # replacement now carries the character itself.
  expect_identical(str_replace("a</font>", "</font>$", paste0("</font>", "\u202f")),
                   paste0("a</font>", "\u202f"))
})

# --- padding: it aligns tooltip numbers under a monospace font --------------------------------

test_that("str_pad pads on the requested side", {
  expect_identical(str_pad(c("1", "22"), 5), c("    1", "   22"))
  expect_identical(str_pad(c("1", "22"), 5, side = "left"), c("    1", "   22"))
  expect_identical(str_pad(c("1", "22"), 5, side = "right"), c("1    ", "22   "))
  expect_identical(str_pad("1", 5, side = "both"), "  1  ")
})

test_that("str_pad takes a width VECTOR", {
  # formatC() errors outright on a vector width; a per-row max_length column needs this.
  expect_identical(str_pad(c("a", "bb"), c(3, 5)), c("  a", "   bb"))
})

test_that("str_pad takes a pad character other than space", {
  # formatC() can only pad with " " or "0".
  expect_identical(str_pad(c("7", "123"), 3, pad = "@"), c("@@7", "123"))
  expect_identical(str_pad("-2", 4, pad = "@"), "@@-2")
})

test_that("str_pad never truncates and coerces numbers", {
  expect_identical(str_pad("toolong", 3), "toolong")
  expect_identical(str_pad(round(c(7.4, 12.6), 0), 2), c(" 7", "13"))
})

test_that("str_pad leaves NA as NA", {
  expect_identical(str_pad(c("a", NA), 4), c("   a", NA))
})
