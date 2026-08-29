# PURPOSE: Internal helpers shared across the package -- factor cleaning, colour conversion,
#   weighted variance, and the base-R string functions that replaced stringr.
# ROLE: Leaf module. Depends on nothing in the package; everything in
#   geometrical_data_analysis.R may call it.
# KEY CONSTRAINTS:
#   - The string helpers reproduce stringr's semantics, NOT base R's. See their section comment
#     and tests/testthat/test-str-shim.R before changing one.
#   - Vendored code is credited in place (stringr, tidyselect, plotwidgets).
# See: dev/dependency-audit.md for why stringr, stringi and ggforce are gone.
#' Pipe operator (deprecated)
#'
#' Re-exported from magrittr for backward compatibility. Deprecated: use the base pipe
#' \code{|>} instead. ggfacto uses \code{|>} everywhere internally, and this re-export will be
#' removed in a future release, taking the magrittr dependency with it.
#'
#' See \code{magrittr::\link[magrittr:pipe]{\%>\%}} for details.
#'
#' @name %>%
#' @rdname pipe
#' @keywords internal
#' @export
#' @importFrom magrittr %>%
#' @usage lhs \%>\% rhs
#' @param lhs A value or the magrittr placeholder.
#' @param rhs A function call using the magrittr semantics.
#' @return Pipe an object forward into a function or call expression.
NULL

# Rlang .data to bind data masking variable in dplyr
#' @keywords internal
#' @importFrom rlang .data
NULL

# Bindings for globals R CMD check cannot see. `.` is no longer a magrittr placeholder
# anywhere (the package uses |>), but purrr/rlang `~ .` lambdas still name it.
. = NULL
globalVariables(c(":="))


#' @keywords internal
.onLoad <- function(libname, pkgname) {
  options("ggfacto.export_dir" = NULL)

  #options("ggfacto.html_font" = )

  invisible()
}



#Fonctions and options to work with factors and lists -------------

#' A regex pattern to clean the names of factors.
#' @keywords internal
# @export
cleannames_condition <- function() {
   "^[^- ]+-(?![[:lower:]])|^[^- ]+(?<![[:lower:]])-| *\\(.+\\)"
}


#Use fct_relabel instead of pers functions ! -----------------------------------
#' Clean factor levels.
#'
#' @param factor A factor.
#' @param pattern A pattern.
#'
#' @return A factor.
#' @keywords internal
# @export
# @examples
fct_clean <- function(factor, pattern = cleannames_condition()) {
  forcats::fct_relabel(factor, ~ str_remove_all(.x, pattern))
}

#' Recode Factor Levels with Detected Pattern inside
#' @description Recode factor levels using \code{\link[stringr]{str_detect}}.
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




## Color conversion -----

# thanks to https://github.com/cran/plotwidgets/blob/master/R/palettes.R

#' @keywords internal
rgb2hsl <- function(rgb) {

  if(nrow(rgb) == 4) {
    alpha <- rgb[4,,drop=F]
    rgb   <- rgb[-4,,drop=F]
  } else {
    alpha <- NULL
  }

  rgb <- rgb / 255

  mins <- apply(rgb, 2, min)
  maxs <- apply(rgb, 2, max)
  d <- maxs - mins
  L <- (maxs+mins)/2

  S <- d/(1 - abs(2*L - 1))
  sel <- d == 0


  S[sel] <- 0


  wmax <- apply(rgb, 2, which.max)

  H <- L
  HR <- (rgb[2,] - rgb[3,])/(maxs - mins)
  HG <- 2 + (rgb[3,] - rgb[1,])/(maxs - mins)
  HB <- 4 + (rgb[1,] - rgb[2,])/(maxs - mins)

  sel <- wmax == 1
  H[sel] <- HR[sel]
  sel <- wmax == 2
  H[sel] <- HG[sel]
  sel <- wmax == 3
  H[sel] <- HB[sel]

  H <- (H * 60) %% 360

  H[ mins == maxs ] <- 0

  ret <- rbind(H=H, S=S, L=L, alpha=alpha)
  return(ret)
}



#' @keywords internal
hsl2rgb <- function(hsl) {

  if(nrow(hsl) == 4) {
    alpha <- hsl[4,,drop=F]
    hsl   <- hsl[-4,,drop=F]
  } else {
    alpha <- NULL
  }

  H <- hsl[1,]
  S <- hsl[2,]
  L <- hsl[3,]

  C <- (1 - abs(2*L - 1)) * S
  X <- C * (1- abs( ((H/60) %% 2) - 1))
  m <- L - C/2

  rgb <- matrix(0, ncol=ncol(hsl), nrow=3)
  rownames(rgb) <- c("R", "G", "B")

  iX <- c(2, 1, 3, 2, 1, 3)
  iC <- c(1, 2, 2, 3, 3, 1)

  for(i in 1:6) {
    sel <- 60 * (i - 1) <= H & H < 60 * i
    kX <- iX[i]
    kC <- iC[i]
    rgb[kX,sel] <- X[sel]
    rgb[kC,sel] <- C[sel]
  }

  rgb <- rgb + rep(m, each=3)

  rgb <- round(rgb * 255)
  if(!is.null(alpha))
    rgb <- rbind(rgb, alpha=alpha)

  rgb
}



#' @keywords internal
material_colors_lighter <- function(by = 0.25) {
  material_colors_lighter <- material_colors_light() |>
    grDevices::col2rgb()|>
    rgb2hsl()
  material_colors_lighter["L", ] <- pmax(pmin(material_colors_lighter["L", ] + by, 1), 0)
  material_colors_lighter <- material_colors_lighter |> hsl2rgb() |>
    as.data.frame() |>
    purrr::map_chr(~ grDevices::rgb(.[1]/255, .[2]/255, .[3]/255)) |>
    purrr::set_names(paste0(colnames(material_colors_lighter), "_", by*100, "%"))
  material_colors_lighter
}

# cat(
#   paste0("c(",
#          paste0(
#            paste0("'",
#                   c(material_colors_lighter(by = 0.25)[1],
#                     material_colors_lighter(by = 0.20)[1],
#                     material_colors_lighter(by = 0.15)[1],
#                     material_colors_lighter(by = 0.10)[1],
#                     material_colors_lighter(by = 0.05)[1]
#                   ),
#                   "'" #,
#            ),
#            collapse = ", "
#          ),
#          ")"
#   )
# )
# c('#DDD3EF', '#CFC0E8', '#C0ADE1', '#B29ADB', '#A388D4')







# #' Convert colors from and to RGB and HSL formats
# #'
# #' Convert colors from and to RGB and HSL formats
# #'
# #' These functions convert between RGB and HSL color spaces, and character
# #' vectors which contain color names or hash-encoded RGB values ("#FFCC00").
# #'
# #' All functions support an alpha channel. For example,
# #' unlike the grDevices::col2rgb, col2rgb.2 returns a matrix with four
# #' rows: three for R, G and B channels and one for the alpha channel.
# #'
# #' @param col a character vector with colors to convert (palette)
# #' @param rgb a numeric matrix with three or four rows (red, green, blue and alpha)
# #' @param hsl a numeric matrix with three or four rows (hue, saturation, luminosity and alpha)
# #' @return col2rgb.2 and col2hsl return a four-row matrix. rgb2col and hsl2col return a character
# #'         vector.
# #' @seealso \code{\link{modCol}}, \code{\link{modhueCol}}, \code{\link{darkenCol}}, \code{\link{saturateCol}}
# #' @examples
# #' haze <- plotPals("haze")
# #' col2rgb(haze)
# #' col2hsl(haze)
# #' @name colorConversions
# NULL
#
# #' @describeIn colorConversions Convert a character vector of color names
# #' (palette) to a matrix with RGB values
# #' @export
# col2rgb.2 <- function(col) {
#
#   alphas <- rep(255, length(col))
#
#   pat <- "^#([[:xdigit:]]{6})([[:xdigit:]]{2})"
#   sel <- grep(pat, col)
#
#   alphas[sel] <- strtoi( paste0("0X", gsub(pat, "\\2", col[sel]))) # / 255
#   if(all(alphas == 255)) {
#     ret <- col2rgb(col)
#   } else {
#     ret <- rbind(col2rgb(col), alpha=alphas)
#   }
#   ret
# }
#
# #' @describeIn colorConversions Convert a character vector of color names (palette) to a matrix with HSL values
# #' @export
# col2hsl <- function(col) {
#   rgb2hsl(col2rgb.2(col))
# }
#
#
#
#
# #' @describeIn colorConversions Convert hsl matrix (3 or 4 row) to character vector of color names
# #' @export
# hsl2col <- function(hsl) {
#   rgb2col(hsl2rgb(hsl))
# }
#
#
#
# #' @describeIn colorConversions Convert rgb matrix (3 or 4 row) to character vector of color names
# #' @export
# rgb2col <- function(rgb) {
#
#   rgb <- round(rgb)
#   if(nrow(rgb) == 4) {
#     #rgb[4,] <- rgb[4,] * 255
#     rgb <- apply(rgb, 2, function(x) sprintf("#%02X%02X%02X%02X", x[1], x[2], x[3], x[4]))
#   } else {
#     rgb <- apply(rgb, 2, function(x) sprintf("#%02X%02X%02X", x[1], x[2], x[3]))
#   }
#
#   rgb
# }
#
#
#
# luminosity.adj <- function(hsl, by=0) {
#
#   #hsl[3,] <- hsl[3,] * (1 + by)
#
#   if(by > 0) { # lighten
#     hsl[3,] <- 1 - (1 - by) * (1 - hsl[3,])
#   } else { # darken
#     hsl[3,] <- hsl[3,] * (1 + by)
#   }
#
#   hsl
# }
#
# saturation.adj <- function(hsl, by=0) {
#   if(by > 0) { # saturate
#     hsl[2,] <- 1 - (1 - by) * (1 - hsl[2,])
#   } else { # desaturate
#     hsl[2,] <- hsl[2,] * (1 + by)
#   }
#
#   hsl
# }
#
# hue.adj <- function(hsl, by=0) {
#   hsl[1,] <- hsl[1,] + by
#   hsl[1,] <- hsl[1,] %% 360
#   hsl
# }
#
#
# #' Modify colors
# #'
# #' Modify colors by shading, saturating and changing hue
# #'
# #' This function use the HSL (hue, saturation, luminosity) scheme to modify
# #' colors in a palette.
# #'
# #' modCol is just a wrapper for the other three functions allowing to
# #' modify three parameters in one go.
# #'
# #' saturateCol, darkenCol and modhueCol modify the saturation, luminosity
# #' and hue in the HSL color model.
# #'
# #' contrastcol() returns black for each light color (with L > 0.5) and
# #' white for each dark color (with L < 0.5).
# #'
# #' @param col a character vector of colors (palette) to modify -- a character vector
# #' @param darken Use negative values to lighten, and positive to darken.
# #' @param saturate Use negative values to desaturate, and positive to saturate
# #' @param modhue Change the hue by a number of degrees (0-360)
# #' @param by parameter for the saturateCol, darkenCol and modhueCol functions
# #' @param alpha alpha value (from 0, transparent, to 255, fully opaque)
# #' @return a character vector containing the modified palette
# #' @examples
# #' plot.new()
# #' ## Loop over a few saturation / lightess values
# #' par(usr=c(-0.5, 0.5, -0.5, 0.5))
# #' v <- c(10, 9, 19, 9, 15, 5)
# #' pal <- plotPals("zeileis")
# #' for(sat in seq.int(-0.4, 0.4, length.out=5)) {
# #'   for(lgh in seq.int(-0.4, 0.4, length.out=5)) {
# #'     cols <- saturateCol(darkenCol(pal, by=sat), by=lgh)
# #'     wgPlanets(x=sat, y=lgh, w=0.16, h=0.16, v=v, col=cols)
# #'   }
# #' }
# #' axis(1)
# #' axis(2)
# #' title(xlab="Darkness (L) by=", ylab="Saturation (S) by=")
# #'
# #' ## Now loop over hues
# #' a2xy <- function(a, r=1, full=FALSE) {
# #'   t <- pi/2 - 2 * pi * a / 360
# #'   list( x=r * cos(t), y=r * sin(t) )
# #' }
# #'
# #' plot.new()
# #' par(usr=c(-1,1,-1,1))
# #' hues <- seq(0, 360, by=30)
# #' pos <- a2xy(hues, r=0.75)
# #' for(i in 1:length(hues)) {
# #'   cols <- modhueCol(pal, by=hues[i])
# #'   wgPlanets(x=pos$x[i], y=pos$y[i], w=0.5, h=0.5, v=v, col=cols)
# #' }
# #'
# #' pos <- a2xy(hues[-1], r=0.4)
# #' text(pos$x, pos$y, hues[-1])
# #' @export
# modCol <- function(col, darken=0, saturate=0, modhue=0) {
#   modhueCol(saturateCol(darkenCol(col, by=darken), by=saturate), by=modhue)
# }
#
# #' @describeIn modCol Change the saturation of a color or palette by a fraction of "by"
# #' @export
# saturateCol <- function(col, by=0) {
#   hsl <- rgb2hsl(col2rgb.2(col))
#   hsl <- saturation.adj(hsl, by=by)
#   rgb2col(hsl2rgb(hsl))
# }
#
#
# #' @describeIn modCol Modify the darkness of a color or palette (positve \code{by} - darken, negative \code{by} -- lighten)
# #' @export
# darkenCol <- function(col, by=0) {
#   hsl <- rgb2hsl(col2rgb.2(col))
#   hsl <- luminosity.adj(hsl, by=-by)
#   rgb2col(hsl2rgb(hsl))
# }
#
#
# #' @describeIn modCol Modify the hue of a character vector of colors by \code{by} degrees
# #' @export
# modhueCol <- function(col, by=0) {
#   hsl <- rgb2hsl(col2rgb.2(col))
#   hsl <- hue.adj(hsl, by=by)
#   rgb2col(hsl2rgb(hsl))
# }
#
# #' @describeIn modCol Return white for dark colors, return black for light colors
# #' @export
# contrastcol <- function(col, alpha=NULL) {
#
#   hsl <- rgb2hsl(col2rgb.2(col))
#
#   sel <- hsl[3,] < 0.5
#   hsl[3,sel] <- 1
#   hsl[3,!sel] <- 0
#
#   if(!is.null(alpha))
#     hsl <- rbind(hsl[1:3,], round(alpha * 255))
#   #hsl[4,] <- round(alpha * 255)
#
#   rgb2col(hsl2rgb(hsl))
# }

