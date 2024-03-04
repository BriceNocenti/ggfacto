
#' Pipe operator
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

# binding for global variables not found by R cmd check
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
  forcats::fct_relabel(factor, ~ stringr::str_remove_all(.x, pattern))
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
  if (!is.factor(factor)) { factor <- factor %>% as.factor() }
  if (negate == FALSE) {
    levels <- factor %>% levels() %>%
      magrittr::set_names(purrr::map(., ~ dplyr::if_else(stringr::str_detect(., pattern), replacement, .) ))
  } else {
    levels <- factor %>% levels() %>%
      magrittr::set_names(purrr::map(., ~ dplyr::if_else(!stringr::str_detect(., pattern), replacement, .) ))
  }
  return(forcats::fct_recode(factor, !!!levels))
}



#' @keywords internal
tab_transpose <- function(tabs, name = "variables") {
  row_var <- tabxplor::tab_get_vars(tabs, "row_var")$row_var
  totrow_names <- dplyr::filter(tabs, tabxplor::is_totrow(tabs)) |>
    dplyr::pull(1) |> as.character()
  if (length(totrow_names) >= 2) stop("not working for now with many total rows")
  totcol_name <- tabxplor::is_totcol(tabs) ; totcol_name <- names(totcol_name[totcol_name])
  if (length(totcol_name) >= 2) stop("not working for now with many total columns")

  tabs |>
    tidyr::pivot_longer(cols = -1, names_to = name, values_to = "value") |>
    tidyr::pivot_wider(names_from = tidyselect::all_of(row_var),
                       values_from = "value",
                       names_sort = TRUE) |>
    dplyr::mutate(dplyr::across(where(is.character), forcats::as_factor)) |>
    dplyr::mutate(dplyr::across(where(tabxplor::is_fmt), ~ tabxplor::set_type(., "col"))) |>
    dplyr::mutate(dplyr::across(where(tabxplor::is_fmt), ~ tabxplor::as_totcol(., FALSE))) |>
    dplyr::mutate(dplyr::across(
      tidyselect::any_of(totrow_names),
      ~ tabxplor::as_totrow(tabxplor::as_totcol(.), FALSE)
    )) |>
    dplyr::mutate(dplyr::across(where(tabxplor::is_fmt), ~ dplyr::if_else(
      !!rlang::sym(name) == totcol_name,
      true  = tabxplor::as_totrow(.),
      false = tabxplor::as_totrow(., FALSE)))) |>
    tabxplor::new_tab()
}


#' @keywords internal
levels_to_na <- function(data, vars, excl, levels_to = "NULL") {
  if (length(excl) == 0) return(data)

  vars <- names(tidyselect::eval_select(rlang::enquo(vars), data))
  excl <- paste0(excl, collapse = "|")

  levels_to_excl <- data |>
    dplyr::select(tidyselect::all_of(vars) & where(~ any(stringr::str_detect(levels(.), excl)))) |>
    purrr::imap(~ levels(.x)[stringr::str_detect(levels(.x), excl)] ) |>
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
unbrk <- stringi::stri_unescape_unicode("\\u202f") # unbreakable space





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

