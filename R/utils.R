#Fonctions et options pour travailler sur des factors ou des listes-------------


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
NULL

#' Assignment pipe operator
#'
#' See \code{magrittr::\link[magrittr:pipe]{\%<>\%}} for details.
#'
#' @name %<>%
#' @rdname pipe
#' @keywords internal
#' @export
#' @importFrom magrittr %<>%
#' @usage lhs \%<>\% rhs
NULL



#Use fct_c and fct_unify !! ----------------------------------------------------

#' A regex pattern to clean the names of factors.
#' @export
cleannames_condition <- function() "^[^- ]+-(?![[:lower:]])|^[^- ]+(?<![[:lower:]])-| *\\(.+\\)"

#Use fct_relabel instead of pers functions ! -----------------------------------
#' Clean factor levels.
#'
#' @param factor A factor.
#' @param pattern A pattern.
#'
#' @return A factor.
#' @export
#'
# @examples
fct_clean <- function(factor, pattern = cleannames_condition()) {
  forcats::fct_relabel(factor, ~ stringr::str_remove_all(.x, pattern))
}


# fct_clean <- function(factor, pattern = cleannames_condition()){
#   if(is.data.frame(factor)) {stop("must be a vector, not a data.frame")}
#   if (!is.factor(factor)) { factor %<>% as.factor() }
#   levels <- factor %>%  levels() %>%
#     magrittr::set_names(purrr::map(., ~stringr::str_remove_all(.,pattern)))
#   return(forcats::fct_recode(factor, !!!levels))
# }
# glm.data %>% dplyr::mutate_if(is.factor, ~ fct_clean(.))
# glm.data %>% dplyr::mutate_at(c(1:6,8), ~ fct_clean(., cleannames_condition()))


#' Replace Factor Levels with NA
#'
#' @param factor A factor.
#' @param patternlist A character vector of levels.
#'
#' @return A factor.
#' @export
#'
#' @examples
#' forcats::gss_cat %>%
#' dplyr::pull(race) %>%
#'   fct_to_na("Other")
fct_to_na <- function(factor, patternlist){
  if (!is.factor(factor)) { factor %<>% as.factor() }
  patternlist %<>% magrittr::set_names(rep("NULL", length(.)))
  forcats::fct_recode(factor, !!!patternlist)
}


#' Recode Factor Levels using one Pattern
#' @description Recode factor levels using \code{\link[stringr]{str_replace_all}}.
#' @param factor A factor.
#' @param pattern A character of length 1.
#' @param replacement A character of length 1.
#'
#' @return A factor
#' @export
#'
# @examples
fct_replace <- function(factor, pattern, replacement){
  if (is.data.frame(factor)) {stop("must be a vector, not a data.frame")}
  if (!is.factor(factor)) { factor %<>% as.factor() }
  levels <- factor %>% levels() %>%
    magrittr::set_names(purrr::map(., ~ stringr::str_replace_all(., pattern, replacement)))
  return(forcats::fct_recode(factor, !!!levels))
}



#' Recode Factor Levels using Multiple Patterns
#'
#' @param factor A factor.
#' @param pattern_replacement_named_vector A named character vector, with
#' regular expressions to find in values, replacements in names.
#'
#' @return A factor.
#' @export
#'
# @examples
fct_rename <- function (factor, pattern_replacement_named_vector){
  if(is.data.frame(factor)) {stop("must be a vector, not a data.frame")}
  if (!is.factor(factor)) { factor %<>% as.factor() }
  if (!is.null(pattern_replacement_named_vector)) {
    factor <- purrr::reduce2(pattern_replacement_named_vector,
                             names(pattern_replacement_named_vector),
                             .init = factor, .f = ~ fct_replace(..1, ..2, ..3))
  }
  return(factor)
}


#' Recode Factor Levels with Detected Pattern inside
#' @description Recode factor levels using \code{\link[stringr]{str_detect}}.
#' @param factor A factor.
#' @param pattern A character vector of length 1.
#' @param replacement A character vector of length 1.
#' @param negate A factor.
#'
#' @return A factor.
#' @export
#'
# @examples
fct_detect_replace <- function(factor, pattern, replacement, negate = FALSE){
  if (is.data.frame(factor)) {stop("must be a vector, not a data.frame")}
  if (!is.factor(factor)) { factor %<>% as.factor() }
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
fct_detect_rename <- function (factor, pattern_replacement_named_vector){
  if(is.data.frame(factor)) {stop("must be a vector, not a data.frame")}
  if (!is.null(pattern_replacement_named_vector)) {
    if (!is.factor(factor)) { factor %<>% as.factor() }
    levels <- factor %>% levels() %>% magrittr::set_names(., .)
    new_levels_list <- purrr::map(levels, function(.lv) purrr::imap(pattern_replacement_named_vector,
                                                                    ~ dplyr::if_else(stringr::str_detect(.lv, .x), .y, .lv) ) %>% purrr::flatten_chr()  )
    new_levels <- purrr::map2(levels, new_levels_list, ~ .y[which(!.y %in% .x)] )
    new_levels %<>% purrr::imap(~ ifelse(length(.) == 0, .y, .x))
    if ( any(purrr::map_lgl(new_levels, ~ length(.) >= 2 )) ) {
      warning_levels <- new_levels[which(purrr::map_lgl(new_levels, ~ length(.) >= 2 ))]
      warning(stringr::str_c(c(" two search patterns or more applies to the same level (only the first was kept) : ",
                               rep("", length(warning_levels) - 1)), warning_levels))
      new_levels %>% purrr::map(~ .[1])
    }
    levels %<>% magrittr::set_names(new_levels)
    factor %<>% forcats::fct_recode(!!!levels) %>% forcats::fct_relevel(sort)

  }
  return(factor)
}



#' Recode Factor Levels with Multiple Patterns Detection
#'
#' @param factor A factor.
#' @param pattern_replacement_named_vector A named character vector, with
#' regular expressions to find in values, replacements in names.
#' @param .else A character vector of length 1 to rename factor levels detected
#' with no pattern.
#'
#' @return A factor.
#' @export
#'
# @examples
fct_case_when_recode <- function (factor, pattern_replacement_named_vector,
                                  .else = levels(factor) ){
  if(is.data.frame(factor)) {stop("must be a vector, not a data.frame")}
  if (!is.factor(factor)) { factor %<>% as.factor() }
  if (!is.null(pattern_replacement_named_vector)) {
    cases_list <-
      purrr::imap(pattern_replacement_named_vector,
                  ~ list(!! levels(factor) %>% stringr::str_detect(.x) ~ .y)
      ) %>% purrr::flatten() %>% append(!! TRUE ~ .else)

    factor %<>% `levels<-`(dplyr::case_when(!!! cases_list)) %>%
      forcats::fct_recode(NULL = "NULL") %>% forcats::fct_relevel(sort)
  }
  return(factor)
}



# #To find code of invisible functions in packages :
# getAnywhere("probe")
# getAnywhere("probe")[2]
# # To use them :
# #purrr:::probe


# .x <- emploi_data_list %>% map(~ dplyr::select(., any_of(c("ANNEE", "STATUT", "TIT", "TITC"))) )
# .l <- list(.x, fct_dplyr::case_when_vars, fct_dplyr::case_when_recode_named_vector_map)
# .p <- emploi_if_82_18
# .f <-  ~ dplyr::mutate(..1, CONTR = fct_cross(!!..2[[1]], fct_explicit_na(!!..2[[2]], "NA-NA")) %>%
#                   fct_replace("^([^-]+)-([^:]+):([^-]+)-(.+)", "\\1:\\3-\\2/\\4") %>%
#                   fct_dplyr::case_when_recode(..3) )  #~ tabw(., PE0, perc = "col")
# .else = NULL

# Adapt map_if function to pmap et map2 ----------------
# (when FALSE the result is the first element of .l, or the content of .else)

#' A generalised map_if
#'
#' @param .l List of lists.
#' @param .p Predicate.
#' @param .f Function if TRUE.
#' @param .else Function if FALSE.
#' @param ... Other parameter to pass to the function.
#'
#' @return A list of same length.
#' @export
#'
# @examples
pmap_if <- function(.l, .p, .f, ..., .else = NULL) {
  .x <- .l[[1]]
  # .f <- as_mapper(.f, ...)
  sel <- probe(.x, .p)
  #   if (rlang::is_logical(.p)) { # fonction probe
  #   stopifnot(length(.p) == length(.x))
  #   .p
  # } else {
  #   .p <- as_predicate(.p, ..., .mapper = TRUE)
  #   map_lgl(.x, .p, ...)
  # }

  out <- purrr::list_along(.x)
  out[sel] <- purrr::pmap(purrr::map(.l, ~ .[sel]), .f, ...) # .Call(pmap_impl, environment(), ".l", ".f", "list")
  if (rlang::is_null(.else)) {
    out[!sel] <- .x[!sel]
  }
  else {
    out[!sel] <- purrr::pmap(purrr::map(.l, ~ .[sel]), .else, ...)
  }
  magrittr::set_names(out, names(.x))
}


#' A 2 arguments map_if
#'
#' @param .x,.y Lists.
#' @param .p Predicate.
#' @param .f Function if TRUE.
#' @param .else Function if FALSE.
#' @param ... Other parameter to pass to the function.
#'
#' @return A list of the same length.
#' @export
#'
# @examples
map2_if <- function(.x, .y, .p, .f, ..., .else = NULL) {
  sel <- probe(.x, .p)
  #   if (rlang::is_logical(.p)) { # fonction probe
  #   stopifnot(length(.p) == length(.x))
  #   .p
  # } else {
  #   .p <- as_predicate(.p, ..., .mapper = TRUE)
  #   map_lgl(.x, .p, ...)
  # }

  out <- purrr::list_along(.x)
  out[sel] <- purrr::map2(.x[sel], .y[sel], .f, ...)
  if (rlang::is_null(.else)) {
    out[!sel] <- .x[!sel]
  }
  else {
    out[!sel] <- purrr::map2(.x[sel], .y[sel], .else, ...)
  }
  magrittr::set_names(out, names(.x))
}

# Simplifier l'alias de list2 (dans purrr) :
# ( pour data %>% list_of_maps %>% pmap(~) )
#list2 <- rlang::list2

#purrr internal functions dependencies (CRAN does'nt accept :::)

#Quote authors of purrr:::probe ---------------------------
#' @keywords internal
probe <- function (.x, .p, ...)
{
  if (rlang::is_logical(.p)) {
    stopifnot(length(.p) == length(.x))
    .p
  }
  else {
    .p <- as_predicate(.p, ..., .mapper = TRUE)
    purrr::map_lgl(.x, .p, ...)
  }
}

#Quote authors of purrr:::as_predicate --------------------------
#' @keywords internal
as_predicate  <- function (.fn, ..., .mapper)
{
  if (.mapper) {
    .fn <- purrr::as_mapper(.fn, ...)
  }
  function(...) { #Simfiied, no purrr:::as_predicate_friendly_type_of
    out <- .fn(...)
    if (!rlang::is_bool(out)) {
      msg <- sprintf("Predicate functions must return a single `TRUE` or `FALSE`")
    }
    out
  }
}

#Needed to use where() (it's internal) : quote aut of tidyselect:::where -------
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

