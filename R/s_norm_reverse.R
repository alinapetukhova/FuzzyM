#' FCM package with functions for reverse S-norms calculations
#'
#' @description
#' s_norm_reverse set of functions is aimed to calculate
#' drastic, einstein, algebraic, hamacher products, min and bounded difference reverse S-norms
#' @param gammaSnorm,piSnorm,typeSnorm norm
#' @param element1,element2 paramater
#' @name s_norm_reverse
NULL

#' @rdname s_norm_reverse
#' @export
drastic_sum_snorm_reverse <- function(element1, element2, gammaSnorm, piSnorm) {
  #ifelse (min(element1, element2) == 0, max(element1, element2), 1)

  theResult <- ifelse(element1>=element2, 0, min(1-element1, element2))
} #

#' @rdname s_norm_reverse
#' @export
bounded_sum_snorm_reverse <- function(element1, element2, gammaSnorm, piSnorm) {
  theResult <- max (0, element2 - element1) # min (1, element1 + element2)
} #

#' @rdname s_norm_reverse
#' @export
einstein_sum_snorm_reverse <- function(element1, element2, gammaSnorm, piSnorm) {
  theResult <- (element1 + element2)/(1 + element1 * element2)
}

#' @rdname s_norm_reverse
#' @export
algebraic_sum_snorm_reverse <- function(element1, element2, gammaSnorm, piSnorm) {
  theResult <- ifelse(element1>=element2, 0, (element2-element1)/(1-element1)) #element1 + element2 - element1 * element2
} #

#' @rdname s_norm_reverse
#' @export
hamacher_sum_snorm_reverse <- function(element1, element2, gammaSnorm, piSnorm) {
  theResult <- ifelse(element1>=element2, 0, (element1 + element2 - 2 * element1 * element2)/(1 - element1 * element2))
} #

#' @rdname s_norm_reverse
#' @export
max_snorm_reverse <- function(element1, element2, gammaSnorm, piSnorm) {
  theResult <-  min (element1, element2) #max (element1, element2)
} #ifelse(element1>=element2, 0, element2) #

#' @rdname s_norm_reverse
#' @export
hamacher_union_operator_snorm_reverse <- function(element1, element2, gammaSnorm, piSnorm) {
  theResult <- (element1 + element2 + (gammaSnorm - 1) * element1 * element2)/(1 + gammaSnorm * element1 * element2)
}

#' @rdname s_norm_reverse
#' @export
yager_union_operator_snorm_reverse <- function(element1, element2, gammaSnorm, piSnorm) {
  theResult <- min (1, (element1^piSnorm + element2^piSnorm)^(1/piSnorm))
}

#' @rdname s_norm_reverse
#' @export
snorm_functions_reverse <- list(drastic_sum_snorm_reverse = drastic_sum_snorm_reverse,
                                bounded_sum_snorm_reverse = bounded_sum_snorm_reverse,
                                einstein_sum_snorm_reverse = einstein_sum_snorm_reverse,
                                algebraic_sum_snorm_reverse = algebraic_sum_snorm_reverse,
                                hamacher_sum_snorm_reverse = hamacher_sum_snorm_reverse,
                                max_snorm_reverse = max_snorm_reverse,
                                hamacher_union_operator_snorm_reverse = hamacher_union_operator_snorm_reverse,
                                yager_union_operator_snorm_reverse = yager_union_operator_snorm_reverse)

#' @rdname s_norm_reverse
#' @export
get_snorm_reverse <- function(typeSnorm) {
  theResult <- ''

  if (typeSnorm == 'Drastic Sum') {
    theResult <- 'drastic_sum_snorm_reverse'
  }
  if (typeSnorm == 'Bounded Sum') {
    theResult <- 'bounded_sum_snorm_reverse'
  }
  if (typeSnorm == 'Einstein Sum') {
    theResult <- 'einstein_sum_snorm_reverse'
  }
  if (typeSnorm == 'Algebraic Sum') {
    theResult <- 'algebraic_sum_snorm_reverse'
  }
  if (typeSnorm == 'Hamacher Sum') {
    theResult <- 'hamacher_sum_snorm_reverse'
  }
  if (typeSnorm == 'Max') {
    theResult <- 'max_snorm_reverse'
  }
  if (typeSnorm == 'Hamacher-union operator') {
    theResult <- 'hamacher_union_operator_snorm_reverse'
  }

  if (typeSnorm == 'Yager-union operator') {
    theResult <- 'yager_union_operator_snorm_reverse'
  }

  theResult
}
