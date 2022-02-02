#' @title t_norm_reverse
#'
#' @description
#' t_norm_reverse set of functions is aimed to calculate
#' drastic, einstein, algebraic, hamacher products, min and bounded difference reverse T-norms
#' @param gammaTnormMean,algaTnorm,gammaTnorm,piTnorm,typeTnorm norm
#' @param element1,element2 paramater
#' @name t_norm_reverse
NULL

#' @rdname t_norm_reverse
#' @export
min_tnorm_reverse <- function(element1, element2, gammaTnormMean, algaTnorm, gammaTnorm, piTnorm) {
  theResult <- ifelse (element1 <= element2, 1, element2)
}

#' @rdname t_norm_reverse
#' @export
hamacher_product_tnorm_reverse <- function(element1, element2, gammaTnormMean, algaTnorm, gammaTnorm, piTnorm) {
  theResult <- element1 * element2/(element1 + element2 - element1 * element2)
}

#' @rdname t_norm_reverse
#' @export
algebraic_product_tnorm_reverse <- function(element1, element2, gammaTnormMean, algaTnorm, gammaTnorm, piTnorm) {
  theResult <- ifelse (element1 <= element2, 1, element2/element1)
}

#' @rdname t_norm_reverse
#' @export
einstein_product_tnorm_reverse <- function(element1, element2, gammaTnormMean, algaTnorm, gammaTnorm, piTnorm) {
  theResult <- element1 * element2/(2 - (element1 + element2 - element1 * element2))
}

#' @rdname t_norm_reverse
#' @export
bounded_difference_tnorm_reverse <- function(element1, element2, gammaTnormMean, algaTnorm, gammaTnorm, piTnorm) {
  theResult <- min(1, 1 - element2 + element1) # +
}

#' @rdname t_norm_reverse
#' @export
drastic_product_tnorm_reverse <- function(element1, element2, gammaTnormMean, algaTnorm, gammaTnorm, piTnorm) {
  theResult <- ifelse (element2 == 0, 0.999, ifelse (element1 == element2, 1, min(element1, element2)))
}

#' @rdname t_norm_reverse
#' @export
parameterized_mean_intersection_operator_tnorm_reverse <- function(element1, element2, gammaTnormMean, algaTnorm, gammaTnorm, piTnorm) {
  theResult <- gammaTnormMean * min (element1, element2) + 0.5 * (1 - gammaTnormMean) * (element1 + element2)
}

#' @rdname t_norm_reverse
#' @export
dubois_intersection_operator_tnorm_reverse <- function(element1, element2, gammaTnormMean, algaTnorm, gammaTnorm, piTnorm) {
  theResult <- (element1 * element2)/(max(element1, element2, algaTnorm))
}

#' @rdname t_norm_reverse
#' @export
hamacher_intersection_operator_tnorm_reverse <- function(element1, element2, gammaTnormMean, algaTnorm, gammaTnorm, piTnorm) {
  theResult <- (element1 * element2)/(gammaTnorm + (1 - gammaTnorm)*(element1 + element2 - element1 * element2))
}


#' @rdname t_norm_reverse
#' @export
yager_intersection_operator_tnorm_reverse <- function(element1, element2, gammaTnormMean, algaTnorm, gammaTnorm, piTnorm) {
  theResult <- 1 - max(0, ((1 - element2)^piTnorm - (1 -element1)^piTnorm)^(1/piTnorm))
}

#' @rdname t_norm_reverse
#' @export
tnorm_functions_reverse <- list(min_tnorm_reverse = min_tnorm_reverse,
                                hamacher_product_tnorm_reverse = hamacher_product_tnorm_reverse,
                                algebraic_product_tnorm_reverse = algebraic_product_tnorm_reverse,
                                einstein_product_tnorm_reverse = einstein_product_tnorm_reverse,
                                bounded_difference_tnorm_reverse = bounded_difference_tnorm_reverse,
                                drastic_product_tnorm_reverse = drastic_product_tnorm_reverse,
                                parameterized_mean_intersection_operator_tnorm_reverse = parameterized_mean_intersection_operator_tnorm_reverse,
                                dubois_intersection_operator_tnorm_reverse = dubois_intersection_operator_tnorm_reverse,
                                hamacher_intersection_operator_tnorm_reverse = hamacher_intersection_operator_tnorm_reverse,
                                yager_intersection_operator_tnorm_reverse = yager_intersection_operator_tnorm_reverse)

#' @rdname t_norm_reverse
#' @export
get_tnorm_reverse <- function(typeTnorm) {
  theResult <- ''
  #if (is.na(element1)) element1 <- 0
  #if (is.na(element2)) element2 <- 0

  if (typeTnorm == 'Min') {
    theResult <- 'min_tnorm_reverse'
  }

  if (typeTnorm == 'Hamacher Product') {
    theResult <- 'hamacher_product_tnorm_reverse'
  }

  if (typeTnorm == 'Algebraic Product') {
    theResult <- 'algebraic_product_tnorm_reverse'
  }

  if (typeTnorm == 'Einstein Product') {
    theResult <- 'einstein_product_tnorm_reverse'
  }

  if (typeTnorm == 'Bounded Difference') {
    theResult <- 'bounded_difference_tnorm_reverse'
  }

  if (typeTnorm == 'Drastic Product') {
    theResult <- 'drastic_product_tnorm_reverse'
  }
  if (typeTnorm == 'Parameterized mean intersection operator') {
    theResult <- 'parameterized_mean_intersection_operator_tnorm_reverse'
  }

  if (typeTnorm == 'Dubois-intersection operator') {
    theResult <- 'dubois_intersection_operator_tnorm_reverse'
  }

  if (typeTnorm == 'Hamacher-intersection operator') {
    theResult <- 'hamacher_intersection_operator_tnorm_reverse'
  }

  if (typeTnorm == 'Yager-intersection operator') {
    theResult <- 'yager_intersection_operator_tnorm_reverse'
  }
  theResult
}
