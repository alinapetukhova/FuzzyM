#' @title t_norm
#'
#' @description
#' t_norm set of functions is aimed to calculate
#' drastic, einstein, algebraic, hamacher products, min and bounded difference T-norms
#' @param gammaTnormMean,algaTnorm,gammaTnorm,piTnorm,typeTnorm norm
#' @param element1,element2 paramater
#' @name t_norm
NULL

#' @rdname t_norm
#' @export
min_tnorm <- function(element1, element2, gammaTnormMean, algaTnorm, gammaTnorm, piTnorm) {
  theResult <- min(element1, element2)
}

#' @rdname t_norm
#' @export
hamacher_product_tnorm <- function(element1, element2, gammaTnormMean, algaTnorm, gammaTnorm, piTnorm) {
  theResult <- ifelse(element1 == 0 & element2 == 0, 0 ,element1 * element2/(element1 + element2 - element1 * element2))
}

#' @rdname t_norm
#' @export
algebraic_product_tnorm <- function(element1, element2, gammaTnormMean, algaTnorm, gammaTnorm, piTnorm) {
  theResult <- element1 * element2
}

#' @rdname t_norm
#' @export
einstein_product_tnorm <- function(element1, element2, gammaTnormMean, algaTnorm, gammaTnorm, piTnorm) {
  theResult <- element1 * element2/(2 - (element1 + element2 - element1 * element2))
}

#' @rdname t_norm
#' @export
bounded_difference_tnorm <- function(element1, element2, gammaTnormMean, algaTnorm, gammaTnorm, piTnorm) {
  theResult <- max (0, element1 + element2)
}

#' @rdname t_norm
#' @export
drastic_product_tnorm <- function(element1, element2, gammaTnormMean, algaTnorm, gammaTnorm, piTnorm) {
  if (max(element1, element2) == 1) {
    theResult <- min(element1, element2)
  }
  else {
    theResult <- 0
  }
}

#' @rdname t_norm
#' @export
parameterized_mean_intersection_operator_tnorm <- function(element1, element2, gammaTnormMean, algaTnorm, gammaTnorm, piTnorm) {
  theResult <- gammaTnormMean * min (element1, element2) + 0.5 * (1 - gammaTnormMean) * (element1 + element2)
}

#' @rdname t_norm
#' @export
dubois_intersection_operator_tnorm <- function(element1, element2, gammaTnormMean, algaTnorm, gammaTnorm, piTnorm) {
  theResult <- (element1 * element2)/(max(element1, element2, algaTnorm))
}

#' @rdname t_norm
#' @export
hamacher_intersection_operator_tnorm <- function(element1, element2, gammaTnormMean, algaTnorm, gammaTnorm, piTnorm) {
  theResult <- (element1 * element2)/(gammaTnorm + (1 - gammaTnorm)*(element1 + element2 - element1 * element2))
}

#' @rdname t_norm
#' @export
yager_intersection_operator_tnorm <- function(element1, element2, gammaTnormMean, algaTnorm, gammaTnorm, piTnorm) {
  theResult <- 1 - min (1, ((1 - element1)^piTnorm + (1 -element2)^piTnorm)^(1/piTnorm))
}

#' @rdname t_norm
#' @export
tnorm_functions <- list(min_tnorm = min_tnorm,
                        hamacher_product_tnorm = hamacher_product_tnorm,
                        algebraic_product_tnorm = algebraic_product_tnorm,
                        einstein_product_tnorm = einstein_product_tnorm,
                        bounded_difference_tnorm = bounded_difference_tnorm,
                        drastic_product_tnorm = drastic_product_tnorm,
                        parameterized_mean_intersection_operator_tnorm = parameterized_mean_intersection_operator_tnorm,
                        dubois_intersection_operator_tnorm = dubois_intersection_operator_tnorm,
                        hamacher_intersection_operator_tnorm = hamacher_intersection_operator_tnorm,
                        yager_intersection_operator_tnorm = yager_intersection_operator_tnorm)

#' @rdname t_norm
#' @export
get_tnorm <- function(typeTnorm) {
  theResult <- ''
  #if (is.na(element1)) element1 <- 0
  #if (is.na(element2)) element2 <- 0

  if (typeTnorm == 'Min') {
    theResult <- 'min_tnorm'
  }

  if (typeTnorm == 'Hamacher Product') {
    theResult <- 'hamacher_product_tnorm'
  }

  if (typeTnorm == 'Algebraic Product') {
    theResult <- 'algebraic_product_tnorm'
  }

  if (typeTnorm == 'Einstein Product') {
    theResult <- 'einstein_product_tnorm'
  }

  if (typeTnorm == 'Bounded Difference') {
    theResult <- 'bounded_difference_tnorm'
  }

  if (typeTnorm == 'Drastic Product') {
    theResult <- 'drastic_product_tnorm'
  }
  if (typeTnorm == 'Parameterized mean intersection operator') {
    theResult <- 'parameterized_mean_intersection_operator_tnorm'
  }

  if (typeTnorm == 'Dubois-intersection operator') {
    theResult <- 'dubois_intersection_operator_tnorm'
  }

  if (typeTnorm == 'Hamacher-intersection operator') {
    theResult <- 'hamacher_intersection_operator_tnorm'
  }

  if (typeTnorm == 'Yager-intersection operator') {
    theResult <- 'yager_intersection_operator_tnorm'
  }
  theResult
}
