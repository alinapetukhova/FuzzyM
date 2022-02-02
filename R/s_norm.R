#' @title s_norm
#'
#' @description
#' s_norm set of functions is aimed to calculate
#' drastic, einstein, algebraic, hamacher products, min and bounded difference S-norms
#' @param gammaSnorm,piSnorm,typeSnorm norm
#' @param element1,element2 paramater
#' @name s_norm

#' @rdname s_norm
#' @export
#'
drastic_sum_snorm <- function(element1, element2, gammaSnorm, piSnorm) {
  #ifelse (min(element1, element2) == 0, max(element1, element2), 1)

  theResult <- ifelse (min(element1, element2) == 0, max(element1, element2), 1)
} #

#' @rdname s_norm
#' @export
bounded_sum_snorm <- function(element1, element2, gammaSnorm, piSnorm) {
  theResult <- min (1, element1 + element2)
} #

#' @rdname s_norm
#' @export
einstein_sum_snorm <- function(element1, element2, gammaSnorm, piSnorm) {
  theResult <- (element1 + element2)/(1 + element1 * element2)
}

#' @rdname s_norm
#' @export
algebraic_sum_snorm <- function(element1, element2, gammaSnorm, piSnorm) {
  theResult <- element1 + element2 - element1 * element2 #
} #

#' @rdname s_norm
#' @export
hamacher_sum_snorm <- function(element1, element2, gammaSnorm, piSnorm) {
  theResult <- (element1 + element2 - 2 * element1 * element2)/(1 - element1 * element2)
} #

#' @rdname s_norm
#' @export
max_snorm <- function(element1, element2, gammaSnorm, piSnorm) {
  theResult <- max (element1, element2)
} #ifelse(element1>=element2, 0, element2) #

#' @rdname s_norm
#' @export
hamacher_union_operator_snorm <- function(element1, element2, gammaSnorm, piSnorm) {
  theResult <- (element1 + element2 + (gammaSnorm - 1) * element1 * element2)/(1 + gammaSnorm * element1 * element2)
}

#' @rdname s_norm
#' @export
yager_union_operator_snorm <- function(element1, element2, gammaSnorm, piSnorm) {
  theResult <- min (1, (element1^piSnorm + element2^piSnorm)^(1/piSnorm))
}

#' @rdname s_norm
#' @export
snorm_functions <- list(drastic_sum_snorm = drastic_sum_snorm,
                        bounded_sum_snorm = bounded_sum_snorm,
                        einstein_sum_snorm = einstein_sum_snorm,
                        algebraic_sum_snorm = algebraic_sum_snorm,
                        hamacher_sum_snorm = hamacher_sum_snorm,
                        max_snorm = max_snorm,
                        hamacher_union_operator_snorm = hamacher_union_operator_snorm,
                        yager_union_operator_snorm = yager_union_operator_snorm)

#' @rdname s_norm
#' @export
get_snorm <- function(typeSnorm) {
  theResult <- ''

  if (typeSnorm == 'Drastic Sum') {
    drastic_sum_snorm <- function(element1, element2, gammaSnorm, piSnorm) {
      theResult <- ifelse (min(element1, element2) == 0, max(element1, element2), 1)
    }
    return(drastic_sum_snorm)
  }
  if (typeSnorm == 'Bounded Sum') {
    bounded_sum_snorm <- function(element1, element2, gammaSnorm, piSnorm) {
      theResult <- min (1, element1 + element2)
    }
    return(bounded_sum_snorm)
  }
  if (typeSnorm == 'Einstein Sum') {
    einstein_sum_snorm <- function(element1, element2, gammaSnorm, piSnorm) {
      theResult <- (element1 + element2)/(1 + element1 * element2)
    }
    return(einstein_sum_snorm)
  }
  if (typeSnorm == 'Algebraic Sum') {
    algebraic_sum_snorm <- function(element1, element2, gammaSnorm, piSnorm) {
      theResult <- element1 + element2 - element1 * element2 #
    }
    return(algebraic_sum_snorm)
  }
  if (typeSnorm == 'Hamacher Sum') {
    hamacher_sum_snorm <- function(element1, element2, gammaSnorm, piSnorm) {
      theResult <- (element1 + element2 - 2 * element1 * element2)/(1 - element1 * element2)
    }
    return(hamacher_sum_snorm)
  }
  if (typeSnorm == 'Max') {
    max_snorm <- function(element1, element2, gammaSnorm, piSnorm) {
      theResult <- max (element1, element2)
    }
    return(max_snorm)
  }
  if (typeSnorm == 'Hamacher-union operator') {
    hamacher_union_operator_snorm <- function(element1, element2, gammaSnorm, piSnorm) {
      theResult <- (element1 + element2 + (gammaSnorm - 1) * element1 * element2)/(1 + gammaSnorm * element1 * element2)
    }
    return(hamacher_union_operator_snorm)
  }

  if (typeSnorm == 'Yager-union operator') {
    yager_union_operator_snorm <- function(element1, element2, gammaSnorm, piSnorm) {
      theResult <- min (1, (element1^piSnorm + element2^piSnorm)^(1/piSnorm))
    }
    return(yager_union_operator_snorm)
  }

  theResult
}
