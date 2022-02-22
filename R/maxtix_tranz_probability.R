#' FCM package with functions for matrix probability calculations
#'
#' @description
#' The maxtix_tranz_probability set of functions is aimed to calculate maximum matrix based on transitive closure
#'
#' @name maxtix_tranz_probability
NULL

#' @rdname maxtix_tranz_probability
#' @param matrix_1 matrix
#' @param matrix_2 matrix
#' @return multiplication of matrix \code{matrix_1} and \code{matrix_2}
#' @export
multiply_matrix_prob <- function(matrix_1, matrix_2)
{
  x <- matrix(data=NA, nrow = nrow(matrix_1), ncol = ncol(matrix_2))

  for(row in 1:nrow(matrix_1))
  {
    for(col in 1:ncol(matrix_2))
    {
      theSum <- -100
      for (k in 1:ncol(matrix_1))
      {
        if (theSum == -100) {theSum <- matrix_1[row, k]*matrix_2[k, col]}
        else
        {
          theSum <- theSum + matrix_1[row, k]*matrix_2[k, col] - theSum * matrix_1[row, k]*matrix_2[k, col]
        }
      }
      x[row,col] <- theSum
    }
  }

  x
}

#' @rdname maxtix_tranz_probability
#' @param positivematrix matrix
#' @return transitive closure of matrix \code{positivematrix}
#' @export
transitive_closure_prob <- function(positivematrix)
{
  newmatrix <- positivematrix
  probmatrix <- positivematrix

  #calculating transitive closure
  for(tran in 1:nrow(positivematrix))
    #for(tran in 1:1)
  {
    x <- matrix(data=NA, nrow=nrow(positivematrix), ncol=ncol(positivematrix))
    for(row in 1:nrow(positivematrix))
    {
      for(col in 1:ncol(newmatrix))
      {
        theSum <- -100
        for (k in 1:ncol(positivematrix))
        {
          if (theSum == -100)
          {
            theSum <- newmatrix[row, k]*positivematrix[k, col]
          }
          else
          {
            theSum <- theSum + newmatrix[row, k]*positivematrix[k, col] - theSum * newmatrix[row, k]*positivematrix[k, col]
          }
        }
        x[row,col] <- theSum
        probmatrix[row,col] <-  theSum + probmatrix[row,col] - theSum * probmatrix[row,col]
      }
    }
    if(isTRUE(all.equal(round(newmatrix, digits=3), round(x, digits=3)))) break
    else
      newmatrix <- x
  }
  probmatrix
}

#' @rdname maxtix_tranz_probability
#' @param positivematrix matrix
#' @return transitive closure of matrix \code{positivematrix} with max function
#' @export
transitive_closure_prob_max <- function(positivematrix)
{
  newmatrix <- positivematrix
  probmatrix <- positivematrix

  #calculating transitive closure
  for(tran in 1:nrow(positivematrix))
    for(tran in 1:2)
    {
      x <- matrix(data=NA, nrow=nrow(positivematrix), ncol=ncol(positivematrix))
      for(row in 1:nrow(positivematrix))
      {
        for(col in 1:ncol(newmatrix))
        {
          theSum <- -100
          for (k in 1:ncol(positivematrix))
          {
            if (theSum == -100) {theSum <- newmatrix[row, k]*positivematrix[k, col]}
            else
            {
              theSum <- theSum + newmatrix[row, k]*positivematrix[k, col] - theSum * newmatrix[row, k]*positivematrix[k, col]
            }
          }
          x[row,col] <- theSum
          probmatrix[row,col] <-  max(theSum, probmatrix[row,col])
        }
      }
      if(isTRUE(all.equal(round(newmatrix, digits=3), round(x, digits=3)))) break
      else
        newmatrix <- x
    }
  probmatrix
}

#' @rdname maxtix_tranz_probability
#' @param maxmatrix matrix
#' @return joined transitive closure of matrix \code{maxmatrix}
#' @export
probability_matrix_transitive <- function(maxmatrix)
{
  #calculating maximum matrix

  finalmatrix <- matrix(data=NA, nrow=nrow(maxmatrix)/2, ncol=ncol(maxmatrix))
  k <- 1

  for(row in seq(from=1, to=nrow(maxmatrix), by=2))
  {
    for(col in seq(from=1, to=ncol(maxmatrix), by=2))
    {
      finalmatrix[k,col] <- maxmatrix[row,col]+maxmatrix[row+1,col+1] - maxmatrix[row,col]*maxmatrix[row+1,col+1]
      finalmatrix[k,col+1] <- -(maxmatrix[row,col+1]+maxmatrix[row+1,col]-maxmatrix[row,col+1]*maxmatrix[row+1,col])
    }
    k <- k+1
  }
  finalmatrix
}
