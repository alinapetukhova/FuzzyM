#' FCM package with functions for matrix manipulations
#'
#' @description
#' polinomial_eq_dominant set contains 1 function: reverse task calculation.
#' Each function takes a matrix, vector and t-norm as arguments and returns a solution matrix.
#' The calculation procedure of the function includes a solution existence check and a solution check.
#' @importFrom utils combn
#' @name polinomial_eq_dominant
NULL

#' @rdname polinomial_eq_dominant
#' @param matrix matrix
#' @param vector vector
#' @param tnorm function
#' @param tnorm_reverse function
#' @param snorm function
#' @param snorm_reverse function
#' @return solution of polynomial equation of matrix \code{matrix}, \code{vector} using \code{tnorm}, \code{tnorm_reverse}, \code{snorm}, \code{snorm_reverse}
#' @export
calc_reverse_task <- function(matrix, vector, tnorm, tnorm_reverse, snorm, snorm_reverse) {

  if ((nrow(matrix) > length(vector)) & (ncol(matrix) == length(vector))) {
    matrix <- t(matrix)
  }

  maxSolution <- rep(1, ncol(matrix))
  solutionExist <- TRUE

  #check if solutions exist
  for (k in 1:length(vector))
  {
    if (all(matrix[k,] < vector[k])) {
      solutionExist <- FALSE
      stop("solutions doesn't exist")
    }
  }
  if (solutionExist) {

    a <- rep(0, ncol(matrix))

    #calculating max-T operator
    for (k in 1:length(vector))
    {
      for(col in 1:ncol(matrix))
      {
        maxSolution[col] <- snorm_functions_reverse[[snorm_reverse]](maxSolution[col],
                                                                     snorm_functions_reverse[[snorm_reverse]]
                                                                     (tnorm_functions_reverse[[tnorm_reverse]]
                                                                       (matrix[k, col], vector[k]), 1))
      }
    }

    maxSolutionCheck <- rep(0, nrow(matrix))
    coveringMatrix <- matrix(data=NA, nrow=nrow(matrix), ncol=ncol(matrix))
    index <- vector(mode="list", length=nrow(matrix))

    #check solution
    for (row in 1:nrow(matrix))
    {
      for(col in 1:ncol(matrix))
      {
        maxSolutionCheck[row] <- snorm_functions[[snorm]](maxSolutionCheck[row], tnorm_functions[[tnorm]](matrix[row, col], maxSolution[col]))

        #calculate covering matrix
        if (tnorm_functions[[tnorm]](matrix[row, col], maxSolution[col]) == vector[row]) {
          coveringMatrix[row, col] <- vector[row]
          if (tnorm_functions[[tnorm]](coveringMatrix[row, col], maxSolution[col]) != vector[row])
            coveringMatrix[row, col] <- maxSolution[col]
          index[[row]] <- c(index[[row]], col)
        }
        else {
          coveringMatrix[row, col] = 0
        }
      }
    }

    if (!all(vector == maxSolutionCheck)) {
      warning("solution doesn't exist")

      message("old target vector")
      message(vector)

      message("new target vector")
      message(maxSolutionCheck)
      calc_reverse_task(matrix, maxSolutionCheck, tnorm, tnorm_reverse, snorm, snorm_reverse)
    }
    else {
      toRemove <- numeric()

      for (rowMain in 1:length(index))
      {
        if (!is.element(rowMain, toRemove)) {
          final <- index[[rowMain]]
          finalInd <- rowMain
          for (row in 1:length(index))
          {
            if (row != rowMain) {
              subarray <- TRUE
              if ((length(index[[row]]) < length(final))) {
                for(elem in 1:length(index[[row]])) {
                  if (!is.element(index[[row]][elem], final)) {
                    subarray <- FALSE
                    break
                  }
                }
                if (subarray)
                  if (!is.element(finalInd, toRemove))
                    toRemove <- c(toRemove, finalInd)
              }
              if ((length(index[[row]]) > length(final))) {
                for(elem in 1:length(final)) {
                  if (!is.element(final[elem], index[[row]])) {
                    subarray <- FALSE
                    break
                  }
                }
                if (subarray)
                  if (!is.element(row, toRemove))
                    toRemove <- c(toRemove, row)
              }
              if ((length(index[[row]]) == length(final)) & (vector[row] == vector[finalInd])) {
                for(elem in 1:length(index[[row]])) {
                  if (!is.element(index[[row]][elem], final)) {
                    subarray <- FALSE
                    break
                  }
                }

                if (subarray)
                  if (!is.element(finalInd, toRemove))
                    toRemove <- c(toRemove, min(row,finalInd))
              }
            }
          }
        }
      }
      if (length(toRemove) > 0) {
        coveringMatrix <- coveringMatrix[-toRemove, , drop = FALSE]
        coverindVector <- vector[-toRemove]
        index <- index[-toRemove]
      }

      solutionMatrixTemp <- matrix(data=NA, nrow=0, ncol=ncol(matrix))

      solutionMatrixFinal <- matrix(data=rep(0, ncol(coveringMatrix)), nrow=1, ncol=ncol(matrix))
      minVector <- rep(0, ncol(coveringMatrix))
      minSolution <- rep(0, ncol(coveringMatrix))

      for (row in 1:nrow(coveringMatrix)) {
        solutionMatrix <- matrix(data=NA, nrow=0, ncol=ncol(coveringMatrix))
        for (col in 1:length(index[[row]])) {
          minVector[index[[row]][col]] <- coveringMatrix[row, index[[row]][col]]
          solutionMatrix <- rbind(solutionMatrix, minVector)
          minVector <- rep(0, ncol(coveringMatrix))
        }
        for (matRowTemp in 1:nrow(solutionMatrix)) {
          for (matRowFin in 1:nrow(solutionMatrixFinal)) {

            for (matCol in 1:ncol(solutionMatrixTemp)) {
              minSolution[matCol] <- snorm_functions[[snorm]](solutionMatrix[matRowTemp, matCol],
                                                              solutionMatrixFinal[matRowFin, matCol])
            }
            solutionMatrixTemp <- rbind(solutionMatrixTemp, minSolution)
          }
        }
        solutionMatrixFinal <- solutionMatrixTemp
        solutionMatrixTemp <- matrix(data=NA, nrow=0, ncol=ncol(matrix))
      }
      solutionMatrixFinal <- rbind(solutionMatrixFinal, maxSolution)

      toRemove <- numeric()
      for (sol in 1:nrow(solutionMatrixFinal)) {
        minSolutionCheck <- rep(0, nrow(matrix))
        #check solution
        for (row in 1:nrow(matrix))
        {
          for(col in 1:ncol(matrix))
          {
            minSolutionCheck[row] <- snorm_functions[[snorm]](minSolutionCheck[row], tnorm_functions[[tnorm]](matrix[row, col], solutionMatrixFinal[sol , col]))
          }
        }
        if (!all(vector == minSolutionCheck))
          toRemove <- c(toRemove, sol)
      }
      message('solution')
      solutionMatrixFinal
    }
  }
}

