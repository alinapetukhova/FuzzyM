#' @title matrix_tranz
#'
#' @description
#' The maxtix_tranz set of functions is aimed to calculate dissonance, consonance and influence
#'
#' @name maxtix_tranz
#' @usage tnorm_functions
#' @usage snorm_functions
#' @usage snorm_functions_reverse
#' @usage tnorm_functions_reverse

#' @rdname maxtix_tranz
#' @param initmatrix matrix
#' @return eigen values of \code{initmatrix}
#' @export
eigen_module <- function(initmatrix)
{
  eigenmat1<- eigen(initmatrix, only.values = TRUE)
  sqrt(Re(eigenmat1[['values']])^2+Im(eigenmat1[['values']])^2)
}

#' @rdname maxtix_tranz
#' @param initmatrix matrix
#' @return positive matrix of \code{initmatrix}
#' @export
positive_matrix_calc <- function(initmatrix)
{
  positivematrix <- matrix(data=NA, nrow=nrow(initmatrix)*2, ncol=ncol(initmatrix)*2)
  k <- 1
  m <- 1

  for(row in 1:nrow(initmatrix))
  {
    for(col in 1:ncol(initmatrix))
    {
      if(initmatrix[row,col]>0)
      {
        positivematrix[m,k] <- initmatrix[row,col]
        positivematrix[m+1,k+1] <- initmatrix[row,col]
        positivematrix[m,k+1] <- 0
        positivematrix[m+1,k] <- 0
      }
      else
      {
        positivematrix[m,k] <- 0
        positivematrix[m+1,k+1] <- 0
        positivematrix[m,k+1] <- -initmatrix[row,col]
        positivematrix[m+1,k] <- -initmatrix[row,col]
      }
      k <- k + 2
    }
    k <- 1
    m <- m + 2
  }
  positivematrix
}

#' @rdname maxtix_tranz
#' @param positivematrix matrix
#' @param tnorm function
#' @param snorm function
#' @param snormMatrix matrix
#' @param gammaTnormMean function
#' @param algaTnorm function
#' @param gammaTnorm function
#' @param piTnorm function
#' @param gammaSnorm function
#' @param piSnorm function
#' @return transitive closure of \code{positivematrix}
#' @export
transitive_closure <- function(positivematrix, tnorm, snorm, snormMatrix,
                               gammaTnormMean, algaTnorm, gammaTnorm, piTnorm,
                               gammaSnorm, piSnorm)
{

  positivematrix[is.na(positivematrix)] <- 0
  colnames(positivematrix) <- colnames(positivematrix, do.NULL = FALSE, prefix = "obj")
  rownames(positivematrix) <- paste('obj', 1:nrow(positivematrix), sep = "")
  class(positivematrix) <- "numeric"

  newmatrix <- positivematrix
  finalmatrix <- positivematrix
  #calculating transitive closure
  for(tran in 1:nrow(positivematrix))
  {
    x <- matrix(data=NA, nrow=nrow(positivematrix), ncol=ncol(positivematrix))
    colnames(x) <- colnames(x, do.NULL = FALSE, prefix = "obj")
    rownames(x) <- rownames(x, do.NULL = FALSE, prefix = "obj")
    #for(row in 1:nrow(positivematrix))
    for(row in rownames(positivematrix))
    {
      for(col in colnames(newmatrix))
      {
        #t norm - 0 - 0 ok
        #s norm - 0 - 0 ok
        condition <- (positivematrix[col,] == 0 & newmatrix[, col] == 0)

        theSum <- 0
        for (k in (1:ncol(positivematrix))[!condition])  {
          if (!condition[k]) {
            theSum <- snorm_functions[[snorm]](theSum,
                                               tnorm_functions[[tnorm]](positivematrix[row, k], newmatrix[k, col], gammaTnormMean, algaTnorm, gammaTnorm, piTnorm), #result t norm
                                               gammaSnorm, piSnorm)
          }
        }
        x[row,col] <- theSum
        #s norm matrix
        ifelse ((theSum != 0) || (finalmatrix[row,col] != 0),
                finalmatrix[row,col] <- snorm_functions[[snormMatrix]](theSum, finalmatrix[row,col], gammaSnorm, piSnorm)
                , finalmatrix[row,col] <- 0)
      }
    }
    ifelse ((isTRUE(all.equal(round(newmatrix, digits=3), round(x, digits=3)))), break, ifelse ((sum(x > 0.005) == 0), break, newmatrix <- x))
  }
  finalmatrix
}

#' @rdname maxtix_tranz
#' @param matrix matrix
#' @param gammaSnorm function
#' @param piSnorm function
#' @return aggregation function for transitive closure of \code{matrix}
#' @export
matrix_transitive_join <- function(matrix, snorm, gammaSnorm, piSnorm)
{
  #calculating joined matrix
  finalmatrix <- matrix(data=NA, nrow=nrow(matrix)/2, ncol=ncol(matrix))
  k <- 1

  for(row in seq(from=1, to=nrow(matrix), by=2))
  {
    for(col in seq(from=1, to=ncol(matrix), by=2))
    {
      finalmatrix[k,col] <- snorm_functions[[snorm]](matrix[row,col], matrix[row+1,col+1], gammaSnorm, piSnorm)
      finalmatrix[k,col+1] <- - snorm_functions[[snorm]](matrix[row,col+1], matrix[row+1,col], gammaSnorm, piSnorm)
    }
    k <- k+1
  }
  finalmatrix
}

#' @rdname maxtix_tranz
#' @param finalmatrix matrix
#' @return system indicators of \code{finalmatrix}
#' @export
consonanse_dissonanse <- function(finalmatrix)
{
  #calculating consonanse + dissonanse maxrix
  c <- matrix(data=NA, nrow=nrow(finalmatrix), ncol=ncol(finalmatrix)/2)
  d <- matrix(data=NA, nrow=nrow(finalmatrix), ncol=ncol(finalmatrix)/2)
  p <- matrix(data=NA, nrow=nrow(finalmatrix), ncol=ncol(finalmatrix)/2)
  k <- 1

  for(row in seq(from=1, to=nrow(finalmatrix), by=1))
  {
    for(col in seq(from=1, to=ncol(finalmatrix), by=2))
    {
      if(finalmatrix[row,col]!=-finalmatrix[row,col+1])
      {
        c[row,k] <- abs(finalmatrix[row,col]+finalmatrix[row,col+1])/(abs(finalmatrix[row,col])+abs(finalmatrix[row,col+1]))
        p[row,k] <- sign(finalmatrix[row,col]+finalmatrix[row,col+1])*max(abs(finalmatrix[row,col]),abs(finalmatrix[row,col+1]))
      }
      else
      {
        c[row,k] <- 0
        p[row,k] <- 0
      }
      k <- k+1
    }
    k <- 1
  }
  d <- 1 - c

  #system consonanse (5.8)
  system_consonanse <- round(colSums(c)/ncol(d), digits = 3)

  #system dissonanse (5.10)
  system_dissonanse <- round(colSums(d)/ncol(c), digits = 3)

  #element consonanse (5.9)
  element_consonanse <- round(rowSums(c)/nrow(c), digits = 3)

  #element dissonanse (5.11)
  element_dissonanse <- round(rowSums(d)/nrow(d), digits = 3)

  #system influence (5.12)
  system_influence <- round(colSums(p)/ncol(p), digits = 3)

  #concept influence (5.13)
  concept_influence <- round(rowSums(p)/nrow(p), digits = 3)

  #central of consonanse (5.13.2)
  central_cons= round(system_consonanse-element_consonanse, digits = 3)

  #influence (5.13.3)
  central_influence= round(system_influence-concept_influence, digits = 3)

  #combined consonanse concept and system (5.14)
  combaned_cons = round(pmax(system_consonanse,element_consonanse), digits = 3)

  #combined dissonanse concept and system (5.15)
  combaned_dis = round(pmax(system_dissonanse,element_dissonanse), digits = 3)

  cbind(c(1:length(system_consonanse)),
        'System consonanse' = system_consonanse,
        'System dissonanse' = system_dissonanse,
        'Element consonanse' = element_consonanse,
        'Element dissonanse' = element_dissonanse,
        'System influence' = system_influence,
        'Concept influence' = concept_influence,
        'Central consonanse' = central_cons,
        'Central influence' = central_influence,
        'Combaned consonanse' = combaned_cons,
        'Combaned dissonanse' = combaned_dis)
}

#' @rdname maxtix_tranz
#' @param finalmatrix matrix
#' @return cross consonanse of \code{finalmatrix}
#' @export
cross_consonanse <- function(finalmatrix)
{
  a <- matrix(data=NA, nrow=nrow(finalmatrix), ncol=ncol(finalmatrix)/2)
  b <- matrix(data=NA, nrow=nrow(finalmatrix), ncol=ncol(finalmatrix)/2)
  k <- 1
  for(row in seq(from=1, to=nrow(finalmatrix), by=1))
  {
    for(col in seq(from=1, to=ncol(finalmatrix), by=2))
    {
      a[row,k] <- finalmatrix[row,col]
      b[row,k] <- finalmatrix[row,col+1]
      k <- k+1
    }
    k <- 1
  }

  #calculating consonanse + dissonanse maxrix
  c <- matrix(data=NA, nrow=nrow(a), ncol=ncol(a))

  for(row in seq(from=1, to=nrow(a), by=1))
  {
    for(col in seq(from=1, to=ncol(a), by=1))
    {
      c[row,col] <- abs((a[row,col]+a[col, row])+(b[row,col]+b[col, row]))/(abs(a[row,col]+a[col, row])+abs(b[row,col]+b[col, row]))
      c[is.na(c)] <- 0
      c[col, row] <- c[row,col]
    }
  }
  c
}

#' @rdname maxtix_tranz
#' @param finalmatrix matrix
#' @return cross dissonanse of \code{finalmatrix}
#' @export
cross_dissonanse <- function(finalmatrix)
{
  a <- matrix(data=NA, nrow=nrow(finalmatrix), ncol=ncol(finalmatrix)/2)
  b <- matrix(data=NA, nrow=nrow(finalmatrix), ncol=ncol(finalmatrix)/2)
  k <- 1
  for(row in seq(from=1, to=nrow(finalmatrix), by=1))
  {
    for(col in seq(from=1, to=ncol(finalmatrix), by=2))
    {
      a[row,k] <- finalmatrix[row,col]
      b[row,k] <- finalmatrix[row,col+1]
      k <- k+1
    }
    k <- 1
  }

  #calculating consonanse + dissonanse maxrix
  c <- matrix(data=NA, nrow=nrow(a), ncol=ncol(a))
  d <- matrix(data=NA, nrow=nrow(a), ncol=ncol(a))

  for(row in seq(from=1, to=nrow(a), by=1))
  {
    for(col in seq(from=1, to=ncol(a), by=1))
    {
      c[row,col] <- abs((a[row,col]+a[col, row])+(b[row,col]+b[col, row]))/(abs(a[row,col]+a[col, row])+abs(b[row,col]+b[col, row]))
      c[is.na(c)] <- 0
      d[row,col] <- 1 - c[row,col]

      c[col, row] <- c[row,col]
      d[col, row] <- d[row,col]

    }
  }
  d
}

#' @rdname maxtix_tranz
#' @param finalmatrix matrix
#' @return cross positive influence of \code{finalmatrix}
#' @export
cross_positive_influence <- function(finalmatrix)
{
  a <- matrix(data=NA, nrow=nrow(finalmatrix), ncol=ncol(finalmatrix)/2)
  k <- 1
  for(row in seq(from=1, to=nrow(finalmatrix), by=1))
  {
    for(col in seq(from=1, to=ncol(finalmatrix), by=2))
    {
      a[row,k] <- finalmatrix[row,col]
      k <- k+1
    }
    k <- 1
  }

  #calculating consonanse + dissonanse maxrix
  p <- matrix(data=NA, nrow=nrow(a), ncol=ncol(a))

  for(row in seq(from=1, to=nrow(a), by=1))
  {
    for(col in seq(from=1, to=ncol(a), by=1))
    {
      p[row,col] <- max(a[row,col], a[col, row])
      p[col, row] <- p[row,col]
    }
  }
  p
}

#' @rdname maxtix_tranz
#' @param finalmatrix matrix
#' @return cross negative influence of \code{finalmatrix}
#' @export
cross_negative_influence <- function(finalmatrix)
{
  b <- matrix(data=NA, nrow=nrow(finalmatrix), ncol=ncol(finalmatrix)/2)
  k <- 1
  for(row in seq(from=1, to=nrow(finalmatrix), by=1))
  {
    for(col in seq(from=1, to=ncol(finalmatrix), by=2))
    {
      b[row,k] <- finalmatrix[row,col+1]
      k <- k+1
    }
    k <- 1
  }

  #calculating consonanse + dissonanse maxrix
  n <- matrix(data=NA, nrow=nrow(b), ncol=ncol(b))

  for(row in seq(from=1, to=nrow(b), by=1))
  {
    for(col in seq(from=1, to=ncol(b), by=1))
    {
      n[row,col] <- -max(abs(b[row,col]), abs(b[col, row]))
      n[col, row] <- n[row,col]

    }
  }
  n
}

#' @rdname maxtix_tranz
#' @param vector matrix
#' @param matrix matrix
#' @return impulse of \code{matrix} based on \code{vector}
#' @export
impuls_vector <- function(vector, matrix)
{
  newvector <- vector

  #calculating max multi
  for(col in 1:ncol(matrix))
  {
    theSum <- 0
    for (k in 1:length(vector))
    {
      theSum <- max(theSum, vector[k] * matrix[k, col])
    }
    newvector[col] <- theSum
  }

  newvector
}

#' @rdname maxtix_tranz
#' @param matrix matrix
#' @param vector matrix
#' @return multiplication of \code{matrix} and \code{vector}
#' @export
multiply_vector <- function(matrix, vector)
{
  newvector <- numeric(0)

  #calculating max multi
  for(row in 1:nrow(matrix))
  {
    theSum <- 0
    for (k in 1:length(vector))
    {
      theSum <- max(theSum, vector[k] * matrix[row, k])
    }
    newvector[row] <- theSum

  }
  newvector
}

#' @rdname maxtix_tranz
#' @param matrix_1 matrix
#' @param matrix_2 matrix
#' @param tnorm function
#' @param snorm function
#' @param gammaTnormMean function
#' @param algaTnorm function
#' @param gammaTnorm function
#' @param piTnorm function
#' @param gammaSnorm function
#' @param piSnorm function
#' @return multiplication of \code{matrix_1} and \code{matrix_2}
#' @export
multiply_matrix <- function(matrix_1, matrix_2, tnorm, snorm, gammaTnormMean, algaTnorm, gammaTnorm, piTnorm,
                            gammaSnorm, piSnorm)
{

  x <- matrix(, nrow = nrow(matrix_1), ncol = ncol(matrix_2))

  for(row in 1:nrow(matrix_1))
  {
    for(col in 1:ncol(matrix_2))
    {
      theSum <- 0
      for (k in 1:ncol(matrix_1))
      {
        tResult <- 0
        tResult <- tnorm_functions[[tnorm]](matrix_1[row, k], matrix_2[k, col], gammaTnormMean, algaTnorm, gammaTnorm, piTnorm)

        theSum <- snorm_functions[[snorm]](theSum, tResult, gammaSnorm, piSnorm)
      }
      x[row,col] <- theSum
    }
  }
  x
}

#' @rdname maxtix_tranz
#' @param matrix_1 matrix
#' @param matrix_2 matrix
#' @return maximum of \code{matrix_1} and \code{matrix_2}
#' @export
maximum_matrix <- function(matrix_1, matrix_2)
{
  x <- matrix_1

  if (ncol(matrix_1) == nrow(matrix_2))
    matrix_2 = t(matrix_2)

  for(row in 1:nrow(matrix_1))
  {
    for(col in 1:ncol(matrix_2))
    {
      x[row,col] <- max (matrix_1[row, col], matrix_2[row, col])
    }
  }

  x
}

#' @rdname maxtix_tranz
#' @param matrix matrix
#' @param initMatrix matrix
#' @param ipath vector
#' @param jpath vector
#' @return ik walk for \code{matrix} based on \code{initMatrix} with \code{ipath} and \code{jpath}
#' @export
ik_pos_maximum <- function(matrix, initMatrix, ipath, jpath)
{
  matrix[is.na(matrix)] <- 0
  initMatrix[is.na(initMatrix)] <- 0
  a <- matrix(data=NA, nrow=nrow(matrix), ncol=ncol(matrix)/2)
  b <- matrix(data=NA, nrow=nrow(matrix), ncol=ncol(matrix)/2)
  k <- 1
  for(row in seq(from=1, to=nrow(matrix), by=1))
  {
    for(col in seq(from=1, to=ncol(matrix), by=2))
    {
      a[row,k] <- matrix[row,col]
      b[row,k] <- matrix[row,col+1]
      k <- k+1
    }
    k <- 1
  }

  j_col <- a[, jpath]
  cnames <- c("C1", "C2")
  resultPath <- matrix(data=NA, nrow=0, ncol=2)

  icurr <- ipath

  jmax <- 0

  #calculating max multi
  while(jmax !=  jpath)
  {
    theSum <- 0
    for(col in 1:ncol(a))
    {
      pos <- a [icurr, col] * j_col[col]
      neg <- b [icurr, col] * j_col[col]
      if (abs(pos) > theSum) {
        if (initMatrix[icurr,col] != 0) {
          theSum <- abs(pos)
          jmax <- col
        }
      }

      if (abs(neg) > theSum) {
        if (initMatrix[icurr,col] != 0) {
          theSum <- abs(neg)
          jmax <- col
        }
      }
    }

    resultPath <- rbind(resultPath, c(icurr, jmax))
    icurr <- jmax
    if (jmax == 0) break
  }
  colnames(resultPath) <- c("from", "to")
  rownames(resultPath) <- rownames(resultPath, do.NULL = FALSE, prefix = "step_")

  resultPath
  # newvector
}

#' @rdname maxtix_tranz
#' @param matrix matrix
#' @param initMatrix matrix
#' @param ipath vector
#' @param jpath vector
#' @return ik negative walk for \code{matrix} based on \code{initMatrix} with \code{ipath} and \code{jpath}
#' @export
ik_neg_maximum <- function(matrix, initMatrix, ipath, jpath)
{
  a <- matrix(data=NA, nrow=nrow(matrix), ncol=ncol(matrix)/2)
  b <- matrix(data=NA, nrow=nrow(matrix), ncol=ncol(matrix)/2)
  k <- 1
  for(row in seq(from=1, to=nrow(matrix), by=1))
  {
    for(col in seq(from=1, to=ncol(matrix), by=2))
    {
      a[row,k] <- matrix[row,col]
      b[row,k] <- matrix[row,col+1]
      k <- k+1
    }
    k <- 1
  }

  j_col <- b[, jpath]
  resultPath <- matrix(, nrow=0, ncol=2)

  icurr <- ipath

  jmax <- 0

  #calculating max multi
  while(jmax !=  jpath)
  {
    theSum <- 0
    for(col in 1:ncol(a))
    {
      pos <- a [icurr, col] * j_col[col]
      neg <- b [icurr, col] * j_col[col]

      if (abs(pos) > theSum) {
        if (initMatrix[icurr,col] != 0) {
          theSum <- abs(pos)
          jmax <- col }
      }
      if (abs(neg)  > theSum) {
        if (initMatrix[icurr,col] != 0) {
          theSum <- abs(neg)
          jmax <- col }
      }
      #   if ((length(resultPath)>0) & (jmax ==  jpath)) break
    }
    resultPath <- rbind(resultPath, c(icurr, jmax))
    icurr <- jmax
    if (jmax == 0) break
  }
  colnames(resultPath) <- c("from", "to")
  rownames(resultPath) <- rownames(resultPath, do.NULL = FALSE, prefix = "step_")

  resultPath
  # newvector
}

#' @rdname maxtix_tranz
#' @param df_matrix matrix
#' @param vectorY vector
#' @param tnorm function
#' @param tnorm_reverse function
#' @param snorm function
#' @param snormMatrix function
#' @param snorm_reverse function
#' @return reverse task solution for \code{df_matrix} with \code{vectorY} using \code{tnorm}, \code{tnorm_reverse}, \code{snorm}, \code{snormMatrix}, \code{snorm_reverse}
#' @export
reverse_task <- function(df_matrix, vectorY, tnorm, tnorm_reverse, snorm, snormMatrix, snorm_reverse)
{
  rownames(df_matrix) <- colnames(df_matrix)

  #count 0 columns B 2 (save number of columns - for rows)
  zeroColName <- names(which(colSums(df_matrix == 0) == ncol(df_matrix)))
  #count 0 rows. 0rowSize - 4 (save number of rows - for columns)
  zeroRowName <- names(which(rowSums(df_matrix == 0) == nrow(df_matrix)))

  message("target nodes")
  message(zeroRowName)

  message("control nodes")
  message(zeroColName)

  DmatInit <- t(df_matrix[zeroColName, zeroRowName])
  Dmat <- positive_matrix_calc(DmatInit)

  BmatInit <- t(df_matrix[zeroColName, !rownames(df_matrix) %in% c(zeroColName, zeroRowName)])
  Bmat <- positive_matrix_calc(BmatInit)
  if (nrow(Bmat) < ncol(Bmat))
    Bmat <- t(Bmat)

  AmatInit <- t(df_matrix[!colnames(df_matrix) %in% c(zeroColName, zeroRowName), !rownames(df_matrix) %in% c(zeroColName, zeroRowName)])
  Amat <- positive_matrix_calc(AmatInit)

  CmatInit <- t(df_matrix[!colnames(df_matrix) %in% c(zeroColName, zeroRowName), zeroRowName])
  Cmat <- positive_matrix_calc(CmatInit)

  Amat_transitive_closure <- transitive_closure(Amat, tnorm, snorm, snormMatrix)
  #add init diagonal matrix
  Amat_transitive_closure <- maximum_matrix(Amat_transitive_closure, diag(1, nrow(Amat_transitive_closure), ncol(Amat_transitive_closure)))
  CAmat <-  multiply_matrix(Cmat, Amat_transitive_closure, tnorm, snorm)
  CABmat <-  multiply_matrix(CAmat, Bmat, tnorm, snorm)
  CABDmat <-  maximum_matrix(CABmat, Dmat)

  result_reverse <- calc_reverse_task(CABDmat, vectorY, tnorm, tnorm_reverse, snorm, snorm_reverse)
  colnames(result_reverse) <- rep(zeroColName, each=2)
  result_reverse
}

#' @rdname maxtix_tranz
#' @param df_matrix matrix
#' @param vectorX vector
#' @param tnorm function
#' @param snorm function
#' @param snormMatrix function
#' @return direct task solution for \code{df_matrix} with \code{vectorX} using \code{tnorm}, \code{snorm}, \code{snormMatrix}
#' @export
direct_task <- function(df_matrix, vectorX, tnorm, snorm, snormMatrix)
{
  rownames(df_matrix) <- colnames(df_matrix)

  #count 0 columns B 2 (save number of columns - for rows)
  zeroColName <- names(which(colSums(df_matrix == 0) == ncol(df_matrix)))
  #count 0 rows. 0rowSize - 4 (save number of rows - for columns)
  zeroRowName <- names(which(rowSums(df_matrix == 0) == nrow(df_matrix)))

  DmatInit <- t(df_matrix[zeroColName, zeroRowName])
  Dmat <- positive_matrix_calc(DmatInit)

  BmatInit <- t(df_matrix[zeroColName, !rownames(df_matrix) %in% c(zeroColName, zeroRowName)])
  Bmat <- positive_matrix_calc(BmatInit)

  AmatInit <- t(df_matrix[!colnames(df_matrix) %in% c(zeroColName, zeroRowName), !rownames(df_matrix) %in% c(zeroColName, zeroRowName)])
  Amat <- positive_matrix_calc(AmatInit)

  CmatInit <- t(df_matrix[!colnames(df_matrix) %in% c(zeroColName, zeroRowName), zeroRowName])
  Cmat <- positive_matrix_calc(CmatInit)

  Amat_transitive_closure <- transitive_closure(Amat, tnorm, snorm, snormMatrix)
  #add init diagonal matrix
  Amat_transitive_closure <- maximum_matrix(Amat_transitive_closure, diag(1, nrow(Amat_transitive_closure), ncol(Amat_transitive_closure)))

  ABmat <- multiply_matrix(Amat_transitive_closure, Bmat, tnorm, snorm)
  x_new <- multiply_vector(ABmat, vectorX)
  message('x_new')
  message(x_new)

  CAmat <-  multiply_matrix(Cmat, Amat_transitive_closure, tnorm, snorm)
  CABmat <-  multiply_matrix(CAmat, Bmat, tnorm, snorm)
  CABDmat <- maximum_matrix(CABmat, Dmat)
  y_new <- multiply_vector(CABDmat, vectorX)
  message('y_new')
  message(y_new)
  message('final')
  finalMatrix <- data.frame(A= numeric(0), B= numeric(0))
  for(col in seq(from=1, to=length(y_new), by=2))
  {
    finalMatrix <- structure(rbind(finalMatrix, c(y_new[col],(y_new[col] - y_new[col+1])/(y_new[col] + y_new[col+1]))), .Names = names(finalMatrix))
  }
  finalMatrix
}
