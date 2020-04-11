parity_sign <- function(x) {
  if (x %% 2 == 0) {
    return(1)
  } else {
    return(-1)
  }
}

matrix_inverse <- function(input_matrix) {
  if (dim(input_matrix)[1] != dim(input_matrix)[2]) {
    return("Matrix must be quadratic")
  }
  if (det(input_matrix) == 0) {
    return("Matrix is not invertable")
  }
  row_count = dim(input_matrix)[1]
  adjugate_matrix = matrix(0, nrow=row_count, ncol=row_count)
  for (i in 1:row_count) {
    for (j in 1:row_count) {
      sub_matrix = matrix(input_matrix[-i, -j], nrow=(row_count - 1))
      adjugate_matrix[i, j] <- parity_sign(i + j) * det(sub_matrix)
    }
  }
  return(adjugate_matrix / det(input_matrix))
}

print(matrix_inverse(matrix(1:6, nrow=2)))
print(matrix_inverse(matrix(c(1, 1, 2, 2), nrow=2)))
print(matrix_inverse(matrix(c(1, 1, -2, 2), nrow=2)), digits=2)
print(matrix_inverse(matrix(c(1, 1, 1, 1, 1, 2, 1, 2, 1), nrow=3)), digits=2)
print(matrix_inverse(matrix(c(-3, 2, -5, -1, 0, -2, 3, -4, 1), nrow=3)), digits=2)
rm(matrix_inverse)