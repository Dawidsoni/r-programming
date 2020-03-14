A = diag(1:5) + matrix(1:25, nrow=5)
print(A)
cat("Det A = ", det(A), "\n")
b = c(3, 2, 4, 1, -5)
print(b)
x = solve(A) %*% b
print(x)
A_inv = solve(A)
print(A_inv)
print(round(A %*% A_inv, 2))
for (i in 1:5) {
  pref_A = if (i != 1) c(A[, 1:(i - 1)]) else c()
  suf_A = if (i != 5) c(A[, (i + 1):5]) else c()
  A_i = matrix(c(pref_A, b, suf_A), nrow=5)
  print(det(A_i) / det(A))
}
