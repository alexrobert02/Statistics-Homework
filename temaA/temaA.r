A1 = function(lambda, p, n, k) {
  par(mfrow = c(2, 2))
  barplot(dpois(k:(k+n), lambda), main = "A1 - Distributia Poisson", col = "yellow", space = 0)
  barplot(dgeom(k:(k+n), p), main = "A1 - Distributia geometrica", col = "green", space = 0)
  barplot(dbinom(0:n,n, p), main = "A1 - Distributia binomiala", col = "red", space = 0)
}

A2a  = function(x) {
  v = vector()
  v[1] = median(x)
  v[2] = mean(x)
  v[3] = sd(x)
  v[4] = as.vector(quantile(x))[1 + 1]
  v[5] = as.vector(quantile(x))[2 + 1]
  v[6] = as.vector(quantile(x))[3 + 1]
  cat("Exercitiul A2:\n")
  cat("(a) Vectorul cu statistici:", v, "\n")
}

A2b = function(x) {
  j = 0;
  m = mean(x)
  s = sd(x)
  v = vector()
  for (i in 1:length(x)) {
    if (x[i] >= m-2*s && x[i] <= m+2*s) {
      j = j+1
      v[j] = x[i]
    }
  }
  return(v)
}
x=scan("notePS.txt")
A2b(x)

A2c = function(x) {
  v = A2b(x)
  interval = seq(0, 100, 10)
  hist(v, breaks = interval, right = T, freq = T, col = "blue", main = "A2 - Distributia frecventelor")
}

A2 = function() {
  x = scan("notePS.txt")
  A2a(x)
  A2b(x)
  A2c(x)
}


TemaA = function() {
  A1(15, 0.25, 15, 10)
  A2()
}

TemaA()