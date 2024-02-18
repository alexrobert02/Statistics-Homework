stable_marriage = function(n, prefw) {
  w_partner = vector(length = n)
  m_partner = vector(length = n)
  for (j in 1:n) {
    w_partner[j] = -1
    m_partner[j] = -1
  }
  free_count = n
  while (free_count > 0) {
    if (length(which(m_partner == -1)) > 1)
      i = sample(which(m_partner == -1), 1)
    else
      i = which(m_partner == -1)
    j = sample(1:n, 1)
    if (w_partner[j] == -1){
      w_partner[j] = i
      m_partner[i] = j
      free_count = free_count-1
    }
    else {
      h = w_partner[j]
      if (prefw[i,j] > prefw[h,j]) {
        w_partner[j] = i
        m_partner[i] = j
        m_partner[h] = -1
      }
    }
  }
  for(i in 1:n){
    cat("  Barbatul", i, "este cuplat cu femeia", m_partner[i], "\n")
  }
}

C2 = function() {
  prefw = matrix(c(3, 1, 3, 2,
                   2, 4, 2, 4,
                   1, 2, 4, 1,
                   4, 3, 1, 3), nrow = 4, ncol = 4, byrow = TRUE)
  stable_marriage(4, prefw)
}

number = function(v) {
  n = length(v)
  sum = 0
  for (i in 1:n)
    sum = sum + (v[i]) * 2^(i-1)
  return (sum)
}

c3_function = function(U,m,n,u) {
  r = vector()
  p = sample(Primes(n^2),1)
  R = number(u) %% p
  for (j in 1:m){
    x = U[,j]
    r[j] = number(x) %% p
  }
  for (j in 1:m)
    if (R == r[j])
      return (TRUE)
  return (FALSE)
}

C3 = function() {
  a1 = c(0,0,1,1,0,1)
  a2 = c(0,1,1,1,0,0)
  a3 = c(0,1,1,1,1,1)
  u = c(0,1,1,1,0,0)
  U = array(c(a1,a2,a3), dim = c(6,3))
  if (c3_function(U,3,6,u) == TRUE)
    cat("  Cuvantul dat apartine multimii de cuvinte.\n")
  else
    cat("  Cuvantul dat nu apartine multimii de cuvinte.\n")
}


TemaC = function() {
  cat("Exercitiul C2:\n")
  C2()
  cat("\n")
  
  cat("Exercitiul C3:\n")
  C3()
  cat("\n")
}

TemaC()