D1 = function(n,x,alfa,miu) {
  sigma = sqrt(miu)
  critical_z = qnorm(1 - alfa/2, 0, 1)
  a = x - critical_z*sigma/sqrt(n)
  b = x + critical_z*sigma/sqrt(n)
  interval = c(a, b)
  cat("  Intervalul de incredere pentru", (1-alfa)*100, "%:", a, b, "\n")
}

D2 = function(N, n, alfa) {
  count = 0
  for (i in 1:N) {
    i = sample(0:9, 40, replace = TRUE)
    x = mean(i)
    s = sd(i)
    se = s/sqrt(n)
    critical_t = qt(1-alfa/2, n - 1)
    a = x - critical_t*se
    b = x + critical_t*se
    interval = c(a, b)
    if (a <= 4.5 & 4.5 <= b)
      count = count + 1
  }
  cat("  Pentru intervalul de incredere", (1-alfa)*100, "%, 4.5 apare de", count, "in interval.\n")
}

test = function(alfa, n, succese, p0, tiptest) {
  p_prim = succese/n
  z_score = (p_prim-p0) / sqrt(p0*(1-p0)/n)
  if (tiptest == "stanga"){
    critical_z = qnorm(alfa, 0, 1);
    if (z_score < critical_z)
      string <- "Se respinge ipoteza nula si se accepta ipoteza alternativa"
    else
      string <- "Se accepta ipoteza nula"
  }
  if (tiptest == "dreapta") {
    critical_z = qnorm(1 - alfa, 0, 1);
    if (z_score > critical_z)
      string <- "Se respinge ipoteza nula si se accepta ipoteza alternativa"
    else
      string <- "Se accepta ipoteza nula"
  }
  if (tiptest == "simetric") {
    critical_z = qnorm(1-alfa/2);
    if (abs(z_score) > abs(critical_z))
      string <- "Se respinge ipoteza nula si se accepta ipoteza alternativa"
    else
      string <- "Se accepta ipoteza nula"
  }
  return (string)
}

D3 = function(alfa) {
  if(test(alfa, 1250, 852, 0.72, "stanga") == "Se accepta ipoteza nula")
    cat("  Cu", alfa*100, "% nivel de semnificatie, este adevarat ca proportia este mai mica decat q72%.\n")
  else
    cat("  Cu", alfa*100, "% nivel de semnificatie, ipoteza este falsa. Deci adevarata proportie este mai mare decat 72%.\n")
}

D4 = function(alfa) {
  
  if(test(alfa, 1020, 623, 0.6, "dreapta") == "Se accepta ipoteza nula")
    cat("  Cu", alfa*100, "% nivel de semnificatie, este adevarat ca proportia este mai mare decat 60%.\n")
  else
    cat("  Cu", alfa*100, "% nivel de semnificatie, ipoteza este falsa. Deci adevarata proportie este mai mica decat 60%.\n")
}


TemaD = function() {
  cat("Exercitiul D1:\n")
  D1(20, 300, 0.1, 900)
  D1(20, 300, 0.05, 900)
  cat("\n")
  
  cat("Exercitiul D2:\n")
  D2(100, 40, 0.01)
  D2(100, 40, 0.05)
  cat("\n")
  
  cat("Exercitiul D3:\n")
  D3(0.01)
  D3(0.05)
  cat("\n")
  
  cat("Exercitiul D4:\n")
  D4(0.01)
  D4(0.05)
  cat("\n")
}

TemaD()
