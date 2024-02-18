t_test = function(tiptest, x, miu, s, n, alfa) {
  t_score = (x-miu)/(s/sqrt(n))
  if (tiptest == "stanga") {
    critical_t = qt(alfa, n - 1)
    if (t_score > critical_t) {
      cat("  Se accepta ipoteza nula.\n")
    }
    else {
      cat("  Se respinge ipoteza nula si se accepta ipoteza alternativa.\n")
    }
  }
  if (tiptest == "dreapta") {
    critical_t = qt(1-alfa, n - 1)
    if (t_score < critical_t) {
      cat("  Se accepta ipoteza nula.")
    }
    else {
      cat("  Se respinge ipoteza nula si se accepta ipoteza alternativa.\n")
    }
  }
  if (tiptest == "simetrica") {
    critical_t = qt(1-alfa/2, n - 1)
    if (abs(t_score) < abs(critical_t)) {
      cat("  Se accepta ipoteza nula.")
    }
    else {
      cat("  Se respinge ipoteza nula si se accepta ipoteza alternativa.\n")
    }
  }
}
E1 = function() {
  cat("  Se formuleaza ipoteza nula: 418 = 420.\n")
  cat("  Se formuleaza ipoteza alternativa: 418 < 420 (ipoteza alternativa la stanga).\n")
  t_test("stanga", 418, 420, 2.75, 125, 0.01)
}

z_test = function(tiptest, x, miu0, sigma, n, alfa) {
  z_score = (x-miu0)/(sigma/sqrt(n))
  if (tiptest == "stanga") {
    critical_z = qnorm(alfa, 0, 1)
    if (z_score > critical_z) {
      cat("    se accepta ipoteza nula.\n")
    }
    else {
      cat("    se respinge ipoteza nula si se accepta ipoteza alternativa.\n")
    }
  }
  if (tiptest == "dreapta") {
    critical_z = qnorm(1-alfa, 0, 1)
    if (z_score < critical_z) {
      cat("    se accepta ipoteza nula.\n")
    }
    else {
      cat("    se respinge ipoteza nula si se accepta ipoteza alternativa.\n")
    }
  }
  if (tiptest == "simetrica") {
    critical_z = qnorm(1-alfa/2, 0, 1)
    if (abs(z_score) < abs(critical_z)) {
      cat("    se accepta ipoteza nula.\n")
    }
    else {
      cat("    se respinge ipoteza nula si se accepta ipoteza alternativa.\n")
    }
  }
}

E2 = function(alfa) {
  cat("  Pentru", alfa*100, "% nivel de semnificatie:\n")
  cat("    Se formuleaza ipoteza nula: 4.9 = 5.17.\n")
  cat("    Se formuleaza ipoteza alternativa: 4.9 > 5.17 (ipoteza alternativa la dreapta).\n")
  z_test("dreapta", 4.9, 5.17, 0.35, 25, alfa)
}

z_test_means = function(tiptest, x1, x2, n1, n2, sigma1, sigma2, m0, alfa) {
  z_score = (x1-x2-m0)/sqrt(sigma1^2/n1+sigma2^ 2/n2)
  if (tiptest == "stanga") {
    critical_z = qnorm(alfa, 0, 1)
    if (z_score > critical_z) {
      cat("se accepta ipoteza nula.\n")
    }
    else {
      cat("se respinge ipoteza nula si se accepta ipoteza alternativa.\n")
    }
  }
  if (tiptest == "dreapta") {
    critical_z = qnorm(1-alfa, 0, 1)
    if (z_score < critical_z) {
      cat("se accepta ipoteza nula.\n")
    }
    else {
      cat("se respinge ipoteza nula si se accepta ipoteza alternativa.\n")
    }
  }
  if (tiptest == "simetrica") {
    critical_z = qnorm(1-alfa/2, 0, 1)
    if (abs(z_score) < abs(critical_z)) {
      cat("se accepta ipoteza nula.\n")
    }
    else {
      cat("se respinge ipoteza nula si se accepta ipoteza alternativa.\n")
    }
  }
}

E3 = function(tiptest) {
  if(tiptest == "simetrica")
    cat("(a) Pentru ipoteza simetrica : ")
  else
    cat("(b) Pentru Ipoteza asimetrica la", tiptest, ": ")
  z_test_means(tiptest, 5.48, 6.12, 25, 28, 1.31, 0.93, 0, 0.01 )
}

F_test = function(tiptest, s1, s2, n1, n2, alfa)
{
  F_score = s1^ 2/s2^2
  if (tiptest == "simetrica"){
    critical_F_s = qf(alfa/2, n1-1, n2-1)
    critical_F_d = qf(1-alfa/2, n1-1, n2-1)
    if (F_score > critical_F_s  || F_score < critical_F_d)
    {
      string <- "Se accepta ipoteza nula"
    }
    else
    {
      string <- "Se respinge ipoteza nula si se accepta ipoteza alternativa"
    }
  }
  if (tiptest == "dreapta"){
    critical_F_d = qf(1-alfa, n1-1, n2-1)
    if (F_score < critical_F_d){
      string <- "Se accepta ipoteza nula"
    }
    else
    {
      string <- "Se respinge ipoteza nula si se accepta ipoteza alternativa"
    }
  }
  if (tiptest == "stanga"){
    critical_F_s = qf(alfa, n1-1, n2-1)
    if (F_score > critical_F_s){
      string <- "Se accepta ipoteza nula."
    }
    else
    {
      string <- "Se respinge ipoteza nula si se accepta ipoteza alternativa"
    }
  }
  return (string)
}

E4 = function() {
  if(F_test("dreapta", 1.24, 0.87, 25, 28, 0.01) == "Se accepta ipoteza nula")
    cat("  Cu 1% nivel de semnificatie, putem trage concluzia ca primul automat are o dispersie mai mare.\n")
}


TemaE = function() {
  cat("Exercitiul E1:\n")
  E1()
  cat("\n")
  
  cat("Exercitiul E2:\n")
  E2(0.01)
  E2(0.05)
  cat("\n")
  
  cat("Exercitiul E3:\n")
  E3("simetrica")
  E3("stanga")
  cat("\n")
  
  cat("Exercitiul E4:\n")
  E4()
  cat("\n")
}

TemaE()