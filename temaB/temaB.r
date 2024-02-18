b1_function = function(n, a, b, c, h) {
  n_c = 0
  for(i in 1:n) {
    x = runif(1, -a*sqrt(h/c), a*sqrt(h/c))
    y = runif(1, -b*sqrt(h/c), b*sqrt(h/c))
    z = runif(1, 0, h)
    if(x^2/a^2 + y^2/b^2 <= z/c) {
      n_c = n_c+1;
    } 
  }
  val_reala = (pi*a*b*h^2)/(2*c)
  val_estimata = ((4*a*b*h/c*h)*n_c)/n
  cat("    Volumul Aproximat:", val_estimata, "\n")
  cat("    Volumul real:", val_reala, "\n")
  cat("    Diferenta:", abs(val_reala - val_estimata), "\n")
  cat("    Eroarea Relativa:", abs((val_reala - val_estimata)/val_estimata), "\n")
}

B1 = function(n) {
  cat("  Pentru un esantion de dimensiune", n,":\n")
  b1_function(n, 4, 3, 4, 4)
}


B2 = function(n, a, b, c, d) {
  n_c = 0
  for(i in 1:n) {
    x = runif(1, a, b)
    y = runif(1, c, d)
    if(x >= 1 & y <= 2 & y >= 0 & y <= x-1 & y <= 7-x) {
      n_c = n_c + 1
    }
  }
  cat("  Aria trapezului T este:", (abs((b-a)*(d-c))*n_c/n), "\n")
}

B3a = function(n) {
  sum = 0
  for(i in 1:n) {
    x = runif(1, 1, 2)
    sum = sum + x/(x^2+2)^3
  }
  cat(" Estimare:", sum/n, "\n")
  cat("    Valoarea exacta:", 1/48, "\n")
}

B3b = function(n) {
  sum = 0
  for(i in 1:n) {
    x = runif(1, -3, 3)
    sum = sum + 1/(x^2+9)
  }
  cat(" Estimare:", 6*sum/n, "\n")
  cat("    Valoarea exacta:", pi/6, "\n")
}
B3b(10000)

B3c = function(n) {
  sum = 0
  for(i in 1:n) {
    x=rexp(1, 1)
    sum = sum + x*exp(-x*x) / exp(-x)
  }
  cat(" Estimare:",sum/n, "\n")
  cat("    Valoarea exacta:", 1/2, "\n")
}
B3c(10000)

B4 = function(n,lambda) {
  sum = 0
  for(i in 1:n){
    first_server = rgamma(1, shape = 4, scale = 3)
    second_server = rgamma(1, shape = 4, scale = 2)
    third_server = rgamma(1, shape = 5, scale = 2)
    forth_server = rgamma(1, shape = 5, scale = 3)
    x = runif(1, 0, 1)
    latenta = rexp(1,lambda)
    if(x<=0.25)
      sum = sum + first_server
    else 
      if(x>0.25 & x<=0.5)
        sum = sum + second_server
    else
      if(x>0.5 & x<=0.8)
        sum = sum + third_server
    else
      sum = sum + forth_server
    sum = sum + latenta
  }
  cat("  Timpul mediu necesar:", sum/n, "\n")
}

B5 = function(p) {
  cont = vector(length = 50)
  for(i in 1:50)
    cont[i] = 0
  cont[sample(1:50,1)] = 1
  days = 1
  nr_infected = 1
  nr_healthy = 49
  while (nr_infected != 0 && nr_healthy != 0) {
    for(i in 1:nr_infected) {
      x = runif(1,0,1)
      if (x < p) {
        if(length(which(cont == 0) > 1))
          j = sample(which(cont == 0), 1)
        else
          j = which(cont == 0)
        cont[j] = 1 
        nr_infected = nr_infected + 1
        nr_healthy = nr_healthy - 1
      }
    }
    if (days > 1) {
      for(i in 1:8) {
        j = sample(1:50,1)
        if (cont[j] == 1) {
          nr_infected = nr_infected - 1
          nr_healthy = nr_healthy + 1
          cont[j] = 0
        }
      }
    }
    days = days + 1
  }
  cat("  Numarul de zile necesari curatarii tuturor conturilor infectate:", days, "\n")
}


TemaB = function() {
  cat("Exercitiul B1:\n")
  B1(20000)
  B1(50000)
  B1(100000)
  cat("\n")
  
  cat("Exercitiul B2:\n")
  B2(30000, 1, 7, 0, 2)
  cat("\n")
  
  cat("Exercitiul B3:\n")
  cat("(a)")
  B3a(10000)
  cat("(b)")
  B3b(10000)
  cat("(c)")
  B3c(10000)
  cat("\n")
  
  cat("Exercitiul B4:\n")
  B4(10000,4)
  cat("\n")
  
  cat("Exercitiul B5:\n")
  B5(0.2)
  cat("\n")
}

TemaB()