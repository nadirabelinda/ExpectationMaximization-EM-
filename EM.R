# Fungsi EM
emrancob <- function(ftn, x,yij,i, j, tol = 1e-9, max.iter = 100) {
  yold <- yij
  ynew <- ftn(x,yold,i,j)
  iter <- 1
  cat("At iteration 1 value of x is:", ynew, "\n")
  
  while ((abs(ynew-yold) > tol) && (iter < max.iter)) {
    yold <- ynew;
    ynew <- ftn(x,yold,i,j);
    iter <- iter + 1
    cat("At iteration", iter, "value of y is:", ynew, "\n")
  }
  
  if (abs(ynew-yold) > tol) {
    cat("Algorithm failed to converge\n")
    return(NULL)
  } else {
    cat("Algorithm converged\n")
    return(ynew)
  }
}

# Misal data hilang : y23
ftn <- function(x,yij,i,j){
  x <- matrix(c(10,15,17,22,23,yij),ncol=3,byrow=TRUE)
  mu <- mean(x)
  alfai <- mean(x[i,])- mu
  betaj <- mean(x[,j])- mu
  yij <- mu + alfai + betaj
  return(yij)
}

# Menduga nilai y23
emrancob(ftn, x,0,2,3, tol = 1e-4, max.iter = 100) #tetapkan nilai awal y23, misal = 0 