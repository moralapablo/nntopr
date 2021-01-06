################################
## Function that takes a `polyreg` output (named as `I(V1^1):I(V2^1)` ) and renames it
## following the convention that I use when obtaining 
## the coefficients with the formula (named as `1,2`)
## Author: Pablo Morala
###############################

renameCoefsPolyreg <- function(coefs_polyreg) {
  
  # Obtain the names
  names_polyreg <- names(coefs_polyreg)
  n <- length(names_polyreg)
  
  #Initialize a vector to store the new names
  names_betas <- rep("0", n)
  for (i in 2:n) {
    position <- stringi::stri_locate_all(pattern = "^", names_polyreg[i], fixed = TRUE)[[1]][, 2]
    vars <- strsplit(names_polyreg[i], "")[[1]][position - 1]
    exponents <- as.numeric(strsplit(names_polyreg[i], "")[[1]][position + 1])
    names_betas[i] <- paste(sort(rep(vars, times = exponents)), collapse = ",")
  }
  names(coefs_polyreg) <- names_betas
  return(coefs_polyreg)
}
