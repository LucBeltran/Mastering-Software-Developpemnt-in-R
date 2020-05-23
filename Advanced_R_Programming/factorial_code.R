### Import Library
library(purrr)
library(microbenchmark)

### Building Functions
# loop version
Factorial_loop = function(n) {
  stopifnot(n >= 0)
  
  if(n == 0){ans = 1}
  
  else{
    ans = 1
    for (i in 1:n){ans =  ans * i}
  }
  ans
} 

# reduce version
Factorial_reduce = function(n) {
  stopifnot(n >= 0)
  
  if(n == 0){1}
  
  else{Reduce('%*%', 1:n)[1]}
} 


# recursive version
Factorial_func = function(n) {
  stopifnot(n >= 0)
  
  if(n == 0){1}
  
  else{n*Factorial_func(n-1)}
} 


# memoization version
fac_table <- c(1)

Factorial_mem <- function(n) {
  stopifnot(n >= 0)
  
  if (n > length(fac_table)) {
    fac_table[n-1] <<- Factorial_mem(n-1)
    fac_table[n-1] * n}
  
  else{fac_table[n]}
} 



### Test if all function return same results

test_functions <- function(l){
  for (n in l){
    if(Factorial_func(n) == Factorial_loop(n) & 
       Factorial_func(n) == Factorial_mem(n) &
       Factorial_func(n) == Factorial_reduce(n)){next()}
    else {return('error')}
  }
  "All factorial function have same outputs"
}




input = c(1, 50, 100, 500, 1000)
individual_results <- map(input, ~ microbenchmark(
  Factorial_loop(.),
  Factorial_reduce(.),
  Factorial_func(.),
  Factorial_mem(.)
))










##### Generate output file
sink('factorial_output.txt')

cat("====== TEST IF ALL FUNCTIONS HAVE SAME OUTPUTS ======\n")
cat("\n")
test_functions(1:100)

cat("\n")
cat("\n")
cat("\n")

cat("====== BENCHMARK RESULTS FOR DIFFERENT INPUT ======\n")
cat("\n")
cat("n = 1\n")
individual_results[1]
cat("\n")
cat("n = 50\n")
individual_results[2]
cat("\n")
cat("n =100\n")
individual_results[3]
cat("\n")
cat("n = 500\n")
individual_results[4]
cat("\n")
cat("n = 1000\n")
individual_results[5]

sink()
