###speed up the r code
library(microbenchmark)
set.seed(4321)
col1 = runif(12 ^ 6, 0, 2)
col2 = rnorm(12 ^ 6, 0, 2)
col3 = rpois(12 ^ 6, 3)
col4 = rchisq(12 ^ 6, 2)
df = data.frame(col1, col2, col3, col4)
original = microbenchmark({
  myfunc = function(df) {
  #  for (i in 1:nrow(df)) {
      if ((df['col1'] + df['col2'] + df['col3'] + df['col4']) > 4) {
       'greater than 4'
        
      }
      else{
        'smaller than 4'
      }
    }
 # } 
  output=apply(df[,c(1:4)],1,FUN=myfunc)
  df$output=output
  },
  times = 10L)
