###speed up the r code
library(microbenchmark)
library(data.table)
set.seed(4321)
col1 = runif(12 ^ 4, 0, 2)
col2 = rnorm(12 ^ 4, 0, 2)
col3 = rpois(12 ^ 4, 3)
col4 = rchisq(12 ^ 4, 2)
df = data.frame(col1, col2, col3, col4)
dim(df)

###method1
original = microbenchmark({
  for (i in 1:nrow(df)) {
    if ((df[i, 'col1'] + df[i, 'col2'] + df[i, 'col3'] + df[i, 'col4']) > 4) {
      df[i, 5] = 'greater than 4'
    } else{
      df[i, 5] = 'smaller than 4'
    }
  }
}, times = 10L)
#####method 1   vectorization and pre-allocation
#### generate the result sperate and use cbind-like method to format the dataframe
output = character(nrow(df))
output
perallocate = microbenchmark({
  for (i in 1:nrow(df)) {
    if ((df[i, 'col1'] + df[i, 'col2'] + df[i, 'col3'] + df[i, 'col4']) > 4) {
      output[i] = 'greater than 4'
    } else{
      output[i] = 'smaller than 4'
    }
  }
  df$output = output
}, times = 10L)
##### use apply family
###more powerful
apply_out = microbenchmark({
  myfun = function(x) {
    if ((x['col1'] + x['col2'] + x['col3'] + x['col4']) > 4) {
      'greater than 4'
    }{
      'smaller than 4'
    }
  }
  output = apply(df[, c(1:4)], 1, FUN = myfun)
}, times = 10L)
####takeing the condition out side the loop
condition = (df$col1 + df$col2 + df$col3 + df$col4) > 4
head(condition)
condition_outside = microbenchmark({
  for (i in 1:nrow(df)) {
    if (condition[i]) {
      output[i] = 'greater than 4'
    } else{
      output[i] = 'smaller than 4'
    }
  }
  df$output = output
}, times = 5L)
### custom the forloop
output1 = rep('smaller than 4', nrow(df))
only_trues = microbenchmark({
  for (i in(1:nrow(df)) [condition]) {
    if (condition[i]) {
      output1[i] = 'great than 4'
    }
  }
  df$output1 = output1
}, times = 10L)
####use ifelse
output = character(nrow(df))
ifelse_output = microbenchmark({
  output = ifelse(df$col1 + df$col2 + df$col3 + df$col4 > 4,
                  'greater than 4',
                  'smaller than 4')
  df$output = output
}, times = 10L)
###which family
which_smart=microbenchmark({
  want=which(rowSums(df)>4)
  output=rep('smaller than 4',nrow(df))
  output[want]='great than 4'
    
},times=10L)
which_smart
######
df$output=''
set_output=microbenchmark({
  for(i in 1: nrow(df)){
    if((df[i,'col1']+df[i,'col2']+df[i,'col3']+df[i,'col4'])>4){
      set(df,i,5L,'greater than 4')
      }
      else{
        set(df,i,5L,'smaller than 4')
      }
    }
   
},times=10L)
set_output
############challenge
m=matrix(0,nrow=10000,ncol=10000)
df=as.data.frame(m)
 
original=microbenchmark({
  for(i in 1:nrow(df)){
    df[i,i]=i
  }
},times=10L)
 
m=matrix(0,nrow=10000,ncol=10000)
df=as.data.frame(m)
library(doParallel)
cl <- makePSOCKcluster(8)
registerDoParallel(cl) 
parallel=microbenchmark({
  for(i in 1:nrow(df)){
    df[i,i]=i
  }
},times=10L)
stopCluster(cl)
m=matrix(0,nrow=10000,ncol=10000)
df=as.data.frame(m)
set_output=microbenchmark({
  for(i in 1: nrow(df)){
   set(df,i,i,i) 
  }
},times=10L)
 original
 parallel
 set_output