####learn how to use doParallel
library(doParallel)
# cl = maleCluster(8)
# #makePSOCKcluster
# 
# registerDoParallel(cl)
# stopCluster(cl)
data(Prestige,package='car')
system.time({
 output=matrix(0,0,7) 
for(i in 1:10000){
  rownums=sample(102,70,replace = T)
  mod=glm(formula=prestige~education+type+income,data=Prestige[rownums,])
  output=rbind(output,broom::glance(mod))
  }
 })

cl=makeCluster(4)
registerDoParallel(cl)

system.time({
  output_par=foreach(i=1:10000,.combine=rbind,.options.snow=list(preschedule=T))%dopar%{
    rownums=sample(102,70,replace = T)
    mod=glm(formula=prestige~education+type+income,data=Prestige[rownums,])
    output= broom::glance(mod)
    return (output)
  }
}) 
stopCluster(cl)
