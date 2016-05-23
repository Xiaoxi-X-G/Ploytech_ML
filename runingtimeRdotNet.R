runingtimeRdotNet <-function(){

############## Testing running time
rm(list = ls())

setMKLthreads(2) # set number of cores
getMKLthreads() # number of threads, needs to be less than number of cores 

StartTime <- Sys.time()

NofElement <- 25000000
NofLoop <- 1
for (i in 1:NofLoop){
  Mtr <- c()
  Mtr <- matrix(rnorm(NofElement, mean = 0, sd = 1), ncol = sqrt(NofElement))
  svd(Mtr)
}

UsedTime <-Sys.time() - StartTime
plot(UsedTime)

return(UsedTime)
}