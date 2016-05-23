############## Testing running time
rm(list = ls())

setMKLthreads(2) # set number of cores
getMKLthreads() # number of threads, needs to be less than number of cores 

StartTime <- Sys.time()

#NofElement <- c(10000, 1000000, 25000000, 64000000, 40000, 9000000, 16000000, 81000000)
#NofLoop <- 6
NofElement <- c(81000000)
for (i in 1:length(NofElement)){
  Mtr <- c()
  Mtr <- matrix(rnorm(NofElement[i], mean = 0, sd = 1), ncol = sqrt(NofElement[i]))
  svd(Mtr)
}

UsedTime <-Sys.time() - StartTime