## PROGRAM OBJECTIVE: Evaluating the blockwise reduced modeling (BRM) method and benchmark
#### methods for handling blockwise missing patterns on the three complete datasets: 
#### BIKE, HOUSE, and ADULT

### NOTE: In each of the modules corresponding to the datasets (bike_.R, adult_.R, and house_.R) 
#### try() has been used to continue executing code despite errors in execution of any method
#### This is by design as we show in the paper that some methods do not execute for certain simulations 
#### while others do.  


## Load the R packages:
source("/root/capsule/code/Load_packages.R")
## Load functions for computing prediction performance, SVI imputing train/test data, etc. 
source("/root/capsule/code/Preliminary_functions.R")
## Load functions for STOA methods: 
source("/root/capsule/code/Benchmarks.R")
## Load functions that will be called by the BRM method - creating overlapping subsets, 
#### training candidate models, making predictions 
source("/root/capsule/code/BRM_functions.R")


## BIKE dataset: 
source("/root/capsule/code/bike_.R")
## HOUSE dataset: 
source("/root/capsule/code/house_.R")
## ADULT dataset: 
source("/root/capsule/code/adult_.R")

### Scalability analysis:
source("/root/capsule/code/scalability.R")


