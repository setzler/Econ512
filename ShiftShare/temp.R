library(devtools) # you may need to install.packages("devtools")
create("/Users/bqs5754/github/Econ512/examplePackage/temp") # choose Yes if prompted
setwd("/Users/bqs5754/github/Econ512/examplePackage/temp")
# you now have a skeleton package in the specified location

## we now modify the DESCRIPTION file to include the following:
use_mit_license("Bradley Setzler")
