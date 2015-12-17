# STAT 243 Fall 2015 - Final Group Project
## ars-stat243
This repo (ars-stat243) contains the source code for the ars.tar.gz R Package for the STAT243 
Final project (Fall 2015)

## Purpose
To build a fully documented Adaptive Rejection Sampling (ARS) Function as an R Package.

## Team
The team members are listed below (alphabetically) by surname:

* Jonathan Larson     [JL]
* Chenzhi Li          [CL]
* Courtney Schiffman  [CS]
* Shamindra Shrotriya [SS]

## Installation Guide
### Run the following commands in `bash` at the command line
1. `export ars_clone_path=/home/oski/Documents # Change this to a location of your choice`
2. `cd $ars_clone_path # change to the clone path dorectory specified`
3. `git clone https://github.com/shamindras/ars.git`
4. `cd ars    # change to the ars directory`
5. `rstudio . # launch rstudio from the command line`

### Run the following commands once in `Rstudio`
1. `getwd() # Check that the working directory is the same as $install_path/ars`
2. `install.packages("./ars.tar.gz", repos=NULL, type="source") # Install ars package`
- Warning: If this results in the following error:
  `ERROR: dependency ‘numDeriv’ is not available for package ‘ars’`
- Then run the following commands in Rstudio
  - `install.packages(c("numDeriv", "testthat"))`
  - `library(numDeriv); library(testthat)`
- Rerun
  - `install.packages("./ars.tar.gz", repos=NULL, type="source")`
  - The ars package should now be installed with required dependencies
  - NOTE: This installation of dependencies from source is a [known issue](https://github.com/ropensci/plotly/issues/247) with no clearly defined solution for all operating systems
3. `library(numDeriv); library(testthat) # Load the dependencies`
4. `library(ars) # Load the ars package`
4. `library(testthat) # Load testthat`
5. `test_package('ars', 'main') # Run the tests`

If the above steps have executed without error, then the `ars` function is ready to use! 

*Happy Adaptive Rejection Sampling!*

## Assignment Instructions
* The assignment instructions are stored in the *project.pdf* document in the **reference** folder of this repo.

## Background and Info
* Team document is [here](https://docs.google.com/document/d/1quckIl2wkElgZmsXXVHc1pX3S2YVGmn3irN-tY27MtA/edit#heading=h.b3zrv9tb4yvu)
* The original ARS paper is stored in *gilks.etal.1992.pdf* document in the **reference** folder of this repo.