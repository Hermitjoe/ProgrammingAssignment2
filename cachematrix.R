## Joe Matteson
## 11 NOV2020

## This assignment is to create 2 functions that can be used in tandem 
## Expected use:
## cacheSolve(makeCacheMatrix(my_CMatrix))       -- This function will induce the <<- operator

## Edit: This cannot be called in tandem like originally thought.
## Doing so never truly caches the inverse matrix, and instead you get null every run
## proper use would be: my_mCM <- makeCacheMatrix(my_Matrix)
##                      cacheSolve(my_mCM)

## Note: Used the given funcitons (makeVector, cachemean) as a road map for creating these
##       Though makeCacheMatrix is a function, it was easier to think about it as a class. commented as such below

## First function for the assignment: makeCacheMatrix 
##   As per the assignment, all passed matrices will be considered invertible.
##   This function creates globals of the passed matrix's info (structure/inverse)
##   The structure will mimic that of the example: makeVector  
makeCacheMatrix <- function(x = matrix()) {         ## Easier to think of this as a "class"
  invMatrix <- NULL                                 ## Default the inverse matrix incase called before acted on
  
  ## What to do if set is called on this "class"
  set <- function(p_Matrix) {                       ## This looks like a default item - "Constructor" 
    x <<- p_Matrix                                  ## Store the value of the passed matrix globally
    invMatrix <<- NULL                              ## AS nothing has been done, store Null for the inverse
  }
  
  ## What to do if get is called on this "class"
  get <- function() x                                ## Pulls the object X
  
  ## Create function that will handle the work of this "class"
  setinv <- function(inverse) invMatrix <<- inverse  ## Set the value of the inverse matrix
  getinv <- function() invMatrix                     ## Pulls the value of the inverse matrix
  
  ## List of possible actions that can be taken
  list(set = set, get = get, setinv = setinv, getinv = getinv)
}



## Write a short comment describing this function
##  cacheSolve: This function computes the inverse of the special "matrix" returned by 
##  This fucntio nwil lcheck for a stored global value, returns it if it exists, otherwise creates and stores one
##  The structure will mimic that of the example: cachemean
cacheSolve <- function(x, ...) {                     ## It is expected that this X is our special matrix type  
  invM <- x$getinv()                                 ## Pull the inverse saved to the special matrix "Class" 
  
  ## Perform a check to determine if we have a historic copy of the inverse matrix
  if (!is.null(invM)) {                              ## If there is a cached version 
    ##print("In if")
    message("getting cached data")                   ## Tell user we are using the cached version
    return(invM)                                     ## Return the cached value to the calling procedure
  }
  
  ##print("OO if")
  data <- x$get()                                    ## Pull current matrix from the "class"
  inv <- solve(data, ...)                            ## Create the inverse of the matrix using solve
  x$setinv(inv)                                      ## Save a copy of the inverse for future calls
  
  #invM <- x$getinv()                                ## Used to test the save to global  
  #print(invM)                                       ##  was working as expected
  
  return(inv)                                        ## Return the newly created inverse
}
