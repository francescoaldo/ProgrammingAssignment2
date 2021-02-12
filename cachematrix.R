setwd("/Users/francescoaldo/Desktop/COURSES MATERIAL/R DATA SCIENCE - Coursera/ProgrammingAssignment2") 

## https://github.com/francescoaldo/ProgrammingAssignment2.git 

## This file contains two functions that compute and cache 
## the inverse of a given matrix, so as to save computing time  

## The two functions closely follow the example provided (for mean of a vector) 
## Only minor adjustments (apart 'solve' instead of 'mean') were necessary! 

## As per instructions, 'makeCacheMatrix' creates a special “matrix” object 
## that can cache its inverse 

makeCacheMatrix <- function( mat = matrix() ) { 
  inv <- NULL 
  ## Im following the same structure as in the example provided: 
  ## get, set, getmean, setmean and then list them all (with inv instead of mean) 
  set <- function(y) { 
    ## The "double assignment" operator <<- allows to refer to a different environment 
    mat <<- y 
    inv <<- NULL 
  } 
  get <- function() {mat} 
  setinv <- function(inverse) {inv <<- inverse} 
  getinv <- function() {inv} 
  list(set = set, get = get, setinv = setinv, getinv = getinv) 
} 
## END of function makeCacheMatrix 

## As per instructions, the second function computes the inverse of the 
## special “matrix” returned by makeCacheMatrix above. 
## If the inverse had already been calculated, and the matrix has not changed, 
## cacheSolve should retrieve the inverse from the cache. 
## We can obtain the inverse of a square invertible matrix with the 'solve' function. 
## (Given a square invertible matrix M, solve(M) returns its inverse) 

cacheSolve <- function(mat, ...) { 
  ## We "load" (assign) whatever is in mat$getinv() into inv 
  inv <- mat$getinv()  
  ## The if statement check whether inv is null ("empty") or not: 
  ## In other words, it checks whther the inverse inv has already been cached and calculate. 
  ## If yes, it returns the cached computation. 
  ## Otherwise, it performs all the hard work calculating it with 'solve'! 
  if(!is.null(inv)) { 
    message("getting cached data") 
    return(inv) 
  } 
  ## We use 'data' as a sort of placeholder for the matrix whose inverse we'll compute 
  data <- mat$get() 
  ## Calculating the inverse... 
  inv <- solve(data, ...) 
  ## ...and return ing the calculation (which will be cached accordingly!) 
  mat$setinv(inv) 
  inv 
} 
## END of function cacheSolve 





