##=====Introduction=====================
## This R Script file contains 2 functions - makeCacheMatrix and cacheSolve
## At the end of the file, there are some test cases to validate the functions
## The objective is to cache the inverse of a matrix
## Assumption for the functions: matrix supplied is always invertible


##=====Some useful reminders=====================
# "solve" is a function in R to compute the inverse of a matrix.
# Example solve(A) will give you the inverse of A.

# <<- operator which can be used to assign a value to an object in an environment
# that is different from the current environment


#==============================================================================
## makeCacheMatrix function creates a special "matrix" object 
## that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  set <- function(y) {      #function to set the value of the matrix
    x <<- y
    m <<- NULL
  }
  
  get <- function() x      #function to get the value of the matrix
  setSolve <- function(solve) m <<- solve         #function to set the inverse of the matrix
  getSolve <- function() m                        #function to get the inverse of the matrix
  
  
  list(set = set, get = get, setSolve = setSolve, getSolve = getSolve)
}



#==============================================================================
## cacheSolve function computes the inverse of the special "matrix" returned by 
## makeCacheMatrix function.
## If the inverse has already been calculated (and the matrix has not changed),
## then the cachesolve should retrieve the inverse from the cache

cacheSolve <- function(x, ...) {
  m <- x$getSolve()
  if(!is.null(m)) {                     #check if matrix has been created via makeCacheMatrix function
    message("getting cached data")
    return(m)
  }
  data <- x$get()                       #proceed to compute the inverse of the parsed matrix
  m <- solve(data, ...)
  x$setSolve(m)
  m
  ## Return a matrix that is the inverse of 'x' parameter
}


##============Test Data================================================

#==============Test 1A==============
j <- makeCacheMatrix(matrix(c(1,2,3,4),2,2))
k <- cacheSolve(j)
k
#Test 1 Matrix
#     [,1] [,2]
#[1,]    1    3
#[2,]    2    4

#Test 1 Expected Results
#     [,1] [,2]
#[1,]   -2  1.5
#[2,]    1 -0.5

#==============Test 1B==============

j <- makeCacheMatrix(matrix(c(-2,1,1.5,-0.5),2,2))
k <- cacheSolve(j)
k
#Expected Results: This should give you the original j value in Test 1A

#==============Test 1C ==============

cacheSolve(j)

#Expected Results: This will give you the same results in Test 1B, 
#except that there is an addition line printed "getting cached data"
#As from Test 1B, this matrix has existed in cache.