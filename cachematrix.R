# Description:
# The below 2 functions makeCacheMatrix and cacheSolve provide 
# way to set a Matrix object and cache the inverse of a matrix.
# This function will help you to avoid re execution of matrix inverse function call
# and reuse the previously computed matrix inverse object
# Usage : CALL function "mSpecialObj = makeCacheMatrix()"
#         SET the Matrix object "mSpecialObj$set(matrixobject)"
#         TO GET Inverse of the Matrix object ( cache or new generation) call
#                       "cacheSolve(mSpecialObj)"



######################################################################
# Function : makeCacheMatrix
# Arguments : 
#     x   Input Matrix 
# Return Value :
#     List containing function to SET & GET the value of Matrix 
#       and SET & GET value of Matrix inverse 
# Details : This function creates a special "matrix" object that can cache its inverse.
makeCacheMatrix <- function(x = matrix()) {
  #Initialize the value of Matrix inverse to null
  i <- NULL
  
  #Set function to assign the value of Matrix object
  set <- function(y) {
    x <<- y
    i <<- NULL
  }
  
  #Get function to retrive stored matrix object
  get <- function() x
  
  #Set inverse of the Matrix object
  setInverse <- function(inverse) i <<- inverse
  
  #Get the inverse of the Matrix object
  getInverse <- function() i
  
  #Return the list of functions for the matrix 
  list(set = set, get = get,
       setInverse = setInverse,
       getInverse = getInverse)
  
}
######################################################################




######################################################################
# Function : cacheSolve
# Arguments : 
#     x   special "Matrix" object returned by  makeCacheMatrix function
# Return Value :
# Inverse value of the Matrix ( For the special Matrix object provided as input )  
# Details : This function computes the inverse of the special "matrix" 
#           returned by makeCacheMatrix above. If the inverse has already been calculated
#           (and the matrix has not changed), this function retrieves the inverse from the #           cache.
cacheSolve <- function(x, ...) {
  #Get the inverse of the Matrix
  i <- x$getInverse()
  
  #If the object value is not null means the special matrix object's Inverse is already present, so use it  
  if(!is.null(i)) {
    message("getting cached matrix")
    return(i)
  }
  
  #Inverse for the Matrix is not present 
  #Needs to be computed
  matrix <- x$get()
  i <- solve(matrix)
  
  #Set the Matrix inverse, so that we don't have to reexecute next time 
  x$setInverse(i)
  
  #Return the Matrix inverse 
  i
}
