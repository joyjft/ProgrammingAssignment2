## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

makeCacheMatrix <- function(x = matrix()) {
  
  inv <- NULL # Initializing inverse matrix with null
  
  set<-function(newMatrix){
    x <<- newMatrix # Update old matrix
    inv <<- NULL # As matrix is updated, resetting inverse matrix 
  }
  
  get <- function() x # return matrix x
  
  set_inverse <- function(inverse) inv <<- inverse # Updating Inverse
  get_inverse <- function() inv
  
  list( set = set, get = get, set_inverse = set_inverse, get_inverse = get_inverse ) # Add all the function to the list which will be returned when we call makeCacheMatrix

}



## Write a short comment describing this function

cacheSolve <- function(x, ...) {
        inv <- x$get_inverse() # get inverse matrix from x
        if (!is.null(inv)){ # if inv is not null
          message("getting cached data")
          return(inv)
          
        }
        # if inv is null
        data <- x$get() # get the matrix
        inv <- solve(data) # compute inverse
        x$set_inverse(inv) # cache the inverse
        inv #return inv
        
        
        ## Return a matrix that is the inverse of 'x'
}


