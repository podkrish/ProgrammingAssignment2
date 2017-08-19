## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

#The makeCacheMatrix function takes a matrix as an input argument

makeCacheMatrix <- function(x = matrix()) {
   inv <- NULL
   ##It sets the value of x from y
   set <- function(y)
   {
      x <<- y
      inv <<- NULL
   }
   ##The get function reads the value of x
   get <- function() x
   #The setinv function takes a matrix as input and assign it as the inverse of the input matrix "x".
   setinv <- function(inverse) inv <<- inverse
   #The getinv function reads the inverse of the matrix
   getinv <- function() inv
   #Put all the function definitions into a list and display it on the terminal
   list(set=set,get=get,setinv=setinv,getinv=getinv)
}


## Write a short comment describing this function

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
   # If the inverse is already present in the cache, return that matrix.
   inv <- x$getinv()
   if(!is.null(inv))
   {
      message("Getting Cached Inverse of Matrix")
      return(inv)
   }
   # Else, freshly calculate the inverse of the matrix x.
   else
   {
      matrix_data <- x$get()
      # solve() function gets the inverse of the matrix x.
      inv <- solve(matrix_data, ...)
      x$setinv(inv)
      return(inv)
   }
}