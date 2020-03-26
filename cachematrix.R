## cachematrix.R allows the user to calculate the inverse matrix of a given matrix x, under the assumption
## that x is invertible. The user may also set/get the values of x and its inverse matrix through the extract
## operator $. Subsequently, the new inverse matrix generated in this way can be quickly recalculated and
## retrieved. The first function, makeCacheMatrix, creates an object in R that stores a matrix and the inverse
## of that matrix. cacheSolve requires an argument returned by makeCacheMatrix to retrieve the inverse matrix
## stored inside makeCacheMatrix's environment. 


## Creates a list containing the "getter/setter" functions. The "set" and "get" functions set/get the 
## matrix x, and the setIM and getIM do the same for the inverse of x. 

makeCacheMatrix <- function(x = matrix()) {
      IM = NULL
      set = function(y) {
           x <<- y
           IM <<- NULL
      }
      get = function() x
      setIM = function(solve) IM <<- solve
      getIM = function() IM
      list(set = set, get = get,
           setIM = setIM,
           getIM = getIM)
}


## Calculates the inverse matrix of the list created by makeCacheMatrix, but first checks whether the inverse
## matrix was already calculated. If IM already exists, prints a message and the IM. Otherwise, it gets the 
## matrix x, calculates the inverse, sets it, then returns it. If the matrix supplied into makeCacheMatrix 
## is not invertible, R will throw an error. 

cacheSolve <- function(x, ...) {
        IM = x$getIM()
        if(!is.null(IM)) {
              message("Retrieving from cache")
              return(IM)
        }
        data = x$get()
        IM = solve(data, ...)
        x$setIM(IM)
        IM
}
