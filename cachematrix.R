## Note 
## no 'setInv' function define as in the 'mean' example
## seting the mean manually enable to give stupid value by hand 
## ... really dangerous capacity

makeCacheMatrix <- function(x = matrix()) {
  ## enhanced Matrix: memoization added
  
  invMat <- NULL
  
    ## Accessor --------------------------
    set = function(y) {
      x <<- y
      invMat <<- NULL
    }
    
    get = function() {x}
    
    ## the Cache solve part -------------
    getInv = function() {
      
      ## retrieve or compute
      if (is.null(invMat)) {
        invMat <<- solve(x)
      }
      else {
        message("get cached value")
      }
      
      return(invMat)}
  
  return(list(set=set, get=get, getInv=getInv))

}

## Expose the memoize solve as a direct function
cacheSolve <- function(x, ...) {
  ## Expose the memoize solve as a direct function
  
  ## Return a matrix that is the inverse of 'x'
  x$getInv()
}

# Test it ------------------
# mm = makeCacheMatrix(matrix(1:4,2,2))
# cacheSolve(mm)
# cacheSolve(mm)
# mm$set(matrix(4:1,2,2))
# mm$get()
# cacheSolve(mm)
# cacheSolve(mm)

