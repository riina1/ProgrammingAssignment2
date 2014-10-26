## This function creates a special "matrix" object that can cache its inverse


makeCacheMatrix <- function(x = matrix()) {

  m <- NULL       ## Creates variable in local environment
  
  set <- function(y) {        
    
    x <<- y       ## Supperassigns value of y to x, where x is makeCacheMatrixr()'s input
    
    m <<- NULL    ## If set() is called, a new vector will be stored in x,
                  ##replacing the existing value
    
  }
  
  get <- function() x            ## This can be called to retrieve the values 
                  ##of x from makeCacheMatrix.cacheSolve will call on this 
                  ##function to provide it with values needed for computation
  
  setmatrix <- function(solve) m <<- solve      
                  ## This sets m to iniverse in the parent env (makeCacheMatrix())
  
  getmatrix <- function() m       ## R will look for the value of m in 
                  ##getmatrix()R won't find one within getmatrix, so it'll look 
                  ## to the parent environment makematrix for m value
  
  list(set = set, get = get,          ## This is needed to make functions public
              setmatrix = setmatrix,  ## Essentially, this allows these functions to be called
              getmatrix = getmatrix)  ## outside the local environment
  
}



## This function computes the inverse of the special "matrix" returned by 
## makeCacheMatrix above. If the inverse has already been calculated 
##(and the matrix has not changed), then the cachesolve should retrieve 
##the inverse from the cache.

cacheSolve <- function(x=matrix(), ...) {
        ## Return a matrix that is the inverse of 'x'
 
    
    m <- x$getmatrix()          ## This calls the getmean() function from x. 
                                ##x is a container environment that consists of 
                      ##a list of functions and variables defined in the 
                      ##makeCacheMatrix() environment.
    
    if(!is.null(m)) {           ## This checks if m has an existing value. 
                                ##If TRUE, then return that value.
      
      message("getting cached data")
      
      return(m)              ## If there's no existing value, this function 
                            ## will calculate it below
      
    }
    
    matrix <- x$get()         ## Calls get() function from x, which 
                              ##won't have an m value
    
    m <- solve(matrix, ...)   ## Computes mean from the retrieved values via x$get()
        x$setmatrix(m)        ## With the new inverse, setmatrix() will be 
                              ##called to to update m in makeVector()
                              ## Recall: setmatrix() supperassigns m in 
                              ##makeCacheMatrix(), thus "caching" the value
  
    m                         ## Prints the new inverse  
}
