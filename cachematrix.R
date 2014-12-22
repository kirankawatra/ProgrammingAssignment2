##These two function create, store and recall a matrix and its inverse in/from cache  


## makeCacheMatrix creates custom matrix type capable of running four functions
## set stores the matrix in cache, get recalls the matrix
## setInverse and getInverse do the same but for the inverse of the original matrix
makeCacheMatrix <- function(x = matrix()){    
  m <- NULL
  set <- function(y){
    x <<- y  
    m <<- NULL #store matrix in cache 
  }
  get <- function() x #get matrix
  setInverse <- function(solve) m<<- solve #set inverse matrix
  getInverse <- function() m #get inverse matrix
  list(set = set, get = get,
       setInverse = setInverse,
       getInverse = getInverse)  ## create list of functions
}

## cacheSolve take a custom matrix type created by the makeCacheMatrix function
## and calculates the inverse matrix of it
## but first it checks to see if the calculation has been done before
## if it has been done before it recalls the data from the cache. If it has not been done 
## before it calculates the inverse matrix then store it in the cache

cacheSolve <- function(x, ...) {
  
  m <- x$getInverse()                
  if(!is.null(m)){                    
    message("getting cached data")    
    return(m)                         
  }
  data <- x$get()                    
  m <- solve(data, ...)               
  x$setInverse(m)                    
}