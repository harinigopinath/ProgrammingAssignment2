## A pair of functions that cache the inverse of a matrix

## Creates a special matrix object that can cache its inverse

makeCacheMatrix <- function(m = matrix()) {
  ##initialize the inverse
  i<-NULL
  
  ##Method to set the Matrix
  set<-function(matrix){
    m<<-matrix
    i<<-NULL
  }
  
  ##Method to get the Matrix
  get<-function(){
    ##Return the Matrix
    m
    
  }
  
  ##Method to set the inverse of the matrix
  setInverse<-function(inverse){
    i<<-inverse
  }
  
  ##Method to get the inverse of the matrix
  getInverse<-function(){
    ##Return the Inverse Property
    i
  }
  
  ##Return list of methods
  list(set = set, get = get,setInverse = setInverse, getInverse = getInverse)
}


## Compute the inverse of the special matrix returned by "makeCacheMatrix"
## above. If the inverse has already been calculated (and the matrix has not
## changed), then the "cachesolve" should retrieve the inverse from the cache

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  m<-x$getInverse()
  
  ##Return the inverse if it is already set
  if(!is.null(m)){
    message("Getting Cached Data")
    return(m)
      }
  ##Get Matrix from the object
  data<-x$get()
  
  ##Calculate the inverse uing matrix multiplication
  m<-solve(data)%*%data
  
  ##Set the inverse to the object
  x$setInverse(m)
  
  ##Return the Matrix
  m
}

