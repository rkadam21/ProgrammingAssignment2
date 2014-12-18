## The functions below calculate the inverse of an invertible matrix.
## The inverse is cached for later use.  This saves computation time
## every time an inverse of the given matrix is needed.

## Developer: rkadam21
## Date created: 12/16/2014
## Date Updated: 12/17/2014

#############################Start of Function#####################################################
## The makeCacheMatrix function below creates a special vector of type list 
##that stores calls to set, get, setinvrs and getinvrs functions.
## This function stores the inverse of the passed matrix in cache.
makeCacheMatrix <- function (x=matrix())
{
  i<-NULL  
  ## If a matrix isn't passed when the function is initialized, 
  ##one can be passed at later point in time
  ## by referencing the set function below.
  set <-    function(y) 
  {
    x <<- y
    i <<-NULL
  }
  ## If the inverse of the passed matrix isn't found in the cache, 
  ## the get function below can be used
  ## to get the matrix to perform run time inversion.
  get <-    function ()
  { x
  }
  ## The setinvrs function below is used to store the inverse (once calculated) 
  ## in the cache by using the <<- operator.
  setinvrs <- function(inverse)
  { i <<- inverse
  }
  ## The getinvrs function below is used to retrieve the inverse of a matrix from the cache.
  getinvrs <- function () {i}
  ## The list below is the return value of this function. 
  ##To reference the function calls in the calling function a $ operator should be used.
  list (set = set, get = get, setinvrs = setinvrs, getinvrs = getinvrs)
}
##################################End of Function##################################################

##################################Start of Function################################################
## The function below is used to retrieve the inverse of the passed matrix if one is stored in the cache.
## If the inverse isn't found then it calculates one in runtime and makes function calls to store it in the cache.
cacheSolve <- function(x, ...)
{
  ## The steps below are used to make a function call to retrieve the inverse of a matrix from the cache.
  ## if the inverse is found it returns the inverse as the output of the function
  i<-x$getinvrs()
  if(!is.null(i))        
  {
    message("getting cached data")
    return(i)
  }
  ## The step below is used to get the matrix (which needs to be inverted) 
  ## to invert it in runtime once the inversion isn't found in cache
  data <- x$get()
  ## The next 2 steps inverts the passed matrix and then makes 
  ##a function call to save the inversion in cache  
  i <- solve(data, ...)
  x$setinvrs(i)
  ## Returns the inversion of the passed matrix as output
  i
}
##################################End of Function####################################################