## Put comments here that give an overall description of what your
## functions do

## This is the primary function that creates the special matrix 
# for caching invertible
makeCacheMatrix <- function(x = matrix()) {
  # defined the variable responsible for storing the inverse value
  m_inverse <- NULL
  
  # create a funcion to establish data for the matrix
  # as new value is setted, the inverse variable is resetted
  set <- function(newMatrix){
    x <<- newMatrix
    m_inverse <<- NULL
  }
  
  # functions defined to return the original matrix data
  get <- function() x
  # function created to indicate the cacheable matrix what is its calculated inverse
  setInverse <- function(newInverse) m_inverse <<- newInverse
  # functions defined to return the inverted matrix data
  getInverse <- function() m_inverse
  
  # return list of functions of the created cacheable matrix
  list(set = set, get = get, setInverse = setInverse, getInverse = getInverse)
}

#Secondary function created to perform the inverse calculation of the matrix and
#storing its cache
cacheSolve <- function(x, ...) {
  # defined temporary variable to store calculated inverse
  mi_temp <- x$getInverse()
  
  # tests whether the inverse was already calculated and,
  # if positive, returns its value
  if(!is.null(mi_temp)){
    return(mi_temp)
  }
  
  # retrives the original matrix data to perform validation and calculations
  m_original <- x$get()

  # extra validation to identify whether the original matrix is squared
  # case negative, prints message indicating the non-conformity and
  # returns NULL
  if(nrow(m_original) != ncol(m_original)){
    message("Not invertible matrix!")
    return(NULL)
  }
  
  # calculates the inverse matrix and stores it on the
  # temporary variable defined at the beginning of the function
  mi_temp <- solve(m_original)
  
  # tests whether there is a valid inverse
  # case negative, a message is shown informing the problem
  # and returns NULL
  if(is.null(mi_temp)){
    message("Not invertible matrix!")
    return(NULL)
  }
  
  # indicates to the cacheable matrix its inverse value
  x$setInverse(mi_temp)
  
  # returns de inverse value
  mi_temp
}