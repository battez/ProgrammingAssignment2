# WEEK3 ~ ASSIGNMENT 
# by battez
# cachematrix.R
# Pair of functions which cache the inverse of a matrix. 
# example usage:
# cacheSolve(makeCacheMatrix(x = someInvertibleMatrix))
# 
# The first function, makeCacheMatrix creates a special "matrix" object, returning a list with
#  functions to
# set - the value of the matrix & reset cache
# get - the value of the matrix object
# 
# setinverse - sets the value of the matrix that is the inverse of matrix x
# getinverse - gets the value of the matrix that is the inverse of matrix x
# 
makeCacheMatrix <- function(x = matrix()) {

	# variable to store the inverse
	i <- NULL


	# function to invalidate the cache. 
	# It sets x to new value y. It also resets the stored inverse: i.
    set <- function(y) {

    	#both lines make use of "Superassignment" operator:
        x <<- y #looks outside the current scope starting with the parents environment.
        i <<- NULL #looks outside the current scope starting with the parents environment.
    }

    # simple getting of x.
    get <- function() x

    # function that sets i  to the "solved" passed to it
    setinverse <- function(solved) i <<- solved 

    # returns the inverse
    getinverse <- function() i

    # returns a list of functions: 
    # it is a holder for the functions to get and set the values, 
    # that are used by the partner function below, cacheSolve().
    list(set = set, get = get, setinverse = setinverse, getinverse = getinverse)
	


}


# computes the inverse of the  "matrix" inside the obj. returned by makeCacheMatrix()
#  (i.e. the list of functions). 
#  
# If the inverse has already been calculated (and the matrix has not changed), 
# then  cachesolve should retrieve the inverse from the cache.
#
# 
# NB assumes matrix supplied is invertible.
cacheSolve <- function(x, ...) {

	

	# Return any cached matrix we may have that is the inverse of 'x', 
	# by referencing the getinverse() function our partner function gave us.
	# 
	i <- x$getinverse()

	# if i was obtained with a value, we can just exit here 
	if(!is.null(i)) {
	   message("getting cached data")
	   return(i) #exit here
	}

	# if no cache exists,  we use the passed matrix to generate the inverse:
	data <- x$get()

	# R-function for obtaining the inverse of a matrix is solve(), assign this to i.
	# 
	i <- solve(data, ...)


	# we need to cache this before exiting:
	x$setinverse(i)

	# return  the inverse value:
	return(i)


}
