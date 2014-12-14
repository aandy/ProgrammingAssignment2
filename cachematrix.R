## [The function is created to take the inverse of the matrix. This function will have two parts.
##
##In part 1 of function, input will be a matrix and its inverse will be intialize to a NULL matrix 
##everytime the function makeCacheMatrix is called. Part 1, will further be subdivided 
##into 4 section. Setction 1 (makeCacheMatrix$set) sets the matrix, section 2(makeCacheMatrix$get)
##gets the matrix, section 3 (makeCacheMatrix$setinverse) sets the inverse of input matrix, 
##section 4(makeCacheMatrix$getinverse) gets the inverse of matrix
##
##In part 2 of function, cacheSolve function input will be the object created by makeCacheMatrix  
## if the stored inverse matrix is not identical with the null matrix - matrix() then inverse will
## be pulled from the cache. Else, a new object will get the input matrix data and R inbuilt
##solve function will calculate the inverse of the matrix and set the inverse of matrix in maekCacheMatrix
##function and finally retrun the matrix inverse]
##
##Code has also been heavily commented to provide the detailed flow of the R program.

makeCacheMatrix <- function(x = matrix()) {      # input x will be a matrix

    m <- matrix()    #  m will be our inverse and it's reset to NULL matrix every 
                 #    time makeCacheMatrix is called

                 #  note these next three functions are defined but not run when makeCacheMatrix is called.
                 #   instead, they will be used by cacheSolve() to get values for x or for
                 #   m (inverse) and for setting the inverse.  These are usually called object 'methods'
    set <- function(y = matrix()) {	# takes an input matrix
                x <<- y		# saves the input input matrix in global environment
                m <<- matrix()	# resets the inverse to 0*0 matrix, which is what happens when a new object is generated via the makeCacheMatrix.
        }
    get <- function() { x }   # this function returns the value of the original matrix

    setinverse <- function(inverse)  { m <<- inverse }
                # this is called by cacheSolve() during the first cacheSolve()
                                #  access and it will store the value using superassignment
                                       
    getinverse <- function() { m } # this will return the cached matrix to cacheSolve() on
                                #  subsequent accesses

    list(set = set, get = get,          #  This is accessed each time makeCacheMatrix() is called,       
         setinverse = setinverse,  #   that is, each time we make a new object.  This is a list of 
         getinverse = getinverse)  #   the internal functions ('methods') so a calling function
                             #   knows how to access those methods.                            
}


cacheSolve <- function(x, ...) {   # the input x is an object created by makeCacheMatrix
    m <- x$getinverse()               # accesses the object 'x' and gets the value of the inverse
    if(!identical(m,matrix())) {              # if inverse was already cached (not IDENTICAL to NULL matrix) ...

        message("getting cached data")  # ... send this message to the console
        return(m)                       # ... and return the inverse ... "return" ends 
                        #   the function cacheSolve()
    }
    data <- x$get()        # we reach this code only if x$getinverse() returned Identical NULL matrix
    m <- solve(data, ...)   # if m was identical NULL matrix then we have to calculate the inverse
    x$setinverse(m)           # store the calculated inverse value in x (see setinverse() in makeCacheMatrix
    m               # return the inverse to the code that called this function
        ## Return a matrix that is the inverse of 'x'
}
