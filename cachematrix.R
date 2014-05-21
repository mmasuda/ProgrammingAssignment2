## This function creates a special "matrix" object that can cache its inverse.
## 
## set          : set the matrix into cashe and initialize it's inverse as NULL
## get          : get the current matrix
## setinverse   : set the inverse matrix in attribute into cashe
## getinverse   : get the inverse matrix
## 
## argument     : x is a matrix
## return       : list of the functions above
##

makeCacheMatrix <- function(x = matrix()) {
        
        ## check x is a square invertible matrix to run solve() in cacheSolve()
        ## if not, issue error message and stop running
        ## and please re-assign x before run cacheSolve()
        if (nrow(x) != ncol(x)){
                stop(message="argument is not a square matrix")
        }
        if (det(x) == 0){
                stop(message="argument is not a invertible matrix")
        }
        
        ## initialize cashe of inverse matrix
        inv_x <- NULL
        
        ## set function
        set <- function(y) {
                x <<- y         ## set the matrix into cashe               
                inv_x <<- NULL  ## initialize it's inverse as NULL
        }
        ## get function
        get <- function() x
        
        ## setinverse function        
        setinverse <- function(inv) inv_x <<- inv ## set the inverse into cashe
        
        ## getinverse function         
        getinverse <- function() inv_x
        
        ## return list of functions set/get special matrix        
        list(set = set, get = get,
             setinverse = setinverse,
             getinverse = getinverse)
}

## This function computes the inverse of the special "matrix" returned by makeCacheMatrix above. 
## If the inverse has already been calculated (and the matrix has not changed), 
## then the cachesolve should retrieve the inverse from the cache.
## 
## 
## argument     : x ; the special "matrix" returned by makeCacheMatrix
## return       : the inverse matrix of x
##

cacheSolve <- function(x, ...) {
        
        ## check the inverse has already been calculated
        x_inv <- x$getinverse()
        if(!is.null(x_inv)) {
                message("getting cached data")
                return(x_inv)   ## return alredy calculated inverse matrix
        }
        
        ## calculate inverse matrix
        data <- x$get()
        x_inv <- solve(data)
        x$setinverse(x_inv)
        
        ## Return a matrix that is the inverse of 'x'
        x_inv
}