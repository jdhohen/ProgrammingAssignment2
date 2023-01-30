#cachematrix.R
#Comments are scattered throughout the code to help understand what is going on


#makeCacheMatrix is a list to set/get value of matrix (likely defined by the user) and its inverse.
makeCacheMatrix <- function(x = matrix()) { #generating a matrix placeholder, which we are calling x
    m <- NULL #m will be the cached value
    set <- function(y) { #setting the variables for the parent environment
        x <<- y
        m <<- NULL 
    }
    get <- function() x #x is outside () because it doesn't exist within this environment, only the parent environment
    setinverse <- function(solve) m <<- solve #use solve() for calculating inverse of matrix. Could try solve.default() if "error: 'a' argument missing with no default" try ?solve. Sometimes I got errors no matter which one I used so I just restarted RStudio and it worked...
    getinverse <- function() m #m is outside () because it doesn't exist within this environment, only the parent environment
    list(set = set, get = get, #do a list because then you can use $ operator in a list, otherwise you have to use [[]]
         setinverse = setinverse,
         getinverse = getinverse) 
}


#cacheSolve calculates the inverse of the matrix created with makeCacheMatrix(). First check whether the information is cached by makeCacheMatrix() and if not, then do the calculation next.
cacheSolve <- function(x, ...) { #Make the function and allow other arguments (...)
    m <- x$getinverse() #calls getinverse() function on the input object within x and assigns to m
    if(!is.null(m)) {       #this if statement is saying: "if m exists" or "if m isn't null (i.e. FALSE), grab the cached data from makeCacheMatrix()"
        message("getting cached data")
        return(m)
    }
    data <- x$get()         #I'd liken this to "if the former statement was FALSE, calculate the correct thing I'm asking you to do (which, in this case, is calculate the matrix's inverse)
    m <- solve(data, ...) #use solve() to calculate the inverse
    x$setinverse(m)
    return(m)
}