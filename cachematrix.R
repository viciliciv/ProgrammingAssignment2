##Create a function that takes a matrix and calcuates the mean after Matrix Inversion
##Additionally, a function will cache the inverted matrix and the mean of the inverted matrix to prevent 
##repeatedly computing the same inverted matrix

##makecachematrix:
#set value of matrix 
#get value of matrix 
#set the value of the inverted matrix
#get the value of the inverted matrix

makeCacheMatrix <- function(x = matrix()) {
        inverted <- NULL
        #set value of matrix 
        set <- function(y) {
                x <<- y
                inverted <<- NULL
        }
        #get value of matrix 
        get <- function() x
        #set the value of the inverted matrix
        set_invert <- function(invert) inverted <<- invert
        #get the value of the inverted matrix
        get_invert <- function() inverted
        list(set = set, get = get,
             set_invert = set_invert,
             get_invert = get_invert)
}
        
##CacheSolve
##Inverts the matrix inputted and determines if it has been cached or not
##calls get_invert() to check if the inverted data is stored. 
##If get_invert() is not null, then prints cached inverted matrix
##If get_invert() is null, then new matrix will be inverted and cached by set_inverted()

cacheSolve <- function(x, ...) {
        inverted <- x$get_invert()
        if(!is.null(inverted)) {
                message("getting cached data")
                return(inverted)
        }
        data <- x$get()
        inverted <- solve(data)
        x$set_invert(inverted)
        inverted
}
