## makeCacheMatrix creat a list of a function to 
## a. set the the matrix, and get it   
## b. sey the inverse matrix. and get it 

makeCacheMatrix<-function (x=matrix()){
	inv<-NULL
	set<-function(y){
		x<<-y
		inv<<-NULL
	}
	   get <- function() x
        setInv <- function(inverse) inv <<- inverse
        getInv <- function() inv
        list(set = set, get = get,
             setInv = setInv,
             getInv = getInv)
}     


 
## function returns the inverse of the matrix 
## 1. check if the inverse matrix has been computed  
## 2. If so, gets the result
## 3. ortherwise, it computes teh inverse, sets the value in the cache via setinv()

cacheSolve<-function(x,...){
	inv<-x$getInv()
	if (!is.null(inv)){
		message("cached data:")
		return(inv)
	}
	mat<-x$get()
	inv<-solve(mat,...)
	x$setInv(inv)
	inv
}

