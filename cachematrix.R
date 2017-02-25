## This script contains two functions:
## makeCacheMatrix is function to create a special "matrix" object that can cache its inverse
## cacheSolve is a function that computes inverse of matrix if it has not been calculated, otherwise it retreives cached value
##  
##-----------------------------------------------------------
## makeCacheMatrix
## Creates a special "matrix" which is really a list that:
##
## 1) sets the value of the Matrix
## 2) gets the value of the Matrix
## 3) sets the value of the Inverse of the Matrix
## 4) gets the value of the Inverse of the Matrix
##
## Matrix is assumed to be always invertible

makeCacheMatrix <- function(x = matrix()) {
	i <- NULL
	set<-function(y){
		x<<- y
		i<<-NULL
	}
	get<-function() x
	setInv<-function(solve) i <<- solve
	getInv<-function() i
	list(set=set, get=get, setInv=setInv, getInv=getInv) 
}


##-----------------------------------------------------------
## CacheSolve
## Calculates the inverse of Matrix created with above function
##
## 1) check to see if inverse of matrix previously calculated
## 2) If yes, return previous calculated value
## 3) Otherwise, calculate inverse and return
##
## Matrix is assumed to be always invertible
##
cacheSolve <- function(x, ...) {
	i<-x$getInv()
	if(!is.null(i)) {
		message("Getting Inverse of matrix from cache")
		return(i)
	}
	data<-x$get()
	i<-solve(data,...)
	x$setInv(i)
	i
}
