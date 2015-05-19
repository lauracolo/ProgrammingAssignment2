
## makeCacheMatrix: This function creates a special "matrix" object that can cache its inverse.

makeCacheMatrix <- function(m = matrix()) {
  
  inv <<- NULL #inv stores the inverse matrix
   
  setM<-function(y){
    
    m<<-y
    inv<<-NULL
  }
  
  getM<-function(){
    m
  }
  
  setInv<-function(inverse){
    inv<<-inverse
  }
  getInv <- function(){
    inv
  }
list(setM=setM, getM=getM, setInv = setInv, getInv=getInv)
}


## cacheSolve : this function computes the inverse of the speciaL matrix returned by makeCacheMatrix()
## If the inverse has already been calculated,then the cacheSolve retrieves the inverse from the cache.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  
  inv<-x$getInv()
  if(!is.null(inv)){                #inv is equal NULL when the inverse has not been calculated yet
    message("Getting cached data")  #if it's different from NULL (i.e. already calculated), inv is taken from the cache
    return(inv)                     #and the function  cacheSolve stops here
  }
  
  data<-x$getM()                    #otherwise, it stores the matrix into 'data'
  inv<-solve(data)                  #calculates the inverse 
  x$setInv(inv)                     #stores the value of inv in the cache
  inv
  
}
