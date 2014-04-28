#
# This function creates the CacheMatrix, a special matrix that cashes its
# own inverse.  Usage: aCacheMatrix<-makeCacheMatrix(aMatrix). Here aMatrix is
# is a square R matrix and aCacheMatrix is the special CacheMatrix returned
# by the function makeCacheMatrix. You should not directly call any of 
# the Set routines.  The matrix aCacheMatrix is the argument to function 
# CacheSolve at the bottom of this file.
#
makeCacheMatrix <- function(myCacheMat=NULL) {
#
# Some Rudimentary Error Checking.  It is not perfect!!!
#
        retNullMsg1<-"The CacheMatrix your are attempting to make is empty, null"
        retNullMsg2<-"or does not exist. Try again."
        retSqMsg1<-"The CacheMatrix you are making is not square or has "
        retSqMsg2<-"a negative number of rows or columns."
        if (is.na(myCacheMat) || is.null(myCacheMat)) {
          cat(retNullMsg1)
          cat(retNullMsg2)
          return
        }

        numCols=ncol(myCacheMat)
        numRows=nrow(myCacheMat)
         if ((numRows != numCols) || numRows<=0 || numCols<=0) {
           cat(retSqMsg1)
           cat(retSqMsg2)
           return
         }
#
        myCacheMatInverse <- NULL
        SetCacheMat <- function(anotherCacheMat) {
             myCacheMat <<- anotherMat
             myCacheMatInverse <<- NULL
        }
        GetCacheMat <- function() myCacheMat

        SetCacheMatInverse <- function(matInverse) myCacheMatInverse <<-matInverse

        GetCacheMatInverse <- function() myCacheMatInverse

        list(SetCacheMat=SetCacheMat,GetCacheMat=GetCacheMat,
        SetCacheMatInverse=SetCacheMatInverse,GetCacheMatInverse=GetCacheMatInverse)
}


#
# This function should be used to compute the inverse of the special
# Cachematrix. It takes a special CacheMatrix as its argument. cacheSolve(aCacheMatrix). 
# The matrix, aCacheMatrix, is a CacheMatrix which is the return value of makeCacheMatrix(aMatrix)
# where aMatrix is an ordinary square R matrix.
#
cacheSolve <- function(x, ...) {
        myCacheMatInverse <- x$GetCacheMatInverse()
        if(!is.null(myCacheMatInverse)) {
                message("getting cached data")
                return(myCacheMatInverse)
        }
        mat <- x$GetCacheMat()
        myCacheMatInverse <-solve(mat)
        x$SetCacheMatInverse(myCacheMatInverse)
        myCacheMatInverse
}
