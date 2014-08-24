## There are two global lists which represent the cache.
## When cacheSolve is invoked, with a matrix passed as argument, the cahce is first checked to see if the matrix exists.
## If yes, the inverse that is stored in the second list is printed. 
## Else, the inverse is computed and stored in cache.

## This is the declartion of the two global lists described above.

assign('recentMatrix', list(matrix()), envir=.GlobalEnv)

assign('recentMatrixInverse', list(matrix()), envir=.GlobalEnv)

##This function computes the inverse and stores the inverse and the matrix in the cache

makeCacheMatrix <- function(x = matrix()) 
{
  
  recentMatrix <<- list(recentMatrix,x);
  recentMatrixInverse <<- list(recentMatrixInverse,solve(x));

  print(recentMatrixInverse[[length(recentMatrixInverse)]])
}

## This function check if the given matrix x, is contained in the cache or not. 
## If yes, the inverse is retruned. If not, an empty matrix is returned, which essentially contains one element "NA"

checkIfInCache <- function(x)
{
  isIn <- lapply(recentMatrix, function(i) all(match(i, x)))
  avail <- matrix()
  yes <- FALSE
  for ( j in 1:length(isIn))
  {
    if (isTRUE(isIn[[j]]))
    {
      avail <- recentMatrixInverse[[j]]
      yes <- TRUE
    }   
  }
  print(avail)
  
}

## This function is the interface function to the oustide world. 
## This is the function to be invoked to get the inverse of a matrix.

cacheSolve <- function(x, ...) 
{
  temp <- checkIfInCache(x)
  if(!is.na(temp[1,1]))
  {
    print("Matrix available in cache. Printing the same.")
    print(temp)    
  }
  else    
  {
    print("Matrix not available in cache. Calculating and storing the same.")
    makeCacheMatrix(x)
  }
}



