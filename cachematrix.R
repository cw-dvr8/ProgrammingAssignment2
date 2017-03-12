# Programming assignment Week 3 - Functions to cache the inverse of a matrix.

# makeCacheMatrix
# This function creates handlers for the matrix "x" that is passed in as a
# parameter.
#
# Note that the curly braces ({}) are not strictly necessary for the defined
# functions as they are all one-liners, but I am choosing to use them to make
# the syntax more clear for my future reference.

makeCacheMatrix <- function(x = matrix()) {
    inv_m <- NULL

    #------------------------------------------------------------------------#
    # "get" returns the matrix "x". Because of lexical scoping, the value of #
    # "x" is retained in the object that is created by the call to the       #
    # makeCacheMatrix function above.                                        #
    #------------------------------------------------------------------------#
    get <- function() {
        x
    }

    #--------------------------------------------------------------------#
    # "set_inv" sets the "inv_m" object in the parent environment to the #
    # value of "matrix_inv".                                             #
    #--------------------------------------------------------------------#
    set_inv <- function(matrix_inv) {
        inv_m <<- matrix_inv
    }

    #---------------------------------------#
    # "get_inv" returns the matrix "inv_m". #
    #---------------------------------------#
    get_inv <- function() {
        inv_m
    }

    #-----------------------------------------------------------------------#
    # Return a list of the functions defined above. This will create an     #
    # object with these functions, along with the matrices "x" and "inv_m". #
    #-----------------------------------------------------------------------#
    list(get=get, set_inv=set_inv, get_inv=get_inv)
}

# cacheSolve
# This function takes an object created by calling the makeCacheMatrix
# function above and checks to see if it has already computed the inverse
# matrix for it. If it has, it returns the previously computed inverse
# matrix. If it has not, it computes and returns the inverse matrix.

cacheSolve <- function(x, ...) {
    #---------------------------------------------------------------------#
    # Use the "get_inv" function defined during the creation of the "x"   #
    # object to retrieve the "inv_m" matrix. If the value returned is not #
    # NULL, it means that "inv_m" was computed sometime previously and it #
    # is not necessary to calculate it again.                             #
    #---------------------------------------------------------------------#
    inv_m <- x$get_inv()
    if (!is.null(inv_m)) {
        message("Inverse matrix previously computed")
        return(inv_m)
    }

    #------------------------------------------------------------------------#
    # According to the homework, the inverse of a matrix can be computed     #
    # using the R solve() function. However, this function will only work on #
    # square matrices, so check to make sure the matrix is square. If it is  #
    # not, return a NULL value.                                              #
    #------------------------------------------------------------------------#
    if (nrow(x$get()) != ncol(x$get())) {
        message ("Cannot compute inverse: matrix is not square")
        return(NULL)
    }

    #-----------------------------#
    # Compute the inverse matrix. #
    #-----------------------------#
    in_matrix <- x$get()
    inv_m <- solve(in_matrix)
    x$set_inv(inv_m)
    inv_m
}

