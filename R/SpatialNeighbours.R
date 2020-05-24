#' Nearest Neighbours Among Data Points on a Two-dimensional Map
#'
#' The function takes a range of points on a two-dimensional surface as input, 
#' specified by vectors of x- and y-coordinates and a vector of identifiers. 
#' For any point in this cloud, the function determins a specified number 
#' of other points with smallest distances to the point in questions. 
#' The calculation of distances can be carried out in a non-projected space or 
#' as distances between geographical coordinates.
#'
#' @param ID A vector of distinct identifiers for all cloud points.
#'
#' @param x,y Numeric vectors containing the x- and y-coordinates of the 
#'    points specified in \code{ID}.
#'
#' @param n An integer specifying the number of individuals that are to form 
#'    the group of spatial neighbours to be determined. \code{l.included} 
#'    specifies whether this group contains the individual defined in \code{l} 
#'    or not.
#'
#' @param l The identifier of the inidvidual for which nearest neighbours are 
#'    to be determined. \code{l.included} specifies whether \code{l} is part 
#'    of the group of spatial neighbours to be determined. This also 
#'    determines whether it needs to be an element of \code{ID} or not.
#'
#' @param l.included A logical operator. If set to TRUE (this is the default), 
#'    the individual defined by \code{l} is part of the group of spatial 
#'    neighbours to be determined. In this case, \code{l} has to be an element 
#'    of \code{ID}. If set to FALSE, \code{l} will not be part of the 
#'    resulting group and it does not matter if it is an element of \code{ID} 
#'    or not.
#'
#' @param l.x,l.y Numeric values specifying the x- and y-coordinates of the 
#'    point specified by \code{l}. Specification of these parameters only 
#'    makes sense if \code{l.included} == FALSE and \code{l} is not an element 
#'    of \code{ID}.
#'
#' @section Note:
#'    As \code{ID}, \code{x} and \code{y} all refer to the same point set, 
#'    they have to be of the same length.
#'
#' @return A vector of length \code{n} is returned, giving the distances to 
#'    \code{l} for all nearest neighbours that have been determined. The 
#'    names of the vector elements give the points identifiers from \code{ID}.
#'
#' @examples
#'
#' ## Data for 50 spatial points
#' ID_x <- paste(rep("grave", 50), 1:50, sep=" ")
#' x_x <- sample(50, 50)
#' y_x <- sample(50, 50)
#'
#' ## The focus point is part of the specified spatial points
#' x1 <- SpatialNeighbours(ID=ID_x, x=x_x, y=y_x, n=20, l="grave 25")
#' plot(x_x, y_x, xlab="x", ylab="y") # all spatial points
#' points(x_x[ID_x %in% names(x1)], y_x[ID_x %in% names(x1)], pch=19) # focus group
#' points(x_x[25], y_x[25], col="red", pch=19) # focus point
#'
#' ## The focus point is outside the specified spatial points
#' x2 <- SpatialNeighbours(ID=ID_x, x=x_x, y=y_x, n=20, l.included=FALSE, l.x=25, l.y=25)
#' plot(x_x, y_x, xlab="x", ylab="y") # all spatial points
#' points(x_x[ID_x %in% names(x2)], y_x[ID_x %in% names(x2)], pch=19) # focus group
#' points(25, 25, col="red", pch=19) # focus point
#'
#' @export

SpatialNeighbours <-
function(ID, x, y, n, l=NA, l.included = TRUE, l.x = NA, l.y = NA) {
  
                                        # assessment of input
  
  if(length(ID)!= length(x)) stop("The vector of x coordinates is of different length than the vector of location names!")
  if(length(ID) != length(y)) stop("The vector of y coordinates is of different length than the vector of location names!")
  if(length(ID) <= n) stop("The number of locations to be selected as nearest neighbours has to be smaller than the total number of locations!")

                                        # separate focus location from the rest

    

  if(l %in% ID){
    focusindex <- which(ID == l)
    focus_x <- x[focusindex]
    focus_y <- y[focusindex]
    comp_ID <- ID[-focusindex]
    comp_x <- x[-focusindex]
    comp_y <- y[-focusindex]
  } else{
    focus_x <- l.x
    focus_y <- l.y
    comp_ID <- ID
    comp_x <- x
    comp_y <- y
  }

                                        # calculate distances
  
  comp_dist <- sqrt((comp_x - focus_x)^2 + (comp_y - focus_y)^2)
  names(comp_dist) <- comp_ID
  comp_rank <- sort(comp_dist)

                                        # select nearest locations

  if(isTRUE(l.included)) m <- n - 1 else m <- n
  comp_select <- comp_rank[1:m]

					# prepare output

  if(isTRUE(l.included)){
  output <- c(0, comp_select)
  names(output) <- c(paste(l), names(comp_select))
} else output <- comp_select

  # colnames(comp_select) <- c("locationID", "distance")

                                        # create output
  
  return(output)
}
