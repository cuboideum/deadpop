#' Proportional Distribution of Age Estimates Among Age Categories
#'
#' Age-at-death estimates from skeletons are usually expressed in age spans, 
#' each delimited by a minimum and a maximum age-at-death estimate, 
#' respectively. The function divides the contribution from each individual by 
#' the number of years covered by the respective age span and assigns the 
#' resulting fraction to each year in the span. Then, it sums up these yearly 
#' contributions from individuals to determine contributions from all 
#' individuals to specified age categories. This procedure is referred to, 
#' here, as 'proportional distribution of age estimates'.
#' 
#' Boldsen (1988) describes the reconstruction of mortality profiles. These 
#' can also be expressed as distributions of age-at-death estimates, as 
#' performed by this function.
#'
#' @param minAge,maxAge Vectors of integers specifying minimum and maximum 
#'    age-at-death estimates for a series of individuals.
#'
#' @param cats A vector of breaks defining age categories. The breaks are 
#'    understood as points on a time line and not as units that might be part 
#'    of one of the categories they divide. If no value is defined, single 
#'    years within the age range defined by \code{minAge} and \code{maxAge} 
#'    are used as categories.
#'
#' @param accept.empty A boolean statement specifying whether the function 
#'    should accept calls not providing age estimates, i.e. with \code{minAge} 
#'    and \code{maxAge} being empty. If set to TRUE, the function will return 
#'    a vector with value zero for all age categories. If set to FALSE (the 
#'    default), the function will abort with an error message. Setting 
#'    accept.empty to TRUE can be helpful in batch processing where empty 
#'    inputs might be created by automated calculations.
#'
#' @section Note:
#'    As \code{minAge} and \code{maxAge} refer to the same number of 
#'    individuals, they have to be of identical length.
#'
#' @return A vector of the same length as numbers of categories 
#'    (length(cats) - 1) is returned, giving the numbers of individuals that 
#'    have been assigned to each age category. As individuals are distributed 
#'    among several categories, these numbers might be fractions.
#'
#' @section References:
#'    Boldsen JL. 1988. Two Methods for Reconstructing the Empirical Mortality 
#'    Profile. Human Evolution 3(5):335-342.
#'
#' @export

ProportionalAgeDistribution <- function(minAge = NA, maxAge = NA, cats = seq(floor(min(minAge)), ceiling(max(maxAge)), 1), accept.empty = FALSE){
    
                                        # DATA EVALUATION
    ## enforce parameter accept.empty
    if(accept.empty == FALSE){

        if(length(minAge) == 0) stop("The specified objects with age estimates contain no data")

    }

    ## test if the numbers of upper and lower limits of age estimates are identical
    if(length(minAge) != length(maxAge)) stop("Vectors minAge and maxAge have to be of identical length!")

    ## test if upper limits of age estimates are larger than or equal to lower limits
    Asub <- maxAge - minAge
    Atest <- Asub < 0
    if(TRUE %in% Atest) stop("At least one maximum age estimate is lower than its corresponding minimum estimate!")

                                        # OBJECT PEPARATION

    ## create index of individuals
    index_i <- 1 : length(minAge)

    ## create output object
    F.t <- rep(0, length(cats) - 1)

    ## create labels for age categories
    index_cat <- 1 : (length(cats) - 1) # index of age categories
    catnames <- c() # vector containing category names
    for(i in index_cat){

        tempcatname <- paste(cats[i], "-", cats[i+1], sep = "")
        catnames <- c(catnames, tempcatname)

    }

                                        # ASSESSMENT OF INDVIDUAL CONTRIBUTIONS TO AGE CATEGORIES

    ## make empty F.t vector output if input objects are empty
    ## this only applies if accept.empty == TRUE, otherwise empty input objects woul have stoped the process in data evaluation
    if(length(minAge) > 0){

        ## go through all individuals to determine their contributions to age categories
        for(i in index_i){

            ## establish age spread for current individual
            amin <- minAge[i] # lower margin of age estimate for current individual
            amax <- maxAge[i] # upper margin of age estimate for current individual
            if(amin == amax){

                estimationspan <- 1

            } else{

                estimationspan <- amax - amin

            } # spread of age estimate for current individual

            ## assess contributions of current individual to individual age classes
            indicontributions <- c() # vector containing class contributions from current individuals

            ## go through all age categories to determine contributions from current individual
            for(j in index_cat){

                ## set individual contribution to category to zero if category is outside age spread
                if(amin > cats[j + 1] | amax <= cats[j]){
                    
                    catcontribution <- 0
                    
                } else{

                    ## assess if lower limit of age spread falls into current age category
                    if(amin > cats[j] & amin <= cats[j + 1]){

                        lower <- amin
                        
                    } else{

                        lower <- cats[j]
                    }

                    ## assess if upper limit of age spread falls into current age category
                    if(amax > cats[j] & amax <= cats[j +1]){

                        upper <- amax
                        
                    } else{

                        upper <- cats[j + 1]
                    }

                    catspan <- upper - lower # time within age category covered by age estimate

                    ## assess if age estimation is a point estimate and calculate which portion of the age spread the current category represents
                    if(amin == amax){

                        catcontribution <- 1
                        
                    } else{

                        catcontribution <- catspan / estimationspan

                    }

                }

                ## add contribution to current age category to vecotor of current individual's contributions
                indicontributions <- c(indicontributions, catcontribution)

            }

        F.t <- F.t + indicontributions

        }

    }

                                        # PREPARE OUTPUT OBJECT
    
    ## Add age category names to output object
    names(F.t) <- catnames

    return(F.t)

}
