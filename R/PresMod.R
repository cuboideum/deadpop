#' Simulated Random Observation of Traits in Partially Preserved Material
#'
#' A given population consists of a number of individuals/elements, a certain 
#' number of which carry a certain trait. These are referred to as 'cases'. 
#' The function creates a series of random subsets from this virtual 
#' population by drawing a specified number of individuals/elements, 
#' simulating random taphonomic processes. The number of iterations (i.e. 
#' draws) can be specified.
#'
#' A vector of length n is created, consisting of values '0' for individuals 
#' not carrying the trait of interest and '1' for cases. For each iteration, a 
#' sample of size \code{s} is drawn from this vector, using the function 
#' \code{\link[base]{sample}}. 
#' The number of cases in these samples (c_count) is determined using the 
#' function \code{\link[base]{sum}}.
#'
#' The resulting data can be used for modelling the impact of material 
#' preservation on the calculation of frequencies. By adapting the parameters 
#' to a real population under study, the model can be applied to specific 
#' research projects.
#'
#' @param n An integer specifying the number of individuals/elements in the 
#'    population.
#'
#' @param c Cases, i.e. the number or portion of individuals/elements carrying 
#'    the trait of interest. Depending on the specification of 
#'    \code{value_spec}, \code{c} is either given as an integer, expressing 
#'    the absolute number of cases (\code{value_spec} = 'count') or as a 
#'    decimal value between 0 and 1, expressing the portion of 
#'    individuals/elements in \code{n} that are affected (\code{value_spec} = 
#'    'portion').
#'
#' @param s Sample, i.e. the number or portion of individuals/elements in the 
#'    population that are to survive taphonomy. Depending on value 
#'    specification (\code{value_spec}), \code{s} is either given as an 
#'    integer, expressing the absolute number of preserved 
#'    individuals/elements (\code{value_spec} = 'count') or as a decimal value 
#'    between 0 and 1, expressing the portion of individuals/elements in the 
#'    population that are preserved (\code{value_spec} = 'portion').
#'
#' @param i Integer specifying the number of iterations. By default, only one 
#'    sample is drawn.
#'
#' @param value_spec The method how \code{c} and \code{s} are specified, 
#'    either as absolute counts of cases and preserved individuals/elements or 
#'    as portions of \code{n}. Options are the strings 'count' and 'portion'; 
#'    the default is 'count'.
#'
#' @return A data frame is returned, giving the specified parameters \code{n}, 
#'    \code{c}, \code{s} and \code{i}, and the number of preserved 
#'    individuals/elements on which the trait of interest could be observed 
#'    (c_count). The rows of the data frame represent the individual iterations.
#'
#' @section References:
#'    Waldron T. 1991. Rates for the job: Measures of Disease Frequency in
#'    Paleopathology. International Journal of Osteoarchaeology 1(1):17–25.
#'
#' @section References:
#'    Waldron T. 2009. Palaeopathology. Cambridge: Cambridge University Press. 
#'    p. 249-261.
#'
#' @examples
#'
#' # Specification of sample size and case number as absolute counts
#' PresMod(n=100, c=9, s=69)
#' PresMod(n=100, c=9, s=69, i=5)
#'
#' # Specification of sample size and case number as portions
#' PresMod(n=100, c=0.09, s=0.69, value_spec="portion")
#' PresMod(n=100, c=0.09, s=0.69, i=5, value_spec="portion")
#'
#' @export

PresMod <- function(n, c, s, i=1, value_spec = "count"){

    ## INPUT ASSESSEMENT

    if(value_spec == "count"){
        if(c > n) stop("The number of cases (c) exceeds the total number of elements (n).")
        if(s > n) stop("The sample of preserved elements (s) exceeds the total number of elements (n).")
    }
    if(value_spec == "portion"){
        if(c > 1) stop("Case frequency is specified as a portion (value_spec = 'portion') but c is larger than 1. Change setting for either c or value_spec.")
        if(s > 1) stop("the sample of preserved elements is specified as a portion (value_spec = 'portion') but s is larger than 1. Change setting for either s or value_spec.")
    }

    if(!value_spec == "count" & !value_spec == "portion") stop("The value for parameter value_spec is invalid. Set it to either 'count' or 'portion'")

    ## VARIABLE CONVERSION

    if(value_spec == "portion"){
        c <- round(n * c)
        s <- round(n * s)
    } # converts into count data

    ## SAMPLE MODEL POPULATION

    samples <- c(replicate(i, sum(sample(c(rep(1,c), rep(0,n-c)), s, replace = FALSE))))

    ## CREATE OUTPUT

    output <- data.frame(rep(n, i), rep(c, i), rep(s, i), rep(i, i), samples)
    names(output) <- c("n", "c", "s", "i", "c_count")

    return(output)
    
}
