#' Derivative of the Prevalence Function
#'
#' The prevalence ratio is calculated as the number of individuals/elements 
#' carrying a specific trait by the number of individuals/elements in the 
#' sample. This function calculates the derivative of this term.
#'
#' The prevalence ratio is determined both by the number of 
#' indivduals/elements carrying the trait of interest and by the number of 
#' individuals/elements in the sample. The graph of a prevalence function with 
#' a fixed number of affected indivduals/elements and a variable sample size 
#' has both the x and the y axis as asymptotes. The derivative of the 
#' prevalence function yields the slope of a tangent line to this graph for a 
#' specific sample size. It has been hypothesised to serve as an indicator of 
#' the effect of sample size on the local prevalence ratio.
#'
#' @param s An integer specifying the number of individuals/elements in the 
#'    sample.
#'
#' @param c An integer, specifying the number of cases, i. e. the number of 
#'    individuals/elements carrying the trait of interest in the sample.
#'
#' @return The derivative of the prevalence function is returned as a rational 
#'    number.

Pderiv <- function(s, c){

    pderiv <- -c / s^2

    return(pderiv)

}
