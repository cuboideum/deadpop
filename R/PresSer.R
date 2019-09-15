#' Series of Simulated Random Observations in Partially Preserved Material
#'
#' The function draws on the function \code{\link{PresMod}} to create a series 
#' of hypothetical preservation states from populations of defined size, with 
#' given numbers of individuals displaying a certain trait and a given portion 
#' of individuals unaffected by taphonomy.
#'
#' The parameters, with the exceptions of \code{value_spec}, can be vectors of 
#' any length, and their lengths do not have to match. The function runs the 
#' \code{\link{PresMod}} function on all possible combinations of parameters.
#'
#' The resulting data can be used for modelling the impact of material 
#' preservation on the calculation of frequencies under diverse conditions.
#'
#' @param n An integer or vector specifying the number(s) of 
#'    individuals/elements in the population(s).
#'
#' @param c Cases, i.e. the number or portion of individuals/elements carrying 
#'    the trait of interest in a scenario. Depending on the specification of 
#'    \code{value_spec}, \code{c} is either given as an integer or vector 
#'    expressing the absolute number(s) of cases (\code{value_spec} = 'count') 
#'    or as a decimal value between 0 and 1 or vector of such values 
#'    expressing the portion(s) of individuals/elements in \code{n} that are 
#'    affected (\code{value_spec} = 'portion').
#'
#' @param s Sample, i.e. the number or portion of individuals/elements in the 
#'    population that are to survive taphonomy. Depending on the specification 
#'    of \code{value_spec}, \code{s} is either given as an integer or vector 
#'    expressing the absolute number(s) of preserved individuals/elements 
#'    (\code{value_spec} = 'count') or as a decimal value between 0 and 1 or 
#'    vector of such values expressing the portion(s) of individuals/elements 
#'    in the population that are preserved (\code{value_spec} = 'portion').
#'
#' @param i Integer or vector specifying the number(s) of iterations. By 
#'    default \code{i} is set to 1.
#'
#' @inheritParams PresMod
#'
#' @return A data frame is returned, giving the output of 
#'    \code{\link{PresMod}} for all combinations of parameters. If 
#'    \code{value_spec} = 'portion', the specified portions of \code{c} and 
#'    \code{s} are also given ('c_portion' and 's_portion').
#'
#' @section Note:
#'    Note that if \code{c} and \code{s} are specified as a portions 
#'    (\code{value_spec} = 'portion'), the function uses the 
#'    \code{\link[base]{round}} function with the rounding behaviour 
#'    implemented there. Please make sure that you approve of this procedure. 
#'    In case of doubt, it is advisable to give absolute numbers of cases and 
#'    preserved individuals/elements (\code{value_spec} = 'count').
#'
#' @seealso \code{\link{PresMod}}

PresSer <- function(n, c, s, i = 1, value_spec = "count"){

    ## INPUT ASSESSMENT

    if(!value_spec == "count" & !value_spec == "portion") stop("The value for 'value_spec' is incorrect. Allowed options are 'count' or 'portion'")

    if(value_spec == "count"){
        if(max(c) > min(n)) stop("The number of cases (c) exceeds the true number of elements (n) for at least one combination of input variables.")
        if(max(s) > min(n)) stop("The number of preserved elements (s) exceeds the true number of elements (n) for at least one combination of input variables.")
    }

    if(value_spec == 'portion'){
        if(any(c > 1)) stop("The number of cases (c) is specified as a portion (value_spec = 'portion') but at least one variable in c is larger than 1. Change settings for either c or value_spec.")
        if(any(s > 1)) stop("Preservation is specified as a portion (value_spec = 'portion') but at least one variable in s is larger than 1. Change settings for either s or value_spec.")
    }

    # PROGRESS BAR

    total <- sum((length(n) * length(c) * length(s)) * i)
    cat(total, "lines of data being created.", sep = " ")
    pb <- utils::txtProgressBar(min = 0, max = total, style = 3)
    pbi <- 0

    ## CALCULATIONS

    c_portion <- c() # will only be used if value_spec == 'portion'
    s_portion <- c() # will only be used if value_spec == 'portion'

    out <- c()

    for (ni in seq_along(n)){

        for(ci in seq_along(c)){

            for(si in seq_along(s)){

                for(ii in seq_along(i)){
                    c_portion <- c(c_portion, c[ci]) # will only be used if value_spec == 'portion'
                    s_portion <- c(s_portion, s[si]) # will only be used if value_spec == 'portion'
                    temp <- PresMod(n = n[ni], c = c[ci], s = s[si], i = i[ii], value_spec = value_spec)
                    out <- rbind(out, temp)
                    pbi <- pbi + i[ii]
                    utils::setTxtProgressBar(pb, pbi)

                }

            }

        }

    }

    if(value_spec == "portion"){
        out <- data.frame(out, c_portion, s_portion)
        }

    close(pb)

    return(out)

}
