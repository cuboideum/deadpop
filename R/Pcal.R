#' Prevalence Calibration
#'
#' \code{Pcal} calculates several parameters describing the prevalence of a 
#' feature in incomplete skeletal material and returns a calibrated prevalence 
#' estimate.
#'
#' The function calculates crude and sample-based prevalences and corrects
#' sample-based prevalence by applying a specified method. The output is a
#' range of approximations of true prevalence.
#'
#' The frequency of a specific trait to occur in a population is expressed
#' by the number of affected individuals ('cases') divided by the number of
#' individuals in the population. If not all individuals in the population
#' could be examined for the trait, the number of known cases divided by
#' the total number of individuals in the population is called the 'crude
#' prevalence'. Sample-based prevalence (a.k.a. corrected prevalence) is
#' the number of observed cases divided by the number of individuals in the
#' sample ready for examination. The relation of sample size and population
#' size (here referred to as the 'sample portion') affects the quality of
#' sample-based prevalence as an estimator of true prevalence.
#'
#' This function calibrates sample-based prevalences by subtracting the
#' estimated estimation error and calculates the difference of the
#' estimated estimation error and the upper level of the prediction
#' interval for estimation error (\code{errorInt}). This value serves as a 
#' quality criterion for the exactness of the calibrated prevalence
#' estimate. Several methods for estimating estimation error exist.
#'
#' Two thresholds can be specified for preventing prevelance estimation for
#' situations where material preservation is deemed inaccaptably
#' low. Estimation error increases with lower sample portions and estimates
#' will not be carried out for sample portions below the specified value
#' for parameter \code{sp_limit}. The difference between crude and 
#' sample-based prevalence, referred to as 'prevalence interval', has also 
#' proved a suitable predictor of estimation error. Estimations will not be 
#' carried out for prevalence intervals above the specified value for 
#' parameter \code{Pint_limit}. For calculations disqualified by these
#' two parameters, NA is returned for calibrated (Pcal) and sample-based
#' prevalence (\code{Ps}).
#'
#' The parameters \code{n}, \code{s} and \code{c} can be specified as 
#' integers for one population or as vectors to perform calculations for a 
#' series of populations. Specification as matrices is also possible. 
#' Here, the matrix rows are interpreted to represent different populations 
#' while the columns are understood to represent skeletal segments for 
#' which data were collected separately. The parameter \code{segnames} 
#' can be used to provide the names of the segments as column names.
#'
#' @param ID A vector containing identifiers of the investigations for which 
#'    prevalences are to be estimated. Vector length has to be identical with 
#'    the length of parameters \code{n}, \code{s} and \code{c}. Provision of 
#'    this item is optional.
#'
#' @param segnames A vector containing names of bone segments for which 
#'    prevalences are to be estimated. This parameter can only be used if 
#'    parameters \code{n}, \code{s} and \code{c} are provided as matrices.
#'
#' @param n An integer, a vector or a matrix of integers specifying the number 
#'    of individuals in the population or populations for which prevalences 
#'    are to be estimated. Vector length or matrix dimensions have to be 
#'    identical with those of parameters \code{s} and \code{c}.
#'
#' @param s An integer, a vector or a matrix of integers specifying the number 
#'    of individuals in the sample of sufficiently preserved material drawn 
#'    from the population or populations for which prevalences are to be
#'    estimated. Vector length or matrix dimensions have to be identical with 
#'    those of parameters \code{n} and \code{c}.
#'
#' @param c An integer, a vector or a matrix of integers specifying the number 
#'    of individuals in the sample with which the condition of interest was 
#'    observed (a.k.a. cases). Vector length or matrix dimensions have to be 
#'    identical with those of parameters \code{n} and \code{s}.
#'
#' @param method Option from provided list of strings denoting the implemented 
#'    methods for prevalence calibration. Currently, the only option is "sp" 
#'    for calibration based on sample portion.
#'
#' @param sp_limit Threshold specifying a value for sample portion below 
#'    which no estimation of prevalence is performed.
#'
#' @param Pint_limit Threshold specifying a value for prevalence interval 
#'    (the difference between crude and sample-based prevalence) above which 
#'    no estimation of prevalence is performed.
#'
#' @return Output are a number of data items approximating true prevalence.
#'
#'    \code{Pc}: Crude prevalence, giving the certain minimum value for true
#'    prevalence.
#'
#'    \code{Pcal}: Calibrated prevalence; the corrected sample-based prevalence
#'    after applying the specified method.
#'
#'    \code{Ps}: Sample-based prevalence, estimating true prevalence by 
#'    assuming that the ratio of cases and unaffected individuals in the 
#'    population is identical to the one observed in the sample.
#'
#'    \code{Pmax}: Maximum prevalence, giving the certain maximum value for true
#'    prevalence by assuming that all unobservable individuals are cases.
#'
#'    \code{errorInt}: Difference between the predicted mean of estimation 
#'    errors and the upper limit of the 0.95 prediction interval.
#'
#'    If paramters \code{n}, \code{s} and \code{c} are provided as vectors, 
#'    output will be a data frame with rows reprenting populations and columns 
#'    the output items. If the parameters are provided as matrices, output 
#'    will be a list of matrices, each providing the data for one output item.

Pcal <- function(ID = NULL, segnames = NULL, n, s, c, method = c("sp"), sp_limit = 0.2, Pint_limit = 0.8){

    ## DATA ASSESSMENT
    if(length(which(s > n)) > 0) stop("Some sample sizes are higher than the respective population sizes.")
    if(length(which(c > s)) > 0) stop("Some case counts are higher than the respective sample sizes.")
    if(is.matrix(n)){
        if(!(is.matrix(s) && is.matrix(c))) stop("Data types of n, s and c are not identical.")
        if(!(dim(n) == dim(s) && dim(s) == dim(c))) stop("Dimensions of n, s and c are not identical.")
        if(!is.null(ID)){
            if(!length(ID) == dim(n)[1]) stop("Length of ID does not match number of data rows.")
        }
        if(!is.null(segnames)){
            if(!length(segnames) == dim(n)[2]) stop("Length of segnames does not match number of data columns.")
        }
        
    }
    if(is.vector(n)){
        if(!(is.vector(s) && is.vector(c))) stop("Data types of n, s and c are not identical.")
        if(!(length(n) == length(s) && length(s) == length(c))) stop("Lengths of n, s, and c are not identical.")
        if(!is.null(ID)){
            if(!length(ID) == length(n)) stop("Length of ID does not match lengths of data vectors.")
        }
        if(!is.null(segnames)) warning("Parameter segnames only makes sense if data is provided as matrices. It was ignored, here.")
    }
    
    ## CALCULATION of VARIABLES
    sp <- s / n # sample portion
    Pc <- c / n # crude prevalence
    Ps <- c / s # sample-based prevalence
    Pint <- Ps - Pc # prevalence interval
    Pmax <- (c + n - s) / n # maximum prevalence

    if(is.matrix(n)){ ########## INPUT as MATRICES

        ## DATA PREDICTION
        if(method == "sp"){

            errorFit <- c() # predicted estimation error
            errorUpr <- c() # upper limit of prediction interval

            for(i in seq_along(sp[1, ])){

                predictedData <- predict(sp_Pserrabs.mod1, newdata = data.frame(sp_dat = sp[ ,i]), interval = "prediction", weights = sp[ ,i]^2)
                errorFit <- cbind(errorFit, predictedData[ ,"fit"])
                errorUpr <- cbind(errorUpr, predictedData[ ,"upr"])

            }
        }

    }else{ ########## INPUT as VECTORS

        ## DATA PREDICTION
        if(method == "sp"){

            predictedData <- predict(sp_Pserrabs.mod1, newdata = data.frame(sp_dat = sp), interval = "prediction", weights = sp^2)
            
            errorFit <- predictedData[ ,"fit"] # predicted estimation error
            errorUpr <- predictedData[ ,"upr"] # upper limit of prediction interval

        }

    }

    ## DATA CALIBRATION
    Pcal <- Ps - errorFit # calibrate prevalence estimates
    Pcal[Pcal < Pc] <- Pc[Pcal < Pc] # make sure that Pcal does not drop below Pcal

    ## THRESHOLD ENFORCEMENT
    Pcal[which(sp < sp_limit | Pint > Pint_limit)] <- NA
    Ps[which(sp < sp_limit | Pint > Pint_limit)] <- NA
    
    errorInt <- errorUpr - errorFit

    ## OUTPUT PREPARATION
    if(is.matrix(n)){ ########## INPUT as MATRICES

        if(!is.null(ID)){

            rownames(Pc) <- ID
            rownames(Pcal) <- ID
            rownames(Ps) <- ID
            rownames(Pmax) <- ID
            rownames(errorInt) <- ID

        }

        if(!is.null(segnames)){

            colnames(Pc) <- segnames
            colnames(Pcal) <- segnames
            colnames(Ps) <- segnames
            colnames(Pmax) <- segnames
            colnames(errorInt) <- segnames

        }
        
        output <- list(Pc, Pcal, Ps, Pmax, errorInt)
        names(output) <- c("Pc", "Pcal", "Ps", "Pmax", "errorInt")

    } else{ ########## INPUT as VECTORS

        output <- data.frame(Pc, Pcal, Ps, Pmax, errorInt)

        if(!is.null(ID)){

            row.names(output) <- ID
        }

        if(!is.null(segnames)){

            names(output) <- segnames

        }
        
    }
            
    return(output)
    
}
