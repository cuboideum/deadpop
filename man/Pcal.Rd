% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/Pcal.R
\name{Pcal}
\alias{Pcal}
\title{Prevalence Calibration}
\usage{
Pcal(
  ID = NULL,
  segnames = NULL,
  n,
  s,
  c,
  method = c("sp"),
  sp_limit = 0.2,
  Pint_limit = 0.8
)
}
\arguments{
\item{ID}{A vector containing identifiers of the investigations for which 
prevalences are to be estimated. Vector length has to be identical with 
the length of parameters \code{n}, \code{s} and \code{c}. Provision of 
this item is optional.}

\item{segnames}{A vector containing names of bone segments for which 
prevalences are to be estimated. This parameter can only be used if 
parameters \code{n}, \code{s} and \code{c} are provided as matrices.}

\item{n}{An integer, a vector or a matrix of integers specifying the number 
of individuals in the population or populations for which prevalences 
are to be estimated. Vector length or matrix dimensions have to be 
identical with those of parameters \code{s} and \code{c}.}

\item{s}{An integer, a vector or a matrix of integers specifying the number 
of individuals in the sample of sufficiently preserved material drawn 
from the population or populations for which prevalences are to be
estimated. Vector length or matrix dimensions have to be identical with 
those of parameters \code{n} and \code{c}.}

\item{c}{An integer, a vector or a matrix of integers specifying the number 
of individuals in the sample with which the condition of interest was 
observed (a.k.a. cases). Vector length or matrix dimensions have to be 
identical with those of parameters \code{n} and \code{s}.}

\item{method}{Option from provided list of strings denoting the implemented 
methods for prevalence calibration. Currently, the only option is "sp" 
for calibration based on sample portion.}

\item{sp_limit}{Threshold specifying a value for sample portion below 
which no estimation of prevalence is performed.}

\item{Pint_limit}{Threshold specifying a value for prevalence interval 
(the difference between crude and sample-based prevalence) above which 
no estimation of prevalence is performed.}
}
\value{
Output are a number of data items approximating true prevalence.

   \code{Pc}: Crude prevalence, giving the certain minimum value for true
   prevalence.

   \code{Pcal}: Calibrated prevalence; the corrected sample-based prevalence
   after applying the specified method.

   \code{Ps}: Sample-based prevalence, estimating true prevalence by 
   assuming that the ratio of cases and unaffected individuals in the 
   population is identical to the one observed in the sample.

   \code{Pmax}: Maximum prevalence, giving the certain maximum value for true
   prevalence by assuming that all unobservable individuals are cases.

   \code{errorInt}: Difference between the predicted mean of estimation 
   errors and the upper limit of the 0.95 prediction interval.

   If paramters \code{n}, \code{s} and \code{c} are provided as vectors, 
   output will be a data frame with rows representing populations and 
   columns the output items. If the parameters are provided as matrices, 
   output will be a list of matrices, each providing the data for one 
   output item.
}
\description{
\code{Pcal} calculates several parameters describing the prevalence of a 
feature in incomplete skeletal material and returns a calibrated prevalence 
estimate.
}
\details{
The function calculates crude and sample-based prevalences and corrects
sample-based prevalence by applying a specified method. The output is a
range of approximations of true prevalence.

The frequency of a specific trait to occur in a population is expressed
by the number of affected individuals ('cases') divided by the number of
individuals in the population. If not all individuals in the population
could be examined for the trait, the number of known cases divided by
the total number of individuals in the population is called the 'crude
prevalence'. Sample-based prevalence (a.k.a. corrected prevalence) is
the number of observed cases divided by the number of individuals in the
sample ready for examination. The relation of sample size and population
size (here referred to as the 'sample portion') affects the quality of
sample-based prevalence as an estimator of true prevalence.

This function calibrates sample-based prevalences by subtracting the
estimated estimation error and calculates the difference of the
estimated estimation error and the upper level of the prediction
interval for estimation error (\code{errorInt}). This value serves as a 
quality criterion for the exactness of the calibrated prevalence
estimate. Several methods for estimating estimation error exist.

Two thresholds can be specified for preventing prevelance estimation for
situations where material preservation is deemed inaccaptably
low. Estimation error increases with lower sample portions and estimates
will not be carried out for sample portions below the specified value
for parameter \code{sp_limit}. The difference between crude and 
sample-based prevalence, referred to as 'prevalence interval', has also 
proved a suitable predictor of estimation error. Estimations will not be 
carried out for prevalence intervals above the specified value for 
parameter \code{Pint_limit}. For calculations disqualified by these
two parameters, NA is returned for calibrated (Pcal) and sample-based
prevalence (\code{Ps}).

The parameters \code{n}, \code{s} and \code{c} can be specified as 
integers for one population or as vectors to perform calculations for a 
series of populations. Specification as matrices is also possible. 
Here, the matrix rows are interpreted to represent different populations 
while the columns are understood to represent skeletal segments for 
which data were collected separately. The parameter \code{segnames} 
can be used to provide the names of the segments as column names.
}
\examples{

## Evaluation of a single sample
Pcal(n=100, s=72, c=5)

## Evaluation of three samples named 'fg1' to 'fg3'
Pcal(ID=c("fg1", "fg2", "fg3"), n=rep(100, 3), s=c(82, 41, 67), c=c(12, 6, 8))

## Evaluation of data collected from three bone segments, 'frl', 'frm' and 'frr', 
## in three samples named 'fg1' to 'fg3'
id_x <- c("fg1", "fg2", "fg3")
segnames_x <- c("frl", "frm", "frr")
n_x <- matrix(rep(100, 9), nrow=3, byrow =TRUE)
s_x <- matrix(c(82, 85, 78, 46, 52, 49, 72, 89, 68), nrow=3, byrow=TRUE)
c_x <- matrix(c(0, 0, 5, 1, 3, 2, 12, 3, 0), nrow=3, byrow=TRUE)
Pcal(ID=id_x, segnames=segnames_x, n=n_x, s=s_x, c=c_x)

}
\seealso{
\code{\link{sp_Pserrabs.mod1}}
}
