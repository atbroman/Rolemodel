\name{optimalRM}
\alias{optimalRM}

\title{ Extract wholes in the smaller incidence matrix}
\description{
 Extract wholes in the smaller incidence matrix, which might be active in the optimal solution with respect to the bigger incidence matrix in the sequential approach introduced in the reference.
}

\usage{
optimalRM(xxbig, ysubbig, xx1, xx2, alpha, gamma, p)
}

\arguments{
  \item{xxbig}{The bigger incidence matrix in the sequential approach.}
  \item{ysubbig}{The corresponding gene-level data.}
  \item{xx1}{The smaller incidence matrix in the sequential approach.}
  \item{xx2}{The difference incidence matrix between xxbig and xx1.}
  \item{alpha}{The false positive rate in role model, numeric value between 0 and 1. See reference.}
  \item{gamma}{The true positive rate in role model, numeric value between 0 and 1. See reference.}
  \item{p}{The prior active probability of wholes in role model, numeric value between 0 and 1. See reference.}
}

\details{
In the sequential approach, we need to extract some other wholes in the smaller incidence matrix besides the active ones, which are already identified by the ILP calculation. See reference for details. This function will be invoked in \code{sequentialRM}.
}

\value{
Return a logical vector with the length of the number of columns of xx1.
 }

\references{Zhishi W., Qiuling H., Bret L. and Michael N.: A multi-functional analyzer uses parameter constaints to improve the efficiency of model-based gene-set analysis (2013).}
\seealso{\code{sequentialRM}}
\author{ Zhishi Wang, Michael Newton, Subhrangshu Nandi}

%\examples{
%
%}
