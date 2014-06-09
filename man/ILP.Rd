\name{ILP}
\alias{ILP}

\title{ Perform an integer linear programming }
\description{
Maximum a posteriori (MAP) estimate via integer linear programming (ILP).
}

\usage{
ILP(I, y, alpha, gamma, p)
}

\arguments{
    \item{I}{The incidence 0-1 matrix with unique row and column names, where rows are parts (genes) and columns are wholes (gene-sets).}
  \item{y}{Gene-level 0-1 data with the same names as the row names of I.}
  \item{alpha}{The false positive rate in role model, numeric value between 0 and 1. See reference.}
  \item{gamma}{The true positive rate in role model, numeric value between 0 and 1. See reference.}
  \item{p}{The prior active probability of wholes in role model, numeric value between 0 and 1. See reference.}
}

\details{R package \code{Rglpk} is used to perform the integer linear programming. Generally, alpha and gamma can be estimated from the gene-level data by users themselves (see reference for examples), and alpha is less than gamma. p can be estimated via R package \code{MGSA} with alpha and gamma fixed. Since ILP is a complex problem in the optimization field, the running time might be very long. This function is invoked in \code{sequentialRM}.
}

\value{The output has the same structure as \code{Rglpk_solve_LP} in the \code{Rglpk} package, which is a list consisting of optimum, solution (in the order of wholes and parts) and status.}

\references{Zhishi W., Qiuling H., Bret L. and Michael N.: A multi-functional analyzer uses parameter constaints to improve the efficiency of model-based gene-set analysis (2013). }
\seealso{\code{sequentialRM}}
\author{ Zhishi Wang, Michael Newton and Subhrangshu Nandi.}

\examples{
data(Type2D)
## Use 5 and 10 as the trimming parameters
newI <- subRM(I, 5, 10)
## the corresponding gene-level data
newy <- y[rownames(newI)]

## set the system parameters
alpha <- 0.00019
gamma <- 0.02279
p <- 0.00331
## perform the ILP
res <- ILP(newI, newy, alpha, gamma, p)
}
