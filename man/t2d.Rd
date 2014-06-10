\name{t2d}
\alias{t2d}
\docType{data}
\title{Example data set}
\description{
The data set includes an incidence matrix I and the corresponding gene-level data y.
\item{I}{The incidence 0-1 matrix with unique row and column names,
  where rows are genes and columns are gene-sets.
  The size of I is 10626 x 6037.}
\item{y}{Gene-level 0-1 data with the same names as the row names of I.}
}
\usage{data(t2d)}
\format{
  The format is:
List of 2
 $ I: num [1:10626, 1:6037] 1 1 1 1 1 1 1 1 1 1 ...
  ..- attr(*, "dimnames")=List of 2
  .. ..$ : chr [1:10626] "291" "1763" "1890" "4205" ...
  .. ..$ : chr [1:6037] "GO:0000002" "GO:0000012" "GO:0000018" "GO:0000028" ...
 $ y: Named num [1:10626] 0 0 0 0 0 0 0 0 0 0 ...
  ..- attr(*, "names")= chr [1:10626] "291" "1763" "1890" "4205" ...
}
\details{
From a large-scale genome-wide association study (GWAS) involving more
than 34,000 cases and 114,000 control subjects, 77 human genes have been
implicated as affecting T2D disease susceptibility (see reference). To
assess the functional content of this gene list, we extracted 6037 gene
ontology terms, each annotating between 5 and 50 genes. These 6037 terms
annotate a total of 10,626 genes; among the 77 T2D-associated genes, 58
are in this moderately annotated class. 
}
\references{
Zhishi W., Qiuling H., Bret L. and Michael N.: A multi-functional
analyzer uses parameter constaints to improve the efficiency of
model-based gene-set analysis (2013).  

Andrew P. M. and others: Large-scale association analysis provides
insights into the genetic architecture and pathophysiology of type 2
diabetes (2012). Nature Genetics, Volume 44-9. 
}
\examples{
data(t2d)

str(t2d$I)
str(t2d$y)
}
\keyword{datasets}
