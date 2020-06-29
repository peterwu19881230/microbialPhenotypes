\name{binary_convert}
\alias{binary_convert}
\title{
Discretize data to binary form
}
\description{
Convert values within a matrix to binary form (0 or 1)
}
\usage{
binary_convert(matrix, threshold)
}
\arguments{
  \item{matrix}{
  A matrix that contains numbers in real values
}
  \item{threshold}{
  A positive real value that determines the cutoff
}
}
\author{
Peter I-Fan Wu
}
\examples{
mat=matrix(c(1:12),nrow=3)
binary_convert(mat,threshold = 5)
}

