\name{hamming_distance}
\alias{hamming_distance}
\title{
Calculate Hamming distance
}
\description{
Linear algebraic way to compute pairwise Hamming distances from a matrix. This is much faster than using double For loops.
}
\usage{
hamming_distance(X)
}
\arguments{
  \item{X}{
A matrix that contains real values
}
}
\references{
https://johanndejong.wordpress.com/2015/09/23/fast-hamming-distance-in-r/
}
\author{
Peter I-Fan Wu
}
\note{
%%  ~~further notes~~
}
\examples{
mat=matrix(c(1,1,0,0,
             1,0,1,1,
             1,0,0,0
          ),nrow=3,byrow=T)
hamming_distance(mat)
}
