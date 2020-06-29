\name{melt_similarity}
\alias{melt_similarity}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Melt a table into long form
}
\description{
Transform a distance object or a similarity matrix to a long form dataframe
}
\usage{
melt_similarity(dat, decreasing = NA)
}
\arguments{
  \item{dat}{
An R disttance object or similarity matrix
}
  \item{decreasing}{
Whether to sort the result based on decreasing values of distance/similarity
}
}
\author{
Peter I-Fan Wu
}
\examples{
mat=matrix(c(1,1,0,0,
             1,0,1,1,
             1,0,0,0
          ),nrow=3,byrow=T)
dist_=hamming_distance(mat)
melt_similarity(dist_,decreasing=T)
}
