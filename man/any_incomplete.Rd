\name{any_incomplete}
\alias{any_incomplete}
\title{
Check for missing values
}
\description{
Check if the matrix or dataframe contains any NA, NAN, NULL or ""
}
\usage{
any_incomplete(table)
}
\arguments{
  \item{table}{
  A matrix or dataframe
}
}
\author{
Peter I-Fan Wu
}
\examples{
mat=matrix(c(1:11,NA),nrow=3)
any_incomplete(mat)
}

