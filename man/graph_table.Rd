\name{graph_table}
\alias{graph_table}
\title{
Plot heatmap that shows missing values
}
\description{
Generate a heatmap to visualize where missing values (NA/NAN/NULL/"") are located. The dependency is the pheatmap package
}
\usage{
graph_table(table)
}
\arguments{
  \item{table}{
A table that contains missing values (NA/NAN/NULL/"")
}
}
\author{
Peter I-Fan Wu
}
\examples{
mat=matrix(c(NA,2:11,NA),nrow=3)
graph_table(mat)
}
