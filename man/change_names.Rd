\name{change_names}
\alias{change_names}
\title{
Change names
}
\description{
Change rownames or colnames of a matrix/dataframe based on another matrix/dataframe
}
\usage{
change_names(rowOrCol, Table, nameForTable)
}
\arguments{
  \item{rowOrCol}{
  Give "row" or "col" as input to decide where row names or column names are to be changed
}
  \item{Table}{
  The table in which the row or column names are to be changed
}
  \item{nameForTable}{
  A 2 column matrix/dataframe: 1st column/row: original names; 2nd column/row: new names
}
}
\author{
Peter I-Fan Wu
}
\examples{
mat=matrix(c(1:12),nrow=3)
rownames(mat)=c("1","2","3")
new_row_names=c("one","two","three")

change_names(rowOrCol="row", mat, data.frame(original=rownames(mat), new=new_col_names))
}

