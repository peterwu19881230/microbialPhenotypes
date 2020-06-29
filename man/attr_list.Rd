\name{attr_list}
\alias{attr_list}
\title{
Organize relational data into a list
}
\description{
Compute the attribute list by inputting relational data
}
\usage{
attr_list(name_attribute)
}
\arguments{
  \item{name_attribute}{
  A matrix or dataframe where 1st column contains the names; 2nd column contains corresponding attributes
}
}
\author{
Peter I-Fan Wu
}
\examples{
 name_attribute=data.frame(name=c(1,2,2,3,3),attribute=c("A","B","C","D","E"))
 attr_list(name_attribute)
}


