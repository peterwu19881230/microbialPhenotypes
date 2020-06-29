ternary_convert <-
function(matrix,threshold){
    ternary<-matrix(,nrow(matrix),ncol(matrix)) #This creates an empty matrix (Precisely, matrix with all NAs)
    rownames(ternary)<-rownames(matrix)
    colnames(ternary)<-colnames(matrix)
    ternary[matrix>-1*threshold&matrix<threshold]=0
    ternary[matrix<=-1*threshold]=-1
    ternary[matrix>=threshold]=1
    return(as.data.frame(ternary)) 
}
