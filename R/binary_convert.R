binary_convert <-
function(matrix,threshold){
    binary<-matrix(,nrow(matrix),ncol(matrix)) #This creates an empty matrix (Precisely, matrix with all NAs)
    rownames(binary)<-rownames(matrix)
    colnames(binary)<-colnames(matrix)
    binary[matrix>=threshold]=1
    binary[matrix<threshold]=0
    return(as.data.frame(binary)) 
}
