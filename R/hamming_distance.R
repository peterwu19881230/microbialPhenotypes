hamming_distance <-
function(X) {
    X=as.matrix(X)
    uniqs <- unique(as.vector(X))
    U <- X == uniqs[1]
    H <- U %*% t(U)
    for ( uniq in uniqs[-1] ) {
        U <- X == uniq
        H <- H + U %*% t(U)
    }
    ncol(X) - H
}
