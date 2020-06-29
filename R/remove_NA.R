remove_NA <-
function(vec){
    vec[!is.na(vec)]
}
