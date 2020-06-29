convert_table <-
function(dat,function_new_type){
    if(class(dat)=="matrix"|class(dat)=="data.frame"){
        for(i in 1:dim(dat)[2]){
            dat[,i]=function_new_type(dat[,i])
        }
    }else print("The input data is not a matrix or dataframe")
    
    return(dat)
}
