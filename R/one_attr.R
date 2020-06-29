one_attr <-
function(attribute_list){
    
    combination=t(combn(names(attribute_list),2))
    
    sameORnot=c()
    for(i in 1:dim(combination)[1]){
        id1=combination[i,1]
        id2=combination[i,2]
        
        sameORnot[i]=ifelse( sum( attribute_list[[id1]] %in% attribute_list[[id2]])>=1 & 
                                 !(anyNA(attribute_list[[id1]])) & 
                                 !(anyNA(attribute_list[[id2]])),
                             1,0)
    }
    
    out=data.frame(combination,sameORnot)
    names(out)[1:2]=c("mutant1","mutant2")
    
    return(out)
}
