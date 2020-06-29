melt_similarity <-
function(dat,decreasing=NA){
    
    m1=as.matrix(dat)
    m1[upper.tri(m1)]=NA #I suppose that no distance can be NA, so I can use this to do filtering
    diag(m1)=NA
    library(reshape2)
    m2=melt(m1) #I suppose that no distance can be NA, so I can use this to do filtering
    if(class(m2$Var1)!="character") m2$Var1=as.character(m2$Var1) #This is to prevent numeric names (the class should still be "character") being converted to "numeric" by melt()
    if(class(m2$Var2)!="character") m2$Var2=as.character(m2$Var2) #This is to prevent numeric names (the class should still be "character") being converted to "numeric" by melt()
    
    m2=m2[!is.na(m2[,3]) | is.nan(m2[,3]),] # "| is.nan(m2[,3])" is used because I want to keep the NaN values
    
    m2=m2[,c(2,1,3)] ##reorder the columns
    names(m2)=c("object_1","object_2","value") ##name the columns
    
    if(!is.na(decreasing)){
        m2=m2[order(m2[,3],decreasing=decreasing),] ##reorder by distance 
    }
    return(m2)
}
