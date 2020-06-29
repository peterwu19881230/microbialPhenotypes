get_confusionMatrix_and_metrics <-
function(df,annot,similarity,seed=103){   
    
    if(length(similarity)!=dim(df)[1]){ #if new similarity is in the the table, just retreive from the table
        similarity=df[,similarity]
    }
    df=cbind(df[,annot],similarity)
    
    if(length(annot)>=2){
        coannotation=(rowSums(df[,annot])>=1)
        cumsum_=cumsum(coannotation[order(df[,dim(df)[2]])])
        return(confusionMatrix_metrics(cumsum_,seed=seed))
    } 
    
    cumsum_=cumsum(df[,1][order(df[,2])])
    return(confusionMatrix_metrics(cumsum_,seed=seed))
}
