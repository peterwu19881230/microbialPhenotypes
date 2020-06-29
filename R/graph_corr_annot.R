graph_corr_annot <-
function(confusionMatrix_obj,metric,similarity,subset,cols,x_lab="High similarity -- Gene pairs ranked by similarity -- Low similarity",ylim,lwd){
    
    x_lab=paste0("high similarity -- ranked pairs -- low similarity (",similarity,")")
    
    for(i in seq(length(metric)*length(similarity))){
        if(i==1){
            plot(1:subset,confusionMatrix_obj[1:subset,][[metric]],xlab=x_lab,ylab=metric,type='l',col=cols[i],
                 ylim=ylim,lwd = lwd, main="Enrichment for co-annotations") 
            grid(lty='solid')
        }else{
            lines(1:subset,confusionMatrix_obj[1:subset,][[metric]],xlab=x_lab,ylab=metric,col=cols[i],lwd = lwd) 
        }
        
        
    }
    
    #add the negative control
    random_metric=paste0("random_",metric)
    lines(1:subset,confusionMatrix_obj[1:subset,][[random_metric]][1:subset],col='black',lty = 'dotted',lwd=2.5)
    
}
