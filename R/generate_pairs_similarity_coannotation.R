generate_pairs_similarity_coannotation <-
function(data,attribute_list,dist_metric){
    if(class(rownames(data))!="character")rownames(data)=as.character(rownames(data))
    
    #get table 1: id1_id2_sortedDistance
    #========================================
    distance=dist_metric(data)
    distance_table=melt_dist(distance)
    #========================================
    
    
    
    #get table 2: id1_id2_coAnnotated
    #========================================
    
    attribute_list=attribute_list[rownames(data)] 
    ##(important) change the order of elements in "attribute_list" so it matches those in "data"
    ##=>this prevents later problem in left_join. If this is not done the following won't be properly joined:
    ##=> Obj1 - Obj2 - dist = ("1","2","0.9"), Obj1 - Obj2 - sameORnot = ("2","1",1)
    
    
    uniqueAttr_vec=unlist(attribute_list) %>% unique
    uniqueAttr_vec=uniqueAttr_vec[!is.na(uniqueAttr_vec)]
    
    
    idRow_attrCol=sapply(attribute_list,FUN = function(attribute){ #idRow_attrCol will be created as a matrix
        uniqueAttr_vec %in% attribute  #c("A","B","C") %in% NA will give FALSE, so no worries here
    }) %>% t
    
    
    ##This gets pairwise comparison of having the same annotations or not
    coAnnotationMatrix=idRow_attrCol %*% t(idRow_attrCol)
    coAnnotationMatrix=ifelse(coAnnotationMatrix>=1,1,0) #The matrix structure will be preserved even after using ifelse()
    
    
    colnames(coAnnotationMatrix)=rownames(coAnnotationMatrix) #Have to manually give the colnames (which are identical to the row names)
    
    
    coAnnotated_table=coAnnotationMatrix %>% as.dist %>% melt_dist #after this, the result becomes a dataframe
    #========================================
    
    #merge table 1 and table 2 and create a cumsum column
    
    result_df=cbind(distance_table,same_annot=coAnnotated_table[[3]]) 
    ##the order of the first 2 column should be the same. No need to use left_join
    ##(!) I am not sure if in rare cases this will not work 
    ##(eg. dist_metirc order the cols and rows in a different way -> melt_dist get different column 1 and column 2)
    
    result_df=result_df[order(result_df[[3]]),]; rownames(result_df)=NULL #this resets the rownames after sorting
    names(result_df)[1:3]=c("mutant1","mutant2","similarity") #have to correct the colnames here
    
    return(result_df)
}
