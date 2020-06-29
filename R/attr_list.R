attr_list <-
function(name_attribute){
    
    name=name_attribute[,1]
    attribute=name_attribute[,2]
    
    uniqueName=unique(name)
    
    attribute_list=list()
    for(i in 1:length(uniqueName)){
        attribute_list[[i]]=attribute[name==uniqueName[i]]
    }
    
    names(attribute_list)=uniqueName
    
    return(attribute_list)
}
