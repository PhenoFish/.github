taxo_sp_check <- function(data){
  #data = fishshape
  #var <- sample(unique(data$species_name), 30)
  # check_ind <- synonyms(str_replace("Acanthobrama_terraesanctae", "_", " "),version = "19.04")
  #i = 5908
  #
  var <- unique(data$species_name)
  
  check <-  do.call(rbind,lapply(1:length(var),function(i){ 
    
    # For sub species keep the first and the last names
    if(is.na(word(var[i],3,sep="_"))){
      
      sp <- var[i]
      
    }else{
      
      sp <- paste0(word(var[i], 1,sep="_"),"_", word(var[i], 3,sep="_"))}
    
    print(paste0(i,"/",length(var),",",round(i/length(var),3)*100,"%"))
    
    check_ind <- synonyms(str_replace(sp, "_", " "),version = "19.04")
    
    check_ind  <- check_ind[check_ind$Status == "accepted name" | 
                              check_ind$Status == "synonym" | 
                              check_ind$Status == "provisionally accept",]
    
    if(nrow(check_ind)>1) { check_ind  <- check_ind[check_ind$Status == "accepted name",] }
    
    check_ind <-  data.frame(species_name = var[i],
                             SpecCode_Fishbase = check_ind$SpecCode, 
                             WoRMS_ID = check_ind$WoRMS_ID,
                             fishbase_name = check_ind$Species,
                             check = nrow(check_ind)) # check if very not a problem in the filter accepted, synonym...should be 1
    
  }))
  
  res <- merge(data,check, by = "species_name")
  
  return(res)
}
