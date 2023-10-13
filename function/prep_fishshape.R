
#<°)))>< <°)))>< <°)))>< <°)))>< <°)))>< <°)))>< <°)))>< <°)))>< <°)))>< <°)))>< <°)))>< <°)))>< 
# _______  __       _______. __    __       _______. __    __       ___      .______    _______ 
#|   ____||  |     /       ||  |  |  |     /       ||  |  |  |     /   \     |   _  \  |   ____|
#|  |__   |  |    |   (----`|  |__|  |    |   (----`|  |__|  |    /  ^  \    |  |_)  | |  |__   
#|   __|  |  |     \   \    |   __   |     \   \    |   __   |   /  /_\  \   |   ___/  |   __|  
#|  |     |  | .----)   |   |  |  |  | .----)   |   |  |  |  |  /  _____  \  |  |      |  |____ 
#|__|     |__| |_______/    |__|  |__| |_______/    |__|  |__| /__/     \__\ | _|      |_______|
#<°)))>< <°)))>< <°)))>< <°)))>< <°)))>< <°)))>< <°)))>< <°)))>< <°)))>< <°)))>< <°)))>< <°)))>< 
prep_fishshape <- function(){
  

  fishshape <- readr::read_delim(here::here("data","FishShapes_specimensv1.csv"),delim = ",")%>%
  janitor::clean_names()%>%
  dplyr::mutate(species_names = str_replace(tree_name, "_", " "))

fishshape$min_caudalpeduncle_width <- str_replace(fishshape$min_caudalpeduncle_width, ";;", "")
fishshape$min_caudalpeduncle_width <- as.numeric(fishshape$min_caudalpeduncle_width)


#TO CHECK 
colnames(fishshape)[1] <- "specimen_id_fishshape"
fishshape$specimen_id  <- paste0("pf_fishshape_", seq(1:nrow(fishshape))) 

fishshape <- reshape2::melt(fishshape,
                            # ID variables - all the variables to keep but not split apart on
                            id.vars=c("specimen_id","species_names"),
                            # The source columns
                            measure.vars=c("total_weight","standard_length","max_body_depth","max_fish_width","head_depth","lower_jaw_length", 
                                           "mouth_width","min_caudalpeduncle_depth","min_caudalpeduncle_width"),
                            # Name of the destination column that will identify the original
                            # column that the measurement came from
                            variable.name="trait_name",
                            value.name="trait_value"
)


fishshape_trait_cat<- data.frame(trait_name = c("total_weight","standard_length","max_body_depth",
                                                "max_fish_width","head_depth","lower_jaw_length", 
                                                "mouth_width","min_caudalpeduncle_depth","min_caudalpeduncle_width"),
                                 unit    = c("gramme","millimeters",
                                             "millimeters","millimeters","millimeters",
                                             "millimeters","millimeters","millimeters",
                                             "millimeters"))




#Remove repetion of some sp
#fishshape$species_name <- sapply(fishshape$species_name, function(x) paste(unique(unlist(str_split(x,"_"))), collapse = "_"))

fishshape$taxo_scale  <- rep("individual", nrow(fishshape)) 
fishshape$method      <- rep("measurement on fish", nrow(fishshape))
fishshape$life_stage  <- rep("adult", nrow(fishshape)) 
fishshape$temperature <- rep(NA, nrow(fishshape)) 
fishshape$number_ind_measured <- rep(1, nrow(fishshape)) 
fishshape$latitude    <- rep(NA, nrow(fishshape)) 
fishshape$longitude   <- rep(NA, nrow(fishshape)) 
fishshape$location    <- rep(NA, nrow(fishshape)) 
fishshape$dataset     <- rep("fishshape", nrow(fishshape))  
fishshape$fieldsources <- rep("museum", nrow(fishshape))  

fishshape <- code_sp_check(data = fishshape)

fishshape <- merge(fishshape,fishshape_trait_cat, by ="trait_name")

save(fishshape, file = here::here("outputs","clean_data_set","fishshape.RData"))
}