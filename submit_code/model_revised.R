###### this file contains the function to run model simulation #######
### notice the main part of model will be uploaded after submission

# import packages
library(tidyverse)

run_model <- function(run_name,para_name, para_list,tag_name,iter_time){
  #### para_name: name of para (such as timevar); para_list: list of values of that estimating para;
  #### tag_name:used for tag, string; iter_time: num of maxround
###### list of para_name:
  # Subject, a_schema, h_schema, Beta_N, Beta_Var, a_generic, h_generic, 
  # Beta_gN, Beta_gVar, decay_speed, decay_speed_thres, thres_schema_init, thres_item_final
  # thres_item_inter, max_recovery, theta_shift, timevar
###### list of tag_name:
  # "SubjectID", "a_schema", "h_schema", "Beta_N", "Beta_Var", "a_generic", "h_generic", 
  # "Beta_gN", "Beta_gVar", "decay_speed", "decay_speed_thres", "thres_schema",
  # "thres_item_final","theta_shift","timevar"
###### NOTICE: we will create a directory named data to save results   
  ## run_name = 'thres4048_50'
  if (!dir.exists("data/")){
    dir.create("data/")
  }  
  if (!dir.exists(paste0("data/",run_name))){
    dir.create(paste0("data/",run_name))
  }  



## save results
savepath = file.path("data",run_name)
write_csv(ALL.df,file.path(savepath,'/allresult.csv'))
write_csv(Param.df, file.path(savepath,'/Paras.csv'))
write_csv(confidence.df, file.path(savepath,'/confidence.csv'))
write_csv(gconfidence.df, file.path(savepath, '/gconfidence.csv'))
write_csv(dwell_table, file.path(savepath,'/dwell_table.csv'))
write_csv(allresult,file.path(savepath,'/all_dwsch_added.csv'))
return(allresult)
}

