
    
 getDataList <-  function(tr_main_data)   {  
     
    main_list <-  map(tr_main_data,function(x){
        instance <-  list( instance_attribute = list(orgunit = as.character(x[["orgunitsId"]]),
                                                     enrollmentdate=dmy(x[["DateFrm"]])),
                           attributes = list(),
                           ProgramStages= list())
        
          tr_mapping_attr <- df_mapping %>% 
            filter(tracker_type == "Attribute") %>%
              select(VariableName,Id) %>%
                        transpose()
          
        instance[["attributes"]] <-  map(tr_mapping_attr, function(y){
              
              var_name <- y[["VariableName"]]
              attribute <- list(id=y[["Id"]], value=x[[var_name]], name =y[["VariableName"]] )
              
          })
        
         tr_programStage <- df_programStages %>%
                        filter(name %in% c("QUESTIONNAIRE DESTINÉ AU PATIENT",
                                           "RESULTAT POUR SELLES",
                                           "RESULTAT POUR SERUM",
                                           "RESULTAT POUR LIQUIDE PLEURAL")) %>%
                        select(name,Id) %>%
                        transpose()
         
        instance[["ProgramStages"]] <- map(tr_programStage, function(v){
         ProgramStage <- list( program_stage_attribute = 
                                       list(id =v[["Id"]], name=v[["name"]]),
                                    events =
                                        list(attrs = list(), dataValues = list()) )
             
             
               stageId <- v[["Id"]]
             # create stage data elements 
             tr_mapping_dataElement <- df_mapping %>% 
            filter(tracker_type == "dataElement", programStage == stageId) %>%
              select(VariableName,Id) %>%
                        transpose()
             
             ProgramStage[["events"]][["attrs"]] <- 
                 list(program =presePiId,orgunit = as.character(x[["orgunitsId"]]), eventDate = dmy(x[["DateFrm"]]) )
             
             ProgramStage[["events"]][["dataValues"]] <-  map(tr_mapping_dataElement, function(s){
             dataElment_name <- s[["VariableName"]]
            dataValue <- list(id=s[["Id"]], value=x[[dataElment_name]], name = s[["VariableName"]])
                 
             })
             
             ProgramStage
         })
        
        instance
         
         
     })
 }
 

 
 
 
 generateFile <- function(){
 
     
  import_group <-  df_main_data %>% 
     filter(!is.na(DateFrm),!is.na(orgunitsId)) %>%
     mutate(new_DateFrm = mdy(DateFrm)) %>%
     mutate(year_case =  year(new_DateFrm), month_case = month(new_DateFrm)) %>%
     group_by(SiteCode, year_case,month_case) %>% 
     summarise(n= n())  %>% 
     arrange(SiteCode, desc(year_case), month_case)
  
    
     import_group_tr <- import_group %>% transpose()
    
     map(import_group_tr, function(x){
         
         code <- x[["SiteCode"]]
         year_c <- x[["year_case"]]
         month_c <- x[["month_case"]]
         
         
        tr_main_data_test <- df_main_data %>%
        filter(!is.na(DateFrm)) %>%
        # test one site
        filter(SiteCode == code,year_case == year_c, month_case == month_c) %>%
        # transpose to list
        transpose() 
        
         batch_list <- getDataList(tr_main_data_test)
         batch_json <- toJSON(batch_list,pretty = T)
         
         filename <- paste(code,year_c,month_c, sep = "_")
         dirfile <- paste0("processed_data/output/",filename,".json")
         write_json(batch_json,dirfile)

         
     })
 }
 
    
      
     
     


 