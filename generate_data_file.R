################################################################################# 
## PResePi data migration
## Mayer Antoine, CDC Haiti
## Date : 12/12/2017
## Purpose : Generate data file
##              
##
##################################################################################


library(RCurl)
library(XML)
library(tidyverse)
library(lubridate)
library(httr)
library(jsonlite)

rm(list = ls())
source("query_presepi_metadata.R")

loadMainData <- function() {
    
    # main_data table
    df_main_data <-
        read_tsv("./data/main_data_final.txt",col_names = T) %>%
        
        # remove site with no code
        filter(!is.na(SiteCode)) %>%
        
        # remove DateFrm NA
        filter(!is.na(DateFrm)) %>%
    
            # include presepi dhis2 site name
      left_join(df_site_mapping,by = c("SiteCode" = "SiteCode")) %>%
        
        #include presepi dhis2 siteId
        left_join(df_orgunits, by = c("site_dhis2" = "name")) %>%
        
        ## year
        mutate( year_case = year(mdy(DateFrm)), month_case = month(mdy(DateFrm)))
        
    
    df_main_data
    # main data complement table
    # meningitis table ?? for meningitis will do a different script

}

loadSiteMapping <- function(){
    
        # facility maping : The file contains site name and site code in the legacy database , site name in dhis2 tracker
    df_site_mapping <- as.data.frame( read_tsv("./data/site_mapping.txt", col_names = T),
                                   stringsAsFactors = FALSE)
    df_site_mapping
}



getVariableMapping <- function() {
    
    # variables mapping table
    # This is the most important input for the data generation process, match presepi variable names, dhis names
   df_mapping <- read_csv("./data/presepi_mapping_v2.csv",col_names = T)
    
    ## clean the mapping ---------------------------------------------------------------------------
    
    # removing NA values and any trailing spaces at the end of dataelements
    # this exclude presepi variable that are not mapped in dhis2 those data will not be transfered
    # This clean the mapping keeping only variables in presepi that we can find in the data load
    # check and allow us to double check if the mapping file is in sync whith the data imported because 
    # the mapping file was not done from the main_data file
        
   df_mapping <-  df_mapping %>%
       
       #trim data removing any trailing spaces at the end of dataelements
       mutate(dataelement = trimws(dataelement, which= c("right"))) %>%
       
       #exclude presepi variable that are not mapped in dhis2 those data will not be transfered
       filter(!is.na(dataelement)) %>%
       
       #keeping only variables in presepi that we can find in the main data
       filter(VariableName %in% names(df_main_data)) %>%
      
       # adding dataelement Ids in the mapping dataframe
       left_join(df_metadata,by=c("dataelement" = "name")) %>%
       
       #exclude presepi variable that are not mapped 
       filter(!is.na(Id)) %>%
       
       # add ProgramStageId in the mapping dataframe, indicates the stage  for each dataelement
       left_join(df_programStageDataElement, by = c("Id" = "dataElement")) %>%
        
       # convert to dataframe
       as.data.frame(stringsAsFactors = FALSE)
    
  
        
}

  
    
    df_site_mapping <- loadSiteMapping()
    
    df_mapping <- getVariableMapping()
    
    df_main_data <- loadMainData()
    
# which sites ? by date ? how many records by site?

generateXMLDataFile <- function() {

    
    df_main_data  <-  df_main_data %>%
        
        # include presepi dhis2 site name
      left_join(df_site_mapping,by = c("SiteCode" = "SiteCode")) %>%
        
        #include presepi dhis2 siteId
        left_join(df_orgunits, by = c("site_dhis2" = "name"))  %>%
        
        # test one site
        filter(SiteCode == "HSN")
       
        
    
    # create root node
    node <- newXMLNode("data", attrs = list(trackedEntity = trackedEntity,program = presePiId))
    nodeCollection <- c("Instances")
    sapply(nodeCollection,newXMLNode,parent = node)
    
    tr_main_data_test <- df_main_data %>%
        
            # transpose to list
                transpose() %>%
        
            # test
              head(10)

     for( i in seq_along(tr_main_data_test)) {
         
         x <- tr_main_data_test[[i]]
         
        # create instance
        instance <- newXMLNode("instance",
                   attrs = list(orgunit = as.character(x[["orgunitsId"]]),enrollmentdate =as.character(x[["DateFrm"]])),
                   parent = node[["Instances"]])
        
        # create attribute
         attributes <- newXMLNode("attributes",parent = instance)
        tr_mapping_attr <- df_mapping %>% 
            filter(tracker_type == "Attribute") %>%
              select(VariableName,Id) %>%
                        transpose()
        
        for( j in seq_along(tr_mapping_attr)) {

            y <- tr_mapping_attr[[j]]
            var_name <- y["VariableName"]$VariableName
            newXMLNode("attribute",
                       attrs = list(id=y[["Id"]], value=x[[var_name]], name =y[["VariableName"]] ),parent = attributes)
        }
        
        
        # create program stage node
        nodeProgramStages <- newXMLNode("ProgramStages",parent = instance)
        tr_programStage <- df_programStages %>%
                        filter(name %in% c("QUESTIONNAIRE DESTINÉ AU PATIENT",
                                           "RESULTAT POUR SELLES",
                                           "RESULTAT POUR SERUM",
                                           "RESULTAT POUR LIQUIDE PLEURAL")) %>%
                        select(name,Id) %>%
                        transpose()
        
        for( k in seq_along(tr_programStage)){
            
            v <- tr_programStage[[k]]    
             nodeprogramStage <- newXMLNode("ProgramStage",
                                              attrs = list(id =v[["Id"]], name=v[["name"]]), parent=nodeProgramStages)
            
            
             stageId <- v[["Id"]]
             # create stage data elements 
             tr_mapping_dataElement <- df_mapping %>% 
            filter(tracker_type == "dataElement", programStage == stageId) %>%
              select(VariableName,Id) %>%
                        transpose()
             
              events <- newXMLNode("events",attrs = list(program =presePiId, orgunit = as.character(x[["orgunitsId"]]),
                                                         eventDate = x[["DateFrm"]] ),parent = nodeprogramStage)
              dataValues <- newXMLNode("dataValues",parent = events)
              
             for(l in seq_along(tr_mapping_dataElement)){
                 s <- tr_mapping_dataElement[[l]]
                 dataElment_name <- s[["VariableName"]]
                  newXMLNode("dataValue",
                             attrs = list(id=s[["Id"]], value=x[[dataElment_name]], name = s[["VariableName"]]),parent = dataValues)
                 
             }
             
            } 
        
     }
    
  
    saveXML(node,"node2.xml")
}

generateXMLDataFile()


# write_csv(main_data_col,"main_data_col.csv")
# write_csv(tb_dataElement,"dataElements2.csv")
# write_csv(tb_programTrackedEntityAttribute,"EntityAttribute.csv")

# Create xml template with the data needed
# review the request sequence to populate the tracker to ensure you have alll the data needed
# how you will create the input data (payload) : merge metadata & datavalue
# how will you validate the import
# test environnement 


## Create and enroll tracked entity instances /api/trackedEntityInstances 
# {
#     "trackedEntity": "tracked-entity-id",
#     "orgUnit": "org-unit-id",
#     "attributes": [ {
#         "attribute": "attribute-id",
#         "value": "attribute-value"
#     } ],
#     "enrollments": [ {
#         "orgUnit": "org-unit-id",
#         "program": "program-id",
#         "enrollmentDate": "2013-09-17",
#         "incidentDate": "2013-09-17"
#     }, {
#         "orgUnit": "org-unit-id",
#         "program": "program-id",
#         "enrollmentDate": "2013-09-17",
#         "incidentDate": "2013-09-17"
#     } ]
# }
## getEnrollmentId
## createEvents /api/26/events
## what about multiple programStage, should be sent for each stage with data
# {
#     "trackedEntityInstance":xxxxxxxxxx,
#     "program": "eBAyeGv0exc",
#     "orgUnit": "DiszpKrYNg8",
#     "eventDate": "2013-05-17",
#     "programStage":xxxxxxxxxx,
#     "enrollment": xxxxxxxxxxx,
#     "status": "COMPLETED",
#     "storedBy": "admin",
#     "coordinate": {
#         "latitude": 59.8,
#         "longitude": 10.9
#     },
#     "dataValues": [
#         { "dataElement": "qrur9Dvnyt5", "value": "22" },
#         { "dataElement": "oZg33kd9taw", "value": "Male" },
#         { "dataElement": "msodh3rEMJa", "value": "2013-05-18" }
#         ]
# }

# map each colunm of the maintable to its attribute or dataElementID in csv
# add a programStageId in this mapping df
#laod maintable and  create a df with column attribute and DE Id and value as row
# create a column orgunit with orgnuit ID in the df
# save the df
# split the df  in block by trackEntityAttributes and ProgramStages
# create the XMl data file

# review code  structure
# fix xml caracter
# call kenold -- atttibute presepi - new program stage
# review mapping
# prepare test batch - send email
# generate csv format

