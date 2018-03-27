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

rm(list = ls())
source("query_presepi_metadata.R")

loadMainData <- function() {
    
    # main_data table
    main_data <- read_tsv("./data/main_data_final.txt",col_names = T)
    
    # main data complement table
    # meningitis table ?? for meningitis will do a different script

}

loadSiteMapping <- function(){
    
        # facility maping : The file contains site name and site code in the legacy database , site name in dhis2 tracker
    site_mapping <- as.data.frame( read_tsv("./data/site_mapping.txt", col_names = T),
                                   stringsAsFactors = FALSE)
}



getVariableMapping <- function() {
    
    # variables mapping table
    # This is the most important input for the data generation process, match presepi variable names, dhis names
   mapping <- read_csv("./data/presepi_mapping_v2.csv",col_names = T)
    
    ## clean the mapping ---------------------------------------------------------------------------
    
    # removing NA values and any trailing spaces at the end of dataelements
    # this exclude presepi variable that are not mapped in dhis2 those data will not be transfered
    # This clean the mapping keeping only variables in presepi that we can find in the data load
    # check and allow us to double check if the mapping file is in sync whith the data imported because 
    # the mapping file was not done from the main_data file
        
   mapping <-  mapping %>%
       
       #trim data removing any trailing spaces at the end of dataelements
       mutate(dataelement = trimws(dataelement, which= c("right"))) %>%
       
       #exclude presepi variable that are not mapped in dhis2 those data will not be transfered
       filter(!is.na(dataelement)) %>%
       
       #keeping only variables in presepi that we can find in the main data
       filter(VariableName %in% names(main_data)) %>%
      
       # adding dataelement Ids in the mapping dataframe
       left_join(df_metadata,by=c("dataelement" = "name")) %>%
       
       #exclude presepi variable that are not mapped 
       filter(!is.na(Id)) %>%
       
       # add ProgramStageId in the mapping dataframe, indicates the stage  for each dataelement
       left_join(df_programStageDataElement, by = c("Id" = "dataElement")) %>%
        
       # convert to dataframe
       as.data.frame(stringsAsFactors = FALSE)
    
  
        
}

    loadMainData()
    
    loadSiteMapping()
    
    getVariableMapping()

generateXMLDataFile <- function() {
    
    # select only data correctly mapped with dhis2 using ?? why ??
    # data needed to create tracker instances
    loop_data <- main_data %>%
        select(SiteCode,DateFrm,mapping$VariableName) %>%
        as.data.frame(stringsAsFactors = FALSE)
    
    
    ##  generate the data file -------------------------------------------------------------------------
    
    # Use the source data,the site mapping, the variable mapping, and the metadata from dhis2
    # to create and xml file that will be use as input for the data transfer
    # your are doing 2 functions 1- assign data element to data 2 - create xml or json
    
    
    node <- newXMLNode("data", attrs = list(trackedEntity = trackedEntity,program = programId[[2]]))
    nodeCollection <- c("Instances")
    sapply(nodeCollection,newXMLNode,parent = node)
    
    # can i use walk iteration function
    # for each row in the main data
    for(drow in 1:nrow(head(loop_data,10))){
        
        ##mapping trackedentityinstances and create xml data
        SiteCode <- loop_data[drow,"SiteCode", drop = TRUE]
        DateFrm <-  loop_data[drow,"DateFrm",drop = TRUE]
        
        # print(SiteCode)
        # print(DateFrm)
        # get site Id from tb_orgunit
        # how to filter to return a caracter string not datatable or tibble ????? 
        site_name <- site_mapping[site_mapping$SiteCode == "HSC","site_dhis2", drop = TRUE]
        
        print(site_name)
        site_name <- as.character(site_name)
        # orgunit <- filter(tb_orgunits, name == "HFSC:Hopital Foyer Ste Camille")$Id
        orgunit <- tb_orgunits[which(tb_orgunits$name == site_name),"Id",drop = TRUE]
        orgunit
        # orgunit <- tb_orgunits[2,"Id",drop = TRUE]
        
        # print(orgunit)
        
        # create instance
        instance <- newXMLNode("instance",
                   attrs = list(orgunit = orgunit,enrollmentdate =as.character(DateFrm)),
                   parent = node[["Instances"]])
        
        #why this should not change
        attr_list <- mapping[mapping$tracker_type == "Attribute",] 
        attributes <- newXMLNode("attributes",parent = instance)
        
        ## can i use walk iteration function ??? this could be a vector
        ## Adding instances attributes
        for(attr_row in 1:nrow(attr_list)) {
        
            attr_Id <- attr_list[attr_row,"Id", drop = TRUE]
            attr_Name <- attr_list[attr_row,"VariableName", drop = TRUE]
            
            # print(attr_Id)
            # print(attr_Name)
            
            attr_Value <- loop_data[drow,attr_Name, drop = TRUE]
            newXMLNode("attribute",
                       attrs = list(id=attr_Id, value=attr_Value, name =attr_Name ),parent = attributes)
                
        }
        
          # create stages tag
          nodeProgramStages <- newXMLNode("ProgramStages",parent = instance)
        
        ## mapping for each programstages dataelement and create xml data
        # add programstage node
         
          for(rowstage in 1:nrow(head(tb_programStage,4)) ) {
    
              name <- tb_programStage[rowstage,"name",drop = TRUE]
              Id <- tb_programStage[rowstage,"Id",drop = TRUE]
              
              # print(name)
              # print(Id)
              
              nodeprogramStage <- newXMLNode("ProgramStage",
                                              attrs = list(id =Id, name=name), parent=nodeProgramStages)
                 # what dataelement are in each stage
                 # 
              DE_stage <- mapping[ which(mapping$programStage == Id),]
              #DE_stage <- filter(mapping, programStage ==  Id)
              
              events <- newXMLNode("events",attrs = list(program =programId[[2]], orgunit = orgunit,
                                                         eventDate = DateFrm ),parent = nodeprogramStage)
              dataValues <- newXMLNode("dataValues",parent = events)
    
              # each data elements is added as datavalue for the stage
              for(de in 1:nrow(DE_stage)){
    
                  de_Id <- DE_stage[de,"Id", drop = TRUE]
                  de_Name <- DE_stage[de,"VariableName", drop = TRUE]
                  
                  # print(de_Id)
                  # print(de_Name)
                    
                   # ?? what best way to get the value may be a function
                   de_Value <- loop_data[drow,de_Name, drop = TRUE]
                   newXMLNode("dataValue",
                             attrs = list(id=de_Id, value=de_Value, name = de_Name),parent = dataValues)
              }
              
        }
            
      
    }
    saveXML(node,"node.xml")
}


# write_csv(main_data_col,"main_data_col.csv")
#write_csv(tb_dataElement,"dataElements2.csv")
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
# 
