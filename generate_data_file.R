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
        read_tsv("./data/main_data_final.txt", col_names = T) 
    
     df_main_completed <-
        read_tsv("./data/main_completed.txt", col_names = T) 
     
     df_main_vaccinal <-
        read_tsv("./data/main_status_vaccinal.txt", col_names = T) 
     
     
     df_main_completed$PTID <- toupper(df_main_completed$PTID)
     
     df_main_vaccinal$PTID <- toupper(df_main_vaccinal$PTID)
   
     df_main_completed <- df_main_completed %>% select(-DateAj,-DateFrm,-DOB,-DeptVive,-AbxDiar)    
     
     df_main_vaccinal <- df_main_vaccinal %>% select(-DateAj,-DateFrm,-VD1rota,-VacPEV,-VD3rota,-VacRota,
                                                     -VD2rota,-VsrcRota,-VacChol,-VNDChol,-VD1Chol,-VNDrota,
                                                     -VD2Chol,-VD3Chol,-VsrcChol,
                                                     -VacMeng,-VNDMeng,-VD1Meng,-VD2Meng,-VD3Meng,-VsrcMeng,
                                                     -VacPent,-VNDPent,-VD1Pent,-VD2Pent,-VD3Pent,-VsrcPent,-VacCopie)
                    
     df_main_data <- df_main_data %>% left_join(df_main_completed,by= c("PTID"="PTID"))
     
     df_main_data <- df_main_data %>% left_join(df_main_vaccinal, by=c("PTID"="PTID"))
      
     # cleaning
    df_main_data <- 
        
        df_main_data %>%  
        # remove site with no code
        filter(!is.na(SiteCode)) %>%
        
        # remove DateFrm NA
        filter(!is.na(DateFrm)) %>%
        
        # include presepi dhis2 site name
        left_join(df_site_mapping, by = c("SiteCode" = "SiteCode")) %>%
        
        #include presepi dhis2 siteId
        left_join(df_orgunits, by = c("site_dhis2" = "name")) %>%
        
        ## year and Date
        mutate( DateFrm = mdy(DateFrm), 
                DateAj= mdy(DateAj),
                DateMod = mdy(DateMod),
                DateVisit = mdy(DateVisit),
                SpecDate = mdy(SpecDate),
                year_case = year(DateFrm), 
                month_case = month(DateFrm)) %>%
    
        ## AntibioticAdmin
        mutate(AntibioticAdmin = if_else(Doxy == -1,1,
                                         if_else(Azithrom == -1,1,
                                                 if_else(Erythrom == -1,1,
                                                         if_else(Tetracyc == -1,1,
                                                                 if_else(Amox == -1,1,
                                                                         if_else(Cipro == -1,1,
                                                                            if_else(TmpSmx == -1,1,0)
                                                                         )
                                                                 )
                                                         )
                                                 )
                                         ))
               ) %>%

     
       ## EtatdesSelles
        mutate(EtatdesSelles = if_else(DiarAqus != 0,1,
                                         if_else(DiarSang !=  0,1,
                                                 if_else(DiarGlar != 0,1,
                                                         if_else(DiarAcun != 0,1,0)
                                                        )
                                                 )
                                         )
               ) %>%
         
         #PlanTraitHydratation
        mutate(PlanTraitHydratation = if_else(is.na(PlanTrait),0,1)) %>%
         #PlanA
        mutate(PlanA = if_else(PlanTrait == 0,1,0)) %>% 
         #PlanB
        mutate(PlanB = if_else(PlanTrait == 1,1,0)) %>% 
         #PlanC
        mutate(PlanC = if_else(PlanTrait == 2,1,0)) %>% 
           #syndromeDiar
        mutate(SyndDiar = if_else( Syndrome == 1 || SyndDiar == -1,1,0)) %>%
         #syndromeFeb
         mutate(SyndFebrile = if_else( Syndrome == 2 || SyndFebrile == -1,1,0)) %>% 
         #syndromeInfRes
         mutate(SyndInfResp = if_else( Syndrome == 3 || SyndInfResp == -1,1,0))
         

    
    df_main_data
    # main data complement table
    # meningitis table ?? for meningitis will do a different script
    
}

loadSiteMapping <- function() {
    # facility maping : The file contains site name and site code in the legacy database , site name in dhis2 tracker
    df_site_mapping <-
        as.data.frame(read_tsv("./data/site_mapping.txt", col_names = T),
                      stringsAsFactors = FALSE)
    df_site_mapping
}



getVariableMapping <- function() {
    # variables mapping table
    # This is the most important input for the data generation process, match presepi variable names, dhis names
    df_mapping <-
        read_csv("./data/presepi_mapping_v3.csv", col_names = T)
    
    ## clean the mapping ---------------------------------------------------------------------------
    
    # removing NA values and any trailing spaces at the end of dataelements
    # this exclude presepi variable that are not mapped in dhis2 those data will not be transfered
    # This clean the mapping keeping only variables in presepi that we can find in the data load
    # check and allow us to double check if the mapping file is in sync whith the data imported because
    # the mapping file was not done from the main_data file
    
    df_mapping <-  df_mapping %>%
        
        #trim data removing any trailing spaces at the end of dataelements
        mutate(dataelement = trimws(dataelement, which = c("right"))) %>%
        
        #exclude presepi variable that are not mapped in dhis2 those data will not be transfered
        filter(!is.na(code)) %>%
        
        #keeping only variables in presepi that we can find in the main data
        filter(VariableName %in% names(df_main_data)) %>%
        
        # adding dataelement Ids in the mapping dataframe
        left_join(df_metadata, by = c("dataelement" = "name")) %>%
        
        #exclude presepi variable that are not mapped??? remove element with no code
        filter(!is.na(Id)) %>%
        
        # add ProgramStageId in the mapping dataframe, indicates the stage  for each dataelement
        left_join(df_programStageDataElement, by = c("Id" = "dataElement")) %>%
        
        # convert to dataframe
        as.data.frame(stringsAsFactors = FALSE)
    
    
    
}



df_site_mapping <- loadSiteMapping()

df_main_data <- loadMainData()

df_mapping <- getVariableMapping()




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
