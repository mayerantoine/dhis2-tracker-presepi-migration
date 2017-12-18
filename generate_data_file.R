################################################################################# 
## PResePi data migration
## Mayer Antoine, CDC Haiti
## Date : 12/12/2017
## Purpose : Generate data file
##              
##
##
##################################################################################



library(RCurl)
library(XML)
library(tidyverse)


#rm(list = ls())

####LOAD MAIN DATA
main_data <- read_tsv("./data/main_data_final.txt",col_names = T)
main_data_col <- as.data.frame(names(main_data))

presepi_sites <- distinct(main_data[,c("SiteName","SiteCode")])
presepi_sites <- presepi_sites[!is.na(presepi_sites$SiteName),]

## LOAD site mapping

site_mapping <- read_tsv("./data/site_mapping.txt", col_names = T)

source("query_presepi_metadata.R")

#### LOAD Mapping
mapping <- read_csv("./data/presepi_mapping_v2.csv",col_names = T)
mapping <- mapping[!is.na(mapping$dataelement),]
mapping$dataelement <- trimws(mapping$dataelement,which = c("right"))

metadata <- rbind(tb_programTrackedEntityAttribute[c(1:2)],tb_dataElement[c(1:2)])

mapping <- left_join(mapping,metadata,by=c("dataelement" = "name"))

node <- newXMLNode("data", attrs = list(trackedEntity = trackedEntity,program = programId[[2]]))

nodeCollection <- c("Instances","ProgramStages")
sapply(nodeCollection,newXMLNode,parent = node)

cat(saveXML(node))

mapping_p <- mapping %>%  filter(!is.na(mapping$Id)) %>% select(VariableName,Id)

mapping_p

mainVariables <- as.data.frame(names(main_data))

is_mapped <- mapping_p$VariableName %in% names(main_data)

mapping_f <- mapping_p[is_mapped,] %>% as.data.frame()

loop_data <- main_data %>%
    select(SiteCode,DateFrm,mapping_f$VariableName) %>%
    as.data.frame()
    


for(drow in 1:nrow(head(loop_data,10))){
    
    
    ##mapping trackedentityinstances and creaate xml data
    SiteCode <- loop_data[drow,"SiteCode"]
    DateFrm <-  loop_data[drow,"DateFrm"]
    
    instance <- newXMLNode("instance",
               attrs = list(orgunit = "f4GhyE4xjY6",enrollmentdate =as.character(DateFrm)),
               parent = node[["Instances"]])
    
    PTID <-  loop_data[drow,"PTID"]
    LNSPid <- loop_data[drow,"LNSPid2"]
    NomPre <- loop_data[drow,"NomPre"]
    NomFam <- loop_data[drow,"NomFam"]
    intervwr <- loop_data[drow,"intervwr"]
    DOB <- loop_data[drow,"DOB"]
    
    ID_PTID <- mapping_f[mapping_f$VariableName == "PTID","Id"]
    ID_LNSpid <- mapping_f[mapping_f$VariableName == "LNSPid2","Id"]
    ID_NomPre <- mapping_f[mapping_f$VariableName == "NomPre","Id"]
    ID_NomFam <- mapping_f[mapping_f$VariableName == "NomFam","Id"]
    ID_intervwr <- mapping_f[mapping_f$VariableName == "intervwr","Id"]
    ID_DOB <- mapping_f[mapping_f$VariableName == "DOB","Id"]
    
    attributes <- newXMLNode("attributes",parent = instance)
    
    newXMLNode("attribute",
                 attrs = list(id=as.character(ID_PTID), value= as.character(PTID)),parent = attributes)
    newXMLNode("attribute",
               attrs = list(id=as.character(ID_LNSpid), value= as.character(LNSPid)),parent = attributes)
    newXMLNode("attribute",
               attrs = list(id=as.character(ID_NomPre), value= as.character(NomPre)),parent = attributes)
    newXMLNode("attribute",
               attrs = list(id=as.character(ID_NomFam), value= as.character(NomFam)),parent = attributes)
    newXMLNode("attribute",
               attrs = list(id=as.character(ID_intervwr), value= as.character(intervwr)),parent = attributes)
    newXMLNode("attribute",
               attrs = list(id=as.character(ID_DOB), value= as.character(DOB)),parent = attributes)
    
    
    ##mapping for each programstages dataelement and create xml data 
        # add programstage node
        # what dataelement  are in each stage
        

  
}
cat(saveXML(node))
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


