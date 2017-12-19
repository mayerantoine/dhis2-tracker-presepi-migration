################################################################################# 
## PResePi data migration
## Mayer Antoine, CDC Haiti
## Date : 12/12/2017
## Purpose : Get attributes, program, programStages, dataElement,
##              identifiers  from DHIS2 to map and create dataset for payload  
##
##
##################################################################################


library(RCurl)
library(XML)
library(tidyverse)


#rm(list = ls())

## Tracked Entity -----------------------------------------------------------------------------------
url_id <- "http://209.61.231.45:8082/dhis/api/trackedEntities.xml?fields=id,name&links=false&paging=false"
entity_response <- getURL(url_id, userpwd = "mantoine:P@ssword001",httpauth = 1L, header=FALSE,ssl.verifypeer = FALSE,
                          encoding = "utf-8" )

doc_id <-xmlTreeParse(entity_response, useInternalNodes = T)
root_id <- xmlRoot(doc_id)

trackedEntitiesId <- xmlSApply(root_id[["trackedEntities"]],xmlGetAttr,"id")
trackedEntitiesName <- xmlSApply(root_id[["trackedEntities"]],xmlGetAttr,"name")

trackedEntity <- trackedEntitiesId[[1]]

## Programs --------------------------------------------------------------------------------------------------------------
#http://209.61.231.45:8082/dhis/api/programs?&fields=id,name&links=false&paging=false

url_program <- "http://209.61.231.45:8082/dhis/api/programs.xml?fields=id,name&links=false&paging=false"
entity_response <- getURL(url_program, userpwd = "mantoine:P@ssword001",httpauth = 1L, header=FALSE,ssl.verifypeer = FALSE,
                          encoding = "utf-8" )

doc_id <-xmlTreeParse(entity_response, useInternalNodes = T)
root_id <- xmlRoot(doc_id)

programId <- xmlSApply(root_id[["programs"]],xmlGetAttr,"id")
programName <- xmlSApply(root_id[["programs"]],xmlGetAttr,"name")
tb_program <-  do.call(rbind, Map(data.frame, Id=programId, name=programName))

PRESEpiId <- tb_program %>% 
    filter(name == "PRESEpi") %>%
    select(Id)

PRESEpiId <- droplevels(PRESEpiId$Id)


## PRESEPI Program -------------------------------------------------------------------------------------------------------
## url <- "http://209.61.231.45:8082/dhis/api/programs/ybHHvBdo1ke.xml?fields=id,name,programTrackedEntityAttributes[id,name,code],organisationUnits[id,name],programStages[id,name]"

url <- "http://209.61.231.45:8082/dhis/api/programs/ybHHvBdo1ke.xml?fields=id,name,programTrackedEntityAttributes[id,name,code],organisationUnits[id,name],programStages[id,name]"

response <- getURL(url, userpwd = "mantoine:P@ssword001",httpauth = 1L, header=FALSE,ssl.verifypeer = FALSE,encoding = "utf-8" )
doc <- xmlTreeParse(response,useInternalNodes = T)
rootNode <- xmlRoot(doc)
programStages <- rootNode[["programStages"]]
programTrackedEntityAttribute <- rootNode[["programTrackedEntityAttributes"]]
orgunits <- rootNode[["organisationUnits"]] 

#programTrackedEntity
programTrackedEntityAttributeId <- as.list(xmlSApply(programTrackedEntityAttribute, xmlGetAttr,"id"))
programTrackedEntityAttributeName <- as.list(xmlSApply(programTrackedEntityAttribute, xmlGetAttr,"name"))
tb_programTrackedEntityAttribute <-  do.call(rbind, Map(data.frame, Id=programTrackedEntityAttributeId, 
                                                        name=programTrackedEntityAttributeName,stringsAsFactors = FALSE))

#programStages
ProgramStageId <- as.list(xmlSApply(programStages,xmlGetAttr,"id"))
ProgramStageName <- as.list(xmlSApply(programStages, xmlGetAttr,"name"))
tb_programStage <-  do.call(rbind, Map(data.frame, Id=ProgramStageId, name=ProgramStageName,stringsAsFactors = FALSE))

#orgunits
orgunitsId <- as.list(xmlSApply(orgunits,xmlGetAttr,"id"))
orgunitsName <- as.list(xmlSApply(orgunits,xmlGetAttr,"name"))
tb_orgunits <- do.call(rbind, Map(data.frame, Id=orgunitsId, name=orgunitsName,stringsAsFactors = FALSE))


## Get a programStage -------------------------------------------------------
# http://209.61.231.45:8082/dhis/api/programStages/Li8CKAWWS1q
# mapping each programStage with his dataelement


base_url <- "http://209.61.231.45:8082/dhis/api/programStages/"
#http://209.61.231.45:8082/dhis/api/programStages/Li8CKAWWS1q.json?fields=id,name,programStageDataElements[id]

#how does map work, map_df, do.call vs lapply
# how to return a dataframe

programStageDataElements_list <- map(ProgramStageId,function(x){
    stage <- x
    url_stage <- paste0(base_url,stage,".xml")
  
    resp <- getURL(url_stage, userpwd = "mantoine:P@ssword001",httpauth = 1L, header=FALSE,
                   ssl.verifypeer = FALSE,encoding = "utf-8" )
    rootNodeStage <- xmlRoot(xmlTreeParse(resp,useInternalNodes = T))
    programStageDataElements <- rootNodeStage[["programStageDataElements"]]
    
    dataElementId <- as.list(xmlSApply(programStageDataElements, 
                      function(x){ xmlGetAttr( x[["dataElement"]],"id")
                          
                      }))
    
     s <- list(programStage = stage, dataElement = dataElementId)
     as.data.frame(do.call(rbind,lapply(s$dataElement,cbind,s$programStage)))
})


tb_programStageDataElement <- as.data.frame(do.call(rbind,programStageDataElements_list))
names(tb_programStageDataElement) <- c("dataElement","programStage")


## dataElements --------------------------------------------------
# http://209.61.231.45:8082/dhis/api/dataElements?fields=id,name,domainType,code&links=false&paging=false

dataelement_url <- "http://209.61.231.45:8082/dhis/api/dataElements.xml?fields=id,name,domainType,code&links=false&paging=false&filter=code:!eq:null"

response_data <- getURL(dataelement_url, userpwd = "mantoine:P@ssword001",httpauth = 1L, header=FALSE,
                                                 ssl.verifypeer = FALSE,encoding = "utf-8" )
doc_data <- xmlTreeParse(response_data,useInternalNodes = T)
rootNode_data <- xmlRoot(doc_data)
dataElements <- rootNode_data[["dataElements"]]

dataElementId <- xmlSApply(dataElements, xmlGetAttr,"id")
dataElementName <- xmlSApply(dataElements, xmlGetAttr,"name")
dataElementCode <- xmlSApply(dataElements, xmlGetAttr,"code")

tb_dataElement <-  do.call(rbind, Map(data.frame, Id=dataElementId, name=dataElementName,code=dataElementCode))


