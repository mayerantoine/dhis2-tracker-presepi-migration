

library(RCurl)
library(XML)
library(tidyverse)


rm(list = ls())
## Programs ----------------------------------------------------------------------------
#http://209.61.231.45:8082/dhis/api/programs?&fields=id,name&links=false&paging=false

## PRESEPI Program
url <- "http://209.61.231.45:8082/dhis/api/programs/ybHHvBdo1ke.xml?fields=id,name,programTrackedEntityAttributes[id,name,code],organisationUnits[id,name],programStages[id,name]"

response <- getURL(url, userpwd = "mantoine:P@ssword001",httpauth = 1L, header=FALSE,ssl.verifypeer = FALSE,encoding = "utf-8" )
doc <- xmlTreeParse(response,useInternalNodes = T)
rootNode <- xmlRoot(doc)
xmlName(rootNode)
programStages <- rootNode[["programStages"]]
programTrackedEntityAttribute <- rootNode[["programTrackedEntityAttributes"]]
orgunits <- rootNode[["organisationUnits"]] 

#programTrackedEntity
programTrackedEntityAttributeId <- as.list(xmlSApply(programTrackedEntityAttribute, xmlGetAttr,"id"))
programTrackedEntityAttributeName <- as.list(xmlSApply(programTrackedEntityAttribute, xmlGetAttr,"name"))
tb_programTrackedEntityAttribute <-  do.call(rbind, Map(data.frame, Id=programTrackedEntityAttributeId, name=programTrackedEntityAttributeName))

#programStages
ProgramStageId <- as.list(xmlSApply(programStages,xmlGetAttr,"id"))
ProgramStageName <- as.list(xmlSApply(programStages, xmlGetAttr,"name"))
tb_programStage <-  do.call(rbind, Map(data.frame, Id=ProgramStageId, name=ProgramStageName))

#orgunits
orgunitsId <- as.list(xmlSApply(orgunits,xmlGetAttr,"id"))
orgunitsName <- as.list(xmlSApply(orgunits,xmlGetAttr,"name"))
tb_orgunits <-  do.call(rbind, Map(data.frame, Id=orgunitsId, name=orgunitsName))

## Get a programStage -------------------------------------------------------
# http://209.61.231.45:8082/dhis/api/programStages/Li8CKAWWS1q
# mapping each programStage with his dataelement


url_1 <- "http://209.61.231.45:8082/dhis/api/programStages/Li8CKAWWS1q.xml"
resp <- getURL(url_1, userpwd = "mantoine:P@ssword001",httpauth = 1L, header=FALSE,
               ssl.verifypeer = FALSE,encoding = "utf-8" )
rootNodeStage <- xmlRoot(xmlTreeParse(resp,useInternalNodes = T))
programStageDataElements <- rootNodeStage[["programStageDataElements"]]

node_1 <- programStageDataElements[[1]]
d <- xpathSApply(node_1, "//dataElement[@id]", xmlGetAttr, "id")

p_data <- as.list(xmlSApply(programStageDataElements, 
                function(x){ xmlGetAttr( x[["dataElement"]],"id")

    }))



base_url <- "http://209.61.231.45:8082/dhis/api/programStages/"
#http://209.61.231.45:8082/dhis/api/programStages/Li8CKAWWS1q.xml?fields=id,name,programStageDataElements[id]


 tab <- map(ProgramStageId,function(x){
    stage <- x
    url_stage <- paste0(base_url,stage,".xml")
    #print(url_stage)
    
    resp <- getURL(url_stage, userpwd = "mantoine:P@ssword001",httpauth = 1L, header=FALSE,
                   ssl.verifypeer = FALSE,encoding = "utf-8" )
    rootNodeStage <- xmlRoot(xmlTreeParse(resp,useInternalNodes = T))
    programStageDataElements <- rootNodeStage[["programStageDataElements"]]
    
    dataElemtId <- as.list(xmlSApply(programStageDataElements, 
                      function(x){ xmlGetAttr( x[["dataElement"]],"id")
                          
                      }))
    
     list(programStage = stage, dataElement = dataElemtId)
})




## dataElements --------------------------------------------------
# http://209.61.231.45:8082/dhis/api/dataElements?fields=id,name,domainType,code&links=false&paging=false

# Get metadata
# Create excel template with the data needed
# review the request sequence to populate the tracker to ensure you have alll the data needed
# how you will create the input data : merge metadata & datavalue
# how will you validate the import
# test environnement 
