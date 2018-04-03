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


rm(list = ls())

# why do i do to download Malaria metadata ? can this be not filter to a specific program ?
# how to organise and display metadata dowloaded

## Query Data -----------------------------------------------------------------------------------
## 

queryURL <- function(urlstring){
    
    url_id <- urlstring
    
    uid <- Sys.getenv("uid")
    pwd <- Sys.getenv("pwd")
    userpwd <- paste(uid,pwd,sep = ":")
    entity_response <-
        getURL(
            url_id,
            userpwd = userpwd,
            httpauth = 1L,
            header = FALSE,
            ssl.verifypeer = FALSE,
            encoding = "utf-8"
        )
    
    if(is.null(entity_response)){
    
        stop("Cannot connect")
        
    }
    doc_id <- xmlTreeParse(entity_response, useInternalNodes = T)
    root_id <- xmlRoot(doc_id)
    
}


## Tracked Entity -----------------------------------------------------------------------------------
## 
## 

getTrackedEntity <- function() {
    
    url_id <-
        "http://209.61.231.45:8082/dhis/api/trackedEntities.xml?fields=id,name&links=false&paging=false"
   
  
    root_id <- queryURL(url_id)
    
    trackedEntitiesId <-
        xmlSApply(root_id[["trackedEntities"]], xmlGetAttr, "id")
    trackedEntitiesName <-
        xmlSApply(root_id[["trackedEntities"]], xmlGetAttr, "name")
    
    trackedEntity <- trackedEntitiesId[[1]]
}

#id tracked entity is null

## Programs --------------------------------------------------------------------------------------------------------------
# http://209.61.231.45:8082/dhis/api/programs?&fields=id,name&links=false&paging=false

getProgram <- function() {

    url_program <-
        "http://209.61.231.45:8082/dhis/api/programs.xml?fields=id,name&links=false&paging=false"
        root_id <- queryURL(url_program)
    
    programId <- xmlSApply(root_id[["programs"]], xmlGetAttr, "id")
    programName <- xmlSApply(root_id[["programs"]], xmlGetAttr, "name")
    df_program <-
        do.call(rbind, Map(data.frame, Id = programId, name = programName))
    
    PresePiId <- as.character(programId[[3]])
    

}


## PRESEPI Program -------------------------------------------------------------------------------------------------------
## url <- "http://209.61.231.45:8082/dhis/api/programs/ybHHvBdo1ke.xml?fields=id,name,programTrackedEntityAttributes[id,name,code],organisationUnits[id,name],programStages[id,name]"

getTrackedEntityAttribute <- function(){
        url <-
        "http://209.61.231.45:8082/dhis/api/programs/ybHHvBdo1ke.xml?fields=id,name,programTrackedEntityAttributes,organisationUnits[id,name],programStages[id,name]"
        
          
    
    rootNode <- queryURL(url)
    programTrackedEntityAttribute <- rootNode[["programTrackedEntityAttributes"]]
 
    xmlChildren(programTrackedEntityAttribute[[1]])$trackedEntityAttribute
    map(xmlChildren(programTrackedEntityAttribute), function(x){ 
        
            child <- xmlChildren(x)[["trackedEntityAttribute"]]
            
            trId <- xmlGetAttr(child,"id")
        })
    
    
        #programTrackedEntity
    TrackedEntityAttributeId <-
        map(xmlChildren(programTrackedEntityAttribute), function(x){ 
        
            child <- xmlChildren(x)[["trackedEntityAttribute"]]
            
            trId <- xmlGetAttr(child,"id")
        })
    programTrackedEntityAttributeName <-
        as.list(xmlSApply(programTrackedEntityAttribute, xmlGetAttr, "name"))
    
    df_TrackedEntityAttribute <-
        do.call(
            rbind,
            Map(
                data.frame,
                Id = TrackedEntityAttributeId,
                name = programTrackedEntityAttributeName,
                stringsAsFactors = FALSE
            )
        )
    
    
    df_TrackedEntityAttribute
    

}


getProgamStages <- function() {
    
    
    url <-
        "http://209.61.231.45:8082/dhis/api/programs/ybHHvBdo1ke.xml?fields=id,name,programTrackedEntityAttributes[id,name,code],organisationUnits[id,name],programStages[id,name]"
    
    rootNode <- queryURL(url)
    programStages <- rootNode[["programStages"]]   
     
    #programStages
    ProgramStageId <- as.list(xmlSApply(programStages, xmlGetAttr, "id"))
    ProgramStageName <-
        as.list(xmlSApply(programStages, xmlGetAttr, "name"))
    df_programStage <-
        do.call(
            rbind,
            Map(
                data.frame,
                Id = ProgramStageId,
                name = ProgramStageName,
                stringsAsFactors = FALSE
            )
        )
    
      Encoding(df_programStage$name) <-"UTF-8"
      
      df_programStage
    
}


getOrgunits <- function(){
    
        url <-
        "http://209.61.231.45:8082/dhis/api/programs/ybHHvBdo1ke.xml?fields=id,name,programTrackedEntityAttributes[id,name,code],organisationUnits[id,name],programStages[id,name]"
    
    rootNode <- queryURL(url)
    orgunits <- rootNode[["organisationUnits"]] 

    
    #orgunits
    orgunitsId <- as.list(xmlSApply(orgunits, xmlGetAttr, "id"))
    orgunitsName <- as.list(xmlSApply(orgunits, xmlGetAttr, "name"))
    df_orgunits <-
        do.call(rbind,
                Map(
                    data.frame,
                    orgunitsId = orgunitsId,
                    name = orgunitsName,
                    stringsAsFactors = FALSE
                ))
    
    Encoding(df_orgunits$name) <- "UTF-8"
    
    df_orgunits
    
    
}


## Get a programStage -------------------------------------------------------
# http://209.61.231.45:8082/dhis/api/programStages/Li8CKAWWS1q
# mapping each programStage with his dataelement


getPresePiProgramStage <- function(ProgramStageId) {

    base_url <- "http://209.61.231.45:8082/dhis/api/programStages/"
    #http://209.61.231.45:8082/dhis/api/programStages/Li8CKAWWS1q.json?fields=id,name,programStageDataElements[id]
    
    #how does map work, map_df, do.call vs lapply
    # how to return a dataframe
    
    programStageDataElements_list <- map(ProgramStageId, function(x) {
        stage <- x
        url_stage <- paste0(base_url, stage, ".xml")
        
        rootNodeStage <- queryURL(url_stage)
        programStageDataElements <-
            rootNodeStage[["programStageDataElements"]]
        
        if( !is.null(programStageDataElements)) {
        
        dataElementId <- as.list(xmlSApply(programStageDataElements,
                                           function(x) {
                                               xmlGetAttr(x[["dataElement"]], "id")
                                           }))
        
          s <- list(programStage = stage, dataElement = dataElementId)
        as.data.frame(do.call(rbind, lapply(s$dataElement, cbind, s$programStage)))
        }
        
      
    })
    
    
    df_programStageDataElement <-
        as.data.frame(do.call(rbind, programStageDataElements_list))
    names(df_programStageDataElement) <- c("dataElement", "programStage")
    
 

    df_programStageDataElement

}


## dataElements --------------------------------------------------
# http://209.61.231.45:8082/dhis/api/dataElements?fields=id,name,domainType,code&links=false&paging=false


getDataElement <- function() {
    
    dataelement_url <-
        "http://209.61.231.45:8082/dhis/api/dataElements.xml?fields=id,name,domainType,code&links=false&paging=false&filter=code:!eq:null"
    

    rootNode_data <- queryURL(dataelement_url)
    dataElements <- rootNode_data[["dataElements"]]
    
    dataElementId <- xmlSApply(dataElements, xmlGetAttr, "id")
    dataElementName <- xmlSApply(dataElements, xmlGetAttr, "name")
    dataElementCode <- xmlSApply(dataElements, xmlGetAttr, "code")
    
    tb_dataElement <-
        do.call(
            rbind,
            Map(
                data.frame,
                Id = dataElementId,
                name = dataElementName,
                code = dataElementCode
            )
        )
}


## run all ------------------------------------------------------------------------------------------------

trackedEntity <- getTrackedEntity()
presePiId <- getProgram()

df_TrackedEntityAttribute <- getTrackedEntityAttribute()
df_programStages <- getProgamStages()
df_programStageDataElement <- getPresePiProgramStage(df_programStages$Id)
df_orgunits <- getOrgunits()
df_dataElement <- getDataElement()
df_metadata <- rbind(df_TrackedEntityAttribute[c(1:2)],df_dataElement[c(1:2)])

