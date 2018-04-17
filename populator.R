
library(httr)
library(jsonlite)
library(tidyverse)
library(purrr)



addTrackedEntityInstance <- function(payload){
    
    url <- "http://209.61.231.45:8082/dhis/api/trackedEntityInstances"
    #payload <- data[[1]]

    
    res <- POST(url = url, body= payload ,encode = "json")
    stop_for_status(res,"POST Tracked Entity Instance")
    
    res <- content(res, mime = "application/json")
    import_summary <- res$response[["importSummaries"]][[1]]
    import_status <- import_summary$status
    
    if(import_status == "SUCCESS") {
        return(import_summary$reference)    
    }
    else {
        return(import_summary$conflicts[[1]]$value )
    }
   

}


enrollInProgram <- function(payload){
    
    url_enrol <- "http://209.61.231.45:8082/dhis/api/enrollments"

    res <- POST(url= url_enrol, body= payload ,encode = "json")
    stop_for_status(res,"POST erollement in Program")
    
    res <- content(res, mime = "application/json")
    import_summary <- res$response[["importSummaries"]][[1]]
    import_status <- import_summary$status
    
    if(import_status == "SUCCESS") {
        return(import_summary$reference)    
    }
    else {
        return(import_summary$conflicts[[1]]$value)
    }
}


addEvent <- function(payload){
    
    url_event <- "http://209.61.231.45:8082/dhis/api/events"

    
    res <- POST(url= url_event, body= event ,encode = "json")
   stop_for_status(res,"POST events")
    
    res <- content(res, mime = "application/json")
    
    import_summary <- res$response[["importSummaries"]][[1]]
    import_status <- import_summary$status
    
    if(import_status == "SUCCESS") {
        return(import_summary$reference)    
    }
    else {
        return(import_summary$description)
    }
    
}


loadJsonData <- function(){
    

    jsonData <- read_json("test_json.json")
    
    map(jsonData,  function(x){
        
        orgunit <- x$instance_attribute$orgunit[[1]]
        enroll_date <-  x$instance_attribute$enrollmentdate[[1]]
        
        attributes <- map(x$attributes, function(atr){
            
            list(attribute = atr$id[[1]] , 
                 value =  atr$value[[1]])
        })
        
        payload <- list( trackedEntity = "MCPQUTHX1Ze",
                         orgUnit = orgunit,
                         attributes = attributes)
        
        ## add TrackedEntity Instances
        ## use try or trycatch
        tei <- addTrackedEntityInstance(payload = payload)
        
        
        
        
        ## enrollement
        enroll_load <- list(trackedEntityInstance = tei,
                            orgUnit = orgunit,
                            program = "ybHHvBdo1ke",
                            enrollmentDate = enroll_date,
                            incidentDate = enroll_date)
        
        enroll_id <- enrollInProgram(enroll_load)
        
        
        ## Program Stages and event
       events_res <-  map(x$ProgramStages, function(stage){
            
            dataValues <- stage$events$dataValues
            
            events <- map(dataValues, function(val){
                
                list(dataElement =val$id[[1]], value= val$value[[1]])
                
            })
            
            programStage <- stage$program_stage_attribute$id[[1]]
            
            event <- list(
                  program = "ybHHvBdo1ke",
                  orgUnit = orgunit,
                  eventDate =  enroll_date,
                  status = "COMPLETED",
                  storedBy = "mantoine",
                  enrollment = enroll_id,
                  programStage = programStage,
                  trackedEntityInstance=  tei,
                  dataValues = events
            )
            
            addEvent(event)
        })
        
        
            
    })
    
}














# load json data 
# Process each instances
    # create Tracked Entity Instances
        #   createpayload
        #   post payload
        #   getTrackedEntityInstanceId
    # enroll in Program
        #   createpayload
        #   post payload
        #   getTrackedEntityInstanceId
    # add Events
        #   createpayload
        #   post payload
        #   getTrackedEntityInstanceId
# Log Errors
# Moves file in different directory
# 


