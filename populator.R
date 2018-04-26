

library(httr)
library(jsonlite)
library(tidyverse)
library(purrr)


is.error <- function(x) inherits(x, "try-error")

login <- function() {
    uid <- Sys.getenv("uid")
    pwd <- Sys.getenv("pwd")
    
    set_config(authenticate(uid, pwd))
    
}

# POST trackedEntityInstances
# Return Instances Id if success and error if not
addTrackedEntityInstance <- function(payload) {
    url <- "http://209.61.231.45:8082/dhis/api/trackedEntityInstances"
    #payload <- data[[1]]
    
    res <- POST(url = url,
                body = body ,
                encode = "json")
    res <- content(res, mime = "application/json")
    
    if(res$status != "OK") {
    
        #is this a conflict
        if(res$httpStatus == "Conflict") {
         
         # we should return the current Id 
         import_message <- res$response[["importSummaries"]][[1]]$conflicts[[1]]$value 
          stop("Conflict while adding Tracked entity.",import_message)   
               
        }
        # return error adding failed
        stop("Adding Tracked entity failed.",res$message)
    }
     
    tei <- res$response$importSummaries[[1]]$reference
    return(tei)
    
}


enrollInProgram <- function(payload) {
    url_enrol <- "http://209.61.231.45:8082/dhis/api/enrollments"
    
    res <- POST(url = url_enrol,
                body = payload ,
                encode = "json")
    res <- content(res, mime = "application/json")
    
     if(res$status != "OK") {
    
        #is this a conflict
        if(res$httpStatus == "Conflict") {
         
         # we should return the current Id 
         import_message <- res$response[["importSummaries"]][[1]]$description
          stop("Conflict enrolling entity in program.",import_message)   
               
        }
        # return error adding failed
        stop("Enrolling in program failed.",res$message)
    }
     
    enrollId <- res$response$importSummaries[[1]]$reference
    return(enrollId)


}


addEvent <- function(payload) {
    url_event <- "http://209.61.231.45:8082/dhis/api/events"
    
    
    res <- POST(url = url_event,
                body = payload ,
                encode = "json")
    res <- content(res, mime = "application/json")
    
   
     if(res$status != "OK") {
    
        #is this a conflict
        if(res$httpStatus == "Conflict") {
         
         # we should return the current Id 
         import_message <- res$response[["importSummaries"]][[1]]$description
          stop("Conflict while adding Tracked entity.",import_message)   
               
        }
        # return error adding failed
        stop("Adding Tracked entity failed.",res$message)
     }
    
    eventId <- res$response$importSummaries[[1]]$reference
    
    return(eventId)
}


loadJsonData <- function() {
    jsonData <- read_json("processed_data/output/HSD_2017_9.json")
    
    map(jsonData,  function(x) {
        orgunit <- x$instance_attribute$orgunit[[1]]
        enroll_date <-  x$instance_attribute$enrollmentdate[[1]]
        
        attributes <- map(x$attributes, function(atr) {
            list(attribute = atr$id[[1]] ,
                 value =  atr$value[[1]])
        })
        
        
        
        ## TrackedEntity Instances payload
        payload <- list(
            trackedEntity = "MCPQUTHX1Ze",
            orgUnit = orgunit,
            attributes = attributes,
            enrollments = list(enroll_load)
        )
        
        
        
        ## add TrackedEntity Instances
        ## use try or trycatch
        tei <-
            try(addTrackedEntityInstance(payload = payload), silent = TRUE)
        
        if (!is.error(Id)) {
            ## enrollement payload
            enroll_load <- list(
                trackedEntityInstance  = tei,
                orgUnit = orgunit,
                program = "ybHHvBdo1ke",
                enrollmentDate = enroll_date,
                incidentDate = enroll_date
            )
            
            enroll_id <-
                try(enrollInProgram(enroll_load), silent = TRUE)
            
            
            if (!is.error(enroll_id)) {
                # Program Stages and event
                events_res <-  map(x$ProgramStages, function(stage) {
                    dataValues <- stage$events$dataValues
                    events <- map(dataValues, function(val) {
                        list(dataElement = val$id[[1]],
                             value = val$value[[1]])
                        
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
                        trackedEntityInstance =  tei,
                        dataValues = events
                    )
                    
                    
                    ## add event
                    addEvent(event)
                })
                
            }
            
            
        }
        
        
        
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


