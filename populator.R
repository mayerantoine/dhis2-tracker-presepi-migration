
library(httr)
library(jsonlite)
library(tidyverse)
library(purrr)


addTrackedEntityInstance <- function(){
    
    url <- "http://209.61.231.45:8082/dhis/api/trackedEntityInstances"
    payload <- upload_file("tei.json")

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


enrollInProgram <- function(){
    
    url_enrol <- "http://209.61.231.45:8082/dhis/api/enrollments"
    payload <-upload_file("enrol.json")
    
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


addEvent <- function(){
    
    url_enrol <- "http://209.61.231.45:8082/dhis/api/events"
    payload <-upload_file("events.json")
    
    res <- POST(url= url_enrol, body= payload ,encode = "json")
   # stop_for_status(res,"POST events")
    
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


loadJsonData() <- function(){
    

    jsonData <- read_json("node3_json.json")
    map(jsonData, ~ .x[["instance_attribute"]])
    
}


processInstances <- function(jsonData) {

    for(x in jsonData)    {
        
        x[[1]][["attributes"]]
        
    }
    
    
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


