library(XML)
library(tidyverse)

rm(list = ls())
filename <- "metadata_presepi_201709.xml"
doc <- xmlTreeParse(filename,useInternalNodes = T)
rootNode <- xmlRoot(doc)
xmlName(rootNode)
dataElement <- rootNode[["dataElements"]]
program <- rootNode[["programs"]]
TrackedEntityAttribute <- rootNode[["trackedEntityAttributes"]]
programStage <- program[[2]][["programStages"]]

programTrackedEntityAttribute <- program[[2]][["programTrackedEntityAttributes"]]
orgunits <- program[[2]][["organisationUnits"]]

## programs
ProgramId <- as.list(xmlSApply(program, function(x) xmlGetAttr(x,"id")))
ProgramName <- as.list(xmlSApply(program, function(x) xmlGetAttr(x,"name")))
tb_program <-  do.call(rbind, Map(data.frame, Id=ProgramId, name=ProgramName))

#programStages
ProgramStageId <- as.list(xmlSApply(programStage, function(x) xmlGetAttr(x,"id")))
ProgramStageName <- as.list(xmlSApply(programStage, function(x) xmlGetAttr(x,"name")))
tb_programStage <-  do.call(rbind, Map(data.frame, Id=ProgramStageId, name=ProgramStageName))

#TrackedEntityAttribute
TrackedEntityAttributeId <- as.list(xmlSApply(TrackedEntityAttribute, function(x) xmlGetAttr(x,"id")))
TrackedEntityAttributeName <- as.list(xmlSApply(TrackedEntityAttribute, function(x) xmlGetAttr(x,"name")))
TrackedEntityAttributeCode <- as.list(xmlSApply(TrackedEntityAttribute, function(x)
{ ifelse ( is.null( xmlGetAttr(x,"code")),"NONE",xmlGetAttr(x,"code"))
}  ))

tb_TrackedEntityAttribute <- do.call(rbind,Map(data.frame, Id=TrackedEntityAttributeId,code=TrackedEntityAttributeCode, name=TrackedEntityAttributeName))

#programTrackedEntity
programTrackedEntityAttributeId <- as.list(xmlSApply(programTrackedEntityAttribute, function(x) xmlGetAttr(x,"id")))
programTrackedEntityAttributeName <- as.list(xmlSApply(programTrackedEntityAttribute, function(x) xmlGetAttr(x,"name")))
tb_programTrackedEntityAttribute <-  do.call(rbind, Map(data.frame, Id=TrackedEntityAttributeId, name=TrackedEntityAttributeName))
#write_csv(tb_programTrackedEntityAttribute,"tb_programTrackedEntityAttribute.csv")

#orgunits
orgunitsId <- as.list(xmlSApply(orgunits, function(x) xmlGetAttr(x,"id")))
orgunitsName <- as.list(xmlSApply(orgunits, function(x) xmlGetAttr(x,"name")))
tb_orgunits <-  do.call(rbind, Map(data.frame, Id=orgunitsId, name=orgunitsName))


#dataElements
dataElementId <- as.list(xmlSApply(dataElement, function(x) xmlGetAttr(x,"id")))
dataElementName <- as.list(xmlSApply(dataElement, function(x) xmlGetAttr(x,"name")))
#dataElementCode <- as.list(xmlSApply(dataElement, function(x) xmlGetAttr(x,"code")))
dataElementDomainType <- as.list(xmlSApply(dataElement, function(x) xmlValue(x[["domainType"]])))
tb_dataElement <- do.call(rbind, Map(data.frame, Id=dataElementId, name=dataElementName,domain=dataElementDomainType))
write_csv(tb_dataElement,"tb_dataElement.csv")


####LOAD MAIN DATA
main_data <- read_tsv("main_data.txt",col_names = T)
main_data_col <- as.data.frame(names(main_data))
names(main_data_col) <- "presepi_variables"

presepi_sites <- distinct(main_data[,c("SiteName")])
presepi_sites <- presepi_sites[!is.na(presepi_sites$SiteName),]




