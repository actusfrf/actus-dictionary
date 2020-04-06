
# global options
options(stringsAsFactors = FALSE)

# load libraries
library(jsonlite)
library(magrittr)
library(readr)
library(readxl)

# define paths
json_path = '../'
excel_path = '../'

# read data dictionary
version = read_excel(paste0(excel_path, "actus-dictionary.xlsx"), sheet="README")
taxonomy = read_excel(paste0(excel_path,"actus-dictionary.xlsx"), sheet="Taxonomy")
terms = read_excel(paste0(excel_path,"actus-dictionary.xlsx"), sheet="Terms")
states = read_excel(paste0(excel_path,"actus-dictionary.xlsx"), sheet="State")
contractStruct = read_excel(paste0(excel_path,"actus-dictionary.xlsx"), sheet="ContractStructure", col_types="text", col_names=FALSE)
eventStruct = read_excel(paste0(excel_path,"actus-dictionary.xlsx"), sheet="Event")

# format column names
tocamel=function(x,delim=" ") {
	s <- strsplit(x, split=delim,fixed=TRUE)
	sapply(s, function(y) {
		if (any(is.na(y))) {
		    y
		}
		else {
		    first <- substring(y, 1, 1)
		    first <- toupper(first)
		    first[1] <- tolower(first[1])
		    paste(first, substring(y, 2), sep = "", collapse = "")
		}
	    })
}
colnames(taxonomy)=tocamel(colnames(taxonomy),delim=" ")
colnames(terms)=tocamel(colnames(terms),delim=" ")
colnames(states)=tocamel(colnames(states),delim=" ")
colnames(eventStruct)=tocamel(colnames(eventStruct),delim=" ")

# create dictionary base structure
dictionary = list()

# add version
version_details = data.frame(Version=as.character(version[which(version[,1]=="Version"),2]),
				Edition=as.character(version[which(version[,1]=="Edition"),2]),
				Date=as.character(version[which(version[,1]=="Date"),2]))
dictionary[["version"]] = unbox(version_details)

# add taxonomy
dictionary[["taxonomy"]] = lapply(split(taxonomy,taxonomy$identifier),unbox)

# add terms
terms_sub=terms[,c("identifier", "group", "name", "acronym", "type", "allowedValues", "default", "description")]

# -> convert enum values to json array
#terms_sub$allowedValues = sapply(sapply(sapply(terms_sub$allowedValues,strsplit,"\n"),strsplit,"="),function(x) sapply(x,function(y) trimws(y[1])))
terms_sub$allowedValues = sapply(sapply(sapply(terms_sub$allowedValues,strsplit,"\n"),strsplit,", "),function(x) {
  obj = sapply(x,strsplit,": ")
  allowedValues = x
  if(!is.null(dim(obj))) allowedValues = do.call("rbind",apply(obj,2,function(x) { df = as.data.frame(x)[2,]; colnames(df)=c("option", "identifier", "name", "acronym", "description"); df } ))
  return(allowedValues)
})

# -> format NA strings
terms_sub[which(is.na(terms_sub$default)),"default"]=""

# -> add to dictionary
dictionary[["terms"]] = lapply(split(terms_sub,terms_sub$identifier),unbox)

# add applicability

# -> reshape data
applic_sub=as.data.frame(t(terms[,-which(colnames(terms)%in%c("identifier","group","name","acronym","type","allowedValues","default","description","cNTRLSensitive"))]))
applic_sub=cbind(rownames(applic_sub),applic_sub)
colnames(applic_sub) = tocamel(c("Contract",terms$identifier))
rownames(applic_sub) = NULL

# -> add to dictionary
dictionary[["applicability"]] = lapply(split(applic_sub,applic_sub$contract),unbox)

# add states
# -> convert enum values to json array
#states$allowedValues = sapply(sapply(sapply(states$allowedValues,strsplit,"\n"),strsplit,"="),function(x) sapply(x,function(y) trimws(y[1])))
states$allowedValues = sapply(sapply(sapply(states$allowedValues,strsplit,"\n"),strsplit,", "),function(x) {
  obj = sapply(x,strsplit,": ")
  allowedValues = x
  if(!is.null(dim(obj))) allowedValues = do.call("rbind",apply(obj,2,function(x) { df = as.data.frame(x)[2,]; colnames(df)=c("option", "identifier", "name", "acronym", "description"); df } ))
  return(allowedValues)
})

# -> add to dictionary
dictionary[["states"]] = lapply(split(states,states$identifier),unbox)

# add event
# -> convert enum values to json array
#states$allowedValues = sapply(sapply(sapply(states$allowedValues,strsplit,"\n"),strsplit,"="),function(x) sapply(x,function(y) trimws(y[1])))
eventStruct$allowedValues = sapply(sapply(sapply(eventStruct$allowedValues,strsplit,"\n"),strsplit,", "),function(x) {
  obj = sapply(x,strsplit,": ")
  allowedValues = x
  if(!is.null(dim(obj))) allowedValues = do.call("rbind",apply(obj,2,function(x) { df = as.data.frame(x)[2,]; colnames(df)=c("option", "identifier", "name", "acronym", "description", "sequence"); df } ))
  return(allowedValues)
})

# -> add to dictionary
dictionary[["event"]] = lapply(split(eventStruct,eventStruct$identifier),unbox)

# add contract structure
dictionary[["contractStructure"]] = fromJSON(as.character(contractStruct))

# write dictionary as single file
# -------------------------------

# parse to json and fix formatting
jsonDict = prettify(toJSON(dictionary,auto_unbox=TRUE,pretty=TRUE,digits=NA))
jsonDict = gsub("{

            }", "[]", jsonDict,fixed=TRUE)

jsonDict = gsub("[
                null
            ]", "[]", jsonDict,fixed=TRUE)

# write json dictionary
jsonDict %>% write_lines(paste0(json_path,'actus-dictionary.json'))

# write dictionary as multiple files
# -------------------------------

# 1. taxonomy
# parse to json and fix formatting
jsonTaxon = prettify(toJSON(dictionary[c("version","taxonomy")],auto_unbox=TRUE,pretty=TRUE,digits=NA))

# write json
jsonTaxon %>% write_lines(paste0(json_path,'actus-dictionary-taxonomy.json'))

# 2. terms
# parse to json and fix formatting
jsonTerms = prettify(toJSON(dictionary[c("version","terms")],auto_unbox=TRUE,pretty=TRUE,digits=NA))
jsonTerms = gsub("{

            }", "[]", jsonTerms,fixed=TRUE)

jsonTerms = gsub("[
                null
            ]", "[]", jsonTerms,fixed=TRUE)

# write json
jsonTerms %>% write_lines(paste0(json_path,'actus-dictionary-terms.json'))

# 3. applicability
# parse to json and fix formatting
jsonApplic = prettify(toJSON(dictionary[c("version","applicability")],auto_unbox=TRUE,pretty=TRUE,digits=NA))

# write json
jsonApplic %>% write_lines(paste0(json_path,'actus-dictionary-applicability.json'))

# 5. event
jsonEvent = prettify(toJSON(dictionary[c("version","event")],auto_unbox=TRUE,pretty=TRUE,digits=NA))
jsonEvent = gsub("{

            }", "[]", jsonEvent,fixed=TRUE)

jsonEvent = gsub("[
                null
            ]", "[]", jsonEvent,fixed=TRUE)

# write json
jsonEvent %>% write_lines(paste0(json_path,'actus-dictionary-event.json'))

# 6. states
# parse to json and fix formatting
jsonStates = prettify(toJSON(dictionary[c("version","states")],auto_unbox=TRUE,pretty=TRUE,digits=NA))
jsonStates = gsub("{

            }", "[]", jsonStates,fixed=TRUE)

jsonStates = gsub("[
                null
            ]", "[]", jsonStates,fixed=TRUE)

# write json
jsonStates %>% write_lines(paste0(json_path,'actus-dictionary-states.json'))

# 7. contract structure
# parse to json and fix formatting
jsonStruct = prettify(toJSON(dictionary[c("version","contractStructure")],auto_unbox=TRUE,pretty=TRUE,digits=NA))

# write json
jsonStruct %>% write_lines(paste0(json_path,'actus-dictionary-contract-structure.json'))

