
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
taxonomy = read_excel(paste0(excel_path,"actus-dictionary.xlsx"), sheet="Taxonomy")
terms = read_excel(paste0(excel_path,"actus-dictionary.xlsx"), sheet="Terms")
states = read_excel(paste0(excel_path,"actus-dictionary.xlsx"), sheet="States")
events = read_excel(paste0(excel_path,"actus-dictionary.xlsx"), sheet="Events")

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
colnames(events)=tocamel(colnames(events),delim=" ")

# create dictionary base structure
dictionary = list()

# add taxonomy
dictionary[["taxonomy"]] = lapply(split(taxonomy,taxonomy$identifier),unbox)

# add terms
terms_sub=terms[,c("identifier", "group", "name", "accronym", "type", "allowedValues", "default", "description")]

# -> convert enum values to json array
terms_sub$allowedValues = sapply(sapply(sapply(terms_sub$allowedValues,strsplit,"\n"),strsplit,"="),function(x) sapply(x,function(y) trimws(y[1])))

# -> format NA strings
terms_sub[which(is.na(terms_sub$default)),"default"]=""

# -> add to dictionary
dictionary[["terms"]] = lapply(split(terms_sub,terms_sub$identifier),unbox)

# add applicability

# -> reshape data
applic_sub=as.data.frame(t(terms[,-which(colnames(terms)%in%c("identifier","group","name","accronym","type","allowedValues","default","description","cNTRLSensitive"))]))
applic_sub=cbind(rownames(applic_sub),applic_sub)
colnames(applic_sub) = tocamel(c("Contract",terms$identifier))
rownames(applic_sub) = NULL

# -> add to dictionary
dictionary[["applicability"]] = lapply(split(applic_sub,applic_sub$contract),unbox)

# add events
dictionary[["events"]] = lapply(split(events,events$identifier),unbox)

# add states
# -> convert enum values to json array
states$allowedValues = sapply(sapply(sapply(states$allowedValues,strsplit,"\n"),strsplit,"="),function(x) sapply(x,function(y) trimws(y[1])))

# -> add to dictionary
dictionary[["states"]] = lapply(split(states,states$identifier),unbox)

# write dictionary as single file
# -------------------------------

# parse to json and fix formatting
jsonDict = prettify(toJSON(dictionary,auto_unbox=TRUE,pretty=TRUE,digits=NA))
jsonDict = gsub("null", "[]", jsonDict, fixed=TRUE)
jsonDict = gsub("\"ISO8601 Datetime\"", "[\"ISO8601 Datetime\"]", jsonDict,fixed=TRUE)
jsonDict = gsub("\"(0,1)\"", "[\"(0,1)\"]", jsonDict,fixed=TRUE)
jsonDict = gsub("\"Positive\"", "[\"Positive\"]", jsonDict,fixed=TRUE)

# write json dictionary
jsonDict %>% write_lines(paste0(json_path,'actus-dictionary.json'))

# write dictionary as multiple files
# -------------------------------

# 1. taxonomy
# parse to json and fix formatting
jsonTaxon = prettify(toJSON(dictionary$taxonomy,auto_unbox=TRUE,pretty=TRUE,digits=NA))

# write json
jsonTaxon %>% write_lines(paste0(json_path,'actus-dictionary-taxonomy.json'))

# 2. terms
# parse to json and fix formatting
jsonTerms = prettify(toJSON(dictionary$terms,auto_unbox=TRUE,pretty=TRUE,digits=NA))
jsonTerms = gsub("null", "[]", jsonTerms, fixed=TRUE)
jsonTerms = gsub("\"ISO8601 Datetime\"", "[\"ISO8601 Datetime\"]", jsonTerms,fixed=TRUE)
jsonTerms = gsub("\"(0,1)\"", "[\"(0,1)\"]", jsonTerms,fixed=TRUE)
jsonTerms = gsub("\"Positive\"", "[\"Positive\"]", jsonTerms,fixed=TRUE)

# write json
jsonTerms %>% write_lines(paste0(json_path,'actus-dictionary-terms.json'))

# 3. applicability
# parse to json and fix formatting
jsonApplic = prettify(toJSON(dictionary$applicability,auto_unbox=TRUE,pretty=TRUE,digits=NA))

# write json
jsonApplic %>% write_lines(paste0(json_path,'actus-dictionary-applicability.json'))

# 4. events
# parse to json and fix formatting
jsonEvents = prettify(toJSON(dictionary$events,auto_unbox=TRUE,pretty=TRUE,digits=NA))

# write json
jsonEvents %>% write_lines(paste0(json_path,'actus-dictionary-events.json'))

# 5. states
# parse to json and fix formatting
jsonStates = prettify(toJSON(dictionary$states,auto_unbox=TRUE,pretty=TRUE,digits=NA))
jsonStates = gsub("null", "[]", jsonStates, fixed=TRUE)
jsonStates = gsub("\"ISO8601 Datetime\"", "[\"ISO8601 Datetime\"]", jsonStates)

# write json
jsonStates %>% write_lines(paste0(json_path,'actus-dictionary-states.json'))
