
# global options
options(stringsAsFactors = FALSE)

# load libraries
library(jsonlite)
library(magrittr)
library(readr)
library(xlsx)

# define paths
json_path = '../'
excel_path = '../'

# read data dictionary
taxonomy = read.xlsx(paste0(excel_path,"actus-dictionary.xlsx"), sheetName="Taxonomy", header=TRUE)
terms = read.xlsx(paste0(excel_path,"actus-dictionary.xlsx"), sheetName="Terms", header=TRUE)
states = read.xlsx(paste0(excel_path,"actus-dictionary.xlsx"), sheetName="States", header=TRUE)
events = read.xlsx(paste0(excel_path,"actus-dictionary.xlsx"), sheetName="Events", header=TRUE)

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
colnames(taxonomy)=tocamel(colnames(taxonomy),delim=".")
colnames(terms)=tocamel(colnames(terms),delim=".")
colnames(states)=tocamel(colnames(states),delim=".")
colnames(events)=tocamel(colnames(events),delim=".")

# create dictionary base structure
dictionary = toJSON(unbox(data.frame(taxonomy="TODO taxonomy",terms="TODO terms",applicability="TODO applicability",events="TODO events",states="TODO states")),auto_unbox=TRUE,pretty=TRUE)

# add taxonomy
dictionary = gsub("\"TODO taxonomy\"", unbox(toJSON(lapply(split(taxonomy,taxonomy$identifier),unbox), auto_unbox=TRUE,pretty=TRUE)),dictionary)

# add terms as sub-json
terms_sub=terms[,c("identifier", "group", "name", "accronym", "type", "allowedValues", "default", "description")]

# -> convert enum values to json array
terms_sub$allowedValues = sapply(sapply(sapply(sapply(terms_sub$allowedValues,strsplit,"\n"),strsplit,"="),function(x) sapply(x,function(y) trimws(y[1]))),toJSON)

# -> format null-arrays and NA strings
terms_sub[which(is.na(terms_sub$default)),"default"]=""
terms_sub[which(terms_sub$allowedValues=="[null]"),"allowedValues"]="[]"

dictionary = gsub("\"TODO terms\"", unbox(toJSON(lapply(split(terms_sub,terms_sub$identifier),unbox), auto_unbox=TRUE,pretty=TRUE)),dictionary)

# add applicability
applic_sub=as.data.frame(t(terms[,-which(colnames(terms)%in%c("identifier","group","name","accronym","type","allowedValues","default","description","cNTRLSensitive"))]))
applic_sub=cbind(rownames(applic_sub),applic_sub)
colnames(applic_sub) = tocamel(c("Contract",terms$identifier))
rownames(applic_sub) = NULL

dictionary = gsub("\"TODO applicability\"", unbox(toJSON(lapply(split(applic_sub,applic_sub$contract),unbox), auto_unbox=TRUE,pretty=TRUE)),dictionary)

# add events
dictionary = gsub("\"TODO events\"", unbox(toJSON(lapply(split(events,events$identifier),unbox), auto_unbox=TRUE,pretty=TRUE)),dictionary)

# add states
# -> convert enum values to json array
states$allowedValues = sapply(sapply(sapply(sapply(states$allowedValues,strsplit,"\n"),strsplit,"="),function(x) sapply(x,function(y) trimws(y[1]))),toJSON)

# -> format null-arrays and NA strings
states[which(states$allowedValues=="[null]"),"allowedValues"]="[]"

dictionary = gsub("\"TODO states\"", unbox(toJSON(lapply(split(states,states$identifier),unbox), auto_unbox=TRUE,pretty=TRUE)),dictionary)

# clean up form
dictionary = gsub("\"[\"", "[\"", dictionary,fixed=TRUE)
dictionary = gsub("\"]\"", "\"]", dictionary,fixed=TRUE)
dictionary = gsub("\"[]\"", "[]", dictionary,fixed=TRUE)


# export final form
dictionary %>% write_lines(paste0(json_path,'atcus-dictionary.json'))
