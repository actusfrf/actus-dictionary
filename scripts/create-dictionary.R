
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
colnames(taxonomy)=gsub("."," ",colnames(taxonomy),fixed=TRUE)
colnames(terms)=gsub("."," ",colnames(terms),fixed=TRUE)
colnames(states)=gsub("."," ",colnames(states),fixed=TRUE)
colnames(events)=gsub("."," ",colnames(events),fixed=TRUE)

# convert enum values to json array
terms$'Allowed Values' = sapply(sapply(sapply(sapply(terms$'Allowed Values',strsplit,"\n"),strsplit,"="),function(x) sapply(x,function(y) trimws(y[1]))),toJSON)

# remove NA strings
terms[which(terms$Type!="Enum"),"Allowed Values"]=""

# create dictionary base structure
dictionary = toJSON(unbox(data.frame(Taxonomy="TODO taxonomy",Terms="TODO terms",Applicability="TODO applicability",Events="TODO events",States="TODO states")),auto_unbox=TRUE,pretty=TRUE)

# add taxonomy
dictionary = gsub("\"TODO taxonomy\"", unbox(toJSON(lapply(split(taxonomy,taxonomy$Identifier),unbox), auto_unbox=TRUE,pretty=TRUE)),dictionary)

# add terms as sub-json
terms_sub=terms[,c("Identifier", "Group", "Name", "Accronym", "Type", "Allowed Values", "Default", "Description")]

dictionary = gsub("\"TODO terms\"", unbox(toJSON(lapply(split(terms_sub,terms_sub$Identifier),unbox), auto_unbox=TRUE,pretty=TRUE)),dictionary)

# add applicability
applic_sub=as.data.frame(t(terms[,-which(colnames(terms)%in%c("Identifier","Group","Name","Accronym","Type","Allowed Values","Default","Description","CNTRL Sensitive"))]))
applic_sub=cbind(rownames(applic_sub),applic_sub)
colnames(applic_sub) = c("Contract",terms$Identifier)
rownames(applic_sub) = NULL

dictionary = gsub("\"TODO applicability\"", unbox(toJSON(lapply(split(applic_sub,applic_sub$Contract),unbox), auto_unbox=TRUE,pretty=TRUE)),dictionary)

# add events
dictionary = gsub("\"TODO events\"", unbox(toJSON(lapply(split(events,events$Identifier),unbox), auto_unbox=TRUE,pretty=TRUE)),dictionary)

# add states
dictionary = gsub("\"TODO states\"", unbox(toJSON(lapply(split(states,states$Identifier),unbox), auto_unbox=TRUE,pretty=TRUE)),dictionary)

# clean up form
dictionary = gsub("\"[\"", "[\"", dictionary,fixed=TRUE)
dictionary = gsub("\"]\"", "\"]", dictionary,fixed=TRUE)

# export final form
dictionary %>% write_lines(paste0(json_path,'atcus-dictionary.json'))
