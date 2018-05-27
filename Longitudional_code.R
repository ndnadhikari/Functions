## Author: Niranjan 

###  Part 2: Longitudinal Data Class and Methods

## Read in the data 

# Loading pakage 
rm(list=ls()) #3 clear up the working environment 
library(readr) # use to read data 
library(magrittr) # applicable for  forward pipe operator %>%
library(dplyr) ## use for mutate function 
library(tidyr) ## use for spread function 

# Reading csv file 
data <- read.csv("Final_assignment/MIE.csv")

# Checking column type of data 
str(data)

# Since room is found to be factor so I want to convert to character 
data$room <- as.character(data$room)

# Define the some essential generic functions 
setGeneric("print")
setGeneric("summary")
setGeneric("subject", function(x,...){
  standardGeneric("subject")
})
setGeneric("visit", function(x,...){
  standardGeneric("visit")
})
setGeneric("room", function(x,...){
  standardGeneric("room")
})

# LongitudinalData Class and Methods
setClass("LongitudinalData",
         representation(id = "numeric", 
                        visit = "numeric",
                        room = "character",
                        value = "numeric",
                        timepoint = "numeric"))

## Define the print of longitudional data 
setMethod("print",
          c(x = "LongitudinalData"),
          function(x){
            paste("Longitudinal dataset with", length(unique(x@id)), "subjects")
          })

## longitudional data function 
make_LD <- function(x) {
  new("LongitudinalData", id = x$id, visit = x$visit,
      room = x$room, value = x$value, timepoint = x$timepoint)
}

## convert data into longitudional data 
x <- make_LD(data)
print(class(x))
# [1] "LongitudinalData"
# attr(,"package")
# [1] ".GlobalEnv"

print(x)
# Longitudinal dataset with 10 subjects 

## A generic function for extracting subject-specific information
setClass("subject_class",
         representation(id = "numeric", 
                        visit = "numeric",
                        room = "character",
                        value = "numeric",
                        timepoint = "numeric"))

setMethod("print",
          c(x = "subject_class"),
          function(x){
            if (length(unique(x@id)) > 0) {
              cat(paste("Subject ID:",unique(x@id)))
            } else {
              NULL
            }
          })
setMethod("subject",
          c(x = "LongitudinalData"),
          function(x,n){
            new("subject_class", id = x@id[x@id == n], visit = x@visit[x@id == n],
                room = x@room[x@id == n], value = x@value[x@id == n],
                timepoint = x@timepoint[x@id == n])
          })

setMethod("summary",
          c(object = "subject_class"),
          function(object){
            new("subject_summary", id = object@id, visit = object@visit, 
                room = object@room, value = object@value)
          })

## Subject 8 doesn't exist
out_1 <- subject(x, 8)
print(out_1)
# NULL

out_2 <- subject(x, 64)
print(out_2)
# Subject ID: 64 

out_3 <- subject(x, 64) %>% summary()
print(out_3)


# generic function for visit_class, method and summary 

setClass("visit_class",
         representation(id = "numeric", 
                        visit = "numeric",
                        room = "character",
                        value = "numeric",
                        timepoint = "numeric"))
         

setMethod("visit",
          c(x = "subject_class"),
          function(x,n){
            new("visit_class", id = x@id[x@visit == n], visit = x@visit[x@visit == n],
                room = x@room[x@visit == n], value = x@value[x@visit == n],
                timepoint = x@timepoint[x@visit == n])
          })

setMethod("print",
          c(x = "visit_class"),
          function(x){
            cat(paste("ID:", unique(x@id)),"\n")
            cat(paste("Visit:",unique(x@visit)),"\n")
          })

setMethod("summary",
          c(object = "visit_class"),
          function(object){
            new("room_summary", id = object@id, value = object@value)
          })

## Experiment 
out_4 <- subject(x, 46) %>% visit(1) 
print(out_4)

out_5 <- subject(x, 46) %>% visit(1) %>% summary()
print(out_5)


# room_class Class and Methods

setClass("room_class",
         representation(id = "numeric", 
                        visit = "numeric",
                        room = "character",
                        value = "numeric",
                        timepoint = "numeric"))

setMethod("room",
          c(x = "visit_class"),
          function(x,n){
            new("room_class", id = x@id[x@room == n], visit = x@visit[x@room == n],
                room = x@room[x@room == n], value = x@value[x@room == n],
                timepoint = x@timepoint[x@room == n])
          })

setMethod("print",
          c(x = "room_class"),
          function(x){
            cat(paste("ID:", unique(x@id)),"\n")
            cat(paste("Visit:",unique(x@visit)),"\n")
            cat(paste("Room:",unique(x@room)))
          })

setMethod("summary",
          c(object = "room_class"),
          function(object){
            new("room_summary", id = object@id, value = object@value)
          })


## Experimental session
## Information of ID, Visit and room  
out_6 <- subject(x, 46) %>% visit(1) %>% room("living room")
print(out_6)
# ID: 46 
# Visit: 1 
# Room: living room

## Show a summary of the pollutant values

out_7 <- subject(x, 46) %>% visit(1) %>% room("living room") %>% summary

print(out_7)
# ID: 46 
# Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
# 2.750   2.750   8.000   9.317  12.000  79.000

out_8 <- subject(x, 46) %>% visit(0) %>% room("bedroom") %>% summary
print(out_8)
# ID: 46 
# Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
# 2.75    2.75    7.00   34.89   45.00  576.00 

