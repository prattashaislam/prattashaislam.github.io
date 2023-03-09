# Hack2

#Manually set directory

library(tidyverse)
library(WDI)

spgrades <- data.frame(read.csv("Spring2022grades.csv"))
fagrades <- data.frame(read.csv("Fall2021grades.csv"))

grades <- rbind(fagrades, spgrades) #Combine two datasets into one dataframe
instructor <- grep("Instructor", colnames(grades), value = TRUE) 
    #Find column names that has "Instructor"
    # We're removing all those columns below in the select function

grades$Course <- paste(grades$Subject, grades$Catalog.Number)
                  # paste is concatenating (combining the values) in two columns
                  # into a new one

#Create Total grades column 

#Create Percentage Columns for Leo
pct <- grades[4:21]/rowSums(grades[4:21], na.rm=TRUE) * ifelse(rowSums(is.na(grades[4:21])) == ncol(grades[4:21]), NA, 1)
pct$Course <- grades$Course
grades <- merge(grades,pct, by="Course")
# x is count and y is percentage

#Min and Max for grades for Kaitlyn
summary(grades[4:21])

#Filter for Prof. Ho's classes
kh_grades <- grades%>%
  filter(Instructor.1=="Ho, Karl")%>%
  select(-instructor,-Section, -Subject, -Catalog.Number)
  # Column names with minus (-) sign means removing.  
  # Here, we are keeping all the columns but those mentioned above

#Filter for EPPS courses
EPPS <- data.frame(grades$Course=EPPS)