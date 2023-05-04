# Hack2

#Manually set directory

library(tidyverse)
library(WDI)
library(ggplot2)
library(fmsb)

spgrades <- data.frame(read.csv("Spring2022grades.csv"))
fagrades <- data.frame(read.csv("Fall2021grades.csv"))

grades <- rbind(fagrades, spgrades) #Combine two datasets into one dataframe
instructor <- grep("Instructor", colnames(grades), value = TRUE) 
    #Find column names that has "Instructor"
    # We're removing all those columns below in the select function

grades$Course <- paste(grades$Subject, grades$Catalog.Number)
                  # paste is concatenating (combining the values) in two columns
                  # into a new one

head(grades[1:2])
head(grades[28])
#Create Total grades column 

#Create Percentage Columns
pct <- grades[4:21]/rowSums(grades[4:21], na.rm=TRUE) * ifelse(rowSums(is.na(grades[4:21])) == ncol(grades[4:21]), NA, 1)
grades <- merge(grades,pct)

#Filter for Prof. Ho's classes
kh_grades <- grades%>%
  filter(Instructor.1=="Ho, Karl")%>%
  select(-instructor,-Section, -Subject, -Catalog.Number)
  # Column names with minus (-) sign means removing.  
  # Here, we are keeping all the columns but those mentioned above

final_scores <- data.frame(
  row.names = c("Karl_Ho"),
  EPPS_6323 = c(0.8),
  EPPS_6354 = c(0.54),
  EPPS_7386 = c(0.95),
  PSCI_4314 = c(0.42),
  EPPS_6356 = c(0.92),
  PSCI_4313 = c(0.61),
  EPPS_6302 = c(0.77),
  EPPS_7V81= c(0.86),
  EPPS_7318 = c(0.9)
)
final_scores

max_min <- data.frame(
  EPPS_6323 = c(1, 0), EPPS_6354 = c(1, 0), EPPS_7386 = c(1, 0),
  PSCI_4314 = c(1, 0),  EPPS_6356 = c(1, 0),  PSCI_4313  = c(1, 0),
  EPPS_6302 = c(1, 0), EPPS_7V81 = c(1, 0), EPPS_7318 = c(1, 0)
)
rownames(max_min) <- c("Max", "Min")
df <- rbind(max_min, final_scores)
EPPS <- df[c("Max", "Min", "Karl_Ho"), ]
radarchart(EPPS, 
           title = "Karl Ho A Grade Frequency", 
           pcol ="chocolate1", 
           cglcol = "chartreuse4", 
           axislabcol = "chartreuse4", 
           axistype = 3)
