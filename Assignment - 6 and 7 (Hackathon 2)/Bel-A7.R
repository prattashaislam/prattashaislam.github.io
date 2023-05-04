library(tidyverse)
# library(ggplot2)
# library(fmsb)
library(shiny)

sp22 <- data.frame(read.csv("https://raw.githubusercontent.com/acmutd/utd-grades/master/raw_data/Spring%202022.csv"))
sp22$sem <- "SP22"
fa21 <- data.frame(read.csv("https://raw.githubusercontent.com/acmutd/utd-grades/master/raw_data/Fall%202021.csv"))
fa21$sem <- "FA21"

grades <- rbind(fa21, sp22)
grades$Course <- paste(grades$Subject, grades$Catalog.Number)
#Create Percentage Columns
pct <- grades[4:21]/rowSums(grades[4:21], na.rm=TRUE) * ifelse(rowSums(is.na(grades[4:21])) == ncol(grades[4:21]), NA, 1)
pct$Course <- grades$Course
grades <- merge(grades,pct, by="Course")

instructor <- grep("Instructor", colnames(grades), value = TRUE)
instructor <- instructor[2:6]

grades <- grades%>%
  select(-instructor)


# Define UI for dataset viewer app ----
ui <- fluidPage(
  
  # App title ----
  titlePanel("UTD Grades"),
  
  # Sidebar layout with a input and output definitions ----
  sidebarLayout(
    
    # Sidebar panel for inputs ----
    sidebarPanel(
      
      # Input: Selector for choosing dataset ----
      checkboxGroupInput(inputId = "Semester",
                         label = "Choose a semester:",
                         choices = c("Spring 2022", "Fall 2021")),
      
      
      # Input: Selector for choosing Subject ----
      selectInput(inputId = "Class",
                  label = "Choose a subject:",
                  choices = unique(grades$Subject),
                  selected = "EPPS"),
      
      uiOutput("Catalog"),
      
      # Input: Selector for choosing an Instructor ----
      selectInput(inputId = "Instructor",
                  label = "Choose an instructor:",
                  choices = unique(grades$Instructor.1),
                  selected = "Ho, Karl"),
    ),
    # Main panel for displaying outputs ----
    mainPanel(
      
      # Output: Plot from Leo ----
      plotOutput("UTD.Overachievers"),
      
      # Output: Plot from Prattasha ----
      plotOutput("UTD.A")
    )
  ))

# Define server logic to summarize and view selected dataset ----
server <- function(input, output, session) {
  
  # Choose Catalog number
  
  output$Catalog <- renderUI({
    
    selectInput("Catalog.Number", label = "Select Catalog Number:",
                # the choices below is filtered based on input$Class
                choices = grades %>% 
                  filter(Subject == input$Class) %>% 
                  pull(Catalog.Number) %>% unique()
    )
    
  })
  
  # Generate Plot - Leo
  # Choose subject
  
  dataInput <- reactive({

    output$UTD.Overachievers <- renderPlot({
      
      class <- grep(toString(input$Class), grades$Course, value = TRUE)
      EPPS <- grades%>%
        subset(Course %in% class)
      
      plotO <- ggplot(EPPS, aes(Course, F.x)) +
        geom_bar(stat = "identity",color="orange") +
        coord_flip() + 
        labs(title = "UTD Overachievers", x = "Subject", y = "F") +
        theme(axis.text.x = element_text(face="bold", color="#008000",
                                         size=8, angle=0),
              axis.text.y = element_text(face="bold", color="#008000",
                                         size=3.5, angle=0))
      plotO
    })
  })
  
  # Generate Plot - Prattasha
  # Choose Instructor
  
  dataInput2 <- reactive({

    output$UTD.Overachievers <- renderPlot({  
      cc <- grades %>%
        filter(Instructor.1 == input$Instructor)%>%
        select(Course, A.x, A.y) 
      
      plotA <- ggplot()+
        geom_col(mapping = aes(x=Course, y=A.x),
                 fill = c("limegreen","limegreen", "springgreen4","limegreen", "limegreen")) +
        labs(x = "Course", y = "A Grade",
             title="A Grades in Dr. Karl Ho's Class",
             subtitle = "Fall 2021 & Spring 2022") +
        geom_text(aes(x=Course, y=A.x,label= A.x),vjust=1.2, size=5,col = "white")
      
      plotA
      
    })
  })
  
}

shinyApp(ui = ui, server = server)
