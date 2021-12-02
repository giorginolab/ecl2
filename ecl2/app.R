library(shiny)
library(dplyr)
library(stringr)

load("data/summary-table.RData")

# Misspelled

st1 <- st %>% select(-c(`Lenght of segment before-Cys45.50`, 
                        `Lenght of segment before-Cys45.50`,
                        `Structures within this structure's cluster`)) %>%
    mutate(Structure=str_replace(Structure,".tif","")) %>%
    rename(Cluster=`Cluster (based on Volume overlaps)`) %>%
    relocate(Cluster, GPCR, Structure, ECL2) %>%
    arrange(Cluster)




#st[["Lenght of segment before-Cys45.50"]]<-NULL
#st[["Lenght of segment after-Cys45.50"]]<-NULL

cls <- sort(unique(st1$Cluster))


# Define UI for application that draws a histogram
ui <- fluidPage(

    # Application title
    titlePanel("ECL2 summary table"),
    p("Write description here"),
    
    fluidRow(
        column(4,
               selectInput("selected.cluster",
                           label="Select ECL2 cluster",
                           choices = c("All",cls))
               )
    ),
    
    h2("Results"),

    DT::dataTableOutput("table")




# Define server logic required to draw a histogram
server <- function(input, output) {

    output$table <- DT::renderDataTable(DT::datatable({
      if(input$selected.cluster=="All") {
          r <- st1
      } else {
          r <- st1 %>% filter(Cluster==input$selected.cluster)
      }
      r %>% 
          mutate(Structure=sprintf('<a href="figures/%s.pdb" target=”_blank”>%s</a',Structure,Structure)) 
    }, escape=FALSE))
    
}

# Run the application 
shinyApp(ui = ui, server = server)
