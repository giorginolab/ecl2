library(shiny)
library(dplyr)

load("data/summary-table.RData")

# Misspelled

st1 <- st


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
)



# Define server logic required to draw a histogram
server <- function(input, output) {

    output$table <- DT::renderDataTable(DT::datatable({
      if(input$selected.cluster=="All") {
          r <- st1
      } else {
          r <- st1 %>% filter(Cluster==input$selected.cluster)
      }
      r %>% 
          mutate(ID=sprintf('<a href="figures/%s.pdb" target=”_blank”>%s</a',Structure,ID)) %>%
          select(ID,Cluster,GPCR,ECL2,
                 `Contacts with TM1`,`Contacts with TM2`,`Contacts with ECL1`,
                 `Contacts with TM3`,`Contacts with TM4`,`Contacts with TM5`,
                 `Contacts with ECL3`,`Contacts with TM6`,
                 `Contacts with TM7`,
                 `Length of segment before-Cys45.50`,`Length of segment after-Cys45.50`
          ) 
    }, escape=FALSE))
    
}

# Run the application 
shinyApp(ui = ui, server = server)
