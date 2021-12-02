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
    p("This interactive view accompanies the paper:"),
    p("A. Di Pizio et al.,",em("Shapes of the Second Extracellular Loop of Class A GPCRs Characterized by Clustering and Intramolecular Interaction Analyses, "),
      "Under review."),
    
    fluidRow(
        column(4,
               selectInput("selected.cluster",
                           label="Selected ECL2 cluster",
                           choices = c("All",cls))
               )
    ),
    
    h2("Results"),

    tableOutput("table")
)



# Define server logic required to draw a histogram
server <- function(input, output) {

    output$table <- renderTable({
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
    }, 
    rownames=TRUE,
    striped=TRUE, hover=TRUE,
    sanitize.text.function = function(x) x)
    
}

# Run the application 
shinyApp(ui = ui, server = server)
