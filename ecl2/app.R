library(shiny)
library(dplyr)

load("data/summary-table.RData")

# Misspelled

st1 <- st


cls <- sort(unique(st1$Cluster))


# Define UI for application that draws a histogram
ui <- fluidPage(
    
    # Application title
    titlePanel("A classification system for GPCR ECL2 domains based on intramolecular interactions"),
    h3("Abstract"),
    p("The extracellular loop 2 (ECL2) is the longest and the most diverse loop among class A G protein-coupled receptors (GPCRs). We propose a 7-cluster classification of currently resolved ECL2 domains on the basis of their intermolecular interactions with the other GPCR regions."),
    h3("Reference"),
    # p("This interactive view accompanies the paper:"),
    p("See: A. Di Pizio et al.,",em("Shapes of the Second Extracellular Loop of Class A GPCRs Characterized by Clustering and Intramolecular Interaction Analyses, "),
      "Under review."),
    
    h3("Query the ECL2 clusters"),
    
    fluidRow(
        column(4,
               wellPanel(
                   selectInput("selected.cluster",
                               label="Selected ECL2 cluster",
                               choices = c("All",cls)),
                   p("Distinct clusters A to G have been assigned on the basis of volumetric overlaps. See the paper for details.")
               )),
        column(8,
               h5("Plot goes here")
        )
    ),
    h3("Summary of selected GPCRs"),
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
