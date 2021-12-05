library(shiny)
library(dplyr)
library(ggplot2)

load("data/summary-table.RData")

st1 <- st
cls <- sort(unique(st1$Cluster))

# https://stackoverflow.com/questions/27004136/position-popup-image-on-mouseover
# http://jsfiddle.net/4hzenxkh/1/
mkhoverlink <- function(txt,img) {
    sprintf('<span class="ecl2img"><a href="#"><img src="figures/%s.png" alt=""/>%s</a></span>', img, txt)
}


# Define UI for application that draws a histogram
ui <- fluidPage(
    tags$head(
        tags$link(rel = "stylesheet", type = "text/css", href = "hover.css")
    ),
    
    # Application title
    titlePanel("A Classification Model for Class A GPCR ECL2 domains"),
    h3("Abstract"),
    p("The extracellular loop 2 (ECL2) is the longest and the most diverse loop among class A G protein-coupled receptors (GPCRs). We propose a 7-cluster classification scheme of currently resolved ECL2 domains based on volume overlaps and their intermolecular interactions with the other GPCR regions."),
    h3("Reference"),
    # p("This interactive view accompanies the paper:"),
    p("See: A. Di Pizio et al.,",em("A Classification Model for the Second Extracellular Loop of Class A GPCRs, "),
      "Under review."),
    
    h3("Query the ECL2 clusters"),
    
    fluidRow(
        column(4,
               wellPanel(
                   selectInput("selected.cluster",
                               label="Selected ECL2 cluster",
                               choices = c("All",cls)),
                   p("Distinct clusters A to G have been assigned on the basis of volumetric overlaps. See the paper for details."),
                   radioButtons("dynamic", label = "Data set",
                                choices = list("GPCRmd (molecular dynamics)" = 1, "RCSB PDB (experimental)" = 2), 
                                selected = 1),
                   
               )),
        column(8,
               plotOutput('plot')
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
          mutate(ECL2=mkhoverlink(ECL2,Structure)) %>%
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
    
    output$plot <- renderPlot({
        if(input$dynamic==1) {
            if(input$selected.cluster == "All" ||
               input$selected.cluster == "F") {
                ggplot() + annotate("text",
                                    x = 4, y = 25, size=8,
                                    label="Please select a cluster") + theme_void()
            } else {
                # csub <- contacts %>% filter(Frame>0 & Cluster=="C")
                csub <- contacts %>% filter(Frame>0)
                csub <- csub %>% filter(Cluster == input$selected.cluster)
                ggplot(csub, aes(x=Region,y=Contacts,color=ID,fill=ID))+geom_boxplot(alpha=0.3)
            }
        } else {
            if(input$selected.cluster == "All") {
                ggplot() + annotate("text",
                                    x = 4, y = 25, size=8,
                                    label="Please select a cluster") + theme_void()
            } else {
                csub <- contacts %>% filter(Frame==-1)
                csub <- csub %>% filter(Cluster == input$selected.cluster)
                ggplot(csub, aes(color=ID,y=Contacts,x=Region))+geom_point(size=3)
            }  
        }
        
        
    })
    
}

# Run the application 
shinyApp(ui = ui, server = server)
