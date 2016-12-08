library(shiny)
library(DT)
library(topicmodels)
library(tidyverse)
library(tidytext)
library(feather)
library(fmsb)
library(stringdist)
library(scales)
library(RColorBrewer)

# Preparation --------------------------------------------------------------
# Read in main dataframe
text_raw <- read_feather("text_raw.feather")

# Load documentTermMatrix and LDA model object
load("model_lda.RData")

# Games with topic scores
games_lda <- model_lda %>%
  tidytext:::tidy.LDA(matrix = "gamma") %>%
  spread(topic, gamma) %>%
  left_join(text_raw, by = c("document" = "GameTitle")) %>%
  select(1:5, GSScore, ESRB, ReleaseDate)
colnames(games_lda)=c("Game", "Social Score", "Achievemental Score", "Explorative Score", "Sensational Score", "GS Score", "ESRB", "Release Date")

# Back end section ---------------------------------------------------------
server <- function(input, output) {
  # Filter data
  tbData <- reactive({
    # Default select all
    if(input$ESRB == "---"){
      filter(games_lda,
             `GS Score` >= input$GSScore[1],
             `GS Score` <= input$GSScore[2]
      )
    }else{
      filter(games_lda,
             `GS Score` >= input$GSScore[1],
             `GS Score` <= input$GSScore[2],
             ESRB == input$ESRB
      )
    }
  })
  
  # Clear selection 
  observeEvent(input$clearSelection, {
    dataTableProxy('queryResult') %>% selectRows(NULL)
  })
  
  # Render output
  # -- Output game topic spider plot
  output$spiderPlot <- renderPlot({
    if(is.null(input$queryResult_rows_selected)){
      return()
    }
    
    # Modify data to conform to radarchart's require form
    pltData <- tbData()[input$queryResult_rows_selected,]
    pltData <- rbind(rep(1, 4), rep(0, 4), select(pltData, 1:5))
    
    # Acquire color palette
    shapeColor <- colorRampPalette(brewer.pal(12, "Accent"))(nrow(pltData) - 2)
    
    # Plotting and plot setting
    radarchart( pltData[2:5] , axistype = 1,
                #custom polygon
                pcol = shapeColor, pfcol = shapeColor, plwd = 1, plty = 1,
                #custom the grid
                cglcol = "grey", cglty = 1, axislabcol = "grey", cglwd = 0.8,
                #custom labels
                vlcex = 1.2
    )
    
    # Add legend
    legend(-2.5, 1.2, legend = levels(as.factor(pltData$Game[3:length(pltData$Game)])), title = "Game", col = shapeColor, seg.len = 2, border = "transparent", pch = 16, lty = 1)
  })
  
  # -- Output data table based on filter criteria  
  output$queryResult <- DT::renderDataTable({
    DT::datatable(tbData() %>%
                    mutate(`Explorative Score` = percent(`Explorative Score`),
                           `Social Score` = percent(`Social Score`),
                           `Achievemental Score` = percent(`Achievemental Score`),
                           `Sensational Score` = percent(`Sensational Score`)) %>%
                    select(Game, `Release Date`, `GS Score`, ESRB, 2:5),
                  options = list(pageLength = 25)
    )
  })
  
  # -- Output download current document
  output$resultFile <- downloadHandler(
    filename = function(){
      paste("data-", Sys.Date(), ".csv", sep = "")
    },
    content = function(file){
      write.csv(tbData(), file)
    }
  )
}