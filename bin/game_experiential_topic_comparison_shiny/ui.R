library(shiny)

# Front end section --------------------------------------------------------
ui <- fluidPage(title = 'Game Experiential Topic',
                # Title panel + some CSS
                titlePanel(h1("Game Experiential Topic Comparison", style = "font-family:'Lobster'; color:#888888")),   
                
                # Side bar
                sidebarLayout(
                  sidebarPanel(
                    h3("Instruction"),
                    "1. Filter right-hand side data table by [GS Score] and [ESRB Rating] options below.",
                    br(),
                    "2. You can choose to download the data table by [Download Result] button.",     
                    br(),                    
                    "3. Select games in the data table to perform Game Experiential Topic comparison.",
                    hr(),
                    h3("Definition of Experiential Topic Types"),        
                    br(),
                    "- Social Type:",
                    br(),
                    "Focuses on providing social values, such as the sense of team work. This type lets players interact with each other or NPC, or serves as a material for real-life social interactions.",
                    br(),
                    br(),
                    "- Achievemental Type:",
                    br(),
                    "Focuses on providing the sense of achievement, the feedbacks after challenges and the goal structure of accomplishments.",
                    br(),
                    br(),
                    "- Explorative Type:",
                    br(),
                    "Focuses on providing exploring new things, new visual experience, or new structures and rules behind phenomena. Value the variety of objects.",
                    br(),
                    br(),
                    "- Sensational Type:",
                    br(),
                    "Focuses on providing the more primitive enjoyment. It can be provided by the sense of speed and brutal forces, or figuratively, allowing players break the laws in the real world, killing, vandalizing, and so forth.",
                    hr(),                    
                    sliderInput(
                      "GSScore",
                      "GS Score",
                      min = 0,
                      max = 10,
                      value = c(0, 10)
                    ),
                    selectInput(
                      "ESRB",
                      "ESRB Rating",
                      choices = c("---", "Everyone", "Everyone 10+", "Teen", "Mature", "Rating Pending")
                    ),
                    plotOutput("spiderPlot"),
                    br(),
                    actionButton('clearSelection', 'Clear Selection')
                  ),
                  
                  # Main panel
                  mainPanel(
                    downloadButton("resultFile", "Download Result"),
                    hr(),
                    DT::dataTableOutput("queryResult")
                  )
                )
)