library(shiny)
library(tidyverse)
library(shinyjs)
library(plotly)
library(shinythemes)
library(httr)
library(dplyr)
library(shinyjs)

#############################################################################
#Get data from the web

#url<- "https://pokemon-go1.p.rapidapi.com/pokemon_stats.json"
#key = "11a0831306msh85289b71ee29d28p1b0d2ajsnd56ec6de06bd"

#pokemon = GET(url, config=add_headers("x-rapidapi-host"= "pokemon-go1.p.rapidapi.com",'x-rapidapi-key' = key))
#load("pokemon_API.RData")
#content(pokemon)


#details<- httr::content(pokemon,as='parsed')
#details

##########################################################################
#cleaning, manipulation and analysis of the retrieved data

# attack <- numeric(length(details))
# defense<- numeric(length(details))
# stamina <- numeric(length(details))
# formas <- numeric(length(details))
# id <- numeric(length(details))
# name <- numeric(length(details))
# 
# for (i in 1:length(details)) {
#     attack[i] <- as.numeric(details[[i]]$base_attack) 
#     defense[i] <- as.numeric(details[[i]]$base_defense)
#     stamina[i] <- as.numeric(details[[i]]$base_stamina)
#     formas<- details[[i]]$form
#     id[i] <- as.numeric(details[[i]]$pokemon_id)
#     name[i] <- details[[i]]$pokemon_name
# }
# 
# data<- as.data.frame(cbind(id, name, formas, stamina, defense, attack))
# data<- distinct(data)

load("data.RData")
data_name<- sort(data$name)


##########################################################################3


headerRow <- div(id="header", useShinyjs(),
                 selectInput(input= "selpok", 
                             label="Select the Pokemon", 
                             multiple = TRUE,
                             choices= data_name),
                 downloadButton("report", "Generate report")
)

plotlyPanel <- tabPanel("Plotly stamina",
                        plotly::plotlyOutput("plotlystam")
)


plotlyPanel2 <- tabPanel("Plotly defense",
                        plotly::plotlyOutput("plotlydef")
)

plotlyPanel3 <- tabPanel("Plotly attack",
                         plotly::plotlyOutput("plotlyatt")
)

ui <- navbarPage(
   "Pokemon App",
    plotlyPanel,
    plotlyPanel2,
    plotlyPanel3,
    id = "navBar",
    header=headerRow
)

# Define server logic required to draw a histogram
server <- function(input, output, session) {

    observe({if(input$navBar=="Map") {
        shinyjs::hide("header")
    } else {
        shinyjs::show("header")
    }
    })

    data_filtered <- reactive({
        req(input$selpok)
        req(input$stamina)
        data %>% filter(name %in% input$selpok,
                             stamina == input$stamina)
    })
    
    
    output$plotlystam <- plotly::renderPlotly({
    data_filtered() %>%
         ggplot(aes(x=name, y=stamina, fill=name)) +
            geom_bar(stat="identity", position=position_dodge())
    })
    
    output$plotlydef <- plotly::renderPlotly({
            ggplot(data, aes(x=name, y=defense, fill=name)) +
            geom_bar(stat="identity", position=position_dodge())
    })
    
    output$plotlyatt <- plotly::renderPlotly({
   ggplot(data, aes(x= name, y=attack, color=name))+
    geom_boxplot()
    })
    
    output$report <- downloadHandler(
        # For PDF output, change this to "report.pdf"
        filename = "report.pdf",
        content = function(file) {
            tempReport <- file.path(tempdir(), "report.Rmd")
            file.copy("report.Rmd", tempReport, overwrite = TRUE)
            
            # Set up parameters to pass to Rmd document
            params <- list(
                selPok = isolate(input$selpok),
                stamina = isolate(input$stamina), 
                defense = isolate(input$defense),
            )
            
            rmarkdown::render(tempReport, output_file = file,
                              params = params,
                              envir = new.env(parent = globalenv())
            )
        })

} 

# Run the application 
shinyApp(ui = ui, server = server)
