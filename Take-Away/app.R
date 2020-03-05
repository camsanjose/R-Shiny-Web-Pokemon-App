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

url<- "https://pokemon-go1.p.rapidapi.com/pokemon_stats.json"
key = "11a0831306msh85289b71ee29d28p1b0d2ajsnd56ec6de06bd"

pokemon = GET(url, config=add_headers("x-rapidapi-host"= "pokemon-go1.p.rapidapi.com",'x-rapidapi-key' = key))
#load("pokemon_API.RData")
content(pokemon)


details<- httr::content(pokemon,as='parsed')
details

##########################################################################
#cleaning, manipulation and analysis of the retrieved data

attack <- numeric(length(details))
defense<- numeric(length(details))
stamina <- numeric(length(details))
formas <- numeric(length(details))
id <- numeric(length(details))
name <- numeric(length(details))

for (i in 1:length(details)) {
    attack[i] <- as.numeric(details[[i]]$base_attack) 
    defense[i] <- as.numeric(details[[i]]$base_defense)
    stamina[i] <- as.numeric(details[[i]]$base_stamina)
    formas<- details[[i]]$form
    id[i] <- as.numeric(details[[i]]$pokemon_id)
    name[i] <- details[[i]]$pokemon_name
}

data<- as.data.frame(cbind(id, name, formas, stamina, defense, attack))
data<- distinct(data)

data_name<- levels(data$name)
#load("data.RData")

##########################################################################3


headerRow <- div(id="header", useShinyjs(),
                 selectInput("selpok", 
                             label="Select the Pokemon", 
                             multiple = TRUE,
                             choices=data_name))
plotlyPanel <- tabPanel("Plotly",
                        plotly::plotlyOutput("plotlyData")
)


ui <- navbarPage(
    "Pokemon App",
    plotlyPanel,
    id = "navBar",
    header=headerRow
)

# Define server logic required to draw a histogram
server <- function(input, output) {

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
                         stamina %in% input$stamina)
})   

output$plotlyData <- plotly::renderPlotly({
ggplot(data_filtered(), aes(x=name, y=stamina, fill=name)) +
        geom_bar(stat="identity", position=position_dodge())
})
} 

# Run the application 
shinyApp(ui = ui, server = server)
