library(shiny)
library(tidyverse)
library(shinyjs)
library(plotly)
library(shinythemes)
library(httr)
library(dplyr)
library(jsonlite)
library(shinyjs)

#library(RCurl)
#getURL
#503072804

url<- "https://pokemon-go1.p.rapidapi.com/pokemon_stats.json"
key = "11a0831306msh85289b71ee29d28p1b0d2ajsnd56ec6de06bd"

pokemon = GET(url, config=add_headers("x-rapidapi-host"= "pokemon-go1.p.rapidapi.com",'x-rapidapi-key' = key))
#save(pokemon, file = "pokemon_API.RData")
#load(pokemon_API.RData)
content(pokemon)


details<- httr::content(pokemon,as='parsed')
details

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

headerRow <- div(id="header", useShinyjs(),
                 selectInput("selpok", 
                             label="Select the Pokemon", 
                             multiple = TRUE,
                             choices=data_name))


plotlyPanel <- tabPanel("Plotly",
                        plotly::plotlyOutput("plotlyData")
)


ui <- fluidPage(
    plotlyPanel, 
    header=headerRow
)

# Define server logic required to draw a histogram
server <- function(input, output) {
output$plotlyData <- plotly::renderPlotly({
        ggplot(data) + aes(x=name, y=sort(stamina), fill=name) +
        geom_bar(stat="identity", position=position_dodge())
    })
}

# Run the application 
shinyApp(ui = ui, server = server)
