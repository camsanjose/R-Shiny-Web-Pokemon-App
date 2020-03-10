library(shiny)
library(tidyverse)
library(shinyjs)
library(plotly)
library(shinythemes)
library(httr)
library(dplyr)
library(shinyjs)
library(reshape2)

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
data_var <- c("stamina", "defense", "attack")

##########################################################################3


headerRow <- div(id="header", useShinyjs(),
                 selectInput(input= "selpok", 
                             label="Select the Pokemon", 
                             multiple = TRUE,
                             choices= data_name,
                             selected = head(data_name,4))
)

button <- div(id="button", useShinyjs(),
    downloadButton("report", "Generate report")
)
headerRow2 <- div(id="secondheader", useShinyjs(),
                 selectInput(input= "selvar", 
                             label="Select the variable", 
                             multiple = TRUE,
                             choices= data_var,
                             selected = data_var[1])
)

plotlyPanel <- tabPanel("Stamina of Pokemon",
                        fluidPage(
                            headerRow,
                            plotly::plotlyOutput("plotlystam") 
                        ))
                        

manyPlots <- tabPanel("Variable of Pokemon",
                      fluidPage(
                          headerRow,
                          headerRow2,
                        plotly::plotlyOutput("plotlymany")
))


dataPanel <- tabPanel("Data",
                      fluidPage(
                          button,
                          headerRow,
                      tableOutput("dataTable")
))

ui <- navbarPage(
   "Pokemon App",
    dataPanel,
    plotlyPanel,
   manyPlots,
    id = "navBar"
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
        data %>% filter(name %in% input$selpok)
    })
    

    #Output del plot de stamina
    output$plotlystam <- plotly::renderPlotly({
    data_filtered2() %>%
         ggplot(aes(x=name, y=data$stamina, fill=name)) +
            geom_bar(stat="identity", position=position_dodge())
    })
    
    dataplot <- melt(data=data, id.vars= "name", measure.vars=c("stamina", "defense", "attack"))
    
    #Output of many plots
    output$plotlymany <- plotly::renderPlotly({
        ggplot(dataplot,aes(x=variable, y= value, color=name, group= name))+ 
            geom_line()
    })

   #output table de los datos que estan llamando
   output$dataTable <- renderTable({
    data_filtered()
   })
    
   output$report <- downloadHandler(
       filename = "report.pdf",
       content = function(file) {
           tempReport <- file.path(tempdir(), "report.Rmd")
           file.copy("report.Rmd", tempReport, overwrite = TRUE)
           
           params <- list(
               selYear = isolate(input$selpok),
               defense = isolate(input$defense),
               stamina = isolate(input$stamina)
           )
           rmarkdown::render(tempReport, output_file = file,
                             params = params,
                             envir = new.env(parent = globalenv())
            )
        })

} 

# Run the application 
shinyApp(ui = ui, server = server)
