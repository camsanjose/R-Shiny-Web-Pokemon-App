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
data_var <- c("stamina"=1, "defense"=2, "attack"=3)
dataplot <- melt(data=data, id.vars= "name", measure.vars=c("stamina", "defense", "attack"))
data2<- data[,3:5]
##########################################################################3

##headers___________________________________________
headerRow <- div(id="header", useShinyjs(),
                 selectInput(input= "selpok", 
                             label="Select the Pokemon", 
                             multiple = TRUE,
                             choices= data_name, 
                             selected=tail(data_name))
)

headerRow2 <- div(id="header2", useShinyjs(),
                 selectInput(input= "selname", 
                             label="Select the Pokemon", 
                             multiple = TRUE,
                             choices= data_name)
)


button <- div(id="button", useShinyjs(),
    downloadButton("report", "Generate report")
)

headervar <- div(id="secondheader", useShinyjs(),
                 selectInput(input= "selvar", 
                             label="Select the variable", 
                             multiple = F,
                             choices= data_var,
                             selected = 1)
)
#Panels________________________________________________________
plotlyPanel <- tabPanel("Variable Selection of Pokemon",
                        fluidPage(
                            headerRow2,
                            headervar,
                            plotly::plotlyOutput("plotlyvar") 
                        ))
                        


dataPanel <- tabPanel("Data",
                      fluidPage(
                          headerRow,
                      tableOutput("dataTable"),
                      plotly::plotlyOutput("plotlymany"))
)

pokePanel<- tabPanel("Pokemon",
                     fluidPage(
                     htmlOutput("picture"),
                     p("For this app, the variable set shown is a Pokemon dataset, which comprises of five variables: 
                         the id of the Pokemon, its name, and the levels of stamina, defense and attack for each Pokemon. 
                         In the following table and graph, you can choose any Pokemon you want"),
                         button))

#Fit together_________________________________________________
ui <- navbarPage(
   "Pokemon App",
   pokePanel,
    dataPanel,
    plotlyPanel,
    id = "navBar"
)

# Define server logic required to draw a histogram_________________________
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
    
     data_filtered2 <- reactive({
        req(input$selpok)
        dataplot %>% filter(name %in% input$selpok)
    })
     
     data_filtered3 <- reactive({
         req(input$selname)
         req(input$selname)
         data2 %>% filter(name %in% input$selname, var %in% input$selvar)
     })

    #Output of the different variable plots 
    output$plotlyvar <- plotly::renderPlotly({
        varsel= as.numeric(input$selvar)
         ggplot(data_filtered3,aes(x=name, y=varsel, fill=name)) +
            geom_bar(stat="identity", position=position_dodge())
    })

   #output table de los datos que estan llamando
   output$dataTable <- renderTable({
    data_filtered()
   })
   
   #Output of many plots
   output$plotlymany <- plotly::renderPlotly({
       data_filtered2() %>%
           ggplot(aes(x=variable, y= value, color=name, group= name))+ 
           geom_line()
   })
   #IMAGE
   output$picture <-
       renderText({
           c(
               '<img src="',
               "https://upload.wikimedia.org/wikipedia/commons/9/98/International_Pok%C3%A9mon_logo.svg",
               '">'
           )
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
