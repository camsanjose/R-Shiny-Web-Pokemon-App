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
d<- as.numeric(as.character(data2[,1]))
dd<- as.numeric(as.character(data2[,2]))
ddd<- as.numeric(as.character(data2[,3]))
data2<- cbind(d,dd,ddd)
c=as.data.frame(data2)
a=cbind(data[,2],c)
namel<-function (x){
    nm<-as.list(x)
    names(nm)<-as.character(unlist(x))
    nm
}

##########################################################################3

##headers___________________________________________
headerRow <- div(id="header", useShinyjs(),
                 selectInput(input= "selpok", 
                             label="Select the Pokemon", 
                             multiple = TRUE,
                             choices= data_name, 
                             selected=tail(data_name))
)


button <- div(id="button", useShinyjs(),
    downloadButton("report", "Generate report")
)

headervar <- div(id="varheader", useShinyjs(),
                 selectInput(input= "selvar",
                             label="Select the variable",
                             multiple = F,
                             choices= data_var,
                             selected = data_var[1])
)

headervar2 <- div(id="secondheader", useShinyjs(),
                 selectInput(input= "var2",
                             label="Select the variable",
                             multiple = F,
                             choices= data_var,
                             selected = data_var[1])
)

#Panels________________________________________________________

dataPanel <- tabPanel("Data",
                      fluidPage(
                          headerRow,
                      tableOutput("dataTable"),
                      tableOutput("datamax"),
                      plotly::plotlyOutput("plotlymany"))
)

pokePanel<- tabPanel("Pokemon",
                     fluidPage(
                     htmlOutput("picture"),
                     button,
                     p("For this app, the variable set shown is a Pokemon dataset, which comprises of five variables: 
                         the id of the Pokemon, its name, and the levels of stamina, defense and attack for each Pokemon. 
                         here are many Pokemons to choose from, 676 Pokemons to be exact. This page is very easy to compare
                       Pokemons of your choice and see how they stand in comparison to other Pokemon. For example, in the 
                       Data Panel on the top part of the app, you can click on it and will find a comparison of any group of 
                       Pokemon of your choice. "),
                     htmlOutput("picture2")))

varPanel <- tabPanel("Characteristics",
                     fluidPage(
                         headervar,
                          plotly::plotlyOutput("plotvar"))
)

switchPanel<- tabPanel("Graphs",
                       fluidPage(
                           headervar2,
                           plotly::plotlyOutput("diffplot"))
)

#Fit together_________________________________________________
ui <- navbarPage(
   "Pokemon App",
   pokePanel,
    dataPanel,
   varPanel,
   switchPanel,
    id = "navBar"
)

# Define server logic required to draw a histogram_________________________
shinyserver <- function(input, output, session) {

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

   
   #Output of the different variable plots
   output$plotvar <- plotly::renderPlotly({
       varsel= as.numeric(input$selvar)
       ggplot(c, aes(c[,varsel])) +
        geom_histogram(fill="lightblue", col="darkgoldenrod1")+ labs(title= paste("Histogram of ", names(data_var[varsel]))) +
                                                           xlab( names(data_var[varsel]))
   })
   
   # #output of different graphs 
   # output$diffplot <- renderUI({
   #     obj<- switch(input$var2, 
   #                  "stamina" = stamina, 
   #                  "defense" = defense, 
   #                  "attack" = attack)
   #     var.opts <- namel(colnames(obj))
   #     selectInput("variable","Variable:", var.opts)
   # })
   # 
  

   #IMAGE
   output$picture <-
       renderText({
           c(
               '<img src="',
               "https://upload.wikimedia.org/wikipedia/commons/9/98/International_Pok%C3%A9mon_logo.svg",
               '">'
           )
       })
   output$picture2 <-
       renderText({
           c(
               '<img src="',
               "https://www.tec.com.pe/wp-content/uploads/2018/10/pokemon0_0.jpg",
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
               stamina = isolate(input$stamina),
               attack = isolate(input$attack)
           )
           rmarkdown::render(tempReport, output_file = file,
                             params = params,
                             envir = new.env(parent = globalenv())
            )
        })

} 

# Run the application 
shinyApp(ui = ui, server = shinyserver)

# plotlyPanel <- tabPanel("Variable Selection of Pokemon",
#                         fluidPage(
#                             headerRow2,
#                             headervar,
#                             plotly::plotlyOutput("plotlyvar") 
#                         ))

# headerRow2 <- div(id="header2", useShinyjs(),
#                   selectInput(input= "selname",
#                               label="Select the Pokemon",
#                               multiple = TRUE,
#                               choices= data_name,
#                               selected=head(data_name))
# )



#  data_filtered3 <- reactive({
#      req(input$selname)
#      data %>% filter(name %in% input$selname)
#  })
# 

