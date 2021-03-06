library(shiny)
library(tidyverse)
library(shinyjs)
library(plotly)
library(shinythemes)
library(httr)
library(dplyr)
library(shinyjs)
library(reshape2)
library(shinydashboard)

#############################################################################
#Get data from the web

url<- "https://pokemon-go1.p.rapidapi.com/pokemon_stats.json"
key = "11a0831306msh85289b71ee29d28p1b0d2ajsnd56ec6de06bd"
pokemon = GET(url, config=add_headers("x-rapidapi-host"= "pokemon-go1.p.rapidapi.com",'x-rapidapi-key' = key))
#load("pokemon_API.RData")
#content(pokemon)


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

# load("data.RData")
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
names(a)<-c("name","stamina", "defense", "attack")
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



#Panels________________________________________________________

dataPanel <- tabPanel("Data",
                      fluidPage(
                          headerRow,
                          p("The automatic selection of Pokemon are random, but please feel free to choose your own! You 
                            can see here the comparison for stamina, defense and attack levels of each Pokemon selected 
                            in the following table and line-graph. The graph is dynamic, so feel free to put your mouse 
                            on top of the variable you would like to know the value of, from who it comes from and the variable
                            it is showing."),
                      tableOutput("dataTable"),
                      tableOutput("datamax"),
                      plotly::plotlyOutput("plotlymany"))
)

pokePanel<- tabPanel("Pokemon",
                     fluidPage(
                     htmlOutput("picture"),
                     p("For this app, the variable set shown is a Pokemon dataset, which comprises of five variables: 
                         the id of the Pokemon, its name, and the levels of stamina, defense and attack for each Pokemon. 
                         There are many Pokemons to choose from, 676 Pokemons to be exact. This page is very easy to compare
                       Pokemons of your choice and see how they stand in comparison to other Pokemon. In the first Panel named  
                       'Data' (on the top part of the app), you can click on it and will find a comparison of any group of 
                       Pokemon of your choice. There you can see its levels of stamina, defense and attack. Likewise, on the
                       panel named 'Characteristics', you can find the histogram for each variable. Additionally, in the panel
                       named 'Images', you can choose from some Pokemon images"),
                     p("Find below the button to generate a pdf file. Most importantly, enjoy the app!"),
                     button,
                     htmlOutput("picture2")))

varPanel <- tabPanel("Characteristics",
                     fluidPage(
                         p("Here you can see the characteristics of the Pokemon. For example, in the stamina, you can see that 
                           the average stamina value of the Pokemon is approximately 153. Please choose from the three variabes:
                           Stamina, Defense and Attack. This is a dynamic plotly, so if you put your mouse on top of the data, 
                           you can see the value."),
                         headervar,
                          plotly::plotlyOutput("plotvar"))
)

imagePanel<- tabPanel("Images",
                       fluidRow(
                           p("In this panel, you can choose from 7 Pokemon to show you what it looks like! 
                             Choose whichever you like. The automatic selection is the most famous Pokemon, Pikachu!"),
                           column(3, selectInput("pokemon", label = h4("Pokemon images"), 
                                                 choices = list("Pikachu" = "Pikachu", 
                                                                "Charmander" = "Charmander", 
                                                                "Snorlax" = "Snorlax",
                                                                "Bulbasaur"= "Bulbasaur",
                                                                "Jigglypuff" = "Jigglypuff",
                                                                "Meowth"= "Meowth",
                                                                "Squirtle"= "Squirtle"),
                                                 selected = "Pikachu"),
                                  htmlOutput("img1")), # here is the image
                           column(9, plotOutput("plot2"))
                       ))

#Fit together_________________________________________________
ui <- navbarPage(
   "Pokemon App",
   pokePanel,
    dataPanel,
   varPanel,
   imagePanel,
   theme= shinytheme("readable"),
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
        a %>% filter(name %in% input$selpok)
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
           geom_line()+ labs(color= "Pokemon selected")
   })

   
   #Output of the different variable plots
   output$plotvar <- plotly::renderPlotly({
       varsel= as.numeric(input$selvar)
       ggplot(c, aes(c[,varsel])) +
        geom_histogram(fill="lightblue", col="darkgoldenrod1")+ labs(title= paste("Histogram of ", names(data_var[varsel]))) +
                                                           xlab( names(data_var[varsel]))
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
   output$picture2 <-
       renderText({
           c(
               '<img src="',
               "https://www.tec.com.pe/wp-content/uploads/2018/10/pokemon0_0.jpg",
               '">'
           )
       })


   #IMAGES of pokemon selected
   output$img1 <- renderText({
       if(input$pokemon == "Pikachu"){            
           c(
               '<img src="',
               "https://upload.wikimedia.org/wikipedia/en/thumb/a/a6/Pok%C3%A9mon_Pikachu_art.png/220px-Pok%C3%A9mon_Pikachu_art.png",
               '">'
           )
       }                                        
       else if(input$pokemon == "Charmander"){
           c(
               '<img src="',
               "https://assets.pokemon.com/assets/cms2/img/pokedex/full/004.png",
               '">'
           )
       }
       else if(input$pokemon == "Snorlax"){
           c(
               '<img src="',
               "https://assets.pokemon.com/assets/cms2/img/pokedex/full/143.png",
               '">'
           )
       }
       else if(input$pokemon == "Bulbasaur"){
           c(
               '<img src="',
               "https://upload.wikimedia.org/wikipedia/en/2/28/Pok%C3%A9mon_Bulbasaur_art.png",
               '">'
           )
       }
       else if(input$pokemon == "Jigglypuff"){
           c(
               '<img src="',
               "https://i.pinimg.com/originals/f3/14/17/f314179b48b6184132860d57759ffbac.png",
               '">'
           )
       }
       else if(input$pokemon == "Meowth"){
           c(
               '<img src="',
               "https://assets.pokemon.com/assets/cms2/img/pokedex/full/052.png",
               '">'
           )
       }
       else if(input$pokemon == "Squirtle"){
           c(
               '<img src="',
               "https://assets.pokemon.com/assets/cms2/img/pokedex/full/007.png",
               '">'
           )
       }
   })
   
   
#PDF FILE!!!    
   output$report <- downloadHandler(
       filename = "report.pdf",
       content = function(file) {
           tempReport <- file.path(tempdir(), "report.Rmd")
           file.copy("report.Rmd", tempReport, overwrite = TRUE)
           
           params <- list(
               pokemon = isolate(input$selpok),
               pokemon2 = isolate(input$pokemon),
               variable = isolate(input$selvar)
           )
           rmarkdown::render(tempReport, output_file = file,
                             params = params,
                             envir = new.env(parent = globalenv())
            )
        })

} 

# Run the application 
shinyApp(ui = ui, server = shinyserver)

