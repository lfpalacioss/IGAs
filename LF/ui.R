#
# This is the user-interface definition of a Shiny web application. You can
# run the application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)

SUBSECTORES=c("CMIN","CPES","CHID","CRES","CAGR","CIND","CELE")

# Define UI for application that draws a histogram
shinyUI(fluidPage( #Cambiar a dashboardPage: Me bota error cuando empleo ese comando, sale "Error in tagAssert: Expected an object with class 'shiny.tag'"

    # Application title
    titlePanel(""),

    # Sidebar with a slider input for number of bins
    sidebarLayout(
        sidebarPanel(

            selectInput("LISTA", "Seleccionar áreas:", 
                        choices=SUBSECTORES,
                        multiple = TRUE, selected = SUBSECTORES),
            hr(),
            helpText("Seleccionar las áreas para mostrar en el gráfico"),
            hr(),
            
            dateRangeInput("RANGO","Seleccionar el periodo:",
                           start = "2020-06-08",
                           format = "dd/mm/yyyy",
                           end = Sys.Date(),
                           separator = "-")
        ),

        # Show a plot of the generated distribution
        mainPanel(
            plotOutput("GRAF_IGA_1",height = "700px"),
            plotOutput("GRAF_IGA_2",height = "700px"),
            sankeyNetworkOutput("GRAF_IGA_3",height = "700px"),
            plotOutput("GRAF_IGA_4",height = "700px"),
            plotOutput("GRAF_IGA_5",height = "700px"),
            plotOutput("GRAF_IGA_6",height = "700px")
            
            
        )
    )
))
