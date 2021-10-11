library(shiny)

# Define UI for application that draws a histogram
ui <- fluidPage( 
  
  # Application title
  titlePanel("Distribuciones de probabilidad"),
  
  
  # Sidebar with a slider input for number of bins 
  sidebarLayout(
    
    sidebarPanel(
      p("Seleccione la distribución de interés y a continuación sus parámetros"),
      
      selectInput(inputId= "Distribucion", 
                  label= "Seleccione la distribución",
                  choices = c("Binomial","Hipergeometrica", "Poisson"),
                  selected = "Binomial"),
      
      conditionalPanel(condition="input.Distribucion=='Poisson'",
                       sliderInput(inputId = "pl",#le sugiero nombrar esas entradas con nombres mas especificos
                                   label =HTML("&lambda;:"),
                                   min = 0,max = 40,
                                   value = 5,
                                   step= .1,
                                   animate = F), 
                       
                       numericInput(inputId= "pN", #por defecto le coloque la inicial de la distribucion
                                    label = "Numero de ocurrencias del evento x",
                                    min = 0,
                                    max = 500,
                                    value = 0,
                                    step= 1)),
      
      conditionalPanel(condition="input.Distribucion=='Binomial'",
                      numericInput(inputId = "bN",
                                    label = "Numero de ensayos",
                                    min = 0,
                                    max = 500,
                                    value = 10,
                                    step= 1),
                      numericInput(inputId = "bn",
                                   label = "X",
                                   min=0,
                                   max=500,
                                   value=5,
                                   step=1),
                
                      sliderInput(inputId = "bP",
                                   label = "Probabilidad de exito",
                                   min = 0,
                                   max = 1,
                                   value = 0.5,
                                   step= 0.01,
                                   animate = F)),
      
      conditionalPanel(condition="input.Distribucion=='Hipergeometrica'",
                       numericInput(inputId = "hm",
                                    label = "Tamaño población 1",
                                    min = 0,
                                    max = 500,
                                    value = 5,
                                    step= 1),
                       numericInput(inputId = "hk",
                                    label = "Tamaño de la muestra extraida",
                                    min = 0,
                                    max = 500,
                                    value = 5,
                                    step= 1),
                       numericInput(inputId = "hnh",
                                    label = "Tamaño población 2 ",
                                    min = 0,
                                    max = 500,
                                    value = 5,
                                    step= 1),
                       numericInput(inputId = "hxh",
                                    label = "Número de elementos en la muestra que pertenecen a dicha categoría",
                                    min = 0,
                                    max = 500,
                                    value = 5,
                                    step= 1)),
      tags$blockquote("Distribuciones de probabilidad discretas")
      
      
      
      
    ),
    
    # Show a plot of the generated distribution
    mainPanel( 
      h3("Probabilidad", align="center"),
      textOutput("Probabilidad"),
      h3("Diagrama de probabilidad", align = "left"),
      plotOutput("Grafico")
    
      
    )
  )
)

