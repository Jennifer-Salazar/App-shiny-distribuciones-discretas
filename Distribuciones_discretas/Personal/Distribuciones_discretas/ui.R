shinyUI(fluidPage(
    titlePanel("Distribuciones discretas"),
    h4(p("Esta aplicación te ayudará a calcular probabilidades y 
       percentiles de distribuciones de probabilidad discretas."),
       align="left"),
    sidebarLayout(
        sidebarPanel(
            selectInput(inputId="Distribucion",
                        label="Elija la distribución:",
                        choices=c("Binomial", "Poisson", "Hipergeometrica"),
                        selected="Binomial"),
            
            
            conditionalPanel(condition="input.Distribucion=='Binomial'",
                             numericInput(inputId="n_binomial",
                                          label=HTML("Ingrese el tamaño de muestra"),
                                          min = 1,
                                          value="30",
                                          step=1),
                             
                             numericInput(inputId="p",
                                          label=HTML("Ingrese la probabilidad de éxito"),
                                          min=0,
                                          value="0.5",
                                          step=0.01) ),
            
            conditionalPanel(condition="input.Distribucion=='Poisson'",
                             numericInput(inputId="lambda",
                                          label="Ingrese la media",
                                          min=0,
                                          step=1,
                                          value="5")),
            
            
            conditionalPanel(condition="input.Distribucion=='Hipergeometrica'",
                             numericInput(inputId="k",
                                          label="Ingrese el número de elementos que tienen la 
                                          caracteristica de intéres (k)",
                                          value="10",
                                          step=1),
                             
                             numericInput(inputId="N",
                                          label="Ingrese el tamaño de la población (N)",
                                          value="25", 
                                          step=1),
                             
                             numericInput(inputId="n_hipergeometrica",
                                          label="Ingrese el tamaño de muestra (n)",
                                          value="5",
                                          step=1)),
            
            
            selectInput(inputId="Propede",
                        label="Opciones para calcular:",
                        choices=c("Probabilidad","Percentil"),
                        selected="Probabilidad"),
            
            conditionalPanel(condition="input.Propede=='Probabilidad'",
                             
                             selectInput(inputId = "Acumulado",
                                         label= "Opciones para calcular:",
                                         choices = c("acumulada", "puntual"),
                                         selected = "acumulada"),
                             
                             
                             numericInput(inputId="Percentil",
                                          label="Ingrese percentil",
                                          value=2, step=1)),
            
            
            conditionalPanel(condition="input.Propede=='Percentil'",
                             numericInput(inputId="Probabilidad",
                                          label="Ingrese probabilidad",
                                          value=0.70, step=0.001,
                                          min=0.001, max=0.999)),
            
            img(src="Nacional.png", height = 56, width = 140),
            br(),
            p("App creada por Jennifer Salazar Galvis y Miguel Angel Londoño Ciceros"),
            tags$a(href="https://srunal.github.io", "https://srunal.github.io")
            
        ),
        
        
        mainPanel(
            tabsetPanel(type = "tabs",
                        tabPanel("Gráfica", plotOutput(outputId="miplot"))
                        
                        
            )
        )
    )))