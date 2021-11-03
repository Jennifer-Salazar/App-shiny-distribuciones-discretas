shinyUI(fluidPage(
    
    # Se concecta con el css
    tags$link(href = "estilo.css", rel = "stylesheet"),
    
    
    # Titulo ------------------------------------------------------------------
    div(id = "header",
        div(id = "title",
            "Distribuciones discretas"
        ),
        
        div(id = "subtitle",
            
            "Esta aplicación te ayudará a calcular probabilidades y 
            cuantiles de distribuciones de probabilidad discretas."
        )
    ),
    
    
    
    
    # Cuestionario ------------------------------------------------------------
    
    
    div(id = "distribucion",
        
        selectInput(inputId="Distribucion",
                    label="Elija la distribución:",
                    choices=c("Binomial", "Poisson", "Hipergeometrica"),
                    selected="Binomial"),
    ),
    
    
    fluidRow(class = "panel",
             
             hr(),
             
             div(id = "opciones",
                 
                 conditionalPanel(condition="input.Distribucion=='Binomial'",
                                  
                                  column( width = 3, 
                                          numericInput(inputId="n_binomial",
                                                       label=HTML("Ingrese el tamaño de muestra (n)"),
                                                       min = 1,
                                                       value="15",
                                                       step=1),
                                  ),
                                  
                                  column( width = 3,
                                          numericInput(inputId="p",
                                                       label=HTML("Ingrese la probabilidad de éxito (p)"),
                                                       min=0,
                                                       max=1,
                                                       value="0.5",
                                                       step=0.01) 
                                  ),
                 ),
                 
                 
                 conditionalPanel(condition="input.Distribucion=='Poisson'",
                                  
                                  column(width = 6,
                                         numericInput(inputId="lambda",
                                                      label="Ingrese la media (λ)",
                                                      min=0,
                                                      step=1,
                                                      value="5")),
                 ),
                 
                 
                 conditionalPanel(condition="input.Distribucion=='Hipergeometrica'",
                                  
                                  column( width = 2,
                                          numericInput(inputId="k",
                                                       label="Ingrese el número de elementos que tienen la 
                                          caracteristica de intéres (k)",
                                                       value="15",
                                                       step=1),
                                  ),
                                  
                                  br(),
                                  
                                  column( width = 2,
                                          numericInput(inputId="N",
                                                       label="Ingrese el tamaño de la población (N)",
                                                       value="25", 
                                                       step=1),
                                  ),
                                  
                                  column( width = 2,
                                          numericInput(inputId="n_hipergeometrica",
                                                       label="Ingrese el tamaño de muestra (n)",
                                                       value="10",
                                                       step=1)
                                  ),
                 ),
                 
                 
                 div(class = "linea_vertical"),
                 
                 
                 column(width = 2,
                        
                        selectInput(inputId="Propede",
                                    label="Opciones para calcular:",
                                    choices=c("Probabilidad","Cuantil"),
                                    selected="Probabilidad"),
                 ),
                 
                 conditionalPanel(condition="input.Propede=='Probabilidad'",
                                  
                                  column(width = 2,
                                         selectInput(inputId = "Acumulado",
                                                     label= "Opciones para calcular:",
                                                     choices = c("acumulada", "puntual", "supervivencia"),
                                                     selected = "acumulada"),
                                  ),
                                  
                                  column(width = 2,
                                         numericInput(inputId="Cuantil",
                                                      label="Ingrese cuantil",
                                                      value=5, step=1)
                                  ),
                 ),
                 
                 
                 conditionalPanel(condition="input.Propede=='Cuantil'",
                                  
                                  column(width = 2,
                                         numericInput(inputId="Probabilidad",
                                                      label="Ingrese probabilidad",
                                                      value=0.70, step=0.001,
                                                      min=0.001, max=0.999)
                                  ),
                 ),
                 
             )
             
    ),
    
    
    # Panel principal ---------------------------------------------------------
    
    sidebarLayout(
        
        
        # Gráfica -----------------------------------------------------------------
        
        mainPanel(
            tabsetPanel(type = "tabs", id = "panel_principal",
                        
                        tabPanel("Gráfica", icon = icon("chart-bar"),
                                 plotOutput(outputId="miplot"),
                        ),
                        
                        tabPanel("Teoria", icon = icon("book"),
                                 
                                 includeHTML("www/Teoria.html")
                                 
                        )
                        
                        
            )
        ),
        
        
        # Teoría ------------------------------------------------------------------
        
        sidebarPanel(
            
            wellPanel(
                
                uiOutput(outputId = "res_probabilidad")
                
            ),
            
            uiOutput(outputId = "prob_cuantiles"),
            
            wellPanel(
              
                uiOutput(outputId = "res_teoria")
            ),
            
            
            wellPanel(
                
                div(id = "distribucion",
                    img(src="Nacional.png", height = 56, width = 140)
                )

            ),
            
        )
        
    )
))