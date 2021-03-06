shinyUI(fluidPage(
  
    # Logo de la app en la web
    tags$head(
      tags$link(rel = "icon", type = "image/png", sizes = "32x32", href = "logo.png"),
    ),
    
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
        ),
        # Titulo de la app en la web
        titlePanel("", windowTitle = "DistDiscretas" ),
    ),
    
    
    
    
    # Cuestionario ------------------------------------------------------------
    
    
    div(id = "distribucion",
        
        selectInput(inputId="Distribucion",
                    label="Elija la distribución:",
                    choices=c("Binomial", "Poisson", "Hipergeometrica", "Binomial Negativa"),
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
                 
                 conditionalPanel(condition="input.Distribucion=='Binomial Negativa'",
                                  
                                  column( width = 3,
                                          
                                          br(),
                                          
                                          numericInput(inputId="r_nbin",
                                                       label="Ingrese el número de éxitos",
                                                       min = 1,
                                                       value="4",
                                                       step=1),
                                  ),
                                  
                                  column( width = 3,
                                          numericInput(inputId="p_nbin",
                                                       label="Ingrese la probabilidad de éxito en cada ensayo",
                                                       min=0,
                                                       max=1,
                                                       value="0.5",
                                                       step=0.01) ),
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
                                                      min = 0,
                                                      value=5, step=1)
                                  ),
                 ),
                 
                 
                 conditionalPanel(condition="input.Propede=='Cuantil'",
                                  
                                  column(width = 2,
                                         sliderInput(inputId="Probabilidad",
                                                      label="Ingrese probabilidad",
                                                      value=0.70, step=0.01,
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
                                 plotOutput(outputId="grafico"),
                        ),
                        
                        tabPanel("Teoria", icon = icon("book"),
                                 
                                 div(id = "header"),
                                 includeMarkdown("www/Teoria.md"),
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
            
        )
    ),
    div(id = "footer",
        column( width = 1,
                br(),
                img(src="logo_escuela.jpeg", height = 80, width = 80),
                #img(src="Nacional.png", height = 56, width = 140)
        ),
        column( width = 1,
                br(),
                #img(src="logo_escuela.jpeg", height = 80, width = 80),
                img(src="Nacional.png", height = 66, width = 150)
        ),
        # column( width = 12,
        # 
        # ),
        div(id = "autores", 
        p(tags$u(strong(em("Autores:")))),
        column( width = 3,
                br(),
                p('Miguel Angel Londoño Ciceros'),
                p('Carlos Mario Lopera Gomez')
        ),
        column( width = 3,
                br(),
                p('Jennifer Salazar Galvis'),
                p('Mario Cesar Jaramillo Elorza'),
        )
        )
    )
))