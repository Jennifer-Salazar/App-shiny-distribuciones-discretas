library(shiny)
library(ggplot2)
library(ggthemes)


shinyServer(function(input, output, session){
    
    #--------------------------------------------------------------
    
    output$miplot <- renderPlot({
        
        #----------------------------- Binomial  ---------------------------------
        
        if(input$Distribucion == "Binomial"){
            n <- input$n_binomial
            p <- input$p
            
            # Eje x
            x <- 0:n
            
            # lista vacia para las etiquetas
            nombres <- rep("", length(x))
            
            # Latex teoria
            mostrar_dist <- paste("$$X \\sim bin(", n, ",", p, ")$$")
            mostrar_media <- paste("$$E[X]=", round(n * p, 4), "$$")
            mostrar_var <- paste("$$V(X)=", round(n*p*(1-p), 4), "$$")
            mostrar_fmp <- paste("$$ p(x) = {", n,  "\\choose x}\\;", p, "^{x} \\; \\left(1- ", p, "\\right)^{", n, "-x}\\quad ;\\quad x = 0\\,,\\,1\\,,\\,2\\,,\\,\\cdots \\,,\\,", n, " \\;.$$")
            
            if(input$Propede == "Cuantil"){
                
                probabilidad <- input$Probabilidad
                
                # Calculo del cuantil
                cuantil <- qbinom(probabilidad, n, p)
                
                # Calculo de la probabilidad
                prob <- pbinom(x, size=n, prob=p)
                
                # Valores para mostrar en la gráfica
                nombres[cuantil:(cuantil + 1)] <- round(prob[cuantil:(cuantil + 1)], 3)
                
                # Parámetros gráficos
                ylabel <- expression(P(X<=x)) 
                titulo <- bquote(q[.(probabilidad)]==.(cuantil))
                res_prop <- paste("$$q_{", probabilidad, "} =", cuantil, "$$")
                tipo <- "Función de distribución acumulada"
                
                
                output$prob_cuantiles <-  renderUI({
                    
                    list(
                        
                        splitLayout(
                            wellPanel(
                                withMathJax(paste("$$P(X \\leq", cuantil-1, ") =" , round( prob[cuantil], 4), "$$")),  
                            ),
                            
                            wellPanel(
                                withMathJax(paste("$$P(X \\leq", cuantil, ") =" , round( prob[cuantil+1], 4), "$$"))
                            )
                        )

                    )
                    
                }) 
                
                
            }
            
            else {
                
                cuantil <- input$Cuantil
                
                output$prob_cuantiles <-  renderUI({
                    
                    
                    
                }) 
                
                
                if(input$Acumulado == "acumulada"){
                    
                    # Calculo de la probabilidad
                    prob <- pbinom(x, size=n, prob=p)
                    
                    # Valores para mostrar en la gráfica
                    nombres[0:cuantil + 1] <- round(prob[0:cuantil + 1], 3)
                    
                    # Parámetros gráficos
                    ylabel <- expression(P(X<=x)) 
                    titulo <- bquote(P(X <= .(cuantil))==.(prob[cuantil + 1]))
                    res_prop <- paste("$$P(X \\leq", cuantil, ") =", prob[cuantil + 1], "$$")
                    tipo <- "Función de distribución acumulada"
                    
                }else if(input$Acumulado == "supervivencia"){
                    
                    # Calculo de las probabilidades
                    prob <- pbinom(-1:(n-1), size=n, prob=p, lower.tail = F)
                    
                    # Valores para mostrar en la gráfica
                    nombres[(cuantil + 2):(n+1)] <- round(prob[(cuantil + 2):(n+1)], 3)
                    
                    # Parámetros gráficos
                    ylabel <- expression(P(X>x))
                    titulo <- bquote(P(X > .(cuantil))==.(prob[cuantil + 2]))
                    res_prop <- paste("$$P(X >", cuantil, ") =", prob[cuantil + 2], "$$")
                    tipo <- "Función de supervivencia"
                    
                    
                }else{
                    
                    # Calculo de las probabilidades
                    prob <- dbinom(x, size=n, prob=p)
                    
                    # Valores para mostrar en la gráfica
                    nombres[cuantil + 1] <- round(prob[cuantil + 1], 3) 
                    
                    # Parámetros gráficos
                    ylabel <- expression(P(X==x)) 
                    titulo <- bquote(P(X == .(cuantil))==.(prob[cuantil + 1]))
                    res_prop <- paste("$$P(X =", cuantil, ") =", prob[cuantil + 1], "$$")
                    tipo <- "Función de masa de probabilidad"
                }
                
                
            }}
        
        # #----------------------------- Poisson ---------------------------------

        if(input$Distribucion == "Poisson"){
            lambda <- input$lambda
            n <- qpois(0.99999, lambda)
            
            # Eje x
            x <- 0:n
            
            # lista vacia para las etiquetas
            nombres <- rep("", length(x))

            if(input$Propede == "Cuantil"){
                
                probabilidad <- input$Probabilidad
                
                # Calculo del cuantil
                cuantil <- qpois(probabilidad, lambda)
                
                # Calculo de la probabilidad
                prob <- ppois(x, lambda=lambda)
                
                # Valores para mostrar en la gráfica
                nombres[cuantil:(cuantil + 1)] <- round(prob[cuantil:(cuantil + 1)], 3)
                
                # Parámetros gráficos
                ylabel <- expression(P(X<=x)) 
                titulo <- bquote(q[.(probabilidad)]==.(cuantil))
                res_prop <- paste("$$q_{", probabilidad, "} =", cuantil, "$$")
                tipo <- "Función de distribución acumulada"
                
                
                output$prob_cuantiles <-  renderUI({
                    
                    list(
                        
                        splitLayout(
                            wellPanel(
                                withMathJax(paste("$$P(X \\leq", cuantil-1, ") =" , round( prob[cuantil], 4), "$$")),  
                            ),
                            
                            wellPanel(
                                withMathJax(paste("$$P(X \\leq", cuantil, ") =" , round( prob[cuantil+1], 4), "$$"))
                            )
                        )
                        
                    )
                    
                })
               
            }

            else {
                
                cuantil <- input$Cuantil
                
                output$prob_cuantiles <-  renderUI({
                    
                    
                    
                }) 
                
                
                if(input$Acumulado == "acumulada"){
                    
                    # Calculo de la probabilidad
                    prob <- ppois(x, lambda=lambda)
                    
                    # Valores para mostrar en la gráfica
                    nombres[0:cuantil + 1] <- round(prob[0:cuantil + 1], 3)
                    
                    # Parámetros gráficos
                    ylabel <- expression(P(X<=x)) 
                    titulo <- bquote(P(X <= .(cuantil))==.(prob[cuantil + 1]))
                    res_prop <- paste("$$P(X \\leq", cuantil, ") =", prob[cuantil + 1], "$$")
                    tipo <- "Función de distribución acumulada"
                    
                }else if(input$Acumulado == "supervivencia"){
                    
                    
                    # Calculo de las probabilidades
                    prob <- ppois(-1:(n-1), lambda=lambda, lower.tail = F)
                    
                    # Valores para mostrar en la gráfica
                    nombres[(cuantil + 2):(n+1)] <- round(prob[(cuantil + 2):(n+1)], 3)
                    
                    # Parámetros gráficos
                    ylabel <- expression(P(X>x))
                    titulo <- bquote(P(X > .(cuantil))==.(prob[cuantil + 2]))
                    res_prop <- paste("$$P(X >", cuantil, ") =", prob[cuantil + 2], "$$")
                    tipo <- "Función de supervivencia"
                    
                    
                }else{
                    
                    # Calculo de las probabilidades
                    prob <- dpois(x, lambda=lambda)
                    
                    # Valores para mostrar en la gráfica
                    nombres[cuantil + 1] <- round(prob[cuantil + 1], 3) 
                    
                    # Parámetros gráficos
                    ylabel <- expression(P(X==x)) 
                    titulo <- bquote(P(X == .(cuantil))==.(prob[cuantil + 1])) 
                    res_prop <- paste("$$P(X =", cuantil, ") =", prob[cuantil + 1], "$$")
                    tipo <- "Función de masa de probabilidad"
                }
                
                

            }

        }

        # #------------------------- Hipergeometrica  ---------------------------------

        if(input$Distribucion == "Hipergeometrica"){
            k <- as.integer(input$k)
            N <- as.integer(input$N)
            n <- as.integer(input$n_hipergeometrica)
            
            # Eje x
            x <- 0:min(n, k)
            
            # lista vacia para las etiquetas
            nombres <- rep("", length(x))
            
            if(input$Propede == "Cuantil"){
                
                probabilidad <- input$Probabilidad
                
                
                # Calculo del cuantil
                cuantil <- qhyper(probabilidad, m = k, n = N-k, k = n)
                
                # Calculo de la probabilidad
                prob <- phyper(q = 0:min(n, k), m = k, n = N-k, k = n)
                
                # Valores para mostrar en la gráfica
                nombres[cuantil:(cuantil + 1)] <- round(prob[cuantil:(cuantil + 1)], 3)
                
                # Parámetros gráficos
                ylabel <- expression(P(X<=x)) 
                titulo <- bquote(q[.(probabilidad)]==.(cuantil))
                res_prop <- paste("$$q_{", probabilidad, "} =", cuantil, "$$")
                tipo <- "Función de distribución acumulada"
                
                
                output$prob_cuantiles <-  renderUI({
                    
                    list(
                        
                        splitLayout(
                            wellPanel(
                                withMathJax(paste("$$P(X \\leq", cuantil-1, ")=" , round( prob[cuantil], 4), "$$")),  
                            ),
                            
                            wellPanel(
                                withMathJax(paste("$$P(X \\leq", cuantil, ")=" , round( prob[cuantil+1], 4), "$$"))
                            )
                        )
                        
                    )
                    
                })
                


            }else{
                
                cuantil <- input$Cuantil
                
                output$prob_cuantiles <-  renderUI({
                    
                    
                    
                }) 
                
                
                if(input$Acumulado == "acumulada"){
                    
                    # Calculo de las probabilidades
                    prob <- phyper(q = 0:min(n, k), m = k, n = N-k, k = n)
                    
                    # Valores para mostrar en la gráfica
                    nombres[0:cuantil + 1] <- round(prob[0:cuantil + 1], 3)
                    
                    # Parámetros gráficos
                    ylabel <- expression(P(X<=x)) 
                    titulo <- bquote(P(X <= .(cuantil))==.(prob[cuantil + 1]))
                    res_prop <- paste("$$P(X \\leq", cuantil, ") =", prob[cuantil + 1], "$$")
                    tipo <- "Función de distribución acumulada"
                    
                }else if(input$Acumulado == "supervivencia"){
                    
                    # Calculo de las probabilidades
                    prob <- phyper(q = -1:(min(n, k)-1), m = k, n = N-k, k = n, lower.tail = F)
                    
                    # Valores para mostrar en la gráfica
                    nombres[(cuantil + 2):(n+1)] <- round(prob[(cuantil + 2):(n+1)], 3)
                    
                    # Parámetros gráficos
                    ylabel <- expression(P(X>x))
                    titulo <- bquote(P(X > .(cuantil))==.(prob[cuantil + 2]))
                    res_prop <- paste("$$P(X >", cuantil, ") =", prob[cuantil + 2], "$$")
                    tipo <- "Función de supervivencia"
                    
                    
                }else{
                    
                    # Calculo de las probabilidades
                    prob <- dhyper(x = 0:min(n, k), m = k, n = N-k, k = n)
                    
                    # Valores para mostrar en la gráfica
                    nombres[cuantil + 1] <- round(prob[cuantil + 1], 3) 
                    
                    # Parámetros gráficos
                    ylabel <- expression(P(X==x)) 
                    titulo <- bquote(P(X == .(cuantil))==.(prob[cuantil + 1])) 
                    res_prop <- paste("$$P(X =", cuantil, ") =", prob[cuantil + 1], "$$")
                    tipo <- "Función de masa de probabilidad"
                }

            }}
        
        
        # Imprimir la probabilidad calculada --------------------------------------
        
        output$res_probabilidad <- renderUI({

            h4(withMathJax(res_prop))
        })
        
        # Imprimir la teoría evaluada ---------------------------------------------
        
        output$res_teoria <- renderUI({
            
            list(
                withMathJax(mostrar_dist),
                withMathJax(mostrar_media),
                withMathJax(mostrar_var),
                withMathJax(mostrar_fmp)
            )
            
        })
        

        # Gráficar la distribución ------------------------------------------------
        
        df <- data.frame(
            x = as.factor(x),
            y = prob,
            name = nombres
        )

        ggplot(df, aes(x=x, y=y, fill = ifelse(name == "", 1, 0) )) +
            geom_bar(stat = "identity", color="black", show.legend = FALSE) +
            geom_text(aes(label=name), vjust=-0.3, size=3.5) + #, fill = "white") +
            labs(
                title = titulo,
                subtitle = tipo,
                caption = "",
                x = "",
                y = ylabel
            ) +
            theme_fivethirtyeight()
        
    
    })
})
