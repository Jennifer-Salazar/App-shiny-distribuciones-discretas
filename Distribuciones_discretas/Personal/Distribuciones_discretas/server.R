library(shiny)
library(markdown)
library(ggplot2)
library(ggthemes)
library(shinyvalidate)


shinyServer(function(input, output, session){
  
  iv_n_binomial <- InputValidator$new()
  iv_n_binomial$add_rule("n_binomial", sv_required(message = "Campo requerido"),)
  iv_n_binomial$add_rule("n_binomial", sv_numeric(message = "Ingrese un valor numérico"))
  iv_n_binomial$add_rule("n_binomial", sv_integer(message = "Ingrese un valor entero"))
  iv_n_binomial$add_rule("n_binomial", sv_between(1, 10000000, message = "Ingrese un entero positivo"))
  iv_n_binomial$enable()
  
  iv_lambda <- InputValidator$new()
  iv_lambda$add_rule("lambda", sv_required(message = "Campo requerido"))
  iv_lambda$add_rule("lambda", sv_numeric(message = "Ingrese un valor numérico"))
  iv_lambda$add_rule("lambda", sv_integer(message = "Ingrese un valor entero"))
  iv_lambda$add_rule("lambda", sv_between(1, 10000000, message = "Ingrese un entero positivo"))
  iv_lambda$enable()
  
  iv_k <- InputValidator$new()
  iv_k$add_rule("k", sv_required(message = "Campo requerido"))
  iv_k$add_rule("k", sv_numeric(message = "Ingrese un valor numérico"))
  iv_k$add_rule("k", sv_integer(message = "Ingrese un valor entero"))
  iv_k$add_rule("k", sv_between(1, 10000000, message = "Ingrese un entero positivo"))
  iv_k$enable()
  
  iv_N <- InputValidator$new()
  iv_N$add_rule("N", sv_required(message = "Campo requerido"))
  iv_N$add_rule("N", sv_numeric(message = "Ingrese un valor numérico"))
  iv_N$add_rule("N", sv_integer(message = "Ingrese un valor entero"))
  iv_N$add_rule("N", sv_between(1, 10000000, message = "Ingrese un entero positivo"))
  iv_N$enable()
  
  iv_n_hiper <- InputValidator$new()
  iv_n_hiper$add_rule("n_hipergeometrica", sv_required(message = "Campo requerido"))
  iv_n_hiper$add_rule("n_hipergeometrica", sv_numeric(message = "Ingrese un valor numérico"))
  iv_n_hiper$add_rule("n_hipergeometrica", sv_integer(message = "Ingrese un valor entero"))
  iv_n_hiper$add_rule("n_hipergeometrica", sv_between(1, 10000000, message = "Ingrese un entero positivo"))
  iv_n_hiper$enable()
  
  iv_cuantil <- InputValidator$new()
  iv_cuantil$add_rule("Cuantil", sv_required(message = "Campo requerido"))
  iv_cuantil$add_rule("Cuantil", sv_numeric(message = "Ingrese un valor numérico"))
  iv_cuantil$add_rule("Cuantil", sv_between(0, 100, message = "Ingrese un número entre 0 y 100"))
  iv_cuantil$enable()
  

    #--------------------------------------------------------------
    
    output$grafico <- renderPlot({
      
      req(iv_cuantil$is_valid())
        
        #----------------------------- Binomial  ---------------------------------
        
        if(input$Distribucion == "Binomial"){
            
            # Evitar reacción si los inputs son invalidos   
            req(iv_n_binomial$is_valid())
          
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
            mostrar_fmp <- paste("$$ p(x) = {", n,  "\\choose x}", p, "^{x}  \\left(1- ", p, "\\right)^{", n, "-x}$$")
            mostrar_rango <- paste("$$x = 0\\,,\\,1\\,,\\,2\\,,\\,\\cdots \\,,\\,", n, ".$$")
            
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
                    res_prop <- paste("$$P(X \\leq", cuantil, ") =", round(  prob[cuantil + 1], 7), "$$")
                    tipo <- "Función de distribución acumulada"
                    
                }else if(input$Acumulado == "supervivencia"){
                    
                    # Calculo de las probabilidades
                    prob <- pbinom(-1:(n-1), size=n, prob=p, lower.tail = F)
                    
                    # Valores para mostrar en la gráfica
                    nombres[(cuantil + 2):(n+1)] <- round(prob[(cuantil + 2):(n+1)], 3)
                    
                    # Parámetros gráficos
                    ylabel <- expression(P(X>x))
                    titulo <- bquote(P(X > .(cuantil))==.(prob[cuantil + 2]))
                    res_prop <- paste("$$P(X >", cuantil, ") =", round(  prob[cuantil + 2], 7 ), "$$")
                    tipo <- "Función de supervivencia"
                    
                    
                }else{
                    
                    # Calculo de las probabilidades
                    prob <- dbinom(x, size=n, prob=p)
                    
                    # Valores para mostrar en la gráfica
                    nombres[cuantil + 1] <- round(prob[cuantil + 1], 3) 
                    
                    # Parámetros gráficos
                    ylabel <- expression(P(X==x)) 
                    titulo <- bquote(P(X == .(cuantil))==.(prob[cuantil + 1]))
                    res_prop <- paste("$$P(X =", cuantil, ") =", round(  prob[cuantil + 1], 7 ), "$$")
                    tipo <- "Función de masa de probabilidad"
                }
                
                
            }}
        
        # #----------------------------- Poisson ---------------------------------

        if(input$Distribucion == "Poisson"){
            
            # Evitar reacción si los inputs son invalidos
            req(iv_lambda$is_valid())
            
            lambda <- input$lambda
            n <- qpois(0.99999, lambda)
            
            # Eje x
            x <- 0:n
            
            # lista vacia para las etiquetas
            nombres <- rep("", length(x))
            
            # Latex teoria
            mostrar_dist <- paste("$$X \\sim Poisson(", lambda, ")$$")
            mostrar_media <- paste("$$E[X]=", round(lambda, 4), "$$")
            mostrar_var <- paste("$$V(X)=", round(lambda, 4), "$$")
            mostrar_fmp <- paste("$$p(x)=\\frac{e^{-" , lambda, "}", lambda,"^{x}}{x!};$$")
            mostrar_rango <- paste("$$x=0,1,2, \\ldots$$")
            
            

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
                    res_prop <- paste("$$P(X \\leq", cuantil, ") =", round(  prob[cuantil + 1], 7 ), "$$")
                    tipo <- "Función de distribución acumulada"
                    
                }else if(input$Acumulado == "supervivencia"){
                    
                    
                    # Calculo de las probabilidades
                    prob <- ppois(-1:(n-1), lambda=lambda, lower.tail = F)
                    
                    # Valores para mostrar en la gráfica
                    nombres[(cuantil + 2):(n+1)] <- round(prob[(cuantil + 2):(n+1)], 3)
                    
                    # Parámetros gráficos
                    ylabel <- expression(P(X>x))
                    titulo <- bquote(P(X > .(cuantil))==.(prob[cuantil + 2]))
                    res_prop <- paste("$$P(X >", cuantil, ") =", round(  prob[cuantil + 2], 7 ), "$$")
                    tipo <- "Función de supervivencia"
                    
                    
                }else{
                    
                    # Calculo de las probabilidades
                    prob <- dpois(x, lambda=lambda)
                    
                    # Valores para mostrar en la gráfica
                    nombres[cuantil + 1] <- round(prob[cuantil + 1], 3) 
                    
                    # Parámetros gráficos
                    ylabel <- expression(P(X==x)) 
                    titulo <- bquote(P(X == .(cuantil))==.(prob[cuantil + 1])) 
                    res_prop <- paste("$$P(X =", cuantil, ") =", round(  prob[cuantil + 1], 7 ), "$$")
                    tipo <- "Función de masa de probabilidad"
                }
                
                

            }

        }

        # #------------------------- Hipergeometrica  ---------------------------------

        if(input$Distribucion == "Hipergeometrica"){
          
            # Evitar reacción si los inputs son invalidos
            req(iv_k$is_valid())
            req(iv_N$is_valid())
            req(iv_n_hiper$is_valid())
          
            k <- as.integer(input$k)
            N <- as.integer(input$N)
            n <- as.integer(input$n_hipergeometrica)
            
            media_hiper <- n*(k/n)
            varianza_hiper <- ((N-n)/(N-1))*n*(k/N)*(1-(k/N))
            
            # Eje x
            #x <- 0:min(n, k)
            minimo <- max(0, k-N+n)
            maximo <- min(n,k)
            x <- minimo:maximo
            
            # lista vacia para las etiquetas
            nombres <- rep("", length(x))
            
            
            # Latex teoria
            mostrar_dist <- paste("$$X \\sim Hiper(", N, ",", k, ",", n, ")$$")
            mostrar_media <- paste("$$E[X]=", round(media_hiper, 4), "$$")
            mostrar_var <- paste("$$V(X)=", round(varianza_hiper, 4), "$$")
            mostrar_fmp <- paste("$$p(x)=\\dfrac{{" ,k,  "\\choose x}\\;{", N-k, " \\choose", n, "-x}}{{", N, "\\choose", n,"}};$$")
            mostrar_rango <- paste("$$x=", minimo,",", minimo+1, ",", minimo + 2, ",", "\\cdots\\;,", maximo, ".$$")
            
            if(input$Propede == "Cuantil"){
                
                probabilidad <- input$Probabilidad
                
                
                # Calculo del cuantil
                cuantil <- qhyper(probabilidad, m = k, n = N-k, k = n)
                
                # Calculo de la probabilidad
                prob <- phyper(q = x, m = k, n = N-k, k = n)
                
                # Valores para mostrar en la gráfica
                #nombres[cuantil:(cuantil + 1)] <- round(prob[cuantil:(cuantil + 1)], 3)
                nombres[(cuantil-minimo):(cuantil-minimo + 1)] <- round(prob[(cuantil-minimo):(cuantil-minimo + 1)], 3)
                
                
                # Parámetros gráficos
                ylabel <- expression(P(X<=x)) 
                titulo <- bquote(q[.(probabilidad)]==.(cuantil))
                res_prop <- paste("$$q_{", probabilidad, "} =", cuantil, "$$")
                tipo <- "Función de distribución acumulada"
                
                
                output$prob_cuantiles <-  renderUI({
                    
                    list(
                        
                        splitLayout(
                            wellPanel(
                                withMathJax(paste("$$P(X \\leq", cuantil-1, ")=" , round( prob[cuantil-minimo], 4), "$$")),  
                            ),
                            
                            wellPanel(
                                withMathJax(paste("$$P(X \\leq", cuantil, ")=" , round( prob[cuantil-minimo+1], 4), "$$"))
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
                    prob <- phyper(q = x, m = k, n = N-k, k = n)
                    
                    # Valores para mostrar en la gráfica
                    nombres[1:(cuantil-minimo + 1)] <- round(prob[1:(cuantil-minimo + 1)], 3)
                    
                    # Parámetros gráficos
                    ylabel <- expression(P(X<=x)) 
                    titulo <- bquote(P(X <= .(cuantil))==.(prob[cuantil - minimo + 1]))
                    res_prop <- paste("$$P(X \\leq", cuantil, ") =", round(  prob[cuantil - minimo + 1], 7 ), "$$")
                    tipo <- "Función de distribución acumulada"
                    
                }else if(input$Acumulado == "supervivencia"){
                    
                    rango <-  min(n, k)
                    
                    # Calculo de las probabilidades
                    prob <- phyper(q = -1:(min(n, k)-1), m = k, n = N-k, k = n, lower.tail = F)
                    
                    # Valores para mostrar en la gráfica
                    nombres[(cuantil + 2):(rango+1)] <- round(prob[(cuantil + 2):(rango+1)], 3)
                    
                    # Parámetros gráficos
                    ylabel <- expression(P(X>x))
                    titulo <- bquote(P(X > .(cuantil))==.(prob[cuantil + 2]))
                    res_prop <- paste("$$P(X >", cuantil, ") =", round(  prob[cuantil + 2], 7 ), "$$")
                    tipo <- "Función de supervivencia"
                    
                    
                }else{
                    
                    # Calculo de las probabilidades
                    prob <- dhyper(x = x, m = k, n = N-k, k = n)
                    
                    # Valores para mostrar en la gráfica
                    nombres[cuantil - minimo + 1] <- round(prob[cuantil - minimo + 1], 3) 
                    
                    # Parámetros gráficos
                    ylabel <- expression(P(X==x)) 
                    titulo <- bquote(P(X == .(cuantil))==.(prob[cuantil - minimo + 1])) 
                    res_prop <- paste("$$P(X =", cuantil, ") =", round(  prob[cuantil - minimo + 1], 7 ), "$$")
                    tipo <- "Función de masa de probabilidad"
                }

            }}
        

        # #------------------------- Binomial negativa  ---------------------------
      
      if(input$Distribucion == "Binomial Negativa"){
        
        r <- input$r_nbin
        p <- input$p_nbin
        
        # Latex teoria
        mostrar_dist <- paste("$$X \\sim BN(", r, ",", p, ")$$")
        mostrar_media <- paste("$$E[X]=", round(r*(1-p)/p, 4), "$$")
        mostrar_var <- paste("$$V(X)=", round(r*(1-p)/p^2, 4), "$$")
        mostrar_fmp <- paste("$$ p(x) = {", r, "+ x - 1", "\\choose x}", p, "^{", r, "}",  "\\left(1- ", p, "\\right)^{x}$$")
        mostrar_rango <- paste("$$x = 0\\,,\\,1\\,,\\,2\\,,\\,\\cdots \\,,\\,", "$$")
        
        
        # if(input$Propede == "Cuantil"){
        #   
        #   probabilidad <- input$Probabilidad
        #   
        #   # k <- 5  # numero de desviaciones
        #   # curve(dnorm(x, media, desvi), xlim=media+c(-k,k)*desvi, lwd=3,
        #   #       main='Distribución normal', ylab="", xlab="", axes=FALSE)
        #   # axis(1, at=seq(media-k*desvi, media+k*desvi, desvi), pos=0)
        #   # axis(2, las=1)
        #   # secuencia <- seq(media-k*desvi, percentil, length.out=10000)
        #   # cord.x <- c(media-k*desvi, secuencia, percentil)
        #   # cord.y <- c(0, dnorm(secuencia, media, desvi), 0)
        #   # polygon(cord.x, cord.y, col='steelblue')
        #   # shadowtext(x=percentil, y=0, round(percentil, 2), col="chartreuse", cex=2)
        #   # title(sub=bquote(P(X<.(percentil))==.(proba)), cex.sub=2)
        #   
        # }
        # 
        # else {
        #   
        #   cuantil <- input$Cuantil
        #   
        #   
        #   if(input$Acumulado == "acumulada"){
        #     
        #     
        #     probabilidad <- pnbinom(cuantil-r, r, p)
        #     prob <- pnbinom(0:(cuantil-r), r, p)
        #     barplot(prob, ylim=c(0, 1), names.arg=r:cuantil,
        #             xlab=" ", ylab=expression(P(X<=x)), col="#D95914", las=1)
        #     grid()
        #     
        #     title(sub=bquote(P(X <= .(cuantil))==.(probabilidad)), cex.sub=2)
        #     
        #   }else if(input$Acumulado == "supervivencia"){
        #     
        #     
        #     
        #     
        #   }else{
        #     
        #     colores <- rep("cyan4", cuantil-r+1)
        #     colores[cuantil- r + 1] <- "#D95914"
        #     
        #     probabilidad <- dnbinom(cuantil-r, r, p)
        #     
        #     prob <- dnbinom(x=0:(cuantil-r), size=r, prob=p)
        #     barplot(prob, ylim=c(0, 1), names.arg=r:cuantil,
        #             xlab=" ", ylab=expression(P(X==x)), col=colores, las=1)
        #     grid()
        #     title(sub=bquote(P(X == .(cuantil))==.(probabilidad)), cex.sub=2)
        #   }
        #   
        #   
        #  }
      }
      
      
        
        # Imprimir la probabilidad calculada --------------------------------------
        
        output$res_probabilidad <- renderUI({

            h4(withMathJax(res_prop))
        })
        
        # Imprimir la teoría evaluada ---------------------------------------------
        
        output$res_teoria <- renderUI({
            
            list(
                withMathJax(mostrar_dist),

                splitLayout(
                    wellPanel(
                        withMathJax(mostrar_media)
                    ),
                    
                    wellPanel(
                        withMathJax(mostrar_var)
                    )
                ),
                
                withMathJax(mostrar_fmp),
                withMathJax(mostrar_rango)
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
