library(shiny)
library(markdown)
library(ggplot2)
library(ggthemes)
library(shinyvalidate)


# Aplicación --------------------------------------------------------------

shinyServer(function(input, output, session){

  # Crear reglas para los inputs de las distribuciones ----------------------
  
  # Para el n de la binomial
  iv_n_binomial <- InputValidator$new()
  iv_n_binomial$add_rule("n_binomial", sv_required(message = "Campo requerido"),)
  iv_n_binomial$add_rule("n_binomial", sv_numeric(message = "Ingrese un valor numérico"))
  iv_n_binomial$add_rule("n_binomial", sv_integer(message = "Ingrese un valor entero"))
  iv_n_binomial$add_rule("n_binomial", sv_between(1, 10000000, message = "Ingrese un entero positivo"))
  iv_n_binomial$enable()
  
  # Para el p de la binomial
  iv_p_binomial <- InputValidator$new()
  iv_p_binomial$add_rule("p", sv_required(message = "Campo requerido"))
  iv_p_binomial$add_rule("p", sv_numeric(message = "Ingrese un valor numérico"))
  iv_p_binomial$add_rule("p", sv_between(0, 1, message = "Ingrese un real positivo"))
  iv_p_binomial$enable()
  
  # Para el lambda de la poisson
  iv_lambda <- InputValidator$new()
  iv_lambda$add_rule("lambda", sv_required(message = "Campo requerido"))
  iv_lambda$add_rule("lambda", sv_numeric(message = "Ingrese un valor numérico"))
  iv_lambda$add_rule("lambda", sv_between(1, 10000000, message = "Ingrese un entero positivo"))
  iv_lambda$enable()

  # Para el k de la hipergeométrica
  iv_k <- InputValidator$new()
  iv_k$add_rule("k", sv_required(message = "Campo requerido"))
  iv_k$add_rule("k", sv_numeric(message = "Ingrese un valor numérico"))
  iv_k$add_rule("k", sv_integer(message = "Ingrese un valor entero"))
  iv_k$add_rule("k", sv_between(1, 10000000, message = "Ingrese un entero positivo"))
  iv_k$enable()
  
  # Para N de la hipergeométrica
  iv_N <- InputValidator$new()
  iv_N$add_rule("N", sv_required(message = "Campo requerido"))
  iv_N$add_rule("N", sv_numeric(message = "Ingrese un valor numérico"))
  iv_N$add_rule("N", sv_integer(message = "Ingrese un valor entero"))
  iv_N$add_rule("N", sv_between(1, 10000000, message = "Ingrese un entero positivo"))
  iv_N$enable()
  
  # Para n de la hipergeométrica
  iv_n_hiper <- InputValidator$new()
  iv_n_hiper$add_rule("n_hipergeometrica", sv_required(message = "Campo requerido"))
  iv_n_hiper$add_rule("n_hipergeometrica", sv_numeric(message = "Ingrese un valor numérico"))
  iv_n_hiper$add_rule("n_hipergeometrica", sv_integer(message = "Ingrese un valor entero"))
  iv_n_hiper$add_rule("n_hipergeometrica", sv_between(1, 10000000, message = "Ingrese un entero positivo"))
  iv_n_hiper$enable()
  
  # Para el r de la binomial negativa
  iv_r_nbin <- InputValidator$new()
  iv_r_nbin$add_rule("r_nbin", sv_required(message = "Campo requerido"))
  iv_r_nbin$add_rule("r_nbin", sv_numeric(message = "Ingrese un valor numérico"))
  iv_r_nbin$add_rule("r_nbin", sv_integer(message = "Ingrese un valor entero"))
  iv_r_nbin$add_rule("r_nbin", sv_between(1, 10000000, message = "Ingrese un entero positivo"))
  iv_r_nbin$enable()
  
  # Para el p de la binomial negativa
  iv_p_nbin <- InputValidator$new()
  iv_p_nbin$add_rule("p_nbin", sv_required(message = "Campo requerido"))
  iv_p_nbin$add_rule("p_nbin", sv_numeric(message = "Ingrese un valor numérico"))
  iv_p_nbin$add_rule("p_nbin", sv_between(0, 1, message = "Ingrese un real positivo"))
  iv_p_nbin$enable()
  
  
  # Realización de calculos y mostrar los resultados ------------------------
  
  output$grafico <- renderPlot({
    
    ##----------------------------- Binomial  ---------------------------------
    
    
    if(input$Distribucion == "Binomial"){
      
      # Evitar reacción si los inputs son invalidos   
      req(iv_n_binomial$is_valid())
      req(iv_p_binomial$is_valid())
      

      ## Calcular rango y teoría -------------------------------------------------
      
      n <- input$n_binomial
      p <- input$p
      
      # Eje x
      x <- 0:n
      
      # lista vacia para las etiquetas de los gráficos
      nombres <- rep("", length(x))
      
      # Latex teoria
      mostrar_dist <- paste("$$X \\sim bin(", n, ",", p, ")$$")
      mostrar_media <- paste("$$E[X]=", round(n * p, 4), "$$")
      mostrar_var <- paste("$$V(X)=", round(n*p*(1-p), 4), "$$")
      mostrar_fmp <- paste("$$ p(x) = {", n,  "\\choose x}", p, "^{x}  \\left(1- ", p, "\\right)^{", n, "-x}$$")
      mostrar_rango <- paste("$$x = 0\\,,\\,1\\,,\\,2\\,,\\,\\cdots \\,,\\,", n, ".$$")
      

      # Calculo de cuantiles binomial -------------------------------------------

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
      
      # Calculo de probabilidades binomial --------------------------------------
      
      else {
        
        # Establecer reglas para el input de cuantil
        iv_cuantil <- InputValidator$new()
        iv_cuantil$add_rule("Cuantil", sv_required(message = "Campo requerido"))
        iv_cuantil$add_rule("Cuantil", sv_numeric(message = "Ingrese un valor numérico"))
        iv_cuantil$add_rule(
          "Cuantil",
          sv_between(min(x), max(x), 
                     message = paste("Ingrese un número entre", min(x), "y", max(x))
          )
        )
        iv_cuantil$enable()
        
        
        cuantil <- input$Cuantil
        
        
        # Evitar reacción cuando cuantil es invalido
        req(iv_cuantil$is_valid())
        
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
          
          if(cuantil == n){
            prob_cuantil <- 0
          }else{
            # Valores para mostrar en la gráfica
            nombres[(cuantil + 2):(n+1)] <- round(prob[(cuantil + 2):(n+1)], 3)
            prob_cuantil <- prob[cuantil + 2]
          }
          
          # Parámetros gráficos
          ylabel <- expression(P(X>x))
          titulo <- bquote(P(X > .(cuantil))==.(prob_cuantil))
          res_prop <- paste("$$P(X >", cuantil, ") =", round(prob_cuantil, 7 ), "$$")
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
    
    ##----------------------------- Poisson -----------------------------------
    
    if(input$Distribucion == "Poisson"){
      
      # Evitar reacción si los inputs son invalidos
      req(iv_lambda$is_valid())
      

      ## Calcular rango y teoría -------------------------------------------------
      
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
      

      # Calculo de cuantiles poisson --------------------------------------------

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

      # Calculo de probabilidades poisson ---------------------------------------

      else {
        
        # Establecer reglas para el input de cuantil
        iv_cuantil <- InputValidator$new()
        iv_cuantil$add_rule("Cuantil", sv_required(message = "Campo requerido"))
        iv_cuantil$add_rule("Cuantil", sv_numeric(message = "Ingrese un valor numérico"))
        iv_cuantil$add_rule(
          "Cuantil",
          sv_between(min(x), 10000000, 
                     message = paste("Ingrese un número mayor a", min(x))
          )
        )
        iv_cuantil$enable()
        
        
        cuantil <- input$Cuantil
        
        
        # Evitar reacción cuando cuantil es invalido
        req(iv_cuantil$is_valid())
        
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
          
          if(cuantil == n){
            prob_cuantil <- 0
          }else{
            # Valores para mostrar en la gráfica
            nombres[(cuantil + 2):(n+1)] <- round(prob[(cuantil + 2):(n+1)], 3)
            prob_cuantil <- prob[cuantil + 2]
          }
          
          
          
          # Parámetros gráficos
          ylabel <- expression(P(X>x))
          titulo <- bquote(P(X > .(cuantil))==.(prob_cuantil))
          res_prop <- paste("$$P(X >", cuantil, ") =", round(prob_cuantil, 7 ), "$$")
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
    
    ##------------------------- Hipergeometrica  ---------------------------------
    
    if(input$Distribucion == "Hipergeometrica"){
      
      # Evitar reacción si los inputs son invalidos
      req(iv_k$is_valid())
      req(iv_N$is_valid())
      req(iv_n_hiper$is_valid())
      

      ## Calcular rango y teoría -------------------------------------------------

      k <- as.integer(input$k)
      N <- as.integer(input$N)
      n <- as.integer(input$n_hipergeometrica)
      
      media_hiper <- n*(k/n)
      varianza_hiper <- ((N-n)/(N-1))*n*(k/N)*(1-(k/N))
      
      # Eje x
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
      

      # Calculo de cuantiles hipergeométrica ------------------------------------
      
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
        
        
        
      }
      
      # Calculo de probabilidades hipergeométrica -------------------------------

      else{
        
        # Establecer reglas para el input de cuantil
        iv_cuantil <- InputValidator$new()
        iv_cuantil$add_rule("Cuantil", sv_required(message = "Campo requerido"))
        iv_cuantil$add_rule("Cuantil", sv_numeric(message = "Ingrese un valor numérico"))
        iv_cuantil$add_rule(
          "Cuantil",
          sv_between(min(x), max(x), 
                     message = paste("Ingrese un número entre", min(x), "y", max(x))
          )
        )
        iv_cuantil$enable()
        
        
        cuantil <- input$Cuantil
        
        
        # Evitar reacción cuando cuantil es invalido
        req(iv_cuantil$is_valid())
        
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
          
          
          # Calculo de las probabilidades
          prob <- phyper(q = (minimo-1):(maximo-1), m = k, n = N-k, k = n, lower.tail = F)
          
          if(cuantil == maximo){
            prob_cuantil <- 0
          }else{
            # Valores para mostrar en la gráfica
            nombres[(cuantil):(maximo-1)] <- round(prob[(cuantil):(maximo-1)], 3)
            prob_cuantil <- prob[cuantil]
            
          }
          
          
          # Parámetros gráficos
          ylabel <- expression(P(X>x))
          titulo <- bquote(P(X > .(cuantil))==.(prob_cuantil))
          res_prop <- paste("$$P(X >", cuantil, ") =", round(prob_cuantil, 7 ), "$$")
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
    
    
    ##------------------------- Binomial negativa  ---------------------------
    
    if(input$Distribucion == "Binomial Negativa"){
      
      # Evitar reacción si los inputs son invalidos
      req(iv_r_nbin$is_valid())
      req(iv_p_nbin$is_valid())

      ## Calcular rango y teoría ------------------------------------------------
      
      r <- input$r_nbin
      p <- input$p_nbin
      
      n <- qnbinom(0.99999, r, p)
      
      # Eje x
      x <- 0:n
      
      # lista vacia para las etiquetas
      nombres <- rep("", length(x))
      
      # Latex teoria
      mostrar_dist <- paste("$$X \\sim BN(", r, ",", p, ")$$")
      mostrar_media <- paste("$$E[X]=", round(r*p/(1-p), 4), "$$")
      mostrar_var <- paste("$$V(X)=", round(r*p/(1-p)^2, 4), "$$")
      mostrar_fmp <- paste("$$ p(x) = {", r, "+ x - 1", "\\choose r - 1}", p, "^{", r, "}",  "\\left(1- ", p, "\\right)^{x}$$")
      mostrar_rango <- paste("$$x = 0\\,,\\,1\\,,\\,2\\,,\\,\\cdots \\,,\\,", "$$")
      

      # Calculo de cuantiles binomial negativa ----------------------------------
      
      if(input$Propede == "Cuantil"){
        
        probabilidad <- input$Probabilidad
        
        # Calculo del cuantil
        cuantil <- qnbinom(p, r, probabilidad)
        
        # Calculo de la probabilidad
        prob <- pnbinom(x, size=r, prob=p)
        
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

      # Calculo de probabilidades binomial negativa -----------------------------
      
      else {
        
        # Establecer reglas para el input de cuantil
        iv_cuantil <- InputValidator$new()
        iv_cuantil$add_rule("Cuantil", sv_required(message = "Campo requerido"))
        iv_cuantil$add_rule("Cuantil", sv_numeric(message = "Ingrese un valor numérico"))
        iv_cuantil$add_rule(
          "Cuantil",
          sv_between(min(x), 10000000, 
                     message = paste("Ingrese un número mayor a", min(x))
          )
        )
        iv_cuantil$enable()
        
        
        cuantil <- input$Cuantil
        
        
        # Evitar reacción cuando cuantil es invalido
        req(iv_cuantil$is_valid())
        
        output$prob_cuantiles <-  renderUI({
          
          
          
        }) 
        
        
        if(input$Acumulado == "acumulada"){
          
          # Calculo de la probabilidad
          prob <- pnbinom(x, size=r, prob=p)
          
          # Valores para mostrar en la gráfica
          nombres[0:cuantil + 1] <- round(prob[0:cuantil + 1], 3)
          
          # Parámetros gráficos
          ylabel <- expression(P(X<=x)) 
          titulo <- bquote(P(X <= .(cuantil))==.(prob[cuantil + 1]))
          res_prop <- paste("$$P(X \\leq", cuantil, ") =", round(  prob[cuantil + 1], 7), "$$")
          tipo <- "Función de distribución acumulada"
          
        }else if(input$Acumulado == "supervivencia"){
          
          
          # Calculo de las probabilidades
          prob <- pnbinom(-1:(n-1), size=r, prob=p, lower.tail = F)
          
          if(cuantil == n){
            prob_cuantil <- 0
          }else{
            # Valores para mostrar en la gráfica
            nombres[(cuantil + 2):(n+1)] <- round(prob[(cuantil + 2):(n+1)], 3)
            prob_cuantil <- prob[cuantil + 2]
          }
          
          
          # Parámetros gráficos
          ylabel <- expression(P(X>x))
          titulo <- bquote(P(X > .(cuantil))==.(prob_cuantil))
          res_prop <- paste("$$P(X >", cuantil, ") =", round(prob_cuantil, 7 ), "$$")
          tipo <- "Función de supervivencia"
          
          
        }else{
          
          # Calculo de las probabilidades
          prob <- dnbinom(x, size=r, prob=p)
          
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
    
    aux_colores <- ifelse(df$name == "", "#4E84C4", "#293352") 
    
    if(input$Acumulado == "supervivencia"){
      uniq_col <- unique(aux_colores)[2:1]
    }else{
      uniq_col <- unique(aux_colores)
    }
    
    ggplot(df, aes(x=x, y=y, fill =  aux_colores)) +
      geom_bar(stat = "identity", color="black", show.legend = F) +
      geom_text(aes(label=name), vjust=-0.3, size=3.5) + 
      labs(
        title = titulo,
        subtitle = tipo,
        caption = "",
        x = "",
        y = ylabel
      ) +
      scale_fill_manual(values = na.omit(uniq_col)) +
      theme_fivethirtyeight()
    
    
  })
})
