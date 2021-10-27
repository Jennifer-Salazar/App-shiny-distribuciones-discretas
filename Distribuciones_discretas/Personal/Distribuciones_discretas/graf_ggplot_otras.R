

# Puntual -----------------------------------------------------------------


# Binomial ----------------------------------------------------------------



# base r ------------------------------------------------------------------

colores <- rep("cyan4", n + 1)
colores[(cuantil+2):(n+1)] <- "#D95914"

probabilidad <- pbinom(cuantil, size = n, prob = p, lower.tail = F)
prob <- pbinom(-1:(n-1), size=n, prob=p, lower.tail = F)
barplot(prob, ylim=c(0, 1), names.arg=0:n,
        xlab=" ", ylab=expression(P(X>x)), col=colores, las=1)
grid()

title(sub=bquote(P(X > .(cuantil))==.(probabilidad)), cex.sub=2)




# PUNTUAL BINOMIAL --------------------------------------------------------

# Eje x
x <- 0:n

# Calculo de las probabilidades
prob <- dbinom(x, size=n, prob=p)

# Valores para mostrar en la gráfica
nombres <- rep("", length(x))
nombres[cuantil + 1] <- round(prob[cuantil + 1], 3) 

# Parámetros gráficos
ylabel <- expression(P(X=x)) 
titulo <- bquote(P(X == .(cuantil))==.(prob[cuantil + 1])) 
tipo <- "Función de masa de probabilidad"


# PUNTUAL POISSON ---------------------------------------------------------

colores <- rep("cyan4", n + 1)
colores[cuantil + 1] <- "#D95914"

probabilidad <- dpois(cuantil, lambda)
prob <- dpois(x=0:n, lambda=lambda)
barplot(prob, ylim=c(0, 1), names.arg=0:n,
        xlab="", ylab=expression(P(X==x)), col=colores, las=1)
grid()

title(sub=bquote(P(X == .(cuantil))==.(probabilidad)), cex.sub=2)



# Eje x
x <- 0:n

# Calculo de las probabilidades
prob <- dpois(x, lambda=lambda)

# Valores para mostrar en la gráfica
nombres <- rep("", length(x))
nombres[cuantil + 1] <- round(prob[cuantil + 1], 3) 

# Parámetros gráficos
ylabel <- expression(P(X=x)) 
titulo <- bquote(P(X == .(cuantil))==.(prob[cuantil + 1])) 
tipo <- "Función de masa de probabilidad"



# PUNTUAL HIPERGEOMETRICA -------------------------------------------------

# Eje x
x <- 0:min(n, k)

# Calculo de las probabilidades
prob <- dhyper(x = 0:min(n, k), m = k, n = N-k, k = n)

# Valores para mostrar en la gráfica
nombres <- rep("", length(x))
nombres[cuantil + 1] <- round(prob[cuantil + 1], 3) 

# Parámetros gráficos
ylabel <- expression(P(X=x)) 
titulo <- bquote(P(X == .(cuantil))==.(prob[cuantil + 1])) 
tipo <- "Función de masa de probabilidad"



# Supervivencia binomial --------------------------------------------------


n <- 15

# Eje x
x <- 0:n

# Calculo de las probabilidades
prob <- pbinom(-1:(n-1), size=n, prob=p, lower.tail = F)

# Valores para mostrar en la gráfica
nombres <- rep("", length(x))
nombres[(cuantil + 2):(n+1)] <- round(prob[(cuantil + 2):(n+1)], 3)

# Parámetros gráficos
ylabel <- expression(P(X>x))
titulo <- bquote(P(X > .(cuantil))==.(prob[cuantil + 2]))
tipo <- "Función de distribución de supervivencia"


# Supervivencia poisson ---------------------------------------------------

# Eje x
x <- 0:n

# Calculo de las probabilidades
prob <- ppois(-1:(n-1), lambda=lambda, lower.tail = F)

# Valores para mostrar en la gráfica
nombres <- rep("", length(x))
nombres[(cuantil + 2):(n+1)] <- round(prob[(cuantil + 2):(n+1)], 3)

# Parámetros gráficos
ylabel <- expression(P(X>x))
titulo <- bquote(P(X > .(cuantil))==.(prob[cuantil + 2]))
tipo <- "Función de distribución de supervivencia"



# supervivencia hipergeometrica -------------------------------------------

# Eje x
x <- 0:min(n, k)

# Calculo de las probabilidades
prob <- phyper(q = -1:(min(n, k)-1), m = k, n = N-k, k = n, lower.tail = F)

# Valores para mostrar en la gráfica
nombres <- rep("", length(x))
nombres[(cuantil + 2):(n+1)] <- round(prob[(cuantil + 2):(n+1)], 3)

# Parámetros gráficos
ylabel <- expression(P(X>x))
titulo <- bquote(P(X > .(cuantil))==.(prob[cuantil + 2]))
tipo <- "Función de distribución de supervivencia"



# Cuantiles binomial ------------------------------------------------------

probabilidad <- 0.7

n <- 15
p <- 0.5

# Calculo del cuantil
cuantil <- qbinom(probabilidad, n, p)

# Calculo de la probabilidad
prob <- pbinom(x, size=n, prob=p)

# Valores para mostrar en la gráfica
nombres[cuantil:cuantil + 1] <- round(prob[cuantil:cuantil + 1], 3)

# Parámetros gráficos
ylabel <- expression(P(X<=x)) 
titulo <- bquote(Q[.(probabilidad)]==.(cuantil))
tipo <- "Función de distribución acumulada"


output$Texto_prueba <-  renderUI({
  
  list(h4(paste("P(X ≤", cuantil-1, ")=" , prob[cuantil], sep="")), 
       h4(paste("P(X ≤", cuantil, ")=" , prob[cuantil+1], sep="")))
  
  
  
})


# Cuantiles poisson -------------------------------------------------------

probabilidad <- input$Probabilidad

# Calculo del cuantil
cuantil <- qpois(probabilidad, lambda)

# Calculo de la probabilidad
prob <- ppois(x, lambda=lambda)

# Valores para mostrar en la gráfica
nombres[cuantil:cuantil + 1] <- round(prob[cuantil:cuantil + 1], 3)

# Parámetros gráficos
ylabel <- expression(P(X<=x)) 
titulo <- bquote(Q[.(probabilidad)]==.(cuantil))
tipo <- "Función de distribución acumulada"


output$Texto_prueba <-  renderUI({
  
  list(h4(paste("P(X ≤", cuantil-1, ")=" , prob[cuantil], sep="")), 
       h4(paste("P(X ≤", cuantil, ")=" , prob[cuantil+1], sep="")))
  
})


# Cuantiles hipergeometrica -----------------------------------------------

probabilidad <- input$Probabilidad


# Calculo del cuantil
cuantil <- qhyper(probabilidad, m = k, n = N-k, k = n)

# Calculo de la probabilidad
prob <- phyper(q = 0:min(n, k), m = k, n = N-k, k = n)

# Valores para mostrar en la gráfica
nombres[cuantil:cuantil + 1] <- round(prob[cuantil:cuantil + 1], 3)

# Parámetros gráficos
ylabel <- expression(P(X<=x)) 
titulo <- bquote(Q[.(probabilidad)]==.(cuantil))
tipo <- "Función de distribución acumulada"


output$Texto_prueba <-  renderUI({
  
  list(h4(paste("P(X ≤", cuantil-1, ")=" , prob[cuantil], sep="")), 
       h4(paste("P(X ≤", cuantil, ")=" , prob[cuantil+1], sep="")))
  
})

