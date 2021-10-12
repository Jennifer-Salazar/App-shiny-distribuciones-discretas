library(shiny)

shinyServer(function(input, output, session){
    
    
    # ------------- Funciones para el calculo de probabilidades --------
    
    binomial <- function(num, size, prob, acumulada = FALSE){
        if(acumulada == FALSE){
            resultado <- pbinom(num, size=size, prob=prob)-pbinom(num-1, size=size, prob = prob)
        }else{
            resultado <- pbinom(num, size=size, prob=prob)
        }
        return(resultado)
    }
    
    poisson <- function(num, lambda, acumulada = FALSE){
        if(acumulada == FALSE){
            resultado <- ppois(num, lambda = lambda)-ppois(num-1, lambda=lambda)
        }else{
            resultado <- ppois(num, lambda = lambda)
        }
        return(resultado)
    }
    
    hipergeometrica <- function(x, k, N, n, acumulada = FALSE){
        if(acumulada == FALSE){
            resultado <- phyper(q=x, m=k, n=N-k, k=n)- phyper(q=x-1, m=k, n=N-k, k=n)
        }else{
            resultado <- phyper(q=x, m=k, n=N-k, k=n)
        }
        return(resultado)
    }
    
    # ----------------------- Shadow function ----------------------
    shadowtext <- function(x, y=NULL, labels, col='white', bg='black',
                           theta= seq(0, 2*pi, length.out=50), r=0.1, ... ) {
        
        xy <- xy.coords(x,y)
        xo <- r*strwidth('A')
        yo <- r*strheight('A')
        
        # draw background text with small shift in x and y in background colour
        for (i in theta) {
            text( xy$x + cos(i)*xo, xy$y + sin(i)*yo, labels, col=bg, ... )
        }
        # draw actual text in exact xy position in foreground colour
        text(xy$x, xy$y, labels, col=col, ... )
    }
    
    #--------------------------------------------------------------
    
    output$miplot <- renderPlot({
        
        #----------------------------- Binomial  ---------------------------------
        
        if(input$Distribucion == "Binomial"){
            n <- input$n_binomial
            p <- input$p
            
            if(input$Propede == "Percentil"){
                
                probabilidad <- input$Probabilidad
                
                # k <- 5  # numero de desviaciones
                # curve(dnorm(x, media, desvi), xlim=media+c(-k,k)*desvi, lwd=3,
                #       main='Distribución normal', ylab="", xlab="", axes=FALSE)
                # axis(1, at=seq(media-k*desvi, media+k*desvi, desvi), pos=0)
                # axis(2, las=1)
                # secuencia <- seq(media-k*desvi, percentil, length.out=10000)
                # cord.x <- c(media-k*desvi, secuencia, percentil)
                # cord.y <- c(0, dnorm(secuencia, media, desvi), 0)
                # polygon(cord.x, cord.y, col='steelblue')
                # shadowtext(x=percentil, y=0, round(percentil, 2), col="chartreuse", cex=2)
                # title(sub=bquote(P(X<.(percentil))==.(proba)), cex.sub=2)
                
            }
            
            else {
                
                percentil <- input$Percentil
                
                
                if(input$Acumulado == "acumulada"){
                    acumulada = TRUE
                }else{
                    acumulada = FALSE
                }
                
                probabilidad <- binomial(num = percentil, size = n, prob = p, acumulada = acumulada)
                
                
                # k <- 5  # numero de desviaciones
                # curve(dnorm(x, media, desvi), xlim=media+c(-k,k)*desvi, lwd=3,
                #       main='Distribución normal', ylab="", xlab="", axes=FALSE)
                # axis(1, at=seq(media-k*desvi, media+k*desvi, desvi), pos=0)
                # axis(2, las=1)
                # if (percentil > media-k*desvi) {
                #     secuencia <- seq(media-k*desvi, percentil, length.out=10000)
                #     cord.x <- c(media-k*desvi, secuencia, percentil)
                #     cord.y <- c(0, dnorm(secuencia, media, desvi), 0)
                #     polygon(cord.x, cord.y, col='steelblue')
                #     altura <- dnorm(percentil, media, desvi)
                #     shadowtext(x=percentil, y=altura/2, round(proba, 2), 
                #                col="orchid2", cex=2)
                # }
                
                if(acumulada){
                    title(sub=bquote(P(X <= .(percentil))==.(probabilidad)), cex.sub=2)
                }else{
                    title(sub=bquote(P(X == .(percentil))==.(probabilidad)), cex.sub=2)
                }
                
                
            }}
        
        # #----------------------------- Poisson ---------------------------------

        if(input$Distribucion == "Poisson"){
            lambda <- input$lambda


            if(input$Propede == "Percentil"){
                probabilidad <- input$Probabilidad
                

                # curve(dt(x, df), xlim=c(-5,5), lwd=3,
                #       main='Distribución t-student', ylab="", xlab="",
                #       axes=FALSE)
                # axis(1, at=seq(-5, 5, by=0.5), pos=0)
                # axis(2, las=1)
                # secuencia <- seq(percentil, 5, length.out=10000)
                # cord.x <- c(percentil, secuencia, 5)
                # cord.y <- c(0, dt(secuencia, df=df), 0)
                # polygon(cord.x, cord.y, col='darkolivegreen3')
                # shadowtext(x=percentil, y=0.01, round(percentil, 2),
                #            col="chartreuse", cex=2)
                # title(sub=bquote(P(t>.(percentil))==.(proba)), cex.sub=2)
                # 
                # output$perce <- renderText(percentil)
            }

            else {
                
                percentil <- input$Percentil
                
                
                if(input$Acumulado == "acumulada"){
                    acumulada = TRUE
                }else{
                    acumulada = FALSE
                }
                
                probabilidad <- poisson(num = percentil, lambda = lambda, acumulada = acumulada)
                

                # curve(dt(x, df), xlim=c(-5,5), lwd=3,
                #       main='Distribución t-student', ylab="", xlab="",
                #       axes=FALSE)
                # axis(1, at=seq(-5, 5, by=0.5), pos=0)
                # axis(2, las=1)
                # secuencia <- seq(percentil, 5, length.out=10000)
                # cord.x <- c(percentil, secuencia, 5)
                # cord.y <- c(0, dt(secuencia, df=df), 0)
                # polygon(cord.x, cord.y, col='darkolivegreen3')
                # altura <- dt(x=percentil, df=df)
                # shadowtext(x=percentil, y=altura/2, round(proba, 2),
                #            col="orchid2", cex=2)
                # title(sub=bquote(P(t>.(percentil))==.(proba)), cex.sub=2)
                
                if(acumulada){
                    title(sub=bquote(P(X <= .(percentil))==.(probabilidad)), cex.sub=2)
                }else{
                    title(sub=bquote(P(X == .(percentil))==.(probabilidad)), cex.sub=2)
                }

            }

        }

        # #------------------------- Hipergeometrica  ---------------------------------

        if(input$Distribucion == "Hipergeometrica"){
            k <- as.integer(input$k)
            N <- as.integer(input$N)
            n <- as.integer(input$n_hipergeometrica)
            

            if(input$Propede == "Percentil"){
                
                probabilidad <- input$Probabilidad

                # max.x <- 3 * percentil
                # curve(df(x, df1, df2), xlim=c(0, max.x), lwd=3,
                #       main='Distribución F', ylab="", xlab="", axes=FALSE)
                # axis(1, at=seq(0, max.x, by=0.5), pos=0)
                # axis(2, las=1)
                # secuencia <- seq(percentil, max.x, length.out=10000)
                # cord.x <- c(percentil, secuencia, max.x)
                # cord.y <- c(0, df(secuencia, df1, df2), 0)
                # polygon(cord.x, cord.y, col='lightsalmon3')
                # shadowtext(x=percentil, y=0.01, round(percentil, 2),
                #            col="chartreuse", cex=2)
                # title(sub=bquote(P(F>.(percentil))==.(proba)), cex.sub=2)

            }else{
                
                percentil <- input$Percentil
                
                
                if(input$Acumulado == "acumulada"){
                    acumulada = TRUE
                }else{
                    acumulada = FALSE
                }
                
                probabilidad <- hipergeometrica(x=percentil, k=k, N=N, n=n, acumulada = acumulada)
                

                # max.x <- 3 * percentil
                # curve(df(x, df1, df2), xlim=c(0, max.x), lwd=3,
                #       main='Distribución F', ylab="", xlab="", axes=FALSE)
                # axis(1, at=seq(0, max.x, by=0.5), pos=0)
                # axis(2, las=1)
                # secuencia <- seq(percentil, max.x, length.out=10000)
                # cord.x <- c(percentil, secuencia, max.x)
                # cord.y <- c(0, df(secuencia, df1, df2), 0)
                # polygon(cord.x, cord.y, col='lightsalmon3')
                # altura <- df(x=percentil, df1, df2)
                # shadowtext(x=percentil, y=altura/2, round(proba, 2),
                #            col="orchid2", cex=2)
                # title(sub=bquote(P(F>.(percentil))==.(proba)), cex.sub=2)
                
                if(acumulada){
                    title(sub=bquote(P(X <= .(percentil))==.(probabilidad)), cex.sub=2)
                }else{
                    title(sub=bquote(P(X == .(percentil))==.(probabilidad)), cex.sub=2)
                }


            }}

    })
})
