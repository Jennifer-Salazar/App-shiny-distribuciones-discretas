
library(latex2exp)


# Acomulada ---------------------------------------------------------------

cuantil <- 8

graf_tema <- theme_stata()


# Binomial ----------------------------------------------------------------

n <- 15
p <- 0.5


# base r ------------------------------------------------------------------

colores <- rep("cyan4", n + 1)
colores[0:cuantil + 1] <- "#D95914"

probabilidad <- pbinom(cuantil, size = n, prob = p)
prob <- pbinom(0:n, size=n, prob=p)
barplot(prob, ylim=c(0, 1), names.arg=0:n,
        xlab=" ", ylab=expression(P(X<=x)), col=colores, las=1)
grid()

title(sub=bquote(P(X <= .(cuantil))==.(probabilidad)), cex.sub=2)


# ggplot ------------------------------------------------------------------

# Eje x
x <- 0:n

# Calculo de la probabilidad
prob <- pbinom(x, size=n, prob=p)

# Valores para mostrar en la gráfica
nombres <- rep("", length(x))
nombres[0:cuantil + 1] <- round(prob[0:cuantil + 1], 3)

# Parámetros gráficos
ylabel <- expression(P(X<=x)) 
titulo <- bquote(P(X <= .(cuantil))==.(prob[cuantil + 1]))


df <- data.frame(
  x = as.factor(x),
  y = prob,
  name = nombres
)

ggplot(df, aes(x=x, y=y, fill = ifelse(name == "", 1, 0) )) + 
  geom_bar(stat = "identity", color="black", show.legend = FALSE) + 
  geom_text(aes(label=name), vjust=-0.3, size=3.5) + #, fill = "white") +
  labs(
    title = bquote(P(X <= .(cuantil))==.(prob[cuantil + 1])),
    subtitle = "Función de masa de probabilidad",
    caption = "",
    x = "",
    y = expression(P(X<=x))
  ) +
  graf_tema



# Poisson -----------------------------------------------------------------

lambda <- 5
n <- qpois(0.99999, lambda)

# base r ------------------------------------------------------------------


colores <- rep("cyan4", n + 1)
colores[0:cuantil + 1] <- "#D95914"

probabilidad <- ppois(cuantil, lambda)

prob <- ppois(0:n, lambda=lambda)
barplot(prob, ylim=c(0, 1), names.arg=0:n,
        xlab="", ylab=expression(P(X<=x)), col=colores, las=1)
grid()

title(sub=bquote(P(X <= .(cuantil))==.(probabilidad)), cex.sub=2)


# ggplot ------------------------------------------------------------------

# Eje x
x <- 0:n

# Calculo de la probabilidad
prob <- ppois(x, lambda=lambda)

# Valores para mostrar en la gráfica
nombres <- rep("", length(x))
nombres[0:cuantil + 1] <- round(prob[0:cuantil + 1], 3)

# Parámetros gráficos
ylabel <- expression(P(X<=x)) 
titulo <- bquote(P(X <= .(cuantil))==.(prob[cuantil + 1]))

df <- data.frame(
  x = as.factor(x),
  y = prob,
  name = nombres
)


ggplot(df, aes(x=x, y=y, fill = ifelse(name == "", 1, 0) )) + 
  geom_bar(stat = "identity", color="black", show.legend = FALSE) + 
  geom_text(aes(label=name), vjust=-0.3, size=3.5) + #, fill = "white") +
  labs(
    title = bquote(P(X <= .(cuantil))==.(prob[cuantil + 1])),
    subtitle = "Función de masa de probabilidad",
    caption = "",
    x = "",
    y = expression(P(X<=x))
  ) +
  graf_tema



# Hipergeometríca ---------------------------------------------------------

k <- 15
N <- 25
n <- 10


# base r ------------------------------------------------------------------

colores <- rep("cyan4", min(n, k) + 1)
colores[0:cuantil + 1] <- "#D95914"

probabilidad <- phyper(q = cuantil, m = k, n = N-k, k = n)

prob <- phyper(q = 0:min(n, k), m = k, n = N-k, k = n)
barplot(prob, ylim=c(0, 1), names.arg=0:min(n, k),
        xlab=" ", ylab=expression(P(X<=x)), col=colores, las=1)
grid()

title(sub=bquote(P(X <= .(cuantil))==.(probabilidad)), cex.sub=2)


# ggplot ------------------------------------------------------------------

# Eje x
x <- 0:min(n, k)

# Calculo de las probabilidades
prob <- phyper(q = 0:min(n, k), m = k, n = N-k, k = n)

# Valores para mostrar en la gráfica
nombres <- rep("", length(x))
nombres[0:cuantil + 1] <- round(prob[0:cuantil + 1], 3)

# Parámetros gráficos
ylabel <- expression(P(X<=x)) 
titulo <- bquote(P(X <= .(cuantil))==.(prob[cuantil + 1]))

df <- data.frame(
  x = as.factor(x),
  y = prob,
  name = nombres
)


ggplot(df, aes(x=x, y=y, fill = ifelse(name == "", 1, 0) )) + 
  geom_bar(stat = "identity", color="black", show.legend = FALSE) + 
  geom_text(aes(label=name), vjust=-0.3, size=3.5) + #, fill = "white") +
  labs(
    title = bquote(P(X <= .(cuantil))==.(prob[cuantil + 1])),
    subtitle = "Función de masa de probabilidad",
    caption = "",
    x = "",
    y = expression(P(X<=x))
  ) +
  graf_tema
