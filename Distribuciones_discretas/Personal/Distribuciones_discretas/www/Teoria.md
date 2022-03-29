---
title: ""
author: ""
date: ""
output:
  html_document: default
---

<h1> <center> Teoría </center> </h1>


<hr>


## Distribución binomial 


Sea $X$: # de éxitos en los $n$ ensayos independientes


entonces:


$$X\; \sim \; \mbox{bin}\left(n\,,\,p\right)$$

cuya función de masa de probabilidad es:

$$ p(x) = {n \choose x}\; p^{x} \; \left(1- p\right)^{n-x}\quad ;\quad x = 0\,,\,1\,,\,2\,,\,\cdots \,,\,n \;.$$

donde $n$ es el número de ensayos independientes y $p$ es la probabilidad de éxito de cada uno de los ensayos

$$E[X]=np, ~~~~~ V(X)=np(1-p)$$ 


<hr>

## Distribución Poisson

Sea $X$: una variable aleatoria que representa el número de eventos aleatorios independientes que ocurren a una rapidez constante sobre una unidad de tiempo o espacio.



entonces 


$$X \sim Poisson(\lambda)$$ 

Cuya función de masa de probabilidad es:


$$p(x)=\frac{e^{-\lambda}\;\lambda^{x}}{x!} \, \, x=0,1,2, \ldots, \,\, \lambda>0$$




El parámetro de la distribución Poisson es $\lambda$, que corresponde al número promedio de ocurrencias del evento aleatorio por unidad de tiempo (o espacio).

$$E[X]=\lambda, ~~~~~ V(X)= \lambda$$

<hr>

## Distribución hipergeométrica


Suponga que una población finita tiene $N$ elementos, cada uno de los cuales tiene una de dos características diferentes, en donde $K$ elementos tienen la característica de interés y $N-K$ no la tienen. Se toman al azar y sin reemplazo $n$ de estos elementos.

Sea $X:$ la variable aleatoria que representa el número de elementos que tienen la característica de interés en los $n$ seleccionados.

entonces:


$$X \sim Hiper(N,K,n)$$

Los valores que toma esta variable aleatoria son: $x=max(0, n+K-N),\cdots,\mbox{min}(n,K)$.

La función masa de probabilidad está dada por:

$$ p(x)=P\left(X=x\right)=\dfrac{{K \choose x}\;{N-K \choose n-x}}{{N \choose n}}\quad;\quad x= max(0, n+K-N),\cdots,\mbox{min}(n,K)\;.$$


$$E[X]=n\frac{K}{N}, ~~~~~ V(X)=\frac{(N-n)}{(N-1)}n\frac{K}{N}\left(1-\frac{K}{N}\right)$$

<hr>

## Probabilidad 


**Acumulada**


$$F(x)=P(X \leq x) = \sum_{x'\leq x}^{} p(x'),~~~ \forall_x \in  \mathbb{R}$$


**Puntual**


$$p(x)=P(X=x),~~ \forall_x \in A_x$$


**Supervivencia**


$$1-F(x)=P(X > x) = \sum_{x'> x}^{} p(x'),~~~ \forall_x \in  \mathbb{R}$$
<hr>

## Cuantil

Sea $X$ una v.a discreta, definimos el cuantil $P$ de la distribucón de $X$, como el valor $q_p$ de la v.a $X$ que cumple:

$$P(X \leq q_p) \geq P ~~~~~ y ~~~~~~ P(X < q_p) \leq P$$
<hr>