datos2 = read.csv("Catalogo1960_2021_0.csv")
library(pacman)
p_load(readxl,tidyverse,GGally,ggplot2,stringr,TeachingSampling)

datos <- datos2 |> 
  filter(EPICENTRO %in% c("OCEANO","PERU")) |> 
  mutate(AÑO = as.numeric(str_sub(FECHA_UTC, start = 0, end = 4)),
         mes = as.numeric(str_sub(FECHA_UTC, start = 5, end = 6)),
         TRIMESTRE = as.factor(case_when(mes <= 4 ~ "1er", mes > 4 & mes <= 8  ~ "2do", mes > 8  ~ "3er")),
         DECADA = as.factor(case_when(AÑO > 1959 & AÑO <= 1970 ~ "60", AÑO > 1970 & AÑO <= 1980 ~ "70",AÑO > 1980 & AÑO <= 1990 ~ "80", AÑO > 1990 & AÑO <= 2000 ~ "90", AÑO > 2000 & AÑO <= 2010 ~ "21", AÑO > 2010 ~ "22"))) |> 
  select("LATITUD","LONGITUD","PROFUNDIDAD","MAGNITUD","EPICENTRO","TRIMESTRE","DECADA")

N<-nrow(datos) 
n<-3000
RNGkind(sample.kind="Rounding")
set.seed(20)
m1<-S.SI(N,n)
mues<-datos[m1,]
data.frame(mues)
mues |> ggpairs(                 # Data frame
                columns = 1:5,        # Columnas
                aes(color = EPICENTRO,  # Color por grupo (var. categórica)
                    alpha = 0.5))

#seleccion de variables
modelo= lm(MAGNITUD~.,mues)
modelo |> summary()
library(olsrr)
modelo |> ols_step_all_possible() |> data.frame() -> resultados
##r2
resultados |> select(n,predrsq,predictors) |> arrange(-predrsq) |> head(10)
##cp mallows  
resultados |> select(n,cp,predictors) |> mutate(dif = n-cp,.after=cp) |>
  arrange(abs(dif)) |> head(10)
##Criterio de información Bayesiano de Sawa
resultados |> select(n,sbic,predictors) |> arrange(sbic) |> head(10)
##Criterio de información de Akaike
resultados |> select(n,aic,predictors) |> arrange(aic) |> head(10)
## Criterio de información Bayesiano de Schwarz
resultados |> select(n,sbc,predictors) |> arrange(sbc) |> head(10)
##Cuadrado medio del error de predicción
resultados |> select(n,msep,predictors) |> arrange(msep) |> head(10)
##Error final de predicción
resultados |>
  select(n,fpe,predictors) |>
  arrange(fpe) |> head(10)
##Criterio de predicción de Amemiya
resultados |> select(n,apc,predictors) |> arrange(apc) |> head(10)
##Sp de Hocking
resultados |>
  select(n,hsp,predictors) |>
  arrange(hsp) |> head(10)
##backward
modelo|> ols_step_backward_p()
modelo |> ols_step_backward_aic()
#forward
modelo|> ols_step_forward_p()
modelo |> ols_step_forward_aic()
#stepwise
modelo |> ols_step_both_p()
modelo |> ols_step_both_aic()

modelo2 <- lm(MAGNITUD ~ DECADA+EPICENTRO+PROFUNDIDAD, mues)
summary(modelo2)
