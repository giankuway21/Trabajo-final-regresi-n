datos2 = read.csv("Catalogo1960_2021_0.csv")
library(pacman)
p_load(readxl,dplyr,GGally,ggplot2)

library(TeachingSampling)

datos3 <- datos2 |> 
  mutate(ZONA = case_when(DEPARTAMENTO == "MAR" ~ "Mar",
                                  DEPARTAMENTO != "MAR" ~ "Continente")) |> #Poner un case_when
    select("LATITUD","LONGITUD","PROFUNDIDAD","MAGNITUD","ZONA")
#max(table(datos2$DEPARTAMENTO))

#datos2 |>  ggplot()+geom_bar(aes(x=ZONA))
#str(datos2$DEPARTAMENTO)

N<-nrow(datos3) 
n<-300
RNGkind(sample.kind="Rounding")
set.seed(20)
m1<-S.SI(N,n)
mues<-datos3[m1,]
data.frame(mues)
mues |> ggpairs(                 # Data frame
                columns = 1:5,        # Columnas
                aes(color = ZONA,  # Color por grupo (var. categórica)
                    alpha = 0.5))


