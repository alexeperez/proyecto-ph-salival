## Distribución de la Población

setwd("C:/Users/aperez/Desktop/proyecto-ph-salival")
list.files()
## informacion utilizada en el analisis
data <- read.table ("data_ph.txt", header=TRUE, sep="\t", dec=",")
data <- data[,1:12]
str(data)
#View(data)

library(ggplot2)
g1 <- ggplot(data, aes(x=Genero, fill=Genero))+
        geom_bar(outlier.colour = "red",color="gray50",width=0.5)+
        labs(y = "Frecuencia", x = "Género")
print(g1)
g2 <- ggplot(data, aes(x=Grupo_Cap_Amort, fill=Grupo_Cap_Amort))+
        geom_bar(outlier.colour = "red",color="gray50",width=0.5)+
        labs(y = "Frecuencia", x = "Capacidad Buffer", fill="Grupo")+
        scale_fill_manual(values = c("green1", "khaki1", "orangered"))
print(g2)


pos1 <- with(data,table(Subg_Cap_Amort)/2)
pos2 <- with(data,table(Subg_Cap_Amort)/2)
 pos()
g3 <- ggplot(data, aes(x=Grupo_Cap_Amort, fill=Subg_Cap_Amort))+
        geom_bar(outlier.colour = "red",color="gray50",width=0.5)+
        labs(y = "Frecuencia", x = "Grupo", fill="Subgrupo")+
        scale_fill_manual(values = c("lightcoral", "lightseagreen","lightcoral", "lightseagreen",
                                     "lightcoral","lightseagreen"))+
        theme(legend.position= "none")+
        annotate("text", x=rep(c(1,2,3),2), y= c(8,4,2,24,12,6), 
                 label=c("A1", "B1", "C1", "A2", "B2", "C2"), colour = "gray30")

print(g3)



## Comparación de tratamientos

setwd("C:/Users/aperez/Desktop/proyecto-ph-salival")
list.files()
## informacion utilizada en el analisis
data <- read.table ("data_ph.txt", header=TRUE, sep="\t", dec=",")
data <- data[,1:12]
str(data)
#View(data)

# analisis en subgrupos control vs tratamiento (ph_inicial y ph_5min)

source(file = "mult_ggplot2.R")

graf.subg <- function(d){
        library(reshape)
        mdata <- melt(data = d, id=names(d)[1:10])
        mdata$variable <- factor(mdata$variable, levels=c("ph_5min","ph_inicial"))        
        
        library(ggplot2)
        ## diag densidad
        g1 <- ggplot(mdata, aes(x=value, fill=variable))+
                geom_density(color="gray60",alpha=0.6) + 
                labs(x="", y="",fill=paste("Grupo", 
                                  substring(mdata[1,"Subg_Cap_Amort"],1,1))) +
                xlim(c(min(mdata$value)-1,max(mdata$value)+1))+
                scale_fill_discrete(labels=c("Tratamiento","Control"))+
                theme(axis.text.y=element_blank(),axis.ticks.y=element_blank(),
                      axis.title.x=element_blank())
        ## diag cajas        
        g2 <- ggplot(mdata, aes(x=1, y=value, fill=variable))+
                geom_boxplot(outlier.colour = "red",color="gray50",width=0.5)+
                labs(y = paste("Ph Salival, Subgrupo: ", mdata[1,"Subg_Cap_Amort"],
                               " (", mdata[1, "Tipo_Vitamina"], ")", sep=""), 
                     x = "", fill=paste("Grupo", 
                                        substring(mdata[1,"Subg_Cap_Amort"],1,1)))+
                scale_fill_discrete(labels=c("Tratamiento","Control"))+
                ylim(c(min(mdata$value)-1,max(mdata$value)+1))+
                theme(axis.text.y=element_blank(),axis.ticks.y=element_blank())+
                coord_flip()
        
        ## multiples graficos ggplot2
        list_graf <- list(g1,g2)
        print(mult_ggplot(list_graf,2,1))
}

data_spl <- split(x = data, f=data[,"Subg_Cap_Amort"])
sapply(data_spl, graf.subg)


# analisis en grupos trat1 vs trat2 (Masticable y Efervesente)
graf.grup <- function(d){
        mdata <- d
        mdata[,"Subg_Cap_Amort"] <- factor(mdata[,"Subg_Cap_Amort"], 
                                           levels=c("A2","A1","B2","B1","C2","C1"))
        
        library(ggplot2)
        ## diag densidad
        g1 <- ggplot(mdata, aes(x=ph_5min, fill=Subg_Cap_Amort))+
                geom_density(color="gray60",alpha=0.6) + 
                labs(x="", y="",fill="Vitamina C") +
                xlim(c(min(mdata$ph_5min)-1,max(mdata$ph_5min)+1))+
                scale_fill_discrete(labels=c(paste(mdata[1,"Grupo_Cap_Amort"],
                   "2 (Efervescente)", sep=""),paste(mdata[1,"Grupo_Cap_Amort"],
                                                    "1 (Masticable)", sep="")))+
                theme(axis.text.y=element_blank(),axis.ticks.y=element_blank(),
                      axis.title.x=element_blank())
        ## diag cajas        
        g2 <- ggplot(mdata, aes(x=1, y=ph_5min, fill=Subg_Cap_Amort))+
                geom_boxplot(outlier.colour = "red",color="gray50",width=0.5)+
                labs(y = paste("Ph Salival, Grupo:", mdata[1,"Grupo_Cap_Amort"]),
                     x = "", fill="Vitamina C")+
                scale_fill_discrete(labels=c(paste(mdata[1,"Grupo_Cap_Amort"],
                   "2 (Efervescente)", sep=""),paste(mdata[1,"Grupo_Cap_Amort"],
                                                   "1 (Masticable)", sep="")))+
                ylim(c(min(mdata$ph_5min)-1,max(mdata$ph_5min)+1))+
                theme(axis.text.y=element_blank(),axis.ticks.y=element_blank())+
                coord_flip()
        
        ## multiples graficos ggplot2
        list_graf <- list(g1,g2)
        print(mult_ggplot(list_graf,2,1))
}

data_spl <- split(x = data, f=data[,"Grupo_Cap_Amort"])
sapply(data_spl, graf.grup)


## Analisis tratamientos por factor: capacidad buffer (A,B,C) 
mdata <- data
mdata[,"Subg_Cap_Amort"] <- factor(mdata[,"Subg_Cap_Amort"], 
                                levels=c("A1","B1","C1","A2","B2","C2"))

g1 <- ggplot(mdata, aes(x=ph_5min, fill=Tipo_Vitamina))+
        geom_density(color="gray60") + xlim(c(5,7))+ ylim(c(0,7.82))+
        facet_grid(Subg_Cap_Amort~.,margins = FALSE)+ 
        labs(x = "Ph Salival", y = "", fill="Tipo Vitamina C")+
        theme(axis.text.y=element_blank(),axis.ticks.y=element_blank(),
              legend.position="none")

g2 <- ggplot(mdata, aes(x=TRUE, y=ph_5min, fill=Tipo_Vitamina))+coord_flip()+
        geom_boxplot(outlier.colour = "red",color="gray50",width=0.55)+
        facet_grid(Subg_Cap_Amort~.,margins = FALSE)+ ylim(c(5.475,7))+
        labs(y = "Ph Salival", x = "", fill="Tipo Vitamina C")+
        theme(axis.text.y=element_blank(),axis.ticks.y=element_blank(),
              legend.position="bottom")
        
## multiples graficos ggplot2
list_graf <- list(g1,g2)
print(mult_ggplot(list_graf,1,2))





## Pruebas de Hipotesis para la diferencia de medias

## Subgrupos Control (ph_inicial) y Vit C masticable (ph_5min)
setwd("C:/Users/aperez/Desktop/proyecto-ph-salival")
list.files()
## informacion utilizada en el análisis
data <- read.table ("data_ph.txt", header=TRUE, sep="\t", dec=",")
data <- data[,1:12]
str(data)

data_spl <- split(x = data, f=data[,"Subg_Cap_Amort"])
d <- data_spl [[1]]
assign(paste(ph_inicial, d[1,"Subg_Cap_Amort"], sep="_"))

ph_inicial <- paste("ph_inicial", d[1,"Subg_Cap_Amort"], sep="_")
assign(ph_inicial, d[,"ph_inicial"])


dif.med.subg <- function(d){
        
        ## Prueba var iguales
        ph_inicial <- d[,"ph_inicial"]
        ph_5min <- d[,"ph_5min"]
        ig.var <- var.test(ph_inicial, ph_5min)
        
        print(paste("ph_inicial y ph_5min en Subgrupo:", 
                    d[1,"Subg_Cap_Amort"], sep=" "))
        print(ig.var)
        ## Rechazo var iguales
        var.igual <- TRUE
        if(ig.var$p.value<0.05) var.igual <- FALSE 
        ## Prueba medias muestras pequeñas, pareadas, var iguales o distintas
        ## desv estand poblacional desconocida
        dif.med <- t.test(ph_inicial, ph_5min, alternative="greater",
                             paired=TRUE, var.equal=var.igual, conf.level=0.95)
        
        print(paste("ph_inicial y ph_5min en Subgrupo:", 
                    d[1,"Subg_Cap_Amort"], sep=" "))
        print(dif.med)
}

data_spl <- split(x = data, f=data[,"Subg_Cap_Amort"])
sapply(data_spl, dif.med.subg)



## Comparacion (ph_5min) en Subgrupos Tratamiento: 
## Vit C masticable y Vit C Efervescente
setwd("C:/Users/aperez/Desktop/proyecto-ph-salival")
list.files()
## informacion utilizada en el análisis
data <- read.table ("data_ph.txt", header=TRUE, sep="\t", dec=",")
data <- data[,1:12]
str(data)

data_spl <- split(x = data, f=data[,"Grupo_Cap_Amort"])
d <- data_spl[[3]]

dif.med.grup <- function(d){
        ## Prueba var iguales
        tabl <- table(d[, "Subg_Cap_Amort"])
        g <- names(tabl)[which(tabl>0)]
        
        ph_5min_1 <- d[d[, "Subg_Cap_Amort"]==g[1], "ph_5min"]
        ph_5min_2 <- d[d[, "Subg_Cap_Amort"]==g[2], "ph_5min"]
        ig.var <- var.test(ph_5min_1, ph_5min_2)
        
        print(paste("ph_5min en Subgrupos:", g[1],"y", g[2], sep=" "))
        print(ig.var)
        ## Rechazo var iguales
        var.igual <- TRUE
        if(ig.var$p.value<0.05) var.igual <- FALSE 
        ## Prueba medias muestras pequeñas, pareadas, var iguales o distintas
        ## desv estand poblacional desconocida
        dif.med <- t.test(ph_5min_1, ph_5min_2, alternative="greater",
                          paired=FALSE, var.equal=var.igual, conf.level=0.95)
        
        print(paste("ph_5min en Subgrupos:", g[1],"y", g[2], sep=" "))
        print(dif.med)
}

data_spl <- split(x = data, f=data[,"Grupo_Cap_Amort"])
sapply(data_spl, dif.med.grup)

## grupo C muestra pequeña
data_spl <- split(x = data, f=data[,"Grupo_Cap_Amort"])
names(data_spl)
data_spl[[3]]
ks.test(ph_5min_1, ph_5min_2) 
wilcox.test(ph_5min_1, ph_5min_2) 
fix(ph_5min_1)

## ANOVA de dos factores
setwd("C:/Users/aperez/Desktop/proyecto-ph-salival")
list.files(,full.names=TRUE)
## informacion utilizada en el analisis
data <- read.table ("data_ph.txt", header=TRUE, sep="\t", dec=",")
data <- data[,1:12]
summary(data)

## Sin interacción de factores
dosf_anova <- aov(ph_5min ~ Grupo_Cap_Amort+Tipo_Vitamina, data = data)
summary(dosf_anova)

## Con interaccion de factores
idosf_anova <- aov(ph_5min ~ Grupo_Cap_Amort*Tipo_Vitamina, data = data)
summary(idosf_anova)

plot(idosf_anova) # diagnostic plots
