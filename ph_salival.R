

## informacion utilizada en el analisis
data <- read.table ("data_ph.txt", header=TRUE, sep="\t", dec=",")
data <- data[,1:12]
str(data)


# analisis en subgrupos control vs tratamiento (inicial y 5 min)

source(file = "mult_ggplot2.R")

graf.subg <- function(d){
        library(reshape)
        mdata <- melt(data = d, id=names(d)[1:10])
        mdata$variable <- factor(mdata$variable, levels=c("ph_5min","ph_inicial"))        
        
        library(ggplot2)
        ## diag densidad
        g1 <- ggplot(mdata, aes(x=value, fill=variable))+
                geom_density(color="gray60",alpha=0.6) + 
                labs(x="", y="",fill="Ph Salival") +
                xlim(c(min(mdata$value)-1,max(mdata$value)+1))+
                scale_fill_discrete(labels=c("Control","Tratamiento"))+
                theme(axis.text.y=element_blank(),axis.ticks.y=element_blank(),
                      axis.title.x=element_blank())
        ## diag cajas        
        g2 <- ggplot(mdata, aes(x=1, y=value, fill=variable))+
                geom_boxplot(outlier.colour = "red",color="gray50",width=0.5)+
                labs(y = "Ph Salival", x = "", fill="Ph Salival")+
                scale_fill_discrete(labels=c("Control","Tratamiento"))+
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
                scale_fill_discrete(labels=c("Efervescente","Masticable"))+
                theme(axis.text.y=element_blank(),axis.ticks.y=element_blank(),
                      axis.title.x=element_blank())
        ## diag cajas        
        g2 <- ggplot(mdata, aes(x=1, y=ph_5min, fill=Subg_Cap_Amort))+
                geom_boxplot(outlier.colour = "red",color="gray50",width=0.5)+
                labs(y = "Ph Salival", x = "", fill="Vitamina C")+
                scale_fill_discrete(labels=c("Efervescente","Masticable"))+
                ylim(c(min(mdata$ph_5min)-1,max(mdata$ph_5min)+1))+
                theme(axis.text.y=element_blank(),axis.ticks.y=element_blank())+
                coord_flip()
        
        ## multiples graficos ggplot2
        list_graf <- list(g1,g2)
        print(mult_ggplot(list_graf,2,1))
}

data_spl <- split(x = data, f=data[,"Grupo_Cap_Amort"])
sapply(data_spl, graf.grup)


## Analisis tratamientos por factor capacidad buffer (A,B,C) 
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

