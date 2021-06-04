library(ggplot2)
library(dplyr)

datosNacionales<-read.csv("200511covid19mexico.csv",header=T,  sep=",")

view(datosNacionales)

header<- c("FECHA_SINTOMAS","FECHA_DEF","RESULTADO","ENTIDAD_RES")

aux1<-subset(datosNacionales,select = header)

SonoraFallecidos<- subset(aux1,ENTIDAD_RES == 26 & RESULTADO == 1 & FECHA_DEF != "9999-99-99")

order(SonoraFallecidos$FECHA_SINTOMAS)
SonoraFallecidos2<-subset(SonoraFallecidos,select =c("FECHA_SINTOMAS","FECHA_DEF","RESULTADO"))



write.csv(SonoraFallecidos2,"Tabla1.csv")



header2<-c("ENTIDAD_RES","TIPO_PACIENTE")

AuxiliarHospitalizados<-subset(datosNacionales, select =header2) 

View(AuxiliarHospitalizados)




hospitalizadosPorEntidades<-subset(AuxiliarHospitalizados, (ENTIDAD_RES == 8 | ENTIDAD_RES==19 | ENTIDAD_RES==21 | ENTIDAD_RES==26) & TIPO_PACIENTE == 2)

View(hospitalizadosPorEntidades)

write.csv(hospitalizadosPorEntidades,"Tabla2.csv")


hospitalizadosPorEntidadesGrafica<- table(hospitalizadosPorEntidades$ENTIDAD_RES)

View(hospitalizadosPorEntidadesGrafica)


 letras<-c("Chihuahua","Nuevo Leon","Puebla","Sonora")

DatosAgrupadosEntidad<- data.frame(hospitalizadosPorEntidadesGrafica)


ggplot(DatosAgrupadosEntidad,aes(x=letras ,y=Freq))+ geom_col()+ labs(title = "Pacientes por entidad",x="Entidad",y="Total")



Nacional <- subset(datosNacionales, select = c("FECHA_SINTOMAS", "RESULTADO"))

NacionalReducida <- subset(Nacional, RESULTADO == 1)

NacionalReducida$FECHA_SINTOMAS <- as.Date(BD_nacional$FECHA_SINTOMAS)

View(TotalPorDia)


ggplot(TotalPorDia, aes(x=FECHA_SINTOMAS,y=Total))+ geom_line(colour="Blue")+labs(title = "Casos Sobre Tiempo",x="Fecha")


TotalPorDia<-NacionalReducida%>%
  select(FECHA_SINTOMAS,RESULTADO)%>%
  group_by(FECHA_SINTOMAS)%>%
 summarise(Total=sum(RESULTADO))











