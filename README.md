# ProgramacionR
Proyecto de programacion con r curso bedu

Mi proyecto involucra un analisis de produccion y consumo de energia en Mexico 
El proposito es 
-Conocer el tipo de energia que es mas viable desarrollar en mexico para una migracion al uso de energias limpias.
-Conocer datos que ayuden a planificar el desarrollo de nuevas energias.
-Estimar o pronosticar la inversion y energia requerida para el cambio.
-Identificar los estados con mayor y menor avance.

# LLAMADO A LA DB Y LISTADO DE TABLAS
library(DBI)
library(RMySQL)
MyDataBase <- dbConnect(
  drv = RMySQL::MySQL(),
  dbname = "miproyecto",
  host = "localhost",
  username = "root",
  password = "hola20")
dbListTables(MyDataBase)

# OBTENER DATOS 
Energia_Generada <- dbGetQuery(MyDataBase, "SELECT 
    MegaWatt_hourTotal, MonthDescription, EnergyTypeName
FROM
    genbrutatech
        JOIN
    months ON Month_Year = months.Month_YearID
        JOIN
    energytypes ON genbrutatech.EnergyTypeID = energytypes.EnergyTypeID;")
View(Energia_Generada)
Energia_Generada <- na.omit(Energia_Generada)
library(lubridate)

EnergiaxEstado <- dbGetQuery(MyDataBase, "SELECT
MegaWatt_hourTotal, MonthDescription, StateName
FROM
genbruperstate
JOIN
months ON genbruperstate.Month_YearID = months.Month_YearID
JOIN
states ON states.State_ID = genbruperstate.State_ID;")
View(EnergiaxEstado)
EnergiaxEstado <- na.omit(EnergiaxEstado)
ConsumoEnergia <- dbGetQuery(MyDataBase,"SELECT 
	MegaWatt_hour, QuantityUsers, Sector, CentsPerKW_hour, MonthDescription
FROM 
	comerciointer
		JOIN
	months ON comerciointer.Month_YearID = months.Month_YearID;")
View(ConsumoEnergia)

#GRAFICA DE DATOS
library(ggplot2)
ggplot(Energia_Generada, aes(x=MonthDescription, y = MegaWatt_hourTotal)) +
  geom_point() +
  facet_wrap("EnergyTypeName") +
  theme_gray()    +
  xlab('Month-Year') +  
  ylab('MegaWatts Generados') +
  ggtitle("Energia Generada por tecnologia")

ggplot(EnergiaxEstado, aes(x=StateName, y = MegaWatt_hourTotal)) +
  geom_point() +
  theme_classic()    +
  xlab('Estado') +  
  ylab('MegaWatts Generados') +
  ggtitle("Energia Generada por Estado")

plot(ConsumoEnergia)

##MODELO ARIMA
FechasConsumoEnergia <- ym(ConsumoEnergia$MonthDescription)
FechasConsumoEnergia <- na.omit(ConsumoEnergia)
ConsumoEnergia.ts <- ts(FechasConsumoEnergia[,5], start = 2005, freq = 12)

print(ConsumoEnergia.ts)

library(forecast)
arima560 = auto.arima(ConsumoEnergia.ts, stepwise = F,
                      approximation = F,
                      trace = T)
print(arima560)
proy = forecast(arima560,d=1,D=1, h = 24, level = c(12))
plot(proy)
autoplot(proy)
checkresiduals(proy)
