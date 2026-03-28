library(tidyr)
library(dplyr)
library(ggplot2)
install.packages("ncdf4")
library(ncdf4)

ncin<-nc_open("cru_ts4.09.1901.2024.tmp.dat.nc")

print(ncin)

lon <- ncvar_get(ncin,"lon")
head(lon) 

nlon <- dim(lon)

nlon

lat <- ncvar_get(ncin,"lat")
head(lat)
nlat <- dim(lat)
nlat
time <- ncvar_get(ncin,"time")
head (time) 

nt <-dim(time) 
nt 

tunits <- ncatt_get(ncin,"time","units") 
tunits

ncatt_get(ncin,0,"title")
ncatt_get(ncin,0,"institution")
ncatt_get(ncin,0,"references")

w1<-which(lon==22.75) 
w2<-which(lat==54.25) 

miejsc_tmp<-ncvar_get(ncin,"tmp",start=c(w1,w2,1),count=c(1,1,1488))
head(miejsc_tmp)
plot(1:1488,miejsc_tmp,t="l") 


tmp_matrix <- matrix(miejsc_tmp, ncol = 12, byrow = TRUE)
head(tmp_matrix)

miejsc_tmp_df <- data.frame(rok=1901:2024, tmp_matrix)
head(miejsc_tmp_df)

colnames(miejsc_tmp_df) <- c("rok", "I", "II", "III", "IV", "V", "VI",
                             "VII", "VIII", "IX", "X", "XI", "XII")
View(miejsc_tmp_df)

miejsc_tmp_df<-round(miejsc_tmp_df, digits=1)
View(miejsc_tmp_df)





ncin_p<-nc_open("cru_ts4.09.1901.2024.pre.dat.nc")
miejsc_pre<-ncvar_get(ncin_p,"pre",start=c(w1,w2,1),count=c(1,1,1488))
head(miejsc_pre)
plot(1:1488,miejsc_pre,t="l")



pre_matrix <- matrix(miejsc_pre, ncol = 12, byrow = TRUE)
head(pre_matrix)

miejsc_pre_df <- data.frame(rok=1901:2024, pre_matrix)
head(miejsc_pre_df)
colnames(miejsc_pre_df) <- c("rok", "I", "II", "III", "IV", "V", "VI",
                             "VII", "VIII", "IX", "X", "XI", "XII")
View(miejsc_pre_df)

miejsc_pre_df<-round(miejsc_pre_df, digits=1)
View(miejsc_pre_df)

srednie_tmp <- colMeans(miejsc_tmp_df[, -1], na.rm = TRUE)
srednie_pre <- colMeans(miejsc_pre_df[, -1], na.rm = TRUE)

miesiace <- names(srednie_tmp)
srednie_df <- data.frame(
  miesiac = factor(miesiace, levels = miesiace),
  temperatura = as.numeric(round(srednie_tmp, digits=1)),
  opady = as.numeric(round(srednie_pre, digits=1))
)
View(srednie_df)

library(patchwork)
library(berryFunctions)

#zad 1
climateGraph(
  temp = srednie_df$temperatura,
  rain = srednie_df$opady,
  labs = levels(srednie_df$miesiac),
  main = "Klimatogram dla miejscowosci",
  coltemp = "red",
  colrain = "blue"
)
#opis
#Wyraznie widac ze jest to miejscowosc nalezaca do umierkowanej strefy klimatycznej


boxplot(miejsc_tmp_df[,2:13], border="blue", col="orchid", main="Rozklad temperatur", xlab="miesiace", ylab="tmp [st.C]")
summary(miejsc_tmp_df)
View(summary(miejsc_tmp_df))
boxplot(miejsc_pre_df[,2:13], border="blue", col="orchid", main="Rozklad opadow", xlab="miesiace", ylab="opady [mm]")
summary(miejsc_tmp_df)
#opis

#zad 2
library(lattice)
wyk1<-ggplot(miejsc_tmp_df,aes(I))+
  geom_histogram(breaks=seq(min(miejsc_tmp_df$I), max(miejsc_tmp_df$I), 1.5),fill="pink",col="black")+
  labs(title="Styczen",x="Temperatura", y="Czestotliowsc wystapien")+
  theme_bw()+
  theme(text=element_text(size=11))

wyk2<-ggplot(miejsc_tmp_df,aes(IV))+
  geom_histogram(breaks=seq(min(miejsc_tmp_df$IV), max(miejsc_tmp_df$IV), 0.9),fill="pink",col="black")+
  labs(title="Kwiecien",x="Temperatura", y="Czestotliowsc wystapien")+
  theme_bw()+
  theme(text=element_text(size=11))

wyk3<-ggplot(miejsc_tmp_df,aes(VII))+
  geom_histogram(breaks=seq(min(miejsc_tmp_df$VII), max(miejsc_tmp_df$VII), 0.6),fill="pink",col="black")+
  labs(title="Lipiec",x="Temperatura", y="Czestotliowsc wystapien")+
  theme_bw()+
  theme(text=element_text(size=11))

wyk4<-ggplot(miejsc_tmp_df,aes(X))+
  geom_histogram(breaks=seq(min(miejsc_tmp_df$X), max(miejsc_tmp_df$X), 0.6),fill="pink",col="black")+
  labs(title="Pazdziernik",x="Temperatura", y="Czestotliowsc wystapien")+
  theme_bw()+
  theme(text=element_text(size=11))

grid.arrange(wyk1, wyk2, wyk3, wyk4, ncol=2)

wyk5<-ggplot(miejsc_pre_df,aes(I))+
  geom_histogram(breaks=seq(min(miejsc_pre_df$I), max(miejsc_pre_df$I), 8),fill="cadetblue3",col="black")+
  labs(title="Styczen",x="Opady", y="Czestotliowsc wystapien")+
  theme_bw()+
  theme(text=element_text(size=11))

wyk6<-ggplot(miejsc_pre_df,aes(IV))+
  geom_histogram(breaks=seq(min(miejsc_pre_df$IV), max(miejsc_pre_df$IV), 8),fill="cadetblue3",col="black")+
  labs(title="Kwiecien",x="Opady", y="Czestotliowsc wystapien")+
  theme_bw()+
  theme(text=element_text(size=11))


wyk7<-ggplot(miejsc_pre_df,aes(VII))+
  geom_histogram(breaks=seq(min(miejsc_pre_df$VII), max(miejsc_pre_df$VII), 12.5),fill="cadetblue3",col="black")+
  labs(title="Lipiec",x="Opady", y="Czestotliowsc wystapien")+
  theme_bw()+
  theme(text=element_text(size=11))

wyk8<-ggplot(miejsc_pre_df,aes(X))+
  geom_histogram(breaks=seq(min(miejsc_pre_df$X), max(miejsc_pre_df$X), 12.5),fill="cadetblue3",col="black")+
  labs(title="Pazdziernik",x="Opady", y="Czestotliowsc wystapien")+
  theme_bw()+
  theme(text=element_text(size=11))
wyk8
grid.arrange(wyk5, wyk6, wyk7, wyk8, ncol=2)

#opis

#zad 3
wy1<-ggplot(miejsc_tmp_df, aes(rok,I))+
  geom_line(color="blueviolet")+
  geom_smooth(method="lm")
lm1<-lm(miejsc_tmp_df$I~miejsc_tmp_df$rok)
summary(lm1)
wy2<-ggplot(miejsc_tmp_df, aes(rok,IV))+
  geom_line(color="blueviolet")+
  geom_smooth(method="lm")
lm2<-lm(miejsc_tmp_df$IV~miejsc_tmp_df$rok)
summary(lm2)
wy3<-ggplot(miejsc_tmp_df, aes(rok,VII))+
  geom_line(color="blueviolet")+
  geom_smooth(method="lm")
lm3<-lm(miejsc_tmp_df$VII~miejsc_tmp_df$rok)
summary(lm3)
wy4<-ggplot(miejsc_tmp_df, aes(rok,X))+
  geom_line(color="blueviolet")+
  geom_smooth(method="lm")
lm4<-lm(miejsc_tmp_df$X~miejsc_tmp_df$rok)
summary(lm4)
grid.arrange(wy1, wy2, wy3, wy4, ncol=2)

wy5<-ggplot(miejsc_pre_df, aes(rok,X1))+
  geom_line(color="palegreen4")+
  geom_smooth(method="lm")+
  labs(y="I")
wy5
lm5<-lm(miejsc_pre_df$X1~miejsc_pre_df$rok)
summary(lm5)
wy6<-ggplot(miejsc_pre_df, aes(rok,X4))+
  geom_line(color="palegreen4")+
  geom_smooth(method="lm")+
  labs(y="IV")
lm6<-lm(miejsc_pre_df$X4~miejsc_pre_df$rok)
summary(lm6)
wy7<-ggplot(miejsc_pre_df, aes(rok,X7))+
  geom_line(color="palegreen4")+
  geom_smooth(method="lm")+
  labs(y="VII")
lm7<-lm(miejsc_pre_df$X7~miejsc_pre_df$rok)
summary(lm7)
wy8<-ggplot(miejsc_pre_df, aes(rok,X10))+
  geom_line(color="palegreen4")+
  geom_smooth(method="lm")+
  labs(y="X")
lm8<-lm(miejsc_pre_df$X10~miejsc_pre_df$rok)
summary(lm8)
grid.arrange(wy5, wy6, wy7, wy8, ncol=2)
#opis

#zad 4

library(dplyr)
col_temp <- names(miejsc_tmp_df)[8]
srednia_dla_dekady <- function(df, start_year, end_year) {
  df %>%
    filter(rok > start_year, rok <= end_year) %>%
    summarise(mean_temp = mean(.data[[col_temp]], na.rm = TRUE)) %>%
    pull(mean_temp)
}
start_years <- seq(1900, 2010, by = 10)
dekady_temp <- sapply(start_years, function(start) {
  end <- start + 9  
  srednia_dla_dekady(miejsc_tmp_df, start, end)
})

x <- 1:length(dekady_temp)
y <- dekady_temp

plot(x, y, type = "o", col = "blue", pch = 16,
     xaxt = "n", xlab="", 
     ylab = "Œrednia temperatura",
     main = "Œrednie temperatury w kolejnych dekadach")
etykiety <- paste(start_years + 1, start_years + 10, sep = "-")

axis(1, at = x, labels = FALSE)  
text(x, par("usr")[3] - 0.1, labels = etykiety, srt = 45, adj = 1, xpd = TRUE, cex = 0.8)

                               
#opis


