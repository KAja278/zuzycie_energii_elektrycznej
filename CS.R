#knn-regresja
#household
library(dplyr)
library(ggplot2)
library(MLmetrics)
library(randomForest)
library(rpart)
library(MASS)
library(rpart.plot)
library(corrplot)
library(tidyr)

df<-readtidyrdf<-read.table("D:/Users/kajas/Desktop/wizualizacja danych/household_power/household_power_consumption.txt", 
               sep=";", header=TRUE,stringsAsFactors=FALSE)
head(df)
str(df)
#praktycznie wszystkie cechy mają typ chr, należy więc zamienić to na numeric
#w danych nie występują bezpośrednio NA, tylko mamy do czynienia z brakami w postaci "?"
#po zastosowaniu as.numeric stringi które nie zawierają liczb zamienią się w NA



#zamiana chr na numeric
num_cols<-c("Global_active_power","Global_reactive_power","Voltage","Global_intensity",
            "Sub_metering_1","Sub_metering_2","Sub_metering_3")
df[num_cols]<-lapply(df[num_cols],function(x) as.numeric(x))
#rozdzielenie daty i godizny 
df$Datetime <- as.POSIXct(paste(df$Date, df$Time), format="%d/%m/%Y %H:%M:%S")

df<- df%>%
  mutate(
year=as.integer(format(df$Datetime, "%Y")),
month=as.integer(format(df$Datetime, "%m")),
day=as.integer(format(df$Datetime,"%d")),
hour=as.integer(format(df$Datetime, "%H")),
minute=as.integer(format(df$Datetime, "%M")),

is_we=ifelse(weekdays(as.Date(Datetime)) %in% c("Saturday","Sunday"),1,0),
light=ifelse(hour>=6 & hour<18, 1,0)
)

#ile NA
sum(is.na(df))
#ile % stanowia wartosci NA
sum(is.na(df))/nrow(df)
#ile wierszy
nrow(df)
#pomiar co 10 minut, nie ma potrzeby probkowac co minute


df_prob<-df%>% filter (minute%%10==0)
nrow(df_prob)
library(zoo)
df_prob[num_cols] <- lapply(df_prob[num_cols], function(x) na.approx(x, na.rm = FALSE))
nrow(df_prob)
head(df_prob)
tail(df_prob)

num_cols1<-c("Global_active_power","Global_reactive_power","Voltage","Global_intensity",
            "Sub_metering_1","Sub_metering_2","Sub_metering_3","light"
            )
#korelacja zmiennych 
sapply(df_prob, class)

korelacja<-cor(df_prob[num_cols])
kor_spear<-cor(df_prob[num_cols],method="spearman")
corrplot(korelacja,method="number")
corrplot(kor_spear,method="number")

#podstawowe statystyki 
library(dplyr)

basic_stats <- df_prob %>%
  summarise(
    GAP_min   = min(Global_active_power, na.rm = TRUE),
    GAP_mean  = mean(Global_active_power, na.rm = TRUE),
    GAP_max   = max(Global_active_power, na.rm = TRUE),
    GAP_sd    = sd(Global_active_power, na.rm = TRUE),
    
    V_min     = min(Voltage, na.rm = TRUE),
    V_mean    = mean(Voltage, na.rm = TRUE),
    V_max     = max(Voltage, na.rm = TRUE),
    V_sd      = sd(Voltage, na.rm = TRUE),
    
    GI_min    = min(Global_intensity, na.rm = TRUE),
    GI_mean   = mean(Global_intensity, na.rm = TRUE),
    GI_max    = max(Global_intensity, na.rm = TRUE),
    GI_sd     = sd(Global_intensity, na.rm = TRUE)
  )

print(basic_stats)




#EDA
range(df_prob$Datetime)
ggplot(df_prob,aes(x=Datetime,y=Global_active_power))+
  geom_line(linewidth=1.5,color="blue")+
  ggtitle("Zmiana Global_active_power z biegiem czasu")+
  labs(x="Czas",y="Global_active_power [kW]")

ggplot(df_prob,aes(x=factor(light),y=Sub_metering_3))+
  geom_boxplot()+
  ggtitle("Wartość Sub_metering_3 w zależności czy jest dzień czy noc")+
  labs(x="1-dzien(6-18), 0-noc(18-6)",
       y="Sub_metering_3 [Wh]")


ggplot(df_prob,aes(x=Global_intensity))+
  geom_histogram(bins=20,col="black",fill="cyan4")+
  ggtitle("Rozkład wartości Global_intensity")+
  labs(y="Ilość",x="Global_intensity [A]")
ggplot(df_prob,aes(x=Voltage))+
  geom_histogram(bins=20,col="black",fill="deepskyblue4")+
  ggtitle("Rozkład wartości Voltage")+
  labs(x="Voltage [V]",y="Ilość")
ggplot(df_prob,aes(x=Global_active_power))+
  geom_histogram(bins=20,col="black",fill="cadetblue4")+
  ggtitle("Rozkład wartości Global_active_power")+
  labs(x="Global_active_power [kW]",y="Ilość")

sub_metering<-c("Sub_metering_1", "Sub_metering_2", "Sub_metering_3")
boxplot(df_prob[sub_metering],
        main="Boxploty", xlab="Sub_metering",ylab="Wartość [Wh] ",
        col=c("steelblue", "darkseagreen3", "indianred3"))


plot(df_prob$Global_active_power,df_prob$Voltage,
     main="Zależności między zmiennymi Global_active_power oraz Voltage",
     xlab="Global_active_power [kW]",
     ylab="Voltage [V]",
     col="royalblue")


ggplot(df_prob,aes(x=factor(month),y=Global_active_power))+
  geom_boxplot(col="black",fill="royalblue3")+
  ggtitle("Rozkład Global_active_power ze względu na miesiąc")+
  labs(x="Miesiąc",y="Global_active_power [kW]")
ggplot(df_prob,aes(x=factor(year),y=Global_active_power))+
  geom_boxplot(fill="firebrick")+
  ggtitle("Rozkład Global_active_power ze względu na rok")+
  labs(x="Rok",y="Global_active_power [kW]")


par(mfrow = c(2, 2))
plot(
  aggregate(Global_active_power ~ year, data = df_prob, mean),
  type = "l",
  main = "Średnia wartość Global_active_power (roczna)",
  xlab = "Rok",
  ylab = "Global_active_power [kW]"
)
plot(
  aggregate(Global_active_power ~ month, data = df_prob, mean),
  type = "l",
  main = "Średnia wartość Global_active_power (miesięczna)",
  xlab = "Miesiąc",
  ylab = "Global_active_power [kW]"
)
plot(
  aggregate(Global_active_power ~ day, data = df_prob, mean),
  type = "l",
  main = "Średnia wartość Global_active_power (dzienna)",
  xlab = "Dzień",
  ylab = "Global_active_power [kW]"
)
plot(
  aggregate(Global_active_power ~ minute, data = df_prob, mean),
  type = "l",
  main = "Średnia wartość Global_active_power (minutowa)",
  xlab = "Minuta",
  ylab = "Global_active_power [kW]"
)




par(mfrow = c(2, 2))
plot(
  aggregate(Global_active_power ~ month,
            data = subset(df_prob, year == 2007),
            mean),
  type = "l",
  main = "Mean global active power by month (2007)",
  xlab = "Month",
  ylab = "Global Active Power"
)
plot(
  aggregate(Global_active_power ~ month,
            data = subset(df_prob, year == 2008),
            mean),
  type = "l",
  main = "Mean global active power by month (2008)",
  xlab = "Month",
  ylab = "Global Active Power"
)
plot(
  aggregate(Global_active_power ~ month,
            data = subset(df_prob, year == 2009),
            mean),
  type = "l",
  main = "Mean global active power by month (2009)",
  xlab = "Month",
  ylab = "Global Active Power"
)
plot(
  aggregate(Global_active_power ~ month,
            data = subset(df_prob, year == 2010),
            mean),
  type = "l",
  main = "Mean global active power by month (2010)",
  xlab = "Month",
  ylab = "Global Active Power"
)

acf(df_prob$Global_active_power, na.action = na.pass, main="ACF: Global_active_power")
pacf(df_prob$Global_active_power, na.action = na.pass, main="PACF: Global_active_power")
df_prob %>%
  group_by(hour) %>%
  summarise(GAP_mean = mean(Global_active_power, na.rm=TRUE)) %>%
  ggplot(aes(hour, GAP_mean)) + geom_line() +
  labs(title="Średnie zużycie wg godziny", x="Godzina", y="kW")

df_prob %>%
  mutate(dow = weekdays(as.Date(Datetime)),
         dow = factor(dow, 
                      levels = c("poniedziałek","wtorek","środa","czwartek",
                                 "piątek","sobota","niedziela"))) %>%
  group_by(dow) %>%
  summarise(GAP_mean = mean(Global_active_power, na.rm=TRUE)) %>%
  ggplot(aes(dow, GAP_mean)) +
  geom_col() +
  labs(title="Średnie zużycie wg dnia tygodnia", 
       x="Dzień tygodnia", y="kW")

#-------------------------------------------------------------------------------------------------------------------
#model bazowy
#zdecydowanie nie pasujący do typu danych szereg czasowy 
#odrzucamy 
x_train_sc <- scale(x_train)
center_vals <- attr(x_train_sc, "scaled:center")
scale_vals  <- attr(x_train_sc, "scaled:scale")

x_test_sc <- scale(x_test, center = center_vals, scale = scale_vals)

library(FNN)
library(MLmetrics)

k_grid <- c(3, 5, 7, 9, 15, 25, 50)

results <- data.frame(k = k_grid, RMSE = NA, MAE = NA)

for (i in seq_along(k_grid)) {
  k <- k_grid[i]
  pred <- FNN::knn.reg(train = x_train_sc, test = x_test_sc, y = y_train, k = k)$pred
  
  results$RMSE[i] <- RMSE(y_pred = pred, y_true = y_test)
  results$MAE[i]  <- MAE(y_pred = pred, y_true = y_test)
}

print(results)
best_k <- results$k[which.min(results$RMSE)]
cat("Najlepsze k wg RMSE =", best_k, "\n")

# Finalny model k-NN na najlepszym k
pred_knn <- FNN::knn.reg(train = x_train_sc, test = x_test_sc, y = y_train, k = best_k)$pred

rmse_knn <- RMSE(pred_knn, y_test)
mae_knn  <- MAE(pred_knn, y_test)
r2_knn   <- 1 - sum((y_test - pred_knn)^2) / sum((y_test - mean(y_test))^2)

cat("\n--- k-NN regresja (test = 4 obserwacje) ---\n")
cat("RMSE:", rmse_knn, "\n")
cat("MAE :", mae_knn, "\n")
cat("R^2 :", r2_knn, "\n")

# Tabelka: rzeczywiste vs predykcja (4 rekordy)
out_knn <- data.frame(
  Datetime = test$Datetime,
  y_true = y_test,
  y_pred_knn = pred_knn,
  error = y_test - pred_knn
)
print(out_knn)

ggplot(out_knn, aes(x = Datetime)) +
  geom_line(aes(y = y_true, color = "Rzeczywiste"), linewidth = 1) +
  geom_line(aes(y = y_pred_knn, color = "Predykcja k-NN"), linewidth = 1) +
  labs(title = "Predykcja k-NN – zbiór testowy (4 obserwacje)",
       y = "Global active power (kW)",
       color = "") +
  theme_minimal()
#-------------------------------------------------------------------------------------------------


#___________________________________________________________________________________________________________


df_prob <- df_prob %>% arrange(Datetime)

n <- nrow(df_prob)
stopifnot(n > 10)

train_end <- floor(0.75 * n)
val_end   <- n - 4

train <- df_prob[1:train_end, ]
val   <- df_prob[(train_end + 1):val_end, ]
test  <- df_prob[(val_end + 1):n, ]

nrow(train)
nrow(val)

stopifnot(nrow(test) == 4)

features <- c(
  "Global_reactive_power","Global_intensity","Voltage",
  "Sub_metering_1", "Sub_metering_2", "Sub_metering_3",
  "year", "month", "day", "hour", "minute", "light"
)

formula <- as.formula(
  paste("Global_active_power ~", paste(features, collapse = " + "))
)

train <- train %>% drop_na(Global_active_power, all_of(features))
val   <- val   %>% drop_na(Global_active_power, all_of(features))
test  <- test  %>% drop_na(Global_active_power, all_of(features))
#regresja wieloraka, automatyczne dobieranie zmiennych
lm_model <- lm(formula, data = train)
summary(lm_model)
summary(lm_model)$coefficients
library(car)
vif(lm_model)
formula(lm_model)
alias(lm_model)
# predykcja na walidacji
lm_val_pred <- predict(lm_model, newdata = val)

rmse_lm_val <- RMSE(lm_val_pred, val$Global_active_power)
mae_lm_val  <- MAE(lm_val_pred,  val$Global_active_power)
r2_lm_val   <- cor(lm_val_pred,  val$Global_active_power)^2

rmse_lm_val; mae_lm_val; r2_lm_val


par(mfrow=c(2,2))
plot(lm_model)
par(mfrow=c(1,1))

#drzewo regresyjne
tree_model <- rpart(
  formula,
  data = train,
  method = "anova",
  control = rpart.control(cp = 0.001, minsplit = 20, minbucket = 10)
)

printcp(tree_model)

# przycinanie wg minimalnego xerror
opt_cp <- tree_model$cptable[which.min(tree_model$cptable[, "xerror"]), "CP"]
tree_pruned <- prune(tree_model, cp = opt_cp)
summary(tree_pruned)
opt_cp
# wizualizacja drzewa 
rpart.plot(tree_pruned, type = 2, extra = 101, fallen.leaves = TRUE)

# predykcja na walidacji
tree_val_pred <- predict(tree_pruned, newdata = val)

rmse_tree_val <- RMSE(tree_val_pred, val$Global_active_power)
mae_tree_val  <- MAE(tree_val_pred,  val$Global_active_power)
r2_tree_val   <- cor(tree_val_pred,  val$Global_active_power)^2

rmse_tree_val; mae_tree_val; r2_tree_val
#porównanie na walidacji 
comparison_val <- data.frame(
  Model = c("Regresja wieloraka (LM)", "Drzewo regresyjne (rpart)"),
  RMSE  = c(rmse_lm_val, rmse_tree_val),
  MAE   = c(mae_lm_val,  mae_tree_val),
  R2    = c(r2_lm_val,   r2_tree_val)
)
comparison_val
#predykcja na zbiorze testowym (4 ostatnie obserwacje)
lm_test_pred   <- predict(lm_model,    newdata = test)
tree_test_pred <- predict(tree_pruned, newdata = test)

final_test <- data.frame(
  Datetime = test$Datetime,
  Actual = test$Global_active_power,
  LM_Pred = lm_test_pred,
  rpart_Pred = tree_test_pred,
  LM_Error = test$Global_active_power - lm_test_pred,
  rpart_Error = test$Global_active_power - tree_test_pred
)
final_test
#metryki 
rmse_lm_test   <- RMSE(lm_test_pred,   test$Global_active_power)
mae_lm_test    <- MAE(lm_test_pred,    test$Global_active_power)
r2_lm_test     <- cor(lm_test_pred,    test$Global_active_power)^2

rmse_tree_test <- RMSE(tree_test_pred, test$Global_active_power)
mae_tree_test  <- MAE(tree_test_pred,  test$Global_active_power)
r2_tree_test   <- cor(tree_test_pred,  test$Global_active_power)^2

data.frame(
  Model = c("LM (test=4)", "rpart (test=4)"),
  RMSE = c(rmse_lm_test, rmse_tree_test),
  MAE  = c(mae_lm_test,  mae_tree_test),
  R2   = c(r2_lm_test,   r2_tree_test)
)
#wykres rzeczywiste a predykcyjne
plot_df <- final_test %>%
  tidyr::pivot_longer(cols = c("LM_Pred", "rpart_Pred"),
                      names_to = "Model",
                      values_to = "Predicted")

ggplot(plot_df, aes(x = Datetime)) +
  geom_line(aes(y = Actual, color = "Rzeczywiste"), linewidth = 1) +
  geom_line(aes(y = Predicted, color = Model), linewidth = 1) +
  labs(title = "Predykcja na zbiorze testowym (4 obserwacje)",
       y = "Global_active_power [kW]",
       color = "") +
  theme_minimal()


