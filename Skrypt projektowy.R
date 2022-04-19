# Zainstalowanie, oraz wczytanie bibliotek odpowiadaj¹cych za importowanie plików xlsx i operacje na tekstach.

#install.packages("readxl")
library("readxl")
#install.packages("dplyr")
library("dplyr")
#install.packages("tidyr")
library("tidyr")
#install.packages("stringi")
library("stringi")

# Sprawdzenie obecnego katalogu roboczego, ewentualne przypisanie nowego, wczytanie tabeli z analizowanymi danymi. 
# Ewentualnie mo¿na wybraæ plik rêcznie.
getwd()
# setwd("TWÓJ_KATALOG_ROBOCZY_Z_PROJEKTEM") # <- Ustawienie domyœlnego katalogu roboczego dla plikóW projektu
my_data <- read_excel("Food_dataset.xlsx")
# my_data <- read_excel(file.choose()) # Wybór pliku.

# Zamiana nazw w ramce na bardziej przejrzyste i czytelne dla przeciêtnego u¿ytkownika, wyœwietlenie informacji o niej.
colnames(my_data) 
colnames(my_data) <- toupper(c(colnames(my_data[1:3]), "efsa_code", "Product_name", paste("GROUP_", 1:3, sep = ""), colnames(my_data[9]), "nutrient_name", colnames(my_data)[11], "amount"))
colnames(my_data)



# Poni¿ej zmieniamy nazwy czêœci sk³adników poprzez usuniêcie zbêdnych wyrazów, skrócenie odpowiednio zbyt d³ugich nazw.

All_Nutrients <- my_data$NUTRIENT_NAME
unique(All_Nutrients)
All_Nutrients <- gsub(c("Total ||, total"), "", All_Nutrients)
unique(All_Nutrients) 
# Powy¿ej Usuwamy napisy "total" razem z przecinkami w celu przejrzystoœci nazwy danych w póŸniejszych funkcjach.

# Poni¿ej skrócimy d³ugie nazwy sk³adników (usuniemy dopiski po znaku œrednika).
# W celu mo¿liwie najkrótszego czasu zamiany obiektów pos³u¿ymy siê zmianami typów zmiennych na czynnikowe, oraz ich poziomami.
Factor_All_Nutrients= as.factor(All_Nutrients)
levels(Factor_All_Nutrients)
Names <- strsplit(levels(Factor_All_Nutrients), split <- ";") # W poziomach (wszystkich nazwach) sk³adników usunê napisy po œredniku.
Names
New_Names <- c()
for (i in 1:length(Names)) New_Names <- c(New_Names,Names[[i]][1]) # Pomijam wszystkie wyrazy po œredniku dla ka¿dego elementu.
New_Names 

levels(Factor_All_Nutrients) <- New_Names
levels(Factor_All_Nutrients)

# W celu powrotnego przypisania zmodyfikowanych rekordów o ramki zamieniamy ich typ na wektorowy.
All_Nutrients <- as.vector(Factor_All_Nutrients) 
unique(All_Nutrients)
my_data$NUTRIENT_NAME <- All_Nutrients
unique(my_data$NUTRIENT_NAME)



# Wyœwietlimy ró¿ne informacje o tabeli, na której operuje dalsza czêœæ kodu w danym projekcie.
summary(my_data)
head(my_data)
str(my_data)


# Lista pañstw, których dane analizujemy.
(Countries <- unique(my_data$COUNTRY))


# Rodzaje produktów ¿ywnoœciowych wraz z ich poszczególnymi grupami.
ProductGroups <- distinct(select(my_data,EFSA_CODE,GROUP_1,GROUP_2,GROUP_3))
FirstGroup <- distinct(select(my_data,GROUP_1))
SecondGroup <- distinct(select(my_data,GROUP_1))
ThirdGroup <- distinct(select(my_data,GROUP_3))
Products <- distinct(select(my_data,PRODUCT_NAME))
head(ProductGroups)
head(FirstGroup)
head(SecondGroup)
head(ThirdGroup)
head(Products)

# Witaminy i inne sk³adniki od¿ywcze.
(Nutrients <- unique(my_data$NUTRIENT_NAME))


# Pod postaci¹ danej funkcji poka¿emy teraz listy analizowanych pañstw, oraz sk³adników mineralnych.
# Z ich pomoc¹, z u¿yciem za jej pomoc¹ mo¿na sprawdziæ zawartoœæ danego sk³adnika w produktach danego pañstwa.
# Dziêki posortowaniu wyników mo¿na tak¿e sprawdziæ, w którym kraju zawartoœæ danego sk³adnika w produktach jest stosunkowo najwiêksza/najmniejsza.
# Istnieje w tej funkcji tak¿e mo¿liwoœæ utworzenia pliku wyjœciowego csv (tabela Excela).
# Nastêpnie poka¿ê dzia³anie tej funkcji na kilku przyk³adach.

Nutrient_Contenct_In_Country <- function(nutrient_name, country, Make_Table = FALSE, Draw_plot = FALSE, Draw_plot_In_R = FALSE){
  Result <- filter(my_data,COUNTRY == country,NUTRIENT_NAME == nutrient_name)
  
  # Poni¿ej posortujemy elementy wzglêdem zawartoœci sk³adnika w produktach (malej¹co)
  SortedResult <- sort(Result$AMOUNT,decreasing=TRUE)
  kolejnosc <- (match(SortedResult,Result$AMOUNT))
  SortedResult <- as.data.frame(as.matrix(Result)[kolejnosc,c(3,5,6,7,8,10,11,12)]) 
  # Powy¿ej sortujemy w prosty sposób wiersze danej ramki, oraz jednoczeœnie wybieramy odpowiednie kolumny.
  
  # Stworzymy odpowiedni plik typu csv (Tabela dla programu Excel), gdy jest takie ¿yczenie (w pliku wyjœciowym).
  if (Make_Table){
    File_name <- paste(nutrient_name, "contenct in", country," .csv", sep = " ")
    write.csv2(SortedResult, file <- File_name)
    cat("Utworzono plik o nazwie:", File_name, "\n\n", sep = " ")
  }
  
  return (SortedResult)
}

Nutrients
Countries

Calcium_Contenct_In_Finland <- Nutrient_Contenct_In_Country(Nutrients[1], Countries[1], Make_Table = FALSE)
head(Calcium_Contenct_In_Finland)

Calcium_Contenct_In_Sweden <- Nutrient_Contenct_In_Country(Nutrients[1], Countries[2], Make_Table = TRUE)
head(Calcium_Contenct_In_Sweden)

Iron_Contenct_In_Germany <- Nutrient_Contenct_In_Country(Nutrients[2], Countries[4], Make_Table = TRUE)
head(Iron_Contenct_In_Germany)

Magnesium_Contenct_In_UnitedKingdom <- Nutrient_Contenct_In_Country(Nutrients[3], Countries[6])
head(Magnesium_Contenct_In_UnitedKingdom)




# Z pomoc¹ tej funkcji mo¿na sprawdziæ, w jakim produkcie danego kraju zawartoœæ danego sk³adnika jest najwiêksza, b¹dŸ najmniejsza.
Nutrient_Contenct_In_Country_MinMax <- function(nutrient_name, country){
  Result <- Nutrient_Contenct_In_Country(nutrient_name, country)
  
  cat("Maksymalna zawartoœæ:",nutrient_name,"w:",country,"wystêpuje dla produktu:", as.vector(Result[1,]$PRODUCT_NAME),
      ".\nPoni¿ej szczegó³y dla tego produktu w danym pañstwie:\n\n", sep = " ")
  print(Result[1,])
  
  cat("\nMinimalna zawartoœæ:", nutrient_name,"w:", country, "wystêpuje dla produktu:", 
      as.vector(Result[length(Result[,8]),]$PRODUCT_NAME),".\nPoni¿ej szczegó³y dla tego produktu w danym pañstwie:\n\n", sep = " ")
  print(Result[length(Result[,8]),])
}

# Lista sk³adników i pañstw z bazy danych, oraz przyk³ady zastosowania funkcji (najproœciej na podstawie elementów listy).
Nutrients
Countries

Nutrient_Contenct_In_Country_MinMax_Italy <- Nutrient_Contenct_In_Country_MinMax(Nutrients[1], Countries[3])
Nutrient_Contenct_In_Country_MinMax_Italy <- Nutrient_Contenct_In_Country_MinMax(Nutrients[2], Countries[6])




# Z u¿yciem poni¿szej funkcji mo¿na sprawdziæ zsumowan¹ zawartoœæ danego sk³adnika we wszystkich produktach ka¿dego z pañstw.
# Dodamy tak¿e dodatkowy wiersz z sum¹ dla wszystkich zawartych powy¿ej pañstw dla porównania wyników.
# Wartoœci zawartoœci odpowiednich sk³adników zosta³y posortowane malej¹co, mo¿na wiêc odczytaæ, w którego kraju produktach jest ich stosunkowo najwiêcej/najmniej.
# W celu zoptymalizowania kodu, oraz zmniejszenia jego iloœci wykorzystam poprzednio stworzon¹ funkcjê w nowej funkcji.
# Istnieje w tej funkcji tak¿e mo¿liwoœæ utworzenia pliku wyjœciowego csv (tabela Excela), oraz wykresu zarówno w RStudio, jak i w pliku wyjœciowym.

Total_Nutrient_In_Products <- function(Nutrient, Make_Table = FALSE, Draw_plot_In_R = FALSE, Draw_plot = FALSE){
  Records=c()
  for (i in 1:length(Countries)){
    x1 <- Nutrient_Contenct_In_Country(Nutrient,Countries[i]) 
    # Powy¿ej wykorzystaliœmy poprzedni¹ funkcjê "wycinaj¹c¹" jedynie informacje dotycz¹ce interesuj¹cego sk³adnika w interesuj¹cym kraju.
    Records=c(Records,sum(c(as.numeric(x1$AMOUNT))))
  }
  
  # Poni¿ej dodajemy nowy rekord zawieraj¹cy sumê dla wszystkich badanych pañstw.
  NewCountry <- "All"
  Countries <- c(Countries, NewCountry)
  NewRecord <- sum(Records)
  Records <- c(Records, NewRecord)
  Units <- rep(x1$UNIT[1], length(Countries)) # Jednostki, w których podajemy dane (takie same dla jednego sk³adnika).
  
  Result <- data.frame(Countries, Records, UNIT <- Units)
  colnames(Result)[2] <- paste(Nutrient, "_total", sep = "") # W nazwie kolumny oznaczaj¹cej sk³adnik dopisujê nazwê funkcji (suma).
  
  # Poni¿ej posortujemy elementy wzglêdem zawartoœci sk³adnika w produktach ró¿nych krajów (malej¹co).
  SortedResult <- sort(Result[[2]], decreasing <- TRUE)
  kolejnosc <- (match(SortedResult, Result[[2]]))
  SortedResult <- as.data.frame(as.matrix(Result)[kolejnosc,]) # Posortujê w prosty sposób wiersze danej ramki.
  
  # Stworzymy odpowiedni plik typu csv (Tabela dla programu Excel), gdy jest takie ¿yczenie (w pliku wyjœciowym).
  if (Make_Table){
    File_name <- paste("Total",Nutrient, "In Products", ".csv", sep = " ")
    write.csv2(SortedResult, file <- File_name)
    cat("Utworzono plik o nazwie:", File_name, "\n\n", sep = " ")
  }
  
  # Narysujemy odpowiedni wykres, gdy jest takie ¿yczenie (w otworzonym programie RStudio, lub na zewn¹trz w pliku wyjœciowym).
  if (Draw_plot){
    File_name <- paste("Total",Nutrient,"in_products",".jpg",sep = " ")
    mean_plot = barplot(as.numeric(SortedResult[[2]]), xlab = "Country name", ylab = colnames(SortedResult)[2],names.arg = NULL)
    text(mean_plot,srt = 90,labels = SortedResult[[1]],par("usr")[2], adj = c(-0.05,0.4))
    dev.off()
    cat("Utworzono wykres o nazwie:", File_name, "\n\n", sep = " ")
  }
  
  if (Draw_plot_In_R){
    mean_plot = barplot(as.numeric(SortedResult[[2]]), xlab = "Country name", ylab = colnames(SortedResult)[2],names.arg = NULL)
    text(mean_plot,srt = 90,labels = SortedResult[[1]],par("usr")[2], adj = c(-0.05,0.4)) }
  
  return (SortedResult)
}

# Lista sk³adników z bazy danych, oraz przyk³ady zastosowania funkcji (najproœciej na podstawie elementów listy).
Nutrients

Total_Nutrient_In_Products(Nutrients[1], Make_Table = TRUE, Draw_plot_In_R = TRUE, Draw_plot = TRUE)
Total_Nutrient_In_Products(Nutrients[9], Make_Table = FALSE, Draw_plot_In_R = TRUE, Draw_plot = FALSE)
Total_Nutrient_In_Products(Nutrients[14])




# Z u¿yciem poni¿szej funkcji mo¿na sprawdziæ œredni¹ zawartoœæ danego sk³adnika w produktach ka¿dego z pañstw.
# W celu zmniejszenia iloœci kodu wykorzystamy poprzednio stworzon¹ funkcjê w poni¿szej funkcji.
# Wyniki podamy w porz¹dku malej¹cym wzglêdem zawartoœci danego sk³adnika w poszczególnych pañstwach.
# Dziêki ma³ej liczbie pañstw mo¿na takze od razu poznaæ, w którym z nich zawartoœæ danego sk³adnika w produktach jest najwiêksza / najmniejsza.
# Dodamy tak¿e dodatkowy wiersz ze œredni¹ dla wszystkich pañstw w bazie danych dla porównania wyników.
# Istnieje w tej funkcji tak¿e mo¿liwoœæ utworzenia pliku wyjœciowego csv (tabela Excela), oraz wykresu zarówno w RStudio, jak i w pliku wyjœciowym.

Mean_Of_Nutrient_In_Products <- function(Nutrient, Make_Table = FALSE, Draw_plot_In_R = FALSE, Draw_plot = FALSE){
  SumOfProducts <- c()
  Records <- c()
  Units <- c()
  for (i in 1:length(Countries)){
    x1 <- Nutrient_Contenct_In_Country(Nutrient, Countries[i]) 
    # Powy¿ej wykorzystaliœmy poprzedni¹ funkcjê "wycinaj¹c¹" jedynie informacje dotycz¹ce interesuj¹cego nas sk³adnika w odpowiednich krajach.
    SumOfProducts <- c(SumOfProducts, sum(as.numeric(c(x1$AMOUNT))))
  }
  AmountOfRecords <- dim(x1)[1]
  Records=c(Records, SumOfProducts/AmountOfRecords)
  
  # Poni¿ej dodajemy nowy rekord zawieraj¹cy œredni¹ dla wszystkich badanych pañstw.
  NewCountry <- "All"
  Countries <- c(Countries, NewCountry)
  NewRecord <- mean(Records)
  Records <- round(c(Records, NewRecord), digits <- 4) # Zaokr¹glimy wszystkie wyniki do 3 cyfr znacz¹cych.
  Units <- rep(x1$UNIT[1], length(Records)) # Jednostki, w których podajemy dane.
  
  Result <- data.frame(Countries, Records, UNIT = Units)
  colnames(Result)[2]=paste(Nutrient,"mean", sep = "") # W nazwie kolumny oznaczaj¹cej sk³adnik dopisujê nazwê funkcji (Œrednia)
  
  # Poni¿ej posortujemy elementy wzglêdem zawartoœci sk³adnika w produktach ró¿nych krajów (malej¹co)
  Sequence_Of_Rows <- sort(Result[[2]][1:dim(Result)[1]-1], decreasing <- TRUE)
  kolejnosc <- (match(Sequence_Of_Rows,Result[[2]]))
  SortedResult <- as.data.frame(as.matrix(Result)[c(8, kolejnosc),])
  
  
  # Stworzymy odpowiedni plik typu csv (Tabela dla programu Excel), gdy jest takie ¿yczenie (w pliku wyjœciowym).
  if (Make_Table){
    File_name <- paste("Mean of", Nutrient, "In Products", ".csv", sep = " ")
    write.csv2(SortedResult, file <- File_name)
    cat("Utworzono plik o nazwie:", File_name, "\n\n", sep = " ")
  }
  
  # Narysujemy odpowiedni wykres, gdy jest takie ¿yczenie (w otworzonym programie RStudio, lub na zewn¹trz w pliku wyjœciowym).
  if (Draw_plot_In_R){
    mean_plot = barplot(as.numeric(SortedResult[[2]]), xlab = "Country name", ylab = colnames(SortedResult)[2],names.arg = NULL)
    text(mean_plot, srt = 90, labels = SortedResult[[1]], par("usr")[2], adj = c(0.2,0.5))
  }
  
  if (Draw_plot){
    File_name <- paste("Mean of", Nutrient, "In Products", ".jpg",sep = " ")
    mean_plot = barplot(as.numeric(SortedResult[[2]]), xlab = "Country name", ylab = colnames(SortedResult)[2],names.arg = NULL)
    text(mean_plot, srt = 90, labels = SortedResult[[1]], par("usr")[2], adj = c(0.2,0.5))
    dev.off()
    cat("Utworzono wykres o nazwie:", File_name, "\n\n", sep = " ")
  }
  
  return (SortedResult)
}

# Lista sk³adników z bazy danych, oraz przyk³ady zastosowania funkcji (najproœciej na podstawie elementów listy)
Nutrients
Mean_Of_Nutrient_In_Products(Nutrients[7], Make_Table <- FALSE, Draw_plot_In_R <- FALSE, Draw_plot <- TRUE)
Mean_Of_Nutrient_In_Products(Nutrients[8], Make_Table <- FALSE, Draw_plot_In_R <- FALSE, Draw_plot <- TRUE)
Mean_Of_Nutrient_In_Products(Nutrients[9], Make_Table <- TRUE, Draw_plot_In_R <- FALSE, Draw_plot <- TRUE)
Mean_Of_Nutrient_In_Products(Nutrients[7], Make_Table <- TRUE, Draw_plot_In_R <- TRUE, Draw_plot <- FALSE)
Mean_Of_Nutrient_In_Products(Nutrients[8],F , Draw_plot_In_R <- TRUE)
Mean_Of_Nutrient_In_Products(Nutrients[9], , Draw_plot_In_R <- TRUE) # Mo¿liwy te¿ krótszy zapis dzia³aj¹cy poprawnie


# Z u¿yciem poni¿szej funkcji mo¿na sprawdziæ procentowe odchy³ki od normy w zawartoœciach danego sk³adnika w produktach ka¿dego z pañstw.
# Wykorzystamy poprzednio stworzon¹ funkcjê w poni¿szej, nowo utworzonej funkcji.
# W tej funkcji wartoœci¹, do której siê odwo³ujemy, jest wartoœæ œrednia dla wszystkich badanych pañstw. 
# Inne (posortowane) wartoœci s¹ zapisane w % i pokazuj¹ ich stosunkow¹ ró¿nicê do podstawowej, wyra¿onej liczbowo.
# Istnieje w tej funkcji tak¿e mo¿liwoœæ utworzenia pliku wyjœciowego csv (tabela Excela), oraz wykresu zarówno w RStudio, jak i w pliku wyjœciowym.

PercentageDifference <- function(Nutrient, Make_Table = FALSE, Draw_plot_In_R = FALSE, Draw_plot = FALSE){
  Data_frame <- Mean_Of_Nutrient_In_Products(Nutrient)
  
  x1 <- as.numeric(as.vector(Data_frame[,2]))
  cat("Mean of magnesium (Mg) in all countries:",x1[1], as.vector(Data_frame[1,3]), ".\n\n")
  x1[1:8] <- paste(round((x1[1:8]/x1[1])*100-100, digits <- 4), "%") 
  Data_frame[2] <- x1
  Data_frame <- Data_frame[2:8,]
  rownames(Data_frame) <- NULL # Ustawiê od nowa kolejno numery wierszy poprzez ich "zresetowanie"
  
  colnames(Data_frame)[2] <- paste("Deviation of", tolower(Nutrient), sep = " ")
  
  # Stworzymy odpowiedni plik typu csv (Tabela dla programu Excel), gdy jest takie ¿yczenie (w pliku wyjœciowym).
  if (Make_Table){
    File_name <- paste("PercentageDifference_For",Nutrient,".csv",sep = " ")
    write.csv2(Data_frame, file <- File_name)
    cat("Utworzono plik o nazwie:", File_name, "\n\n", sep = " ")
  }
  
  # Narysujemy odpowiedni wykres, gdy jest takie ¿yczenie (w otworzonym programie RStudio, lub na zewn¹trz w pliku wyjœciowym).
  if (Draw_plot_In_R) {
    mean_plot = barplot(as.vector(sapply(Data_frame[[2]], function (x) return(as.numeric(stri_split_fixed(x, " ")[[1]][1])))),
                        xlab = "Country name", ylab=paste(colnames(Data_frame)[2], "[%]"), names.arg = NULL)
    text(mean_plot,srt = 90,labels = Data_frame[[1]],par("usr")[2], adj = c(0.2,0.5))
  }
  
  if (Draw_plot) {
    File_name <- paste("PercentageDifference_For", Nutrient, ".jpg", sup <- " ")
    jpeg(file = File_name)
    mean_plot = barplot(as.vector(sapply(Data_frame[[2]], function (x) return(as.numeric(stri_split_fixed(x, " ")[[1]][1])))),
                        xlab = "Country name", ylab = paste(colnames(Data_frame)[2], "[%]"),names.arg = NULL)
    text(mean_plot,srt = 90,labels = Data_frame[[1]],par("usr")[2], adj = c(0.2,0.5))
    dev.off()
    cat("Utworzono wykres o nazwie:", File_name, "\n\n", sep = " ")
  }
  
  return (Data_frame)
}

# Lista sk³adników z bazy danych, oraz przyk³ady zastosowania funkcji (najproœciej na podstawie elementów listy)
Nutrients
PercentageDifference(Nutrients[1], Make_Table <- TRUE, Draw_plot_In_R <- TRUE, Draw_plot <- TRUE)
PercentageDifference(Nutrients[2], Make_Table <- TRUE, Draw_plot_In_R <- TRUE, Draw_plot <- TRUE)
PercentageDifference(Nutrients[3], Make_Table <- FALSE, Draw_plot_In_R <- FALSE, Draw_plot <- FALSE)
PercentageDifference(Nutrients[4], Make_Table <- FALSE, Draw_plot_In_R <- FALSE, Draw_plot <- FALSE)
PercentageDifference(Nutrients[5])
PercentageDifference(Nutrients[6])



# W poni¿szej funkcji mo¿na sprawdziæ, które z pañstw posiadaj¹ wiêksz¹ od normy œredni¹ zawartoœæ danego sk³adnika w produktach.
# W celu zoptymalizowania kodu, oraz zmniejszenia jego iloœci wykorzystamy powy¿sz¹ funkcjê.
# Wyniki poka¿emy w kolejnoœci od "najzdrowszych" pañstw, malej¹co.
# Istnieje w tej funkcji tak¿e mo¿liwoœæ utworzenia pliku wyjœciowego csv (tabela Excela), oraz wykresu zarówno w RStudio, jak i w pliku wyjœciowym.

Healthier_Countries_Relative_To_Nutrient <- function(nutrient, Make_Table = FALSE, Draw_plot_In_R = FALSE, Draw_plot = FALSE){
  List <- PercentageDifference(nutrient) # U¿yjemy tutaj poprzedniej funkcji pokazuj¹cej odchy³ki procentowe od œredniej ogólnej.
  Test <- (List[2:8,2]>0)
  # Powy¿ej sprawdzamy, które wartoœci s¹ wiêksze od œredniej. Umieszczam je w nowej ramce, bêd¹cej wynikiem nowej funkcji.
  
  Nr_Of_Records=which(Test == TRUE)+1
  Result <- List[Nr_Of_Records,]
  Result[,2] <- (paste("+",Result[,2])) # Dodajemy znak mówi¹cy o stosunku do panuj¹cej normy.
  rownames(Result)=NULL # Ustawiê od nowa kolejno numery wierszy poprzez ich "zresetowanie".
  
  
  # Stworzymy odpowiedni plik typu csv (Tabela dla programu Excel), gdy jest takie ¿yczenie (w pliku wyjœciowym).
  if (Make_Table){
    File_name <- paste("Healthier countries relative to",nutrient,".csv", sep = " ")
    write.csv2(Result,file <- File_name)
    cat("Utworzono plik o nazwie:", File_name, "\n\n", sep <- " ")
  }
  
  # Narysujemy odpowiedni wykres, gdy jest takie ¿yczenie (w otworzonym programie RStudio, lub na zewn¹trz w pliku wyjœciowym).
  if (Draw_plot_In_R) {
    mean_plot <- barplot(as.vector(sapply(Result[[2]], function (x) as.numeric(paste(stri_split_fixed(x, ' ')[[1]][1], "1", sep = "")) * as.numeric(stri_split_fixed(x, ' ')[[1]][2]))),
                         xlab = "Country name", ylab=paste(colnames(Result)[2], "[%]"),names.arg = NULL)
    text(mean_plot,srt = 90,labels = Result[[1]],par("usr")[2], adj = c(0.1,0.5))
  }
  
  if (Draw_plot) {
    File_name <- paste("Healthier countries relative to",nutrient,".jpg", sep = " ")
    jpeg(file = File_name)
    mean_plot <- barplot(as.vector(sapply(Result[[2]], function (x) as.numeric(paste(stri_split_fixed(x, ' ')[[1]][1], "1", sep = "")) * as.numeric(stri_split_fixed(x, ' ')[[1]][2]))),
                         xlab = "Country name", ylab=paste(colnames(Result)[2], "[%]"),names.arg = NULL)
    text(mean_plot,srt = 90,labels = Result[[1]],par("usr")[2], adj = c(0.1,0.5))
    dev.off()
    cat("Utworzono wykres o nazwie:", File_name, "\n\n", sep = " ")
  }
  
  return(Result)
}

# Lista sk³adników z bazy danych, oraz przyk³ady zastosowania funkcji
Nutrients
Healthier_Countries_Relative_To_Nutrient(Nutrients[1], Make_Table <- TRUE, Draw_plot_In_R <- TRUE, Draw_plot <- FALSE)
Healthier_Countries_Relative_To_Nutrient(Nutrients[2], Make_Table <- TRUE, Draw_plot_In_R <- TRUE, Draw_plot <- FALSE)
Healthier_Countries_Relative_To_Nutrient(Nutrients[3], Make_Table <- FALSE, Draw_plot <- TRUE)
Healthier_Countries_Relative_To_Nutrient(Nutrients[4], Make_Table <- FALSE, Draw_plot <- TRUE)
Healthier_Countries_Relative_To_Nutrient(Nutrients[5], , Draw_plot_In_R <- FALSE)
Healthier_Countries_Relative_To_Nutrient(Nutrients[6], , Draw_plot_In_R <- FALSE)



# Analogicznie w kolejnej funkcji mo¿na sprawdziæ, które z pañstw posiadaj¹ mniejsz¹ œredni¹ zawartoœæ danego sk³adnika od normy.
# Poka¿emy je w kolejnoœci od tych "najmniej zdrowych".
# Istnieje w tej funkcji tak¿e mo¿liwoœæ utworzenia pliku wyjœciowego csv (tabela Excela), oraz wykresu zarówno w RStudio, jak i w pliku wyjœciowym.

Less_healthy_Countries_Relative_To_Nutrient <- function(nutrient, Make_Table = FALSE, Draw_plot_In_R = FALSE, Draw_plot = FALSE){
  List <- PercentageDifference(nutrient) # U¿yjê tutaj poprzedniej funkcji pokazuj¹cej odchy³ki procentowe od œredniej ogólnej.
  Test <- (List[2:8,2] < 0)
  # Powy¿ej sprawdzamy, które wartoœci s¹ wiêksze od œredniej, umieszczamy je w nowej ramce, bêd¹cej wynikiem nowej funkcji.
  
  Nr_Of_Records=rev(which(Test==TRUE)+1) # Poka¿emy w kolejnoœci od tego najbardziej "niezdrowego".
  
  Result <- List[Nr_Of_Records,]
  rownames(Result) <- NULL # Ustawimy od nowa kolejno numery wierszy poprzez ich "zresetowanie".
  
  # Stworzymy odpowiedni plik typu csv (Tabela dla programu Excel), gdy jest takie ¿yczenie (w pliku wyjœciowym).
  if (Make_Table){
    File_name <- paste("Less_healthy_Countries_Relative_To", nutrient, ".csv", sep = " ")
    write.csv2(Result, file <- File_name)
    cat("Utworzono plik o nazwie:", File_name, "\n\n", sep = " ")
  }
  
  # Narysujemy odpowiedni wykres, gdy jest takie ¿yczenie (w otworzonym programie RStudio, lub na zewn¹trz w pliku wyjœciowym).
  if (Draw_plot_In_R) {
    mean_plot <- barplot(as.vector(sapply(Result[[2]], function (x) return(as.numeric(stri_split_fixed(x, " ")[[1]][1])))),
                         xlab = "Country name", ylab=paste(colnames(Result)[2], "[%]"), names.arg = NULL, ylim=c(-70, 25))
    text(mean_plot,srt = 90, labels = levels(Result[[1]])[Result[[1]]], par("usr")[2], adj = c(0.2, 0.5))
  }
  
  if (Draw_plot) {
    File_name <- paste("Less_healthy_Countries_Relative_To", nutrient, ".jpg", sep = " ")
    jpeg(file = File_name)
    mean_plot <- barplot(as.vector(sapply(Result[[2]], function (x) return(as.numeric(stri_split_fixed(x, " ")[[1]][1])))),
                         xlab = "Country name", ylab=paste(colnames(Result)[2], "[%]"), names.arg = NULL, ylim = c(-70, 25))
    text(mean_plot, srt = 90, labels = levels(Result[[1]])[Result[[1]]], par("usr")[2], adj = c(0.2, 0.5))
    
    dev.off()
    cat("Utworzono wykres o nazwie:", File_name, "\n\n", sep = " ")
  }
  
  return(Result)
}

# Lista sk³adników z bazy danych, oraz przyk³ady zastosowania funkcji
Nutrients
Less_healthy_Countries_Relative_To_Nutrient(Nutrients[1], Make_Table <- FALSE, Draw_plot_In_R <- FALSE, Draw_plot <- TRUE)
Less_healthy_Countries_Relative_To_Nutrient(Nutrients[2], Make_Table <- FALSE, Draw_plot_In_R <- FALSE, Draw_plot <- TRUE)
Less_healthy_Countries_Relative_To_Nutrient(Nutrients[3], Make_Table <- TRUE, Draw_plot <- FALSE)
Less_healthy_Countries_Relative_To_Nutrient(Nutrients[4], Make_Table <- TRUE, Draw_plot <- FALSE)
Less_healthy_Countries_Relative_To_Nutrient(Nutrients[5],F , Draw_plot_In_R <- TRUE)
Less_healthy_Countries_Relative_To_Nutrient(Nutrients[6], , Draw_plot_In_R <- TRUE) # Mo¿liwy te¿ krótszy zapis dzia³aj¹cy poprawnie



# Pod postaci¹ danej funkcji sprawdzimy, jakie sk³adniki od¿ywcze, oraz w jakiej iloœci s¹ w danego rodzaju produkcie w ró¿nych pañstwach.
# Istnieje w tej funkcji tak¿e mo¿liwoœæ utworzenia pliku wyjœciowego csv (tabela Excela).
Nutrients_Contenct_In_Product <- function(Product, Make_Table = FALSE){
  Number_Of_Rows <- which(my_data$PRODUCT_NAME == Product)
  Result <- data.frame(as.matrix(my_data)[Number_Of_Rows,c(3,5,6,7,8,10,11,12)]) # Wybieramy odpowiednie wiersze i kolumny ramki danych.
  
  # Stworzymy odpowiedni plik typu csv (Tabela dla programu Excel), gdy jest takie ¿yczenie (w pliku wyjœciowym).
  if (Make_Table){
    File_name <- paste("Nutrients_Contenct_In", Product,".csv", sep = " ")
    write.csv2(Result, file <- File_name)
    cat("Utworzono plik o nazwie:", File_name, "\n\n", sep = " ")
  }
  
  return (Result)
}

# Lista produków z bazy danych, oraz przyk³ady zastosowania funkcji.
Products
# Products[[1]]
Nutrients_Contenct_In_Product(Products[[1]][1], Make_Table <- TRUE)
Nutrients_Contenct_In_Product(Products[[1]][1], Make_Table <- TRUE)[1:4,] 

# W celu przejrzystoœci i sprawdzenia poprawnoœci dzia³ania funkcji wyœwietlimy raz tylko pierwsze 4 rekordy (powy¿ej).
Nutrients_Contenct_In_Product(Products[[1]][3], Make_Table <- FALSE)
Nutrients_Contenct_In_Product(Products[[1]][5])




# Pod postaci¹ danej funkcji sprawdzimy, jakie sk³adniki od¿ywcze s¹ w danego rodzaju produkcie, w danym pañstwie.
# Sprawdzimy tak¿e tutaj ich iloœæ i wyœwietlimy wszystko w kolejnoœci malej¹cem wzglêdem iloœci sk³adnika w produkcie.
# Wykorzystamy tu poprzedni¹ funkcjê w celu optymalizacji kodu i uproszczenia sk³adni.
# Istnieje w tej funkcji tak¿e mo¿liwoœæ utworzenia pliku wyjœciowego csv (tabela Excela), oraz wykresu zarówno w RStudio, jak i w pliku wyjœciowym.

Nutrients_Contenct_In_Product_In_Country <- function(Product, Country, Make_Table = FALSE, Draw_plot_In_R = FALSE, Draw_plot = FALSE){
  
  Number_Of_Rows_Test1 <- which(my_data$PRODUCT_NAME==Product)
  Number_Of_Rows_Test2 <- which(my_data$COUNTRY==Country)
  
  Number_Of_Test2 <- unique((match(Number_Of_Rows_Test1, Number_Of_Rows_Test2)))
  Number_Of_Test2 <- Number_Of_Test2[which(!is.na(Number_Of_Test2))]
  Number_Of_Rows <- Number_Of_Rows_Test2[Number_Of_Test2]
  
  Result=as.matrix(my_data)[Number_Of_Rows,c(3,5,6,7,8,10,11,12)] # Wybieramy odpowiednie wiersze z danym produktem i pañstwem.
  
  Sequence_Of_Rows <- match(sort(Result[,8], decreasing=TRUE), Result[,8]) # Uporz¹dkujemy w kolejnoœci malej¹cej.
  Result=Result[Sequence_Of_Rows,]
  
  # Stworzymy odpowiedni plik typu csv (Tabela dla programu Excel), gdy jest takie ¿yczenie (w pliku wyjœciowym).
  if (Make_Table){
    File_name <- paste("Nutrients contenct in", Product, "in", Country, ".csv", sep = " ")
    write.csv2(Result, file <- File_name)
    cat("Utworzono plik o nazwie:", File_name, "\n\n", sep = " ")
  }
  
  # Narysujemy odpowiedni wykres, gdy jest takie ¿yczenie (w otworzonym programie RStudio, lub na zewn¹trz w pliku wyjœciowym).
  if (Draw_plot_In_R){
    mean_plot = barplot(as.numeric(Result[,8]),
                        xlab = colnames(Result)[6], ylab = paste(colnames(Result)[8]), names.arg = NULL)
    text(mean_plot,srt = 90, labels = Result[,6], par("usr")[2], adj = c(0.2,0.5))
  }
  
  if (Draw_plot){
    File_name <- paste("Nutrients contenct in", Product, "in", Country, ".jpg", sep = " ")
    jpeg(file <- File_name)
    mean_plot = barplot(as.numeric(Result[,8]),
                        xlab = colnames(Result)[6], ylab = paste(colnames(Result)[8]), names.arg = NULL)
    text(mean_plot,srt = 90, labels = Result[,6], par("usr")[2], adj = c(0.2,0.5))
    dev.off()
    cat("Utworzono wykres o nazwie:", File_name, "\n\n", sep = " ")
  }
  
  return (Result)
}

# Lista produków, oraz pañstw z bazy danych, oraz przyk³ady zastosowania funkcji.
Products
# Products[[1]]
Countries
Nutrients_Contenct_In_Product_In_Country(Products[[1]][1], Countries[1], Make_Table <- TRUE, Draw_plot_In_R <- TRUE, Draw_plot <- FALSE)
Nutrients_Contenct_In_Product_In_Country(Products[[1]][3], Countries[5], Make_Table <- TRUE, Draw_plot_In_R <- FALSE, Draw_plot <- TRUE)
Nutrients_Contenct_In_Product_In_Country(Products[[1]][5], Countries[6], Make_Table <- FALSE, Draw_plot_In_R <- TRUE, Draw_plot <- FALSE)
Nutrients_Contenct_In_Product_In_Country(Products[[1]][5], Countries[3], Make_Table <- FALSE, Draw_plot_In_R <- TRUE)
Nutrients_Contenct_In_Product_In_Country(Products[[1]][5], Countries[7], Make_Table <- FALSE, Draw_plot_In_R <- TRUE, Draw_plot <- FALSE)

# Pod postaci¹ danej funkcji sprawdzimy, jakie sk³adniki od¿ywcze s¹ w danego rodzaju produkcie, w danym pañstwie.
# Sprawdzimy tak¿e tutaj ich iloœæ i wyœwietlê wszystko w kolejnoœci malej¹cem wzglêdem iloœci sk³adnika w produkcie.
# Wykorzystamy tu poprzedni¹ funkcjê w celu optymalizacji kodu i uproszczenia sk³adni.
# Istnieje w tej funkcji tak¿e mo¿liwoœæ utworzenia pliku wyjœciowego csv (tabela Excela), oraz wykresu zarówno w RStudio, jak i w pliku wyjœciowym.

Average_Nutrients_In_Product <- function(Product, Make_Table = FALSE, Draw_plot_In_R = FALSE, Draw_plot = FALSE){
  x1=Nutrients_Contenct_In_Product(Product) # U¿yjemy tutaj poprzedniej funkcji pokazuj¹cej wartoœci sk³adników dla wybranego produktu.
  NutrientsForProduct <- unique(x1[,6]) # Lista sk³adników zawartych w danym produkcie.
  Result <- data.frame()
  
  
  for (i in 1:length(NutrientsForProduct)){
    Rows <- which(x1$NUTRIENT_NAME == NutrientsForProduct[i])
    srednieskladnikowprodukcie <- round(mean(as.numeric(as.vector(x1[Rows,]$AMOUNT))), digits <- 4) 
    # Œrednia danego sk³adnika w pañstwach dla danego produktu.
    # W celu poprawnoœci dzia³ania œredniej dla zmiennej powy¿ej musia³em zmieniæ jej typ z ramki danych na typ wektorowy.
    # Nastêpnie zamieniliœmy typ zmiennej wektorowej na liczbowy, aby wyci¹gn¹æ z niej odpowiednie wartoœci liczbowe.
    x2 <- x1[Rows[1],] # Pobieram nazwê, kod produktu, itp. (takie same dla wszystkich rekordów)
    colnames(x2)[8] <- ("MEAN_OF_NUTRIENTS")
    x2[8] <- srednieskladnikowprodukcie
    Result <- rbind(x2, Result)
    
  }
  rownames(Result) <- NULL # Ustawimy od nowa kolejno numery wierszy poprzez ich "zresetowanie".
  
  # Ustawimy wyniki w kolejnoœci od najbardziej znacz¹cych sk³adników produktu.
  Sequence_Of_Rows <- match(sort(Result[,8], decreasing <- TRUE), Result[,8]) # Ustawimy wyniki w kolejnoœci od najbardziej znacz¹cych sk³adników produktu.
  Result <- as.data.frame(as.matrix(Result)[Sequence_Of_Rows,])
  
  # Stworzymy odpowiedni plik typu csv (Tabela dla programu Excel), gdy jest takie ¿yczenie (w pliku wyjœciowym).
  if (Make_Table){
    File_name <- paste("Average nutrients in",Product, ".csv", sep = " ")
    write.csv2(Result, file <- File_name)
    cat("Utworzono plik o nazwie:", File_name, "\n\n", sep =  "")
  }
  
  # Narysujemy odpowiedni wykres, gdy jest takie ¿yczenie (w otworzonym programie RStudio, lub na zewn¹trz w pliku wyjœciowym).
  if (Draw_plot_In_R){
    mean_plot = barplot(as.numeric(Result[[8]]),
                        xlab = colnames(Result)[6], ylab = paste(colnames(Result)[8]), names.arg = NULL)
    text(mean_plot, srt = 90, labels = Result[[6]], par("usr")[2], adj = c(0.2,0.5))
  }
  
  if (Draw_plot){
    File_name <- paste("Average nutrients in", Product, ".jpg", sep = " ")
    jpeg(file <- File_name)
    mean_plot = barplot(as.numeric(Result[[8]]),
                        xlab = colnames(Result)[6], ylab = paste(colnames(Result)[8]), names.arg = NULL)
    text(mean_plot,srt = 90, labels = Result[[6]], par("usr")[2], adj = c(0.2,0.5))
    dev.off()
    cat("Utworzono wykres o nazwie:", File_name, "\n\n", sep = " ")
  }
  
  return (Result)
}


# Lista produków, oraz pañstw z bazy danych, oraz przyk³ady zastosowania funkcji.
Products
# Products[[1]]
Average_Nutrients_In_Product(Products[[1]][4], Make_Table <- FALSE, Draw_plot_In_R <- FALSE, Draw_plot <- TRUE)
Average_Nutrients_In_Product(Products[[1]][5], Make_Table <- FALSE, Draw_plot_In_R <- TRUE, Draw_plot <- FALSE)
Average_Nutrients_In_Product(Products[[1]][8], Make_Table <- TRUE, Draw_plot_In_R <- FALSE, Draw_plot <- FALSE)

Average_Nutrients_In_Product(Products[[1]][10], Make_Table <- TRUE, Draw_plot_In_R <- TRUE, Draw_plot <- TRUE)
Average_Nutrients_In_Product(Products[[1]][12], Make_Table <- FALSE, Draw_plot_In_R <- TRUE, Draw_plot <- TRUE)
Average_Nutrients_In_Product(Products[[1]][13], Make_Table <- TRUE, Draw_plot_In_R <- FALSE, Draw_plot <- TRUE)
Average_Nutrients_In_Product(Products[[1]][14], Make_Table <- FALSE, Draw_plot_In_R <- FALSE, Draw_plot <- FALSE)

# Dziêki tej funkcji mo¿emy sprawdziæ, w jakim produkcie zawartoœæ danego sk³adnika jest najwiêksza / najmniejsza. 
# Sprawdziæ mo¿na tak¿e, w jakim kraju ta wartoœæ jest szczególna.
# Istnieje w tej funkcji tak¿e mo¿liwoœæ utworzenia pliku wyjœciowego csv (tabela Excela).
Nutrient_In_Products <- function(Nutrient_name, Country, Make_Table = FALSE){
  Result <- filter(my_data,COUNTRY == Country,NUTRIENT_NAME == Nutrient_name)
  
  # Poni¿ej posortujemy elementy wzglêdem zawartoœci sk³adnika w produktach (malej¹co).
  SortedResult <- sort(Result$AMOUNT, decreasing <- TRUE)
  kolejnosc <- (match(SortedResult,Result$AMOUNT))
  SortedResult <- as.data.frame(as.matrix(Result)[kolejnosc,c(3,5,6,7,8,10,11,12)]) # Posortujê w prosty sposób wiersze danej ramki.
  
  # Stworzymy odpowiedni plik typu csv (Tabela dla programu Excel), gdy jest takie ¿yczenie (w pliku wyjœciowym).
  if(Make_Table){
    File_name <- paste(Nutrient_name,"in", "Products", "of", Country, ".csv", sup <- "")
    write.csv2(SortedResult, file <- File_name)
    cat("Utworzono plik o nazwie:", File_name, sup <- "")
  }
  
  return (SortedResult)
}

# Lista sk³adników, oraz pañstw z bazy danych, oraz przyk³ady zastosowania funkcji.
Nutrients
Countries

Calcium_Contenct_In_Finland <- Nutrient_In_Products(Nutrients[1], Countries[1], Make_Table <- TRUE)
head(Calcium_Contenct_In_Finland)

Magnesium_Contenct_In_Italy <- Nutrient_In_Products(Nutrients[3], Countries[3], Make_Table <- FALSE)
head(Magnesium_Contenct_In_Italy)

Phosphorus_Contenct_In_France <- Nutrient_In_Products(Nutrients[5], Countries[5])
head(Phosphorus_Contenct_In_France)

