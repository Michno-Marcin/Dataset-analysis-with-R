# Zainstalowanie, oraz wczytanie bibliotek odpowiadaj�cych za importowanie plik�w xlsx i operacje na tekstach.

#install.packages("readxl")
library("readxl")
#install.packages("dplyr")
library("dplyr")
#install.packages("tidyr")
library("tidyr")
#install.packages("stringi")
library("stringi")

# Sprawdzenie obecnego katalogu roboczego, ewentualne przypisanie nowego, wczytanie tabeli z analizowanymi danymi. 
# Ewentualnie mo�na wybra� plik r�cznie.
getwd()
# setwd("TW�J_KATALOG_ROBOCZY_Z_PROJEKTEM") # <- Ustawienie domy�lnego katalogu roboczego dla plik�W projektu
my_data <- read_excel("Food_dataset.xlsx")
# my_data <- read_excel(file.choose()) # Wyb�r pliku.

# Zamiana nazw w ramce na bardziej przejrzyste i czytelne dla przeci�tnego u�ytkownika, wy�wietlenie informacji o niej.
colnames(my_data) 
colnames(my_data) <- toupper(c(colnames(my_data[1:3]), "efsa_code", "Product_name", paste("GROUP_", 1:3, sep = ""), colnames(my_data[9]), "nutrient_name", colnames(my_data)[11], "amount"))
colnames(my_data)



# Poni�ej zmieniamy nazwy cz�ci sk�adnik�w poprzez usuni�cie zb�dnych wyraz�w, skr�cenie odpowiednio zbyt d�ugich nazw.

All_Nutrients <- my_data$NUTRIENT_NAME
unique(All_Nutrients)
All_Nutrients <- gsub(c("Total ||, total"), "", All_Nutrients)
unique(All_Nutrients) 
# Powy�ej Usuwamy napisy "total" razem z przecinkami w celu przejrzysto�ci nazwy danych w p�niejszych funkcjach.

# Poni�ej skr�cimy d�ugie nazwy sk�adnik�w (usuniemy dopiski po znaku �rednika).
# W celu mo�liwie najkr�tszego czasu zamiany obiekt�w pos�u�ymy si� zmianami typ�w zmiennych na czynnikowe, oraz ich poziomami.
Factor_All_Nutrients= as.factor(All_Nutrients)
levels(Factor_All_Nutrients)
Names <- strsplit(levels(Factor_All_Nutrients), split <- ";") # W poziomach (wszystkich nazwach) sk�adnik�w usun� napisy po �redniku.
Names
New_Names <- c()
for (i in 1:length(Names)) New_Names <- c(New_Names,Names[[i]][1]) # Pomijam wszystkie wyrazy po �redniku dla ka�dego elementu.
New_Names 

levels(Factor_All_Nutrients) <- New_Names
levels(Factor_All_Nutrients)

# W celu powrotnego przypisania zmodyfikowanych rekord�w o ramki zamieniamy ich typ na wektorowy.
All_Nutrients <- as.vector(Factor_All_Nutrients) 
unique(All_Nutrients)
my_data$NUTRIENT_NAME <- All_Nutrients
unique(my_data$NUTRIENT_NAME)



# Wy�wietlimy r�ne informacje o tabeli, na kt�rej operuje dalsza cz�� kodu w danym projekcie.
summary(my_data)
head(my_data)
str(my_data)


# Lista pa�stw, kt�rych dane analizujemy.
(Countries <- unique(my_data$COUNTRY))


# Rodzaje produkt�w �ywno�ciowych wraz z ich poszczeg�lnymi grupami.
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

# Witaminy i inne sk�adniki od�ywcze.
(Nutrients <- unique(my_data$NUTRIENT_NAME))


# Pod postaci� danej funkcji poka�emy teraz listy analizowanych pa�stw, oraz sk�adnik�w mineralnych.
# Z ich pomoc�, z u�yciem za jej pomoc� mo�na sprawdzi� zawarto�� danego sk�adnika w produktach danego pa�stwa.
# Dzi�ki posortowaniu wynik�w mo�na tak�e sprawdzi�, w kt�rym kraju zawarto�� danego sk�adnika w produktach jest stosunkowo najwi�ksza/najmniejsza.
# Istnieje w tej funkcji tak�e mo�liwo�� utworzenia pliku wyj�ciowego csv (tabela Excela).
# Nast�pnie poka�� dzia�anie tej funkcji na kilku przyk�adach.

Nutrient_Contenct_In_Country <- function(nutrient_name, country, Make_Table = FALSE, Draw_plot = FALSE, Draw_plot_In_R = FALSE){
  Result <- filter(my_data,COUNTRY == country,NUTRIENT_NAME == nutrient_name)
  
  # Poni�ej posortujemy elementy wzgl�dem zawarto�ci sk�adnika w produktach (malej�co)
  SortedResult <- sort(Result$AMOUNT,decreasing=TRUE)
  kolejnosc <- (match(SortedResult,Result$AMOUNT))
  SortedResult <- as.data.frame(as.matrix(Result)[kolejnosc,c(3,5,6,7,8,10,11,12)]) 
  # Powy�ej sortujemy w prosty spos�b wiersze danej ramki, oraz jednocze�nie wybieramy odpowiednie kolumny.
  
  # Stworzymy odpowiedni plik typu csv (Tabela dla programu Excel), gdy jest takie �yczenie (w pliku wyj�ciowym).
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




# Z pomoc� tej funkcji mo�na sprawdzi�, w jakim produkcie danego kraju zawarto�� danego sk�adnika jest najwi�ksza, b�d� najmniejsza.
Nutrient_Contenct_In_Country_MinMax <- function(nutrient_name, country){
  Result <- Nutrient_Contenct_In_Country(nutrient_name, country)
  
  cat("Maksymalna zawarto��:",nutrient_name,"w:",country,"wyst�puje dla produktu:", as.vector(Result[1,]$PRODUCT_NAME),
      ".\nPoni�ej szczeg�y dla tego produktu w danym pa�stwie:\n\n", sep = " ")
  print(Result[1,])
  
  cat("\nMinimalna zawarto��:", nutrient_name,"w:", country, "wyst�puje dla produktu:", 
      as.vector(Result[length(Result[,8]),]$PRODUCT_NAME),".\nPoni�ej szczeg�y dla tego produktu w danym pa�stwie:\n\n", sep = " ")
  print(Result[length(Result[,8]),])
}

# Lista sk�adnik�w i pa�stw z bazy danych, oraz przyk�ady zastosowania funkcji (najpro�ciej na podstawie element�w listy).
Nutrients
Countries

Nutrient_Contenct_In_Country_MinMax_Italy <- Nutrient_Contenct_In_Country_MinMax(Nutrients[1], Countries[3])
Nutrient_Contenct_In_Country_MinMax_Italy <- Nutrient_Contenct_In_Country_MinMax(Nutrients[2], Countries[6])




# Z u�yciem poni�szej funkcji mo�na sprawdzi� zsumowan� zawarto�� danego sk�adnika we wszystkich produktach ka�dego z pa�stw.
# Dodamy tak�e dodatkowy wiersz z sum� dla wszystkich zawartych powy�ej pa�stw dla por�wnania wynik�w.
# Warto�ci zawarto�ci odpowiednich sk�adnik�w zosta�y posortowane malej�co, mo�na wi�c odczyta�, w kt�rego kraju produktach jest ich stosunkowo najwi�cej/najmniej.
# W celu zoptymalizowania kodu, oraz zmniejszenia jego ilo�ci wykorzystam poprzednio stworzon� funkcj� w nowej funkcji.
# Istnieje w tej funkcji tak�e mo�liwo�� utworzenia pliku wyj�ciowego csv (tabela Excela), oraz wykresu zar�wno w RStudio, jak i w pliku wyj�ciowym.

Total_Nutrient_In_Products <- function(Nutrient, Make_Table = FALSE, Draw_plot_In_R = FALSE, Draw_plot = FALSE){
  Records=c()
  for (i in 1:length(Countries)){
    x1 <- Nutrient_Contenct_In_Country(Nutrient,Countries[i]) 
    # Powy�ej wykorzystali�my poprzedni� funkcj� "wycinaj�c�" jedynie informacje dotycz�ce interesuj�cego sk�adnika w interesuj�cym kraju.
    Records=c(Records,sum(c(as.numeric(x1$AMOUNT))))
  }
  
  # Poni�ej dodajemy nowy rekord zawieraj�cy sum� dla wszystkich badanych pa�stw.
  NewCountry <- "All"
  Countries <- c(Countries, NewCountry)
  NewRecord <- sum(Records)
  Records <- c(Records, NewRecord)
  Units <- rep(x1$UNIT[1], length(Countries)) # Jednostki, w kt�rych podajemy dane (takie same dla jednego sk�adnika).
  
  Result <- data.frame(Countries, Records, UNIT <- Units)
  colnames(Result)[2] <- paste(Nutrient, "_total", sep = "") # W nazwie kolumny oznaczaj�cej sk�adnik dopisuj� nazw� funkcji (suma).
  
  # Poni�ej posortujemy elementy wzgl�dem zawarto�ci sk�adnika w produktach r�nych kraj�w (malej�co).
  SortedResult <- sort(Result[[2]], decreasing <- TRUE)
  kolejnosc <- (match(SortedResult, Result[[2]]))
  SortedResult <- as.data.frame(as.matrix(Result)[kolejnosc,]) # Posortuj� w prosty spos�b wiersze danej ramki.
  
  # Stworzymy odpowiedni plik typu csv (Tabela dla programu Excel), gdy jest takie �yczenie (w pliku wyj�ciowym).
  if (Make_Table){
    File_name <- paste("Total",Nutrient, "In Products", ".csv", sep = " ")
    write.csv2(SortedResult, file <- File_name)
    cat("Utworzono plik o nazwie:", File_name, "\n\n", sep = " ")
  }
  
  # Narysujemy odpowiedni wykres, gdy jest takie �yczenie (w otworzonym programie RStudio, lub na zewn�trz w pliku wyj�ciowym).
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

# Lista sk�adnik�w z bazy danych, oraz przyk�ady zastosowania funkcji (najpro�ciej na podstawie element�w listy).
Nutrients

Total_Nutrient_In_Products(Nutrients[1], Make_Table = TRUE, Draw_plot_In_R = TRUE, Draw_plot = TRUE)
Total_Nutrient_In_Products(Nutrients[9], Make_Table = FALSE, Draw_plot_In_R = TRUE, Draw_plot = FALSE)
Total_Nutrient_In_Products(Nutrients[14])




# Z u�yciem poni�szej funkcji mo�na sprawdzi� �redni� zawarto�� danego sk�adnika w produktach ka�dego z pa�stw.
# W celu zmniejszenia ilo�ci kodu wykorzystamy poprzednio stworzon� funkcj� w poni�szej funkcji.
# Wyniki podamy w porz�dku malej�cym wzgl�dem zawarto�ci danego sk�adnika w poszczeg�lnych pa�stwach.
# Dzi�ki ma�ej liczbie pa�stw mo�na takze od razu pozna�, w kt�rym z nich zawarto�� danego sk�adnika w produktach jest najwi�ksza / najmniejsza.
# Dodamy tak�e dodatkowy wiersz ze �redni� dla wszystkich pa�stw w bazie danych dla por�wnania wynik�w.
# Istnieje w tej funkcji tak�e mo�liwo�� utworzenia pliku wyj�ciowego csv (tabela Excela), oraz wykresu zar�wno w RStudio, jak i w pliku wyj�ciowym.

Mean_Of_Nutrient_In_Products <- function(Nutrient, Make_Table = FALSE, Draw_plot_In_R = FALSE, Draw_plot = FALSE){
  SumOfProducts <- c()
  Records <- c()
  Units <- c()
  for (i in 1:length(Countries)){
    x1 <- Nutrient_Contenct_In_Country(Nutrient, Countries[i]) 
    # Powy�ej wykorzystali�my poprzedni� funkcj� "wycinaj�c�" jedynie informacje dotycz�ce interesuj�cego nas sk�adnika w odpowiednich krajach.
    SumOfProducts <- c(SumOfProducts, sum(as.numeric(c(x1$AMOUNT))))
  }
  AmountOfRecords <- dim(x1)[1]
  Records=c(Records, SumOfProducts/AmountOfRecords)
  
  # Poni�ej dodajemy nowy rekord zawieraj�cy �redni� dla wszystkich badanych pa�stw.
  NewCountry <- "All"
  Countries <- c(Countries, NewCountry)
  NewRecord <- mean(Records)
  Records <- round(c(Records, NewRecord), digits <- 4) # Zaokr�glimy wszystkie wyniki do 3 cyfr znacz�cych.
  Units <- rep(x1$UNIT[1], length(Records)) # Jednostki, w kt�rych podajemy dane.
  
  Result <- data.frame(Countries, Records, UNIT = Units)
  colnames(Result)[2]=paste(Nutrient,"mean", sep = "") # W nazwie kolumny oznaczaj�cej sk�adnik dopisuj� nazw� funkcji (�rednia)
  
  # Poni�ej posortujemy elementy wzgl�dem zawarto�ci sk�adnika w produktach r�nych kraj�w (malej�co)
  Sequence_Of_Rows <- sort(Result[[2]][1:dim(Result)[1]-1], decreasing <- TRUE)
  kolejnosc <- (match(Sequence_Of_Rows,Result[[2]]))
  SortedResult <- as.data.frame(as.matrix(Result)[c(8, kolejnosc),])
  
  
  # Stworzymy odpowiedni plik typu csv (Tabela dla programu Excel), gdy jest takie �yczenie (w pliku wyj�ciowym).
  if (Make_Table){
    File_name <- paste("Mean of", Nutrient, "In Products", ".csv", sep = " ")
    write.csv2(SortedResult, file <- File_name)
    cat("Utworzono plik o nazwie:", File_name, "\n\n", sep = " ")
  }
  
  # Narysujemy odpowiedni wykres, gdy jest takie �yczenie (w otworzonym programie RStudio, lub na zewn�trz w pliku wyj�ciowym).
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

# Lista sk�adnik�w z bazy danych, oraz przyk�ady zastosowania funkcji (najpro�ciej na podstawie element�w listy)
Nutrients
Mean_Of_Nutrient_In_Products(Nutrients[7], Make_Table <- FALSE, Draw_plot_In_R <- FALSE, Draw_plot <- TRUE)
Mean_Of_Nutrient_In_Products(Nutrients[8], Make_Table <- FALSE, Draw_plot_In_R <- FALSE, Draw_plot <- TRUE)
Mean_Of_Nutrient_In_Products(Nutrients[9], Make_Table <- TRUE, Draw_plot_In_R <- FALSE, Draw_plot <- TRUE)
Mean_Of_Nutrient_In_Products(Nutrients[7], Make_Table <- TRUE, Draw_plot_In_R <- TRUE, Draw_plot <- FALSE)
Mean_Of_Nutrient_In_Products(Nutrients[8],F , Draw_plot_In_R <- TRUE)
Mean_Of_Nutrient_In_Products(Nutrients[9], , Draw_plot_In_R <- TRUE) # Mo�liwy te� kr�tszy zapis dzia�aj�cy poprawnie


# Z u�yciem poni�szej funkcji mo�na sprawdzi� procentowe odchy�ki od normy w zawarto�ciach danego sk�adnika w produktach ka�dego z pa�stw.
# Wykorzystamy poprzednio stworzon� funkcj� w poni�szej, nowo utworzonej funkcji.
# W tej funkcji warto�ci�, do kt�rej si� odwo�ujemy, jest warto�� �rednia dla wszystkich badanych pa�stw. 
# Inne (posortowane) warto�ci s� zapisane w % i pokazuj� ich stosunkow� r�nic� do podstawowej, wyra�onej liczbowo.
# Istnieje w tej funkcji tak�e mo�liwo�� utworzenia pliku wyj�ciowego csv (tabela Excela), oraz wykresu zar�wno w RStudio, jak i w pliku wyj�ciowym.

PercentageDifference <- function(Nutrient, Make_Table = FALSE, Draw_plot_In_R = FALSE, Draw_plot = FALSE){
  Data_frame <- Mean_Of_Nutrient_In_Products(Nutrient)
  
  x1 <- as.numeric(as.vector(Data_frame[,2]))
  cat("Mean of magnesium (Mg) in all countries:",x1[1], as.vector(Data_frame[1,3]), ".\n\n")
  x1[1:8] <- paste(round((x1[1:8]/x1[1])*100-100, digits <- 4), "%") 
  Data_frame[2] <- x1
  Data_frame <- Data_frame[2:8,]
  rownames(Data_frame) <- NULL # Ustawi� od nowa kolejno numery wierszy poprzez ich "zresetowanie"
  
  colnames(Data_frame)[2] <- paste("Deviation of", tolower(Nutrient), sep = " ")
  
  # Stworzymy odpowiedni plik typu csv (Tabela dla programu Excel), gdy jest takie �yczenie (w pliku wyj�ciowym).
  if (Make_Table){
    File_name <- paste("PercentageDifference_For",Nutrient,".csv",sep = " ")
    write.csv2(Data_frame, file <- File_name)
    cat("Utworzono plik o nazwie:", File_name, "\n\n", sep = " ")
  }
  
  # Narysujemy odpowiedni wykres, gdy jest takie �yczenie (w otworzonym programie RStudio, lub na zewn�trz w pliku wyj�ciowym).
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

# Lista sk�adnik�w z bazy danych, oraz przyk�ady zastosowania funkcji (najpro�ciej na podstawie element�w listy)
Nutrients
PercentageDifference(Nutrients[1], Make_Table <- TRUE, Draw_plot_In_R <- TRUE, Draw_plot <- TRUE)
PercentageDifference(Nutrients[2], Make_Table <- TRUE, Draw_plot_In_R <- TRUE, Draw_plot <- TRUE)
PercentageDifference(Nutrients[3], Make_Table <- FALSE, Draw_plot_In_R <- FALSE, Draw_plot <- FALSE)
PercentageDifference(Nutrients[4], Make_Table <- FALSE, Draw_plot_In_R <- FALSE, Draw_plot <- FALSE)
PercentageDifference(Nutrients[5])
PercentageDifference(Nutrients[6])



# W poni�szej funkcji mo�na sprawdzi�, kt�re z pa�stw posiadaj� wi�ksz� od normy �redni� zawarto�� danego sk�adnika w produktach.
# W celu zoptymalizowania kodu, oraz zmniejszenia jego ilo�ci wykorzystamy powy�sz� funkcj�.
# Wyniki poka�emy w kolejno�ci od "najzdrowszych" pa�stw, malej�co.
# Istnieje w tej funkcji tak�e mo�liwo�� utworzenia pliku wyj�ciowego csv (tabela Excela), oraz wykresu zar�wno w RStudio, jak i w pliku wyj�ciowym.

Healthier_Countries_Relative_To_Nutrient <- function(nutrient, Make_Table = FALSE, Draw_plot_In_R = FALSE, Draw_plot = FALSE){
  List <- PercentageDifference(nutrient) # U�yjemy tutaj poprzedniej funkcji pokazuj�cej odchy�ki procentowe od �redniej og�lnej.
  Test <- (List[2:8,2]>0)
  # Powy�ej sprawdzamy, kt�re warto�ci s� wi�ksze od �redniej. Umieszczam je w nowej ramce, b�d�cej wynikiem nowej funkcji.
  
  Nr_Of_Records=which(Test == TRUE)+1
  Result <- List[Nr_Of_Records,]
  Result[,2] <- (paste("+",Result[,2])) # Dodajemy znak m�wi�cy o stosunku do panuj�cej normy.
  rownames(Result)=NULL # Ustawi� od nowa kolejno numery wierszy poprzez ich "zresetowanie".
  
  
  # Stworzymy odpowiedni plik typu csv (Tabela dla programu Excel), gdy jest takie �yczenie (w pliku wyj�ciowym).
  if (Make_Table){
    File_name <- paste("Healthier countries relative to",nutrient,".csv", sep = " ")
    write.csv2(Result,file <- File_name)
    cat("Utworzono plik o nazwie:", File_name, "\n\n", sep <- " ")
  }
  
  # Narysujemy odpowiedni wykres, gdy jest takie �yczenie (w otworzonym programie RStudio, lub na zewn�trz w pliku wyj�ciowym).
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

# Lista sk�adnik�w z bazy danych, oraz przyk�ady zastosowania funkcji
Nutrients
Healthier_Countries_Relative_To_Nutrient(Nutrients[1], Make_Table <- TRUE, Draw_plot_In_R <- TRUE, Draw_plot <- FALSE)
Healthier_Countries_Relative_To_Nutrient(Nutrients[2], Make_Table <- TRUE, Draw_plot_In_R <- TRUE, Draw_plot <- FALSE)
Healthier_Countries_Relative_To_Nutrient(Nutrients[3], Make_Table <- FALSE, Draw_plot <- TRUE)
Healthier_Countries_Relative_To_Nutrient(Nutrients[4], Make_Table <- FALSE, Draw_plot <- TRUE)
Healthier_Countries_Relative_To_Nutrient(Nutrients[5], , Draw_plot_In_R <- FALSE)
Healthier_Countries_Relative_To_Nutrient(Nutrients[6], , Draw_plot_In_R <- FALSE)



# Analogicznie w kolejnej funkcji mo�na sprawdzi�, kt�re z pa�stw posiadaj� mniejsz� �redni� zawarto�� danego sk�adnika od normy.
# Poka�emy je w kolejno�ci od tych "najmniej zdrowych".
# Istnieje w tej funkcji tak�e mo�liwo�� utworzenia pliku wyj�ciowego csv (tabela Excela), oraz wykresu zar�wno w RStudio, jak i w pliku wyj�ciowym.

Less_healthy_Countries_Relative_To_Nutrient <- function(nutrient, Make_Table = FALSE, Draw_plot_In_R = FALSE, Draw_plot = FALSE){
  List <- PercentageDifference(nutrient) # U�yj� tutaj poprzedniej funkcji pokazuj�cej odchy�ki procentowe od �redniej og�lnej.
  Test <- (List[2:8,2] < 0)
  # Powy�ej sprawdzamy, kt�re warto�ci s� wi�ksze od �redniej, umieszczamy je w nowej ramce, b�d�cej wynikiem nowej funkcji.
  
  Nr_Of_Records=rev(which(Test==TRUE)+1) # Poka�emy w kolejno�ci od tego najbardziej "niezdrowego".
  
  Result <- List[Nr_Of_Records,]
  rownames(Result) <- NULL # Ustawimy od nowa kolejno numery wierszy poprzez ich "zresetowanie".
  
  # Stworzymy odpowiedni plik typu csv (Tabela dla programu Excel), gdy jest takie �yczenie (w pliku wyj�ciowym).
  if (Make_Table){
    File_name <- paste("Less_healthy_Countries_Relative_To", nutrient, ".csv", sep = " ")
    write.csv2(Result, file <- File_name)
    cat("Utworzono plik o nazwie:", File_name, "\n\n", sep = " ")
  }
  
  # Narysujemy odpowiedni wykres, gdy jest takie �yczenie (w otworzonym programie RStudio, lub na zewn�trz w pliku wyj�ciowym).
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

# Lista sk�adnik�w z bazy danych, oraz przyk�ady zastosowania funkcji
Nutrients
Less_healthy_Countries_Relative_To_Nutrient(Nutrients[1], Make_Table <- FALSE, Draw_plot_In_R <- FALSE, Draw_plot <- TRUE)
Less_healthy_Countries_Relative_To_Nutrient(Nutrients[2], Make_Table <- FALSE, Draw_plot_In_R <- FALSE, Draw_plot <- TRUE)
Less_healthy_Countries_Relative_To_Nutrient(Nutrients[3], Make_Table <- TRUE, Draw_plot <- FALSE)
Less_healthy_Countries_Relative_To_Nutrient(Nutrients[4], Make_Table <- TRUE, Draw_plot <- FALSE)
Less_healthy_Countries_Relative_To_Nutrient(Nutrients[5],F , Draw_plot_In_R <- TRUE)
Less_healthy_Countries_Relative_To_Nutrient(Nutrients[6], , Draw_plot_In_R <- TRUE) # Mo�liwy te� kr�tszy zapis dzia�aj�cy poprawnie



# Pod postaci� danej funkcji sprawdzimy, jakie sk�adniki od�ywcze, oraz w jakiej ilo�ci s� w danego rodzaju produkcie w r�nych pa�stwach.
# Istnieje w tej funkcji tak�e mo�liwo�� utworzenia pliku wyj�ciowego csv (tabela Excela).
Nutrients_Contenct_In_Product <- function(Product, Make_Table = FALSE){
  Number_Of_Rows <- which(my_data$PRODUCT_NAME == Product)
  Result <- data.frame(as.matrix(my_data)[Number_Of_Rows,c(3,5,6,7,8,10,11,12)]) # Wybieramy odpowiednie wiersze i kolumny ramki danych.
  
  # Stworzymy odpowiedni plik typu csv (Tabela dla programu Excel), gdy jest takie �yczenie (w pliku wyj�ciowym).
  if (Make_Table){
    File_name <- paste("Nutrients_Contenct_In", Product,".csv", sep = " ")
    write.csv2(Result, file <- File_name)
    cat("Utworzono plik o nazwie:", File_name, "\n\n", sep = " ")
  }
  
  return (Result)
}

# Lista produk�w z bazy danych, oraz przyk�ady zastosowania funkcji.
Products
# Products[[1]]
Nutrients_Contenct_In_Product(Products[[1]][1], Make_Table <- TRUE)
Nutrients_Contenct_In_Product(Products[[1]][1], Make_Table <- TRUE)[1:4,] 

# W celu przejrzysto�ci i sprawdzenia poprawno�ci dzia�ania funkcji wy�wietlimy raz tylko pierwsze 4 rekordy (powy�ej).
Nutrients_Contenct_In_Product(Products[[1]][3], Make_Table <- FALSE)
Nutrients_Contenct_In_Product(Products[[1]][5])




# Pod postaci� danej funkcji sprawdzimy, jakie sk�adniki od�ywcze s� w danego rodzaju produkcie, w danym pa�stwie.
# Sprawdzimy tak�e tutaj ich ilo�� i wy�wietlimy wszystko w kolejno�ci malej�cem wzgl�dem ilo�ci sk�adnika w produkcie.
# Wykorzystamy tu poprzedni� funkcj� w celu optymalizacji kodu i uproszczenia sk�adni.
# Istnieje w tej funkcji tak�e mo�liwo�� utworzenia pliku wyj�ciowego csv (tabela Excela), oraz wykresu zar�wno w RStudio, jak i w pliku wyj�ciowym.

Nutrients_Contenct_In_Product_In_Country <- function(Product, Country, Make_Table = FALSE, Draw_plot_In_R = FALSE, Draw_plot = FALSE){
  
  Number_Of_Rows_Test1 <- which(my_data$PRODUCT_NAME==Product)
  Number_Of_Rows_Test2 <- which(my_data$COUNTRY==Country)
  
  Number_Of_Test2 <- unique((match(Number_Of_Rows_Test1, Number_Of_Rows_Test2)))
  Number_Of_Test2 <- Number_Of_Test2[which(!is.na(Number_Of_Test2))]
  Number_Of_Rows <- Number_Of_Rows_Test2[Number_Of_Test2]
  
  Result=as.matrix(my_data)[Number_Of_Rows,c(3,5,6,7,8,10,11,12)] # Wybieramy odpowiednie wiersze z danym produktem i pa�stwem.
  
  Sequence_Of_Rows <- match(sort(Result[,8], decreasing=TRUE), Result[,8]) # Uporz�dkujemy w kolejno�ci malej�cej.
  Result=Result[Sequence_Of_Rows,]
  
  # Stworzymy odpowiedni plik typu csv (Tabela dla programu Excel), gdy jest takie �yczenie (w pliku wyj�ciowym).
  if (Make_Table){
    File_name <- paste("Nutrients contenct in", Product, "in", Country, ".csv", sep = " ")
    write.csv2(Result, file <- File_name)
    cat("Utworzono plik o nazwie:", File_name, "\n\n", sep = " ")
  }
  
  # Narysujemy odpowiedni wykres, gdy jest takie �yczenie (w otworzonym programie RStudio, lub na zewn�trz w pliku wyj�ciowym).
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

# Lista produk�w, oraz pa�stw z bazy danych, oraz przyk�ady zastosowania funkcji.
Products
# Products[[1]]
Countries
Nutrients_Contenct_In_Product_In_Country(Products[[1]][1], Countries[1], Make_Table <- TRUE, Draw_plot_In_R <- TRUE, Draw_plot <- FALSE)
Nutrients_Contenct_In_Product_In_Country(Products[[1]][3], Countries[5], Make_Table <- TRUE, Draw_plot_In_R <- FALSE, Draw_plot <- TRUE)
Nutrients_Contenct_In_Product_In_Country(Products[[1]][5], Countries[6], Make_Table <- FALSE, Draw_plot_In_R <- TRUE, Draw_plot <- FALSE)
Nutrients_Contenct_In_Product_In_Country(Products[[1]][5], Countries[3], Make_Table <- FALSE, Draw_plot_In_R <- TRUE)
Nutrients_Contenct_In_Product_In_Country(Products[[1]][5], Countries[7], Make_Table <- FALSE, Draw_plot_In_R <- TRUE, Draw_plot <- FALSE)

# Pod postaci� danej funkcji sprawdzimy, jakie sk�adniki od�ywcze s� w danego rodzaju produkcie, w danym pa�stwie.
# Sprawdzimy tak�e tutaj ich ilo�� i wy�wietl� wszystko w kolejno�ci malej�cem wzgl�dem ilo�ci sk�adnika w produkcie.
# Wykorzystamy tu poprzedni� funkcj� w celu optymalizacji kodu i uproszczenia sk�adni.
# Istnieje w tej funkcji tak�e mo�liwo�� utworzenia pliku wyj�ciowego csv (tabela Excela), oraz wykresu zar�wno w RStudio, jak i w pliku wyj�ciowym.

Average_Nutrients_In_Product <- function(Product, Make_Table = FALSE, Draw_plot_In_R = FALSE, Draw_plot = FALSE){
  x1=Nutrients_Contenct_In_Product(Product) # U�yjemy tutaj poprzedniej funkcji pokazuj�cej warto�ci sk�adnik�w dla wybranego produktu.
  NutrientsForProduct <- unique(x1[,6]) # Lista sk�adnik�w zawartych w danym produkcie.
  Result <- data.frame()
  
  
  for (i in 1:length(NutrientsForProduct)){
    Rows <- which(x1$NUTRIENT_NAME == NutrientsForProduct[i])
    srednieskladnikowprodukcie <- round(mean(as.numeric(as.vector(x1[Rows,]$AMOUNT))), digits <- 4) 
    # �rednia danego sk�adnika w pa�stwach dla danego produktu.
    # W celu poprawno�ci dzia�ania �redniej dla zmiennej powy�ej musia�em zmieni� jej typ z ramki danych na typ wektorowy.
    # Nast�pnie zamienili�my typ zmiennej wektorowej na liczbowy, aby wyci�gn�� z niej odpowiednie warto�ci liczbowe.
    x2 <- x1[Rows[1],] # Pobieram nazw�, kod produktu, itp. (takie same dla wszystkich rekord�w)
    colnames(x2)[8] <- ("MEAN_OF_NUTRIENTS")
    x2[8] <- srednieskladnikowprodukcie
    Result <- rbind(x2, Result)
    
  }
  rownames(Result) <- NULL # Ustawimy od nowa kolejno numery wierszy poprzez ich "zresetowanie".
  
  # Ustawimy wyniki w kolejno�ci od najbardziej znacz�cych sk�adnik�w produktu.
  Sequence_Of_Rows <- match(sort(Result[,8], decreasing <- TRUE), Result[,8]) # Ustawimy wyniki w kolejno�ci od najbardziej znacz�cych sk�adnik�w produktu.
  Result <- as.data.frame(as.matrix(Result)[Sequence_Of_Rows,])
  
  # Stworzymy odpowiedni plik typu csv (Tabela dla programu Excel), gdy jest takie �yczenie (w pliku wyj�ciowym).
  if (Make_Table){
    File_name <- paste("Average nutrients in",Product, ".csv", sep = " ")
    write.csv2(Result, file <- File_name)
    cat("Utworzono plik o nazwie:", File_name, "\n\n", sep =  "")
  }
  
  # Narysujemy odpowiedni wykres, gdy jest takie �yczenie (w otworzonym programie RStudio, lub na zewn�trz w pliku wyj�ciowym).
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


# Lista produk�w, oraz pa�stw z bazy danych, oraz przyk�ady zastosowania funkcji.
Products
# Products[[1]]
Average_Nutrients_In_Product(Products[[1]][4], Make_Table <- FALSE, Draw_plot_In_R <- FALSE, Draw_plot <- TRUE)
Average_Nutrients_In_Product(Products[[1]][5], Make_Table <- FALSE, Draw_plot_In_R <- TRUE, Draw_plot <- FALSE)
Average_Nutrients_In_Product(Products[[1]][8], Make_Table <- TRUE, Draw_plot_In_R <- FALSE, Draw_plot <- FALSE)

Average_Nutrients_In_Product(Products[[1]][10], Make_Table <- TRUE, Draw_plot_In_R <- TRUE, Draw_plot <- TRUE)
Average_Nutrients_In_Product(Products[[1]][12], Make_Table <- FALSE, Draw_plot_In_R <- TRUE, Draw_plot <- TRUE)
Average_Nutrients_In_Product(Products[[1]][13], Make_Table <- TRUE, Draw_plot_In_R <- FALSE, Draw_plot <- TRUE)
Average_Nutrients_In_Product(Products[[1]][14], Make_Table <- FALSE, Draw_plot_In_R <- FALSE, Draw_plot <- FALSE)

# Dzi�ki tej funkcji mo�emy sprawdzi�, w jakim produkcie zawarto�� danego sk�adnika jest najwi�ksza / najmniejsza. 
# Sprawdzi� mo�na tak�e, w jakim kraju ta warto�� jest szczeg�lna.
# Istnieje w tej funkcji tak�e mo�liwo�� utworzenia pliku wyj�ciowego csv (tabela Excela).
Nutrient_In_Products <- function(Nutrient_name, Country, Make_Table = FALSE){
  Result <- filter(my_data,COUNTRY == Country,NUTRIENT_NAME == Nutrient_name)
  
  # Poni�ej posortujemy elementy wzgl�dem zawarto�ci sk�adnika w produktach (malej�co).
  SortedResult <- sort(Result$AMOUNT, decreasing <- TRUE)
  kolejnosc <- (match(SortedResult,Result$AMOUNT))
  SortedResult <- as.data.frame(as.matrix(Result)[kolejnosc,c(3,5,6,7,8,10,11,12)]) # Posortuj� w prosty spos�b wiersze danej ramki.
  
  # Stworzymy odpowiedni plik typu csv (Tabela dla programu Excel), gdy jest takie �yczenie (w pliku wyj�ciowym).
  if(Make_Table){
    File_name <- paste(Nutrient_name,"in", "Products", "of", Country, ".csv", sup <- "")
    write.csv2(SortedResult, file <- File_name)
    cat("Utworzono plik o nazwie:", File_name, sup <- "")
  }
  
  return (SortedResult)
}

# Lista sk�adnik�w, oraz pa�stw z bazy danych, oraz przyk�ady zastosowania funkcji.
Nutrients
Countries

Calcium_Contenct_In_Finland <- Nutrient_In_Products(Nutrients[1], Countries[1], Make_Table <- TRUE)
head(Calcium_Contenct_In_Finland)

Magnesium_Contenct_In_Italy <- Nutrient_In_Products(Nutrients[3], Countries[3], Make_Table <- FALSE)
head(Magnesium_Contenct_In_Italy)

Phosphorus_Contenct_In_France <- Nutrient_In_Products(Nutrients[5], Countries[5])
head(Phosphorus_Contenct_In_France)

