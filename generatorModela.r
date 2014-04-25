setwd("D:\\Martin delo\\zavPolice\\intervali zaupanja") # Nastavite si delovno okolje, torej pot do mape, v kateri delate
zp<-read.csv("LTV avtomatizacija avtorizacij - podatki v2.csv", sep=";")
#print(head(zp))
library(stats)
library(base)
library(e1071)

dolociMejo <- function(novaPopulacija, realnaPopulacija, zaOdstranit, simuliranaPopulacija, povZp, sdZp){
	vsotaP1 <- sum(simuliranaPopulacija, na.rm = TRUE)
	vsotaP2 <- sum(novaPopulacija, na.rm = TRUE)
		
	resitev <- ((length(novaPopulacija)+length(simuliranaPopulacija))*povZp-vsotaP2)/(vsotaP1)
	resitev*(povZp+sdZp);
}

dolociMejo2 <- function(novaPopulacija, realnaPopulacija, zaOdstranit, simuliranaPopulacija, povZp, sdZp){
	vsotaP1 <- sum(simuliranaPopulacija, na.rm = TRUE)
	vsotaP2 <- sum(realnaPopulacija, na.rm = TRUE)
		
	resitev <- ((length(realnaPopulacija)+length(simuliranaPopulacija))*povZp-vsotaP2)/(vsotaP1)
	resitev*(povZp+sdZp);
}

dolociMejo3 <- function(novaPopulacija, realnaPopulacija, zaOdstranit, simuliranaPopulacija, povZp, sdZp){
	vsotaP1 <- sum(zaOdstranit, na.rm = TRUE)
	vsotaP2 <- sum(realnaPopulacija, na.rm = TRUE)
		
	resitev <- ((length(realnaPopulacija)+length(zaOdstranit))*povZp-vsotaP2)/(vsotaP1)
	resitev*(povZp+sdZp);
}

namigZaAvtorizanta <- function(novaPopulacija, realnaPopulacija, zaOdstranit, simuliranaPopulacija, povZp, sdZp, meja){
	vsotaP1 <- sum(simuliranaPopulacija[simuliranaPopulacija>=meja], na.rm = TRUE)
	vsotaP2 <- sum(realnaPopulacija, na.rm = TRUE) + sum(simuliranaPopulacija[simuliranaPopulacija < meja], na.rm = TRUE)
		
	resitev <- ((length(realnaPopulacija)+length(simuliranaPopulacija))*povZp-vsotaP2)/(vsotaP1)
	1 - resitev;
}



generirajModel <- function(datumOd, datumDo, datumOd2, datumDo2, privzetaMejaZaAvtorizacijo, dPovprecje, dSD, dAvtorizacije = 0, indeksRasti = 1,stNakljucnihPopulacij = 1, mesto = NULL){
	#Model se nauči na podatkih datumOd do datumDo, kjer izračuna povprečje in standardni odklon. datum2Od in datum2Do določata, kdaj bomo simulirali podatke
	# dPovprecje .... za koliko procentov zelimo spremeniti povprečje (če ga hočemo zmanjšati, mora biti negativno)
	# dSD ... analogno kot dPovprečje, le da gre za standardni odklon
	#mesto .... opcijsko, če hočemo gledati kakšno konkretno poslovalnico
	gledaniDatumOd <- zp$DT_SKLENITVE >= datumOd
	gledaniDatumDo <- zp$DT_SKLENITVE <= datumDo
	gledaniDatumOd2 <- zp$DT_SKLENITVE >= toString(as.numeric(datumOd2)-10000)
	gledaniDatumDo2 <- zp$DT_SKLENITVE <= toString(as.numeric(datumDo2)-10000)
	neveljavniPodatki <- zp$PREMIJA_PRED_POPUSTI_952_NOM == 0
	if (!is.null(mesto)){
		gledaniPodatki <- (gledaniDatumOd & gledaniDatumDo) & zp$POSLOVALNICA == mesto & !neveljavniPodatki
		stariPodatki <- (gledaniDatumOd2 & gledaniDatumDo2) & zp$POSLOVALNICA == mesto & !neveljavniPodatki
	} else 
	{
		
		gledaniPodatki <- (gledaniDatumOd & gledaniDatumDo) & !neveljavniPodatki
		stariPodatki <- (gledaniDatumOd2 & gledaniDatumDo2) & !neveljavniPodatki		
	}
	print(sum(gledaniPodatki))
	sdZp <- sd(zp$DODELJENI_POPUSTI_952_ODS[gledaniPodatki], na.rm=TRUE) + dSD #standardni odklon
	povZp <- mean(zp$DODELJENI_POPUSTI_952_ODS[gledaniPodatki], na.rm=TRUE) + dPovprecje #povprecje
	#print(sprintf("povprecje : %.3f, standardniOdklon: %.3f", %povZp sdZp)))
	stPodatkov=as.integer(sum(stariPodatki, na.rm=TRUE)*indeksRasti/100)
	print (sprintf("Pricakujemo %s podatkov, izracunanih z %s indeksom rasti", stPodatkov, indeksRasti))
	celotnaPopulacija <- simuliranaPopulacija <- list()
	for (i in 1:stNakljucnihPopulacij){
		celotnaPopulacija[[i]] <- simuliranaPopulacija[[i]] <- rnorm(n=stPodatkov, m=povZp, sd=sdZp) #generiramo nakljucno populacijo, celotna je na začetku naključna
	 }
	realnaPopulacija <- array()
	#m <- povZp+(1.96/sqrt(100))*sdZp
	sum=0
	stAvtorizacij = 0;
	stAvtorizacijPrivzeto = 0;
	for (dt in datumOd2:datumDo2){ #modeliramo dan po dan
		if ((dt %% 100)>31 || as.integer(dt %% 10000 / 100) > 12) next; #nepravilni datum
		
		gledaniDatum2 <- zp$DT_SKLENITVE == dt
		if (!is.null(mesto)){
			gledaniPodatki2 <- gledaniDatum2 & mesto & !neveljavniPodatki
		} else 
		{
			gledaniPodatki2 <- gledaniDatum2 & !neveljavniPodatki	
		}
		novaPopulacija <- zp$DODELJENI_POPUSTI_952_ODS[gledaniPodatki2]
		lenNovaPopulacija <- length(novaPopulacija);
		mejaArray <- namigArray <- array()
		for (i in 1:stNakljucnihPopulacij){
			if (length(simuliranaPopulacija) >= lenNovaPopulacija){
				zaOdstranit <- sample(simuliranaPopulacija[[i]], lenNovaPopulacija)
			}else zaOdstranit = array()
			#iz simulirane populacije izbrišemo toliko primerov
			#kolikor smo jih dobili iz nove Populacije		
			
			#vstavimo funkcijo, katera nam določa mejo.
			mejaArray[i] <- dolociMejo2(novaPopulacija, realnaPopulacija, zaOdstranit, simuliranaPopulacija[[i]], povZp, sdZp)
			namigArray[i] <- namigZaAvtorizanta(novaPopulacija, realnaPopulacija, zaOdstranit, simuliranaPopulacija[[i]], povZp, sdZp, mejaArray[i])
			simuliranaPopulacija[[i]] <- simuliranaPopulacija[[i]][!simuliranaPopulacija[[i]] %in% zaOdstranit] 
			celotnaPopulacija[[i]] <- c(simuliranaPopulacija[[i]], realnaPopulacija)
		}
		
		meja = median(mejaArray, na.rm = TRUE)
		namig = median (namigArray, na.rm = TRUE)
		realnaPopulacija <- c(realnaPopulacija, novaPopulacija[novaPopulacija < meja], novaPopulacija[novaPopulacija>meja] + dAvtorizacije)
		#realnaPopulacija <- c(realnaPopulacija, novaPopulacija)
		
		print(sprintf("nova meja je %.4f, povprecje do tega trenutka je %.4f, standardni odklon: %.4f, stNovihPopustov: %d, skewness = %.4f, namig = %.4f", meja, 
				mean(realnaPopulacija, na.rm=TRUE), sd(realnaPopulacija, na.rm=TRUE), length(novaPopulacija), skewness(realnaPopulacija, na.rm=TRUE), namig))
		
		
		
		
		#Preverimo, koliko avtorizacij bi na dan  dt imeli.
		for (primer in novaPopulacija){
			if (!is.na(primer) && primer > meja){
				stAvtorizacij <- stAvtorizacij+1;
			}
		}
		#koliko avtorizacij bi imeli s privzeto mejo
		stAvtorizacijPrivzeto <-stAvtorizacijPrivzeto + sum(novaPopulacija > privzetaMejaZaAvtorizacijo, na.rm=TRUE) 
		
	}
	a <- mean(realnaPopulacija, na.rm=TRUE)
	print(sprintf("Povprecje Pred %.4f", povZp))
	print(sprintf("povprecje po %.4f", a))
	rezultati <- list("stAvtorizacij" = stAvtorizacij, "stAvtorizacijPrivzeto" = stAvtorizacijPrivzeto, "razlika" = stAvtorizacijPrivzeto-stAvtorizacij,
				"razmerje" = 1-stAvtorizacij/stAvtorizacijPrivzeto, "novoRazmerje" = stAvtorizacij/ length(realnaPopulacija), 
				"staroRazmerje" = stAvtorizacijPrivzeto/ length(realnaPopulacija));
	rezultati
}
#Simulacija ohranjanja porazdelitve 
#simulator <- generirajModel("20130101","20130131","20140101","20140131", 0.37, 0, 0, 0)
#simulator

#Simulacija zmanjsanje povprecja in standardnega odklona
simulator <- generirajModel("20140101","20140131","20140101","20140228", 0.37, -0.02, -0.02, -0.05, stNakljucnihPopulacij = 1, indeksRasti = 103.2)
simulator

#simulator <- generirajModel("20131210","20131212","20140101","20140228", 0.37, -0.02, -0.02, -0.05)
#simulator



#mean(celotnaPopulacija, na.rm=TRUE)
#max(celotnaPopulacija, na.rm=TRUE)
#length(celotnaPopulacija)
