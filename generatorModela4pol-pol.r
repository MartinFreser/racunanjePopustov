
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



generirajModel <- function(datumOd, datumDo, datumOd2, datumDo2, privzetaMejaZaAvtorizacijo, dPovprecje, dSD, 
				dAvtorizacije = 0, indeksRasti = 1,stNakljucnihPopulacij = 1,malaMeja = 0.25, dMalaMeja=0, delezRealnih=0.0, 
				outputName="output.txt", izpis=FALSE, mesto = NULL){
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
	if (izpis) {print(sprintf("Ucimo se na %s podatkih", sum(gledaniPodatki)))}
	sdZp <- sd(zp$DODELJENI_POPUSTI_952_ODS[gledaniPodatki], na.rm=TRUE) + dSD #standardni odklon
	povZp <- mean(zp$DODELJENI_POPUSTI_952_ODS[gledaniPodatki], na.rm=TRUE) + dPovprecje #povprecje
	#print(sprintf("povprecje : %.3f, standardniOdklon: %.3f", %povZp sdZp)))
	stPodatkov=as.integer(sum(stariPodatki, na.rm=TRUE)*indeksRasti/100)
	if(izpis) {print (sprintf("Pricakujemo %s podatkov, izracunanih z %s indeksom rasti", stPodatkov, indeksRasti))}
	celotnaPopulacija <- simuliranaPopulacija <- list()
	stRealnihDodanih = as.integer(stPodatkov*delezRealnih)
	if(length(zp$DODELJENI_POPUSTI_952_ODS[gledaniPodatki]) > stRealnihDodanih){
		zaDodat = sample(zp$DODELJENI_POPUSTI_952_ODS[gledaniPodatki], stRealnihDodanih)
	}else{
		zaDodat = zp$DODELJENI_POPUSTI_952_ODS[gledaniPodatki]
		stRealnihDodanih = length(zp$DODELJENI_POPUSTI_952_ODS[gledaniPodatki])
	}
	for (i in 1:stNakljucnihPopulacij){
		
		celotnaPopulacija[[i]] <- simuliranaPopulacija[[i]] <- c(rnorm(stPodatkov-stRealnihDodanih, m=povZp, sd=sdZp),zaDodat) #generiramo nakljucno populacijo, celotna je na začetku naključna
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
		realnaPopulacija <- c(realnaPopulacija, novaPopulacija[novaPopulacija<=malaMeja],novaPopulacija[malaMeja< novaPopulacija && novaPopulacija <= meja]+dMalaMeja, novaPopulacija[novaPopulacija>meja] + dAvtorizacije)
		#realnaPopulacija <- c(realnaPopulacija, novaPopulacija)
		x <- c(dt, meja, mean(realnaPopulacija, na.rm=TRUE), sd(realnaPopulacija, na.rm=TRUE), length(novaPopulacija), skewness(realnaPopulacija, na.rm=TRUE), namig)
		if(dt==datumOd2){ zdruzi = FALSE} #da povozimo star file.
		else {zdruzi = TRUE}
		write(x, file = outputName, ncolumns = length(x), append = zdruzi, sep = ";")		
		if(izpis){
			print(sprintf("nova meja je %.4f, povprecje do tega trenutka je %.4f, standardni odklon: %.4f, stNovihPopustov: %d, skewness = %.4f, namig = %.4f", meja, 
				mean(realnaPopulacija, na.rm=TRUE), sd(realnaPopulacija, na.rm=TRUE), length(novaPopulacija), skewness(realnaPopulacija, na.rm=TRUE), namig))
		}
		
		
		
		#Preverimo, koliko avtorizacij bi na dan  dt imeli.
		for (primer in novaPopulacija){
			if (!is.na(primer) && primer > meja){
				stAvtorizacij <- stAvtorizacij+1;
			}
		}
		#koliko avtorizacij bi imeli s privzeto mejo
		stAvtorizacijPrivzeto <-stAvtorizacijPrivzeto + sum(novaPopulacija > privzetaMejaZaAvtorizacijo, na.rm=TRUE) 
		
	}
	legenda = c("datum", "meja", "mean(realnaPopulacija)", "sd(realnaPopulacija", "length(novaPopulacija)", "skewness(realnaPopulacija)", "namig")
	write(legenda, file = paste("legenda",outputName,sep=""), ncolumns = length(legenda), append = FALSE, sep = ";")
	a <- mean(realnaPopulacija, na.rm=TRUE)
	if (izpis){
		print(sprintf("Povprecje Pred %.4f", povZp))
		print(sprintf("povprecje po %.4f", a))
	}
	rezultati <- list("stAvtorizacij" = stAvtorizacij, "stAvtorizacijPrivzeto" = stAvtorizacijPrivzeto, "razlika" = stAvtorizacijPrivzeto-stAvtorizacij,
				"razmerje" = 1-stAvtorizacij/stAvtorizacijPrivzeto, "novoRazmerje" = stAvtorizacij/ length(realnaPopulacija), 
				"staroRazmerje" = stAvtorizacijPrivzeto/ length(realnaPopulacija));
	rezultati
}

#Simulacija zmanjsanje povprecja in standardnega odklona
#sink("output.txt", append=TRUE, split=FALSE)
args <- commandArgs(trailingOnly = TRUE)

if (length(args) == 16){
	print("Program JE sprejel argumente iz ukazne vrstice")
	setwd(args[14])
	zp<-read.csv(args[15], sep=";")
	simulator <- generirajModel(args[1],args[2],args[3],args[4], as.numeric(args[5]), as.numeric(args[6]), 
									as.numeric(args[7]), as.numeric(args[8]),malaMeja= as.numeric(args[9]),dMalaMeja= as.numeric(args[10]), 
									delezRealnih = as.numeric(args[11]), stNakljucnihPopulacij = as.numeric(args[12]), 
									indeksRasti = as.numeric(args[13]), outputName=args[16])
	#simulator
}else{
	setwd("D:\\Martin delo\\zavPolice\\intervali zaupanja")
	zp<-read.csv("LTV avtomatizacija avtorizacij - podatki v2.csv", sep=";")
	simulator <- generirajModel("20140101","20140131","20140201","20140228", 0.37, -0.00, -0.00, -0.00,malaMeja=0.25,dMalaMeja=-0.00, delezRealnih = 0.5, stNakljucnihPopulacij = 1000, indeksRasti = 103.2, izpis=TRUE)
	print("POZOR, program ni sprejel argumentov iz ukazne vrstice!")
	#simulator
}



#Rscript generatorModela4pol-pol.r 20140101 20140131 20140101 20140228 0.37 -0.00 -0.02 -0.05 0.25 -0.01 0.5 100 103.2 "D:\\Martin delo\\zavPolice\\intervali zaupanja" "LTV avtomatizacija avtorizacij - podatki v2.csv" "output2.txt"
#Rscript generatorModela4pol-pol.r datumOd datumDo datumOd2 datumDo2 privzetaMejaZaAvtorizacijo dPovprecje dSD dAvtorizacije indeksRasti stNakljucnihPopulacij malaMeja dMalaMeja delezRealnih pathToData dataFileName outputName 
