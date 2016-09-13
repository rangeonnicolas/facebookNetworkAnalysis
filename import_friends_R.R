require("Rfacebook") 
require(RPostgreSQL)


dbQuery <- function(ret,conn,query,debug=0,note=""){
	execute <-1
	if(debug){
		print(paste0("--",note," 
								",query))
	}
	if(execute)
		if(ret)
			return (fetch(dbSendQuery(conn,query),n=-1))
		else
			dbGetQuery(conn,query)
}
delete.two.firsts.spaces <- function(str){
	if(nchar(str) > 1){

		c <- substring(str,1,1)
		if(c %in% c(" ")){
			str = substring(str,2)
		}

		c <- substring(str,1,1)
		if(c %in% c(" ")){
			str = substring(str,2)
		}
	}else{
		return(str)
	}
	return(str)
}
delete.two.lasts.spaces <- function(str){
	if(nchar(str) > 1){

		c <- substring(str,nchar(str),nchar(str))
		if(c %in% c(" ")){
			str = substring(str,1,nchar(str)-1)
		}

		c <- substring(str,nchar(str),nchar(str))
		if(c %in% c(" ")){
			str = substring(str,1,nchar(str)-1)
		}
	}else{
		return(str)
	}
	return(str)
}
ds <- function(po){
	if(!is.na(po)){
		while(substr(po,1,1) == " "){
			po <- delete.two.firsts.spaces(po)
		}
		while(substr(po,nchar(po),nchar(po)) == " "){
			po <- delete.two.lasts.spaces(po)
		}
		return(po)
	}else{
		return(po)
	}
}

getClasses <- function(table){
	ns <- names(table)
	classes <- rep(NA,length(ns))
	i<-1
	for(nam in ns){
		classes[i] <- class(table[,nam])
		i <- i+1}
	return(classes)
}

crypt <- function(str){
	n <- 4
	if(nchar(str)>n)
		for(i in 1:(ceiling((nchar(str)/n))-1))
			str <- paste0(substr(str,0,i*n),"%",substr(str,i*n+2,nchar(str)))
	# spl <- unlist(strsplit(str,""))
	# spl2 <- c(spl,spl)
	# seq <- 2*(1:ceiling((length(spl2)/2)-0.6))
	# spl2[seq] <- rep("_",length(seq))
	# spl2[seq-1] <- spl
	# str <- paste(spl2,collapse="")
	str <- substr(str,0,13)
	return(str)
}
norm.na <- function(str){
	ret <- ""
	str <- gsub("\r","",str)
	str <- gsub("\n","",str)
	str <- gsub("\\r","",str)
	str <- gsub("\\r","",str)
	if(is.null(str))
		return (ret)
	if(is.na(str))
		return (ret)
	if(str=="")
		return (ret)
	if(str==" ")
		return (ret)
	if(str=="  ")
		return (ret)
}


norm.utf8 <- function(str){
	## Il est conseillé de mettre en miniscules avant

	str <- gsub("â‚¬"  ,"___cdbz_112__",str)
	str <- gsub("â€š"  ,"___cdbz_113__",str)
	str <- gsub("â€ž"  ,"___cdbz_114__",str)
	str <- gsub("â€¦"  ,"___cdbz_115__",str)
	str <- gsub("â€¡"  ,"___cdbz_116__",str)
	str <- gsub("â€°"  ,"___cdbz_117__",str)
	str <- gsub("â€¹"  ,"___cdbz_118__",str)
	str <- gsub("â€˜"  ,"___cdbz_119__",str)
	str <- gsub("â€™"  ,"___cdbz_121__",str)
	str <- gsub("â€œ"  ,"___cdbz_122__",str)
	str <- gsub("â€¢"  ,"___cdbz_123__",str)
	str <- gsub("â€“"  ,"___cdbz_124__",str)
	str <- gsub("â€”"  ,"___cdbz_125__",str)
	str <- gsub("â„¢"  ,"___cdbz_126__",str)
	str <- gsub("â€º"  ,"___cdbz_127__",str)
	str <- gsub("Æ’"   ,"___cdbz_001__",str)
	str <- gsub("Ë†"   ,"___cdbz_002__",str)
	str <- gsub("Å’"   ,"___cdbz_003__",str)
	str <- gsub("Å½"   ,"___cdbz_004__",str)
	str <- gsub("Ëœ"   ,"___cdbz_005__",str)
	str <- gsub("Å¡"   ,"___cdbz_006__",str)
	str <- gsub("Å“"   ,"___cdbz_007__",str)
	str <- gsub("Å¾"   ,"___cdbz_008__",str)
	str <- gsub("Å¸"   ,"___cdbz_009__",str)
	str <- gsub("Â¡"   ,"___cdbz_011__",str)
	str <- gsub("Â¢"   ,"___cdbz_012__",str)
	str <- gsub("Â£"   ,"___cdbz_013__",str)
	str <- gsub("Â¤"   ,"___cdbz_014__",str)
	str <- gsub("Â¥"   ,"___cdbz_015__",str)
	str <- gsub("Â¦"   ,"___cdbz_016__",str)
	str <- gsub("Â§"   ,"___cdbz_017__",str)
	str <- gsub("Â¨"   ,"___cdbz_018__",str)
	str <- gsub("Â©"   ,"___cdbz_019__",str)
	str <- gsub("Âª"   ,"___cdbz_021__",str)
	str <- gsub("Â«"   ,"___cdbz_022__",str)
	str <- gsub("Â¬"   ,"___cdbz_023__",str)
	str <- gsub("Â­"   ,"___cdbz_024__",str)
	str <- gsub("Â®"   ,"___cdbz_025__",str)
	str <- gsub("Â¯"   ,"___cdbz_026__",str)
	str <- gsub("Â°"   ,"___cdbz_027__",str)
	str <- gsub("Â±"   ,"___cdbz_028__",str)
	str <- gsub("Â²"   ,"___cdbz_029__",str)
	str <- gsub("Â³"   ,"___cdbz_031__",str)
	str <- gsub("Â´"   ,"___cdbz_032__",str)
	str <- gsub("Âµ"   ,"___cdbz_033__",str)
	str <- gsub("Â¶"   ,"___cdbz_034__",str)
	str <- gsub("Â·"   ,"___cdbz_035__",str)
	str <- gsub("Â¸"   ,"___cdbz_036__",str)
	str <- gsub("Â¹"   ,"___cdbz_037__",str)
	str <- gsub("Âº"   ,"___cdbz_038__",str)
	str <- gsub("Â»"   ,"___cdbz_039__",str)
	str <- gsub("Â¼"   ,"___cdbz_041__",str)
	str <- gsub("Â½"   ,"___cdbz_042__",str)
	str <- gsub("Â¾"   ,"___cdbz_043__",str)
	str <- gsub("Â¿"   ,"___cdbz_044__",str)
	str <- gsub("Ãƒ"   ,"___cdbz_045__",str)
	str <- gsub("Ãˆ"   ,"___cdbz_046__",str)
	str <- gsub("ÃŠ"   ,"___cdbz_047__",str)
	str <- gsub("ÃŒ"   ,"___cdbz_048__",str)
	str <- gsub("ÃŽ"   ,"___cdbz_049__",str)
	str <- gsub("Ã˜"   ,"___cdbz_051__",str)
	str <- gsub("Ãš"   ,"___cdbz_052__",str)
	str <- gsub("Ãœ"   ,"___cdbz_053__",str)
	str <- gsub("Ãž"   ,"___cdbz_054__",str)
	str <- gsub("ÃŸ"   ,"___cdbz_055__",str)
	str <- gsub("Ã¡"   ,"___cdbz_056__",str)
	str <- gsub("Ã¢"   ,"___cdbz_057__",str)
	str <- gsub("Ã£"   ,"___cdbz_058__",str)
	str <- gsub("Ã¤"   ,"___cdbz_059__",str)
	str <- gsub("Ã¥"   ,"___cdbz_061__",str)
	str <- gsub("Ã¦"   ,"___cdbz_062__",str)
	str <- gsub("Ã§"   ,"___cdbz_063__",str)
	str <- gsub("Ã¨"   ,"___cdbz_064__",str)
	str <- gsub("Ã©"   ,"___cdbz_065__",str)
	str <- gsub("Ãª"   ,"___cdbz_066__",str)
	str <- gsub("Ã«"   ,"___cdbz_067__",str)
	str <- gsub("Ã¬"   ,"___cdbz_068__",str)
	str <- gsub("Ã­"   ,"___cdbz_069__",str)
	str <- gsub("Ã®"   ,"___cdbz_071__",str)
	str <- gsub("Ã¯"   ,"___cdbz_072__",str)
	str <- gsub("Ã°"   ,"___cdbz_073__",str)
	str <- gsub("Ã±"   ,"___cdbz_074__",str)
	str <- gsub("Ã²"   ,"___cdbz_075__",str)
	str <- gsub("Ã³"   ,"___cdbz_076__",str)
	str <- gsub("Ã´"   ,"___cdbz_077__",str)
	str <- gsub("Ãµ"   ,"___cdbz_078__",str)
	str <- gsub("Ã¶"   ,"___cdbz_079__",str)
	str <- gsub("Ã·"   ,"___cdbz_081__",str)
	str <- gsub("Ã¸"   ,"___cdbz_082__",str)
	str <- gsub("Ã¹"   ,"___cdbz_083__",str)
	str <- gsub("Ãº"   ,"___cdbz_084__",str)
	str <- gsub("Ã»"   ,"___cdbz_085__",str)
	str <- gsub("Ã¼"   ,"___cdbz_086__",str)
	str <- gsub("Ã½"   ,"___cdbz_087__",str)
	str <- gsub("Ã¾"   ,"___cdbz_088__",str)
	str <- gsub("Ã¿"   ,"___cdbz_089__",str)
	str <- gsub("Ã€"   ,"___cdbz_091__",str)
	str <- gsub("Ã‚"   ,"___cdbz_092__",str)
	str <- gsub("Ã„"   ,"___cdbz_093__",str)
	str <- gsub("Ã…"   ,"___cdbz_094__",str)
	str <- gsub("Ã‡"   ,"___cdbz_095__",str)
	str <- gsub("Ã‰"   ,"___cdbz_096__",str)
	str <- gsub("Ã‹"   ,"___cdbz_097__",str)
	str <- gsub("Ã‘"   ,"___cdbz_098__",str)
	str <- gsub("Ã’"   ,"___cdbz_099__",str)
	str <- gsub("Ã“"   ,"___cdbz_101__",str)
	str <- gsub("Ã•"   ,"___cdbz_102__",str)
	str <- gsub("Ã–"   ,"___cdbz_103__",str)
	str <- gsub("Ã—"   ,"___cdbz_104__",str)
	str <- gsub("Ã™"   ,"___cdbz_105__",str)
	str <- gsub("Ã›"   ,"___cdbz_106__",str)
	str <- gsub("â€"   ,"___cdbz_107__",str)
	str <- gsub("Ã†"   ,"___cdbz_108__",str)
	str <- gsub("â€"   ,"___cdbz_109__",str)
	str <- gsub("Ã”"   ,"___cdbz_111__",str)

	str <- gsub("___cdbz_112__","€" ,str)
	str <- gsub("___cdbz_113__","‚" ,str)
	str <- gsub("___cdbz_114__","„" ,str)
	str <- gsub("___cdbz_115__","…" ,str)
	str <- gsub("___cdbz_116__","‡" ,str)
	str <- gsub("___cdbz_117__","‰" ,str)
	str <- gsub("___cdbz_118__","‹" ,str)
	str <- gsub("___cdbz_119__","‘" ,str)
	str <- gsub("___cdbz_121__","’" ,str)
	str <- gsub("___cdbz_122__","“" ,str)
	str <- gsub("___cdbz_123__","•" ,str)
	str <- gsub("___cdbz_124__","–" ,str)
	str <- gsub("___cdbz_125__","—" ,str)
	str <- gsub("___cdbz_126__","™" ,str)
	str <- gsub("___cdbz_127__","›" ,str)
	str <- gsub("___cdbz_001__","ƒ" ,str)
	str <- gsub("___cdbz_002__","ˆ" ,str)
	str <- gsub("___cdbz_003__","Œ" ,str)
	str <- gsub("___cdbz_004__","Ž" ,str)
	str <- gsub("___cdbz_005__","˜" ,str)
	str <- gsub("___cdbz_006__","š" ,str)
	str <- gsub("___cdbz_007__","œ" ,str)
	str <- gsub("___cdbz_008__","ž" ,str)
	str <- gsub("___cdbz_009__","Ÿ" ,str)
	str <- gsub("___cdbz_011__","¡" ,str)
	str <- gsub("___cdbz_012__","¢" ,str)
	str <- gsub("___cdbz_013__","£" ,str)
	str <- gsub("___cdbz_014__","¤" ,str)
	str <- gsub("___cdbz_015__","¥" ,str)
	str <- gsub("___cdbz_016__","¦" ,str)
	str <- gsub("___cdbz_017__","§" ,str)
	str <- gsub("___cdbz_018__","¨" ,str)
	str <- gsub("___cdbz_019__","©" ,str)
	str <- gsub("___cdbz_021__","ª" ,str)
	str <- gsub("___cdbz_022__","«" ,str)
	str <- gsub("___cdbz_023__","¬" ,str)
	str <- gsub("___cdbz_024__","­" ,str)
	str <- gsub("___cdbz_025__","®" ,str)
	str <- gsub("___cdbz_026__","¯" ,str)
	str <- gsub("___cdbz_027__","°" ,str)
	str <- gsub("___cdbz_028__","±" ,str)
	str <- gsub("___cdbz_029__","²" ,str)
	str <- gsub("___cdbz_031__","³" ,str)
	str <- gsub("___cdbz_032__","´" ,str)
	str <- gsub("___cdbz_033__","µ" ,str)
	str <- gsub("___cdbz_034__","¶" ,str)
	str <- gsub("___cdbz_035__","·" ,str)
	str <- gsub("___cdbz_036__","¸" ,str)
	str <- gsub("___cdbz_037__","¹" ,str)
	str <- gsub("___cdbz_038__","º" ,str)
	str <- gsub("___cdbz_039__","»" ,str)
	str <- gsub("___cdbz_041__","¼" ,str)
	str <- gsub("___cdbz_042__","½" ,str)
	str <- gsub("___cdbz_043__","¾" ,str)
	str <- gsub("___cdbz_044__","¿" ,str)	
	str <- gsub("___cdbz_045__","Ã" ,str)
	str <- gsub("___cdbz_046__","È" ,str)
	str <- gsub("___cdbz_047__","Ê" ,str)
	str <- gsub("___cdbz_048__","Ì" ,str)
	str <- gsub("___cdbz_049__","Î" ,str)
	str <- gsub("___cdbz_051__","Ø" ,str)
	str <- gsub("___cdbz_052__","Ú" ,str)
	str <- gsub("___cdbz_053__","Ü" ,str)
	str <- gsub("___cdbz_054__","Þ" ,str)
	str <- gsub("___cdbz_055__","ß" ,str)
	str <- gsub("___cdbz_056__","á" ,str)
	str <- gsub("___cdbz_057__","â" ,str)
	str <- gsub("___cdbz_058__","ã" ,str)
	str <- gsub("___cdbz_059__","ä" ,str)
	str <- gsub("___cdbz_061__","å" ,str)
	str <- gsub("___cdbz_062__","æ" ,str)
	str <- gsub("___cdbz_063__","ç" ,str)
	str <- gsub("___cdbz_064__","è" ,str)
	str <- gsub("___cdbz_065__","é" ,str)
	str <- gsub("___cdbz_066__","ê" ,str)
	str <- gsub("___cdbz_067__","ë" ,str)
	str <- gsub("___cdbz_068__","ì" ,str)
	str <- gsub("___cdbz_069__","í" ,str)
	str <- gsub("___cdbz_071__","î" ,str)
	str <- gsub("___cdbz_072__","ï" ,str)
	str <- gsub("___cdbz_073__","ð" ,str)
	str <- gsub("___cdbz_074__","ñ" ,str)
	str <- gsub("___cdbz_075__","ò" ,str)
	str <- gsub("___cdbz_076__","ó" ,str)
	str <- gsub("___cdbz_077__","ô" ,str)
	str <- gsub("___cdbz_078__","õ" ,str)
	str <- gsub("___cdbz_079__","ö" ,str)
	str <- gsub("___cdbz_081__","÷" ,str)
	str <- gsub("___cdbz_082__","ø" ,str)
	str <- gsub("___cdbz_083__","ù" ,str)
	str <- gsub("___cdbz_084__","ú" ,str)
	str <- gsub("___cdbz_085__","û" ,str)
	str <- gsub("___cdbz_086__","ü" ,str)
	str <- gsub("___cdbz_087__","ý" ,str)
	str <- gsub("___cdbz_088__","þ" ,str)
	str <- gsub("___cdbz_089__","ÿ" ,str)
	str <- gsub("___cdbz_091__","À" ,str)
	str <- gsub("___cdbz_092__","Â" ,str)
	str <- gsub("___cdbz_093__","Ä" ,str)
	str <- gsub("___cdbz_094__","Å" ,str)
	str <- gsub("___cdbz_095__","Ç" ,str)
	str <- gsub("___cdbz_096__","É" ,str)
	str <- gsub("___cdbz_097__","Ë" ,str)
	str <- gsub("___cdbz_098__","Ñ" ,str)
	str <- gsub("___cdbz_099__","Ò" ,str)
	str <- gsub("___cdbz_101__","Ó" ,str)
	str <- gsub("___cdbz_102__","Õ" ,str)
	str <- gsub("___cdbz_103__","Ö" ,str)
	str <- gsub("___cdbz_104__","×" ,str)
	str <- gsub("___cdbz_105__","Ù" ,str)
	str <- gsub("___cdbz_106__","Û" ,str)
	str <- gsub("___cdbz_107__","†" ,str)
	str <- gsub("___cdbz_108__","Æ" ,str)
	str <- gsub("___cdbz_109__","”" ,str)
	str <- gsub("___cdbz_111__","Ô" ,str)

	return(str)
}

norm.del.spaces <- function(str){
	str <- gsub("                                                                "," ",str)
	str <- gsub("                                "," ",str)
	str <- gsub("                "," ",str)
	str <- gsub("        "," ",str)
	str <- gsub("        "," ",str)
	str <- gsub("    "," ",str)
	str <- gsub("  "," ",str)

	return(str)
}

## CONFIG

	setwd("C:\\Users\\CKW7\\Documents\\Dropbox\\POSTE\\adresses\\gmaps")

## IMPORT FACEBOOK 

	fb_oauth <- fbOAuth("352707841538465", "b8f05679dfe0e18bf69de1d26b9589f6", extended_permissions = TRUE)
	me <- getUsers("me", token=fb_oauth)
	my_friends <- getFriends(token=fb_oauth, simplify=TRUE)
	ams_fb <- getUsers(my_friends$id, token=fb_oauth, private_info=TRUE)

## IMOORT BDD

	conn <- dbConnect(PostgreSQL(), host = "localhost", user= "postgres", password="connect", dbname="postgresams",port="5432")
	dbQuery(0,conn," SET search_path to public;")
	ams_db <- dbReadTable(conn,"ams")

	set <- which(getClasses(ams_db)=="character")
	ams_db[,set] <- apply(ams_db[,set],2,norm.utf8)
	ams_db[,set] <- apply(ams_db[,set],2,norm.del.spaces)
	# ams_db[,set] <- apply(ams_db[,set],2,ds)
	# ams_db[,set] <- apply(ams_db[,set],2,norm.na)

## JOINTURE DES DEUX TABLES

	ams_fb$jointure = paste0("http://www.facebook.com/",ams_fb$id)

	big <- merge(ams_fb,ams_db,by.x="jointure",by.y="fb_uid",all=TRUE)
	big["fb_uid"] <- big["jointure"]
	#pour visualiser, afficher les colonnes : big[,c(1,2,4,16,18,22)]
	new_fb_friends <- which(is.na(big["id_ams"]))
	# NOUVEAUX AMIS FB PAS ENCORE DANS LA BASE :
	big[new_fb_friends,c("name","username")]
		# Detection de l'id où commencer pour attribuer les nouveaux IDs
		id_ams  <- big["id_ams"]
		id_ams2 <- apply(id_ams,1,substr,4,200)
		begin <- max(as.integer(id_ams2),na.rm=TRUE)
		# generation des IDs
		nums <- begin + (1:length(new_fb_friends))
		nums2 <- paste0("0000000",nums)
		nch <- nchar(nums2)
		nums3 <- apply(data.frame(nums2),1,substr,nch-4,100)
		new_ids <- paste0("AMS",nums3)
		# affectation des ids
		big[new_fb_friends,"id_ams"] <- new_ids
		# remplissage des champs
		a <- new_fb_friends
		#modified <- a
		big[a,"nom"] 			<- big[a,"name"]#big[a,"surnom_au12aout11"] 			<- big[a,"name"]
		big[a,"dernier_surnom_facebook" ] 	<- big[a,"name"]
		big[a,"sexe"] 						<- apply(data.frame(big[a,"gender"]),1,substr,1,1)
		big[a,"facebook"] 					<- "oui"
		big[a,"ville"] 						<- big[a,"location"]

		big[a,"ville_secondaire"] 			<- big[a,"hometown"]
		set_na <- which(is.na(big[a,"ville_secondaire"]))
		big[a,"ville_secondaire"][set_na] 	<- big[a,"locale"][set_na]
		big[a,"date_ajout"] 				<- format(Sys.time(), "%Y-%m-%d")
		#big[a,"fb_uid"] 					<- big[a,"jointure"]

		chieurs 							<- which(apply(data.frame(big[a,"birthday"]),1,nchar)==5)
		big[a,"birthday"][chieurs]			<- paste0(big[a,"birthday"][chieurs],"/1500")
		big[a,"anniv"] 						<- as.Date(big[a,"birthday"],"%m/%d/%Y")

	# ACTUALISATION DES AMIS DEJA PRESENTS

	set_nna <- which(!is.na(big["name"]))
	big[set_nna,"dernier_surnom_facebook"] 	<- big[set_nna,"name"]	
	#modified <- c(modified,set_nna)

	set_na <- which(is.na(big["sexe"]))
	big[set_na,"sexe"] 	<- apply(data.frame(big[set_na,"gender"]),1,substr,1,1)	
	#modified <- c(modified,set_na)

	set_na 								<- which(is.na(big["anniv"]))
	chieurs 							<- which(apply(data.frame(big[set_na,"birthday"]),1,nchar)==5)
	big[set_na,"birthday"][chieurs]		<- paste0(big[set_na,"birthday"][chieurs],"/1500")
	big[set_na,"anniv"] 				<- as.Date(big[set_na,"birthday"],"%m/%d/%Y")
	#modified <- c(modified,set_na)

	# ACTUALISATION DE LA VILLE POUR LES AMIS DEJA PRESENTS
		#suppression des espaces avant et apres
		big["ville"] 			<-  apply(big["ville"],1,ds)
		big["ville_secondaire"] <-  apply(big["ville_secondaire"],1,ds)
		#remplacement des NA par ""
		big[which(is.na(big["ville"])),"ville"] 						<- ""
		big[which(is.na(big["ville_secondaire"])),"ville_secondaire"] 	<- ""
		big[which(is.na(big["location"])),"location"] 					<- ""
		big[which(is.na(big["hometown"])),"hometown"] 					<- ""
		big[which(is.na(big["lieux_precedents"])),"lieux_precedents"] 	<- ""
		#remplacement des "" par l'eventuelle ville nouvellement saisie
		strnul1 <- which(big["ville"           ] == "")
		strnul2 <- which(big["ville_secondaire"] == "")

		big[strnul1,"ville"           ] <- big[strnul1,"location"]
		big[strnul2,"ville_secondaire"] <- big[strnul2,"hometown"]

		set_nna <- which(big["hometown"]!="")
		diff <- which(big[set_nna,"hometown"] != big[set_nna,"ville_secondaire"])
		big[set_nna,"lieux_precedents"][diff] <- paste0( big[set_nna,"lieux_precedents"][diff]  ,  "  //  "  ,  big[set_nna,"ville_secondaire"][diff] )
		big[set_nna,"ville_secondaire"][diff] <- big[set_nna,"hometown"][diff]
		set_nna <- which(big["location"]!="")
		diff <- which(big[set_nna,"location"] != big[set_nna,"ville"])
		big[set_nna,"lieux_precedents"][diff] <- paste0( big[set_nna,"lieux_precedents"][diff]  ,  "  //  "  ,  big[set_nna,"ville"][diff] )
		big[set_nna,"ville"][diff] <- big[set_nna,"location"][diff]

	# ACTUALISATION DE LA COLONNE GMAPS POUR TOUS
	big[,"gmaps"] <- big[,"ville"]
	big$gm_proven <- "fb"
	setnnavil <- which( (big$adresse_voie!="" & !is.na( big$adresse_voie)) | (big$adresse_ville!="" & !is.na( big$adresse_ville)) | (big$adresse_CP!="" & !is.na( big$adresse_CP)) |(big$adresse_pays!="" & !is.na( big$adresse_pays)) )
	big[setnnavil,"gm_proven"] <- "manu"
	big[setnnavil,"gmaps"] <- paste(big[setnnavil,"adresse_voie"],paste(big[setnnavil,"adresse_CP"],big[setnnavil,"adresse_ville"],sep=" "),big[setnnavil,"adresse_pays"],sep=" , ")
	setnnacg <- which( big$gmaps_correcmanuelle!="" & !is.na( big$gmaps_correcmanuelle) )
	big[setnnacg,"gmaps"] <- big[setnnacg,"gmaps_correcmanuelle"]

## REMISE EN FORNE

	# suppression des doubles espaces et des espaces de début et fin
	big$nom <- unlist(lapply(lapply(big$nom,norm.del.spaces),ds))
	
## SUPPRESSION DES COLONES INNUTILES

	big <- big[,c(names(ams_db))]

## ENRAGISTREMENT DANS LA BDD

	num <- as.character(format(Sys.time(),"%Y%m%d%H%M%S"))
	q <- paste0("create table sauv",num," as (select * from ams);")
	dbQuery(0,conn,q)

	dbWriteTable(conn,paste0("temps",num),big,row.names=FALSE)

	# q <- paste0("ALTER TABLE temps",num," DROP \"row.names\" ")
	# dbQuery(0,conn,q)

	q <- paste0("delete from ams;")
	dbQuery(0,conn,q)

	q <- paste0("INSERT INTO ams SELECT * FROM temps",num)
	dbQuery(0,conn,q)
	
## Export pour Google maps

	export <- big
		# doublage des lignes ayant a la fois une adresse rentree manuellement et une ville renseignee par facebook
		temp1 <- big[setnnavil,]
		temp2 <- big[which(apply(big["ville"],1,norm.na)!=""),]
		temp1$ville            <- NA
		temp1$ville_secondaire <- NA
		temp2$adresse_voie   <- NA
		temp2$adresse_ville   <- NA
		temp2$adresse_CP   <- NA
		temp2$adresse_pays   <- NA
		temp1$gm_proven <- "manu"
		temp2$gm_proven <- "fb"
		temp2$gmaps <- temp2$ville 
		temp <- rbind(temp1,temp2)
		export <- temp
	# selection des colonnes
	export <- export[,c("nom","groupe","adresse_ville"	,"adresse_pays"	,"ville","ville_secondaire"	,"gmaps","gm_proven")]
	# cryptage du nom
 	export["nom"] <- apply(export["nom"],1,crypt)
 	export["nom"] <- gsub(" ","",export$nom)
 	# filtrage par groupe
 	export <- subset(export,! groupe %in% c(4:6))
 	# filtrage des valeurs nulles
 	export <- subset(export,nchar(gmaps)>2)
 	# aggregation pour les gens habitant exactement au même endroit selon goggle maps
 	export <- export[order(export$group),]
 	exportag <- aggregate(export[c("nom","groupe")],list(gmaps=export$gmaps,gm_proven=export$gm_proven),paste0,collapse=" ")
 	exportag[which(!exportag$groupe %in% c("1","2","3","4","5","6")),"groupe"] <- "mixed_or_NA"

 	# # tri
 	# export <- export[order(export$adresse_pays,export$adresse_ville,-export$groupe,export$ville),]
 	# export <- export[nrow(export):1,]

 	# # export des CSV
 	# for(i in -1+(1:ceiling( nrow(export)/100)))
 	# 	write.csv(export[(1+(100*i)):min((100*(i+1)),nrow(export)),],file=paste0("ams_",(1+(100*i)),"-",(100*(i+1)),".csv"),sep=",",row.names=false)

 	write.csv(exportag[which(exportag$gm_proven=="manu"),],file="adresse_manuelle.csv",row.names=FALSE)
 	write.csv(exportag[which(exportag$gm_proven!="manu" & exportag$group %in% c("1","2","mixed_or_NA")),],file="fb_gpes12.csv",row.names=FALSE)
 	write.csv(exportag[which(exportag$gm_proven!="manu" & exportag$group %in% c("3","4")),],file="fb_gpe3.csv",  row.names=FALSE)






NOT( a."id_ams"                                 = b."id_ams"                                )    OR
NOT( a."surnom_au12aout11"                      = b."surnom_au12aout11"                     )    OR
NOT( a."nom_reel"                              = b."nom_reel"                               )  OR
NOT( a."sexe"                                   = b."sexe"                                  )    OR
NOT( a."facebook"                               = b."facebook"                              )    OR
NOT( a."ville"                                  = b."ville"                                 )    OR
NOT( a."ville_secondaire"                       = b."ville_secondaire"                      )    OR
NOT( a."fb_uid"                                 = b."fb_uid"                                )    OR



NOT( a."dernier_surnom_facebook"                = b."dernier_surnom_facebook"               )    OR
NOT( a."note_explicite"                         = b."note_explicite"                        )    OR
NOT( a."note_explicite_complement"              = b."note_explicite_complement"             )    OR
NOT( a."groupe"                                 = b."groupe"                                )    OR
NOT( a."groupeBIS"                             = b."groupeBIS"                              )  OR
NOT( a."coeff_perso"                            = b."coeff_perso"                           )    OR
NOT( a."wm"                                     = b."wm"                                    )    OR
NOT( a."tel"                                   = b."tel"                                    )  OR
NOT( a."inconnu"                                = b."inconnu"                               )    OR
NOT( a."affich"                                 = b."affich"                                )    OR
NOT( a."en_double"                             = b."en_double"                              )  OR
NOT( a."lien_actif"                             = b."lien_actif"                            )    OR
NOT( a."derniers_sujets_traites"               = b."derniers_sujets_traites"                )  OR
NOT( a."idees_futures"                          = b."idees_futures"                         )    OR
NOT( a."lien_temp"                              = b."lien_temp"                             )    OR
NOT( a."date_ajout"                             = b."date_ajout"                            )    OR
NOT( a."dernier_contact"                       = b."dernier_contact"                        )  OR
NOT( a."nb_annulations_depuis_dernier_contact"  = b."nb_annulations_depuis_dernier_contact" )    OR
NOT( a."derniere_annulation"                    = b."derniere_annulation"                   )    OR
NOT( a."affich_innutile"                        = b."affich_innutile"                       )    OR
NOT( a."nombre_fois_premier"                   = b."nombre_fois_premier"                    )  OR
NOT( a."nombre_fois_top10"                      = b."nombre_fois_top10"                     )    OR
NOT( a."position_max"                           = b."position_max"                          )    OR
NOT( a."dernier_score"                          = b."dernier_score"                         )    OR
NOT( a."non_valide"                            = b."non_valide"                             )  OR
NOT( a."changement_pseudofb"                    = b."changement_pseudofb"                   )    OR
NOT( a."message_maintenance"                    = b."message_maintenance"                   )    OR
NOT( a."S"                                      = b."S"                                     )    OR
NOT( a."autres_infos"                          = b."autres_infos"                           )  OR
NOT( a."S_inv"                                  = b."S_inv"                                 )    OR
NOT( a."S_oui"                                  = b."S_oui"                                 )    OR
NOT( a."S_non"                                  = b."S_non"                                 )    OR
NOT( a."S_peu"                                 = b."S_peu"                                  )  OR
NOT( a."groupe_facebook"                        = b."groupe_facebook"                       )    OR
NOT( a."anniv"                                  = b."anniv"                                 )    OR
NOT( a."adresse_voie"                          = b."adresse_voie"                           )  OR
NOT( a."adresse_CP"                             = b."adresse_CP"                            )    OR
NOT( a."adresse_ville"                          = b."adresse_ville"                         )    OR
NOT( a."adresse_pays"                           = b."adresse_pays"                          )    OR
NOT( a."lieux_precedents"             = b."lieux_precedents"                              )	







a."id_ams"                          , b."id_ams"            ,
a."surnom_au12aout11"               , b."surnom_au12aout11" ,
a."nom_reel"                        ,b."nom_reel"           ,
a."sexe"                            , b."sexe"              ,
a."facebook"                        , b."facebook"          ,
a."ville"                           , b."ville"             ,
a."ville_secondaire"                , b."ville_secondaire"  ,
a."fb_uid"                          , b."fb_uid"            ,
