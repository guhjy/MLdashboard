#csv1 <- read.csv("NBA (2009-2013) All Games.csv")
csv1 <-  read.csv("NBA (2009-2013) All Games_RevB.csv")
csv2 <- read.csv("NBA (2013-2016) All Games.csv")

csv.merged <- merge(csv1, csv2, all=T)
csv.merged$sMargin <- csv.merged$sPTS - csv.merged$oPTS
ints <- sapply(csv.merged, is.integer)
csv.merged[,ints] <- sapply(csv.merged[,ints], as.numeric)
chars <- sapply(csv.merged, is.character)
csv.merged[,chars] <- sapply(csv.merged[,chars], as.factor)
csv <- csv.merged

save(csv, file="NBA full data_RevC.RData")




# csv <- read.csv("JANINE4.csv")
# 
# 
# csv <- read.csv("MODeigengene.csv")
# csv$Age <- as.numeric(as.character(csv$Age))
# csv$PatientID <- as.factor(csv$PatientID)
# csv$SOFA <- as.numeric(csv$SOFA)
# csv$Platelets <- as.numeric(csv$Platelets)
# csv$GCS <- as.numeric(csv$GCS)
# csv$Glucose <- as.numeric(csv$Glucose)
# csv$RespiratoryRate <- as.numeric(csv$RespiratoryRate)
# 
# csv.t1 <- read.csv("t1_fixed.csv", na.strings = "")
# csv.t1
# 
# csv.t3 <- read.csv("t3.csv", na.strings="")
# 
# csv.demo4 <- read.csv("joannademographics.csv", na.strings=c("","NA"))
# 
# merge(csv,csv.t1, by.x="")
# 
# small1 <- csv
# small2 <- droplevels(subset(csv.t1, !is.na(csv.t1$Timepoint)))
# small3 <- droplevels(subset(csv.t3, !is.na(csv.t3$Timepoint)))
# demo4 <- csv.demo4
# 
# total1 <- merge(small1, small2, by=c("PatientID","Timepoint"), all=T)
# total2 <- merge(total1, small3, by=c("PatientID","Timepoint"), all=T)
# total3 <- merge(total2, demo4, by=c("PatientID"), all=T)
# 
# totaldf <- total3[,c(1:3, 167:168, 154:156, 161, 4:46, 57:98, 115:150)]
# names(totaldf)
# totaldf$Device
# totaldf$ImplantType
# write.csv(totaldf[,-c(15)], "combinedMODdata.csv")
# 
# 
# 
# csv.b <- read.csv("bcells.csv", na.strings=c("","NA"))
# csv.cyto <- read.csv("Cytokine.csv", na.strings=c("","NA"))
# as.numeric(csv.cyto$Timepoint)
# csv.cyto$Timepoint[csv.cyto$Timepoint==1] <- "t1"
# csv.cyto$Timepoint[csv.cyto$Timepoint==2] <- "t2"
# csv.cyto$Timepoint[csv.cyto$Timepoint==3] <- "t3"
# csv.cyto$Timepoint[csv.cyto$Timepoint==4] <- "t4"
# csv.cyto$Timepoint[csv.cyto$Timepoint==5] <- "t5"
# csv.cyto$Timepoint <- as.factor(csv.cyto$Timepoint)
# 
# 
# total4 <- merge(csv, csv.b, by=c("PatientID","Timepoint"), all=T)
# total5 <- merge(total4, csv.cyto, by=c("PatientID","Timepoint"), all=T)
# 
# final.tmp <- subset(total5, !is.na(total5$PatientID) & !is.na(total5$Timepoint))
# final <- final.tmp[,-c(128:146, 170:177)]
# 
# names(final)[5] <- "Sensitized"
# names(final)
# write.csv(final, "combinedMODdata2.csv")
