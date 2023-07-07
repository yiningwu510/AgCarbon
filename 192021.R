r <- stack( "data/Temp/CDLcropped/CDL_IA_2019.tif",
           "data/Temp/CDLcropped/CDL_IA_2020.tif", "data/Temp/CDLcropped/CDL_IA_2021.tif")

raster <-r
# which cells are not NA? These ones:
#notna <- which(!is.na(values(r)))
notna <-which(!is.na(values(raster[[1]])))
# grab 20k cell index numbers at random
samp <- sample(notna, 40000, replace = FALSE)
# and their values
sampdata <- raster[samp]
# and their location coordinates
samplocs <- xyFromCell(raster, samp)

# convert to a data frame
samp <- as.data.frame(cbind(samplocs, samp, sampdata))
names(samp) <- c('x', 'y', 'index', 'CDL19', 
                 'CDL20','CDL21')


# samp$value[samp$value=="1"] <-'corn'
# samp$value[samp$value=="5"] <-'soybean'
# samp$value[samp$value=="61"] <-'fallow'

sampCSF <- samp[samp$CDL19=="1" | samp$CDL19=="5"| samp$CDL19=="61", ]
sampCSF <- sampCSF[sampCSF$CDL20=="1" | sampCSF$CDL20=="5"| sampCSF$CDL20=="61", ]
sampCSF <- sampCSF[sampCSF$CDL21=="1" | sampCSF$CDL21=="5"| sampCSF$CDL21=="61", ]
sampCSF[ sampCSF == "5" ] <- 100
sampCSF[ sampCSF == "61" ] <- 10000
sampCSF$sum <- apply(sampCSF[,c(4:6)], 1, sum)
table(sampCSF$sum)
write.csv(sampCSF, "data/Temp/IACSF192021.csv", row.names=TRUE)