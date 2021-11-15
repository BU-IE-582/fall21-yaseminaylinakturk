#IE582 hw2-yasemin aylin akturk
#task2
klasor =  "C:/Users/y.akturk/Documents/"
setwd(klasor)
install.packages("readxl")
library("readxl")
library("jpeg")

# xls files
citydist <- read_excel("ilmesafe.xls",col_names = TRUE)

head(citydist[1:6,1:6])
dim(citydist)
#only the lower part
#do not take the row names
distance_matrix <- as.dist(citydist[,-1])
print(distance_matrix)


# MDS, default k=2
city_mds <- stats:::cmdscale(distance_matrix)
# print the coordinates for each city
city_mds[1:10,]
summary(city_mds)

#plot map
plot(city_mds)
#text(city_mds[,1], city_mds[,2], labels=rownames(city_mds) , cex= 0.6)
text(city_mds[,1], city_mds[,2], labels=rownames(city_mds),
     col=ifelse(rownames(city_mds) == "ÝSTANBUL" |
                  rownames(city_mds) ==  "ANKARA" |
                  rownames(city_mds) ==  "SÝNOP"|
                  rownames(city_mds) ==  "HATAY"|
                  rownames(city_mds) ==  "ÝZMÝR"  |
                  rownames(city_mds) ==  "VAN"|
                  rownames(city_mds) ==  "ANTALYA"  |
                  rownames(city_mds) ==  "ÇANAKKALE" |
                  rownames(city_mds) ==  "SÝVAS" |
                  rownames(city_mds) ==  "TRABZON" 
                , "red", "black") 
     , cex= 0.5)


mapimg <- readJPEG("turkeymap.jpg")

plot(city_mds, xlab= "", ylab="", axes = F, col="white")
rasterImage(mapimg,-920,-490,1050,450)

