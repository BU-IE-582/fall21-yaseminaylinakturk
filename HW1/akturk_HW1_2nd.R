install.packages('jpeg')
install.packages('imager')

library(jpeg)
library(imager)

klasor =  "C:/Users/y.akturk/Documents/"
setwd(klasor)
img <- readJPEG("ya.jpg")

#class of image 
class(img)  
str(img)

#dimension 
#there is 3 layers: they correspond to your R, G and B values. In each layer, each cell is a pixel.
dim(img)   

N=NCOL(img)
M=NROW(img)

N
M

#sample of the format
img[35:39,50:54,]


#plot with rasterimage ----
par(mfrow=c(2,2))

plot(0:1,0:1,type="n",axes=FALSE,xlab='',ylab='', main="original")
rasterImage(img,0,0,1,1)

redme <- img
redme[1:M,1:N,2] = 0
redme[1:M,1:N,3] = 0
plot(0:1,0:1,type="n",axes=FALSE,xlab='',ylab='', main="Channel1")
rasterImage(redme,0,0,1,1)

blueme <- img
blueme[1:M,1:N,1] = 0
blueme[1:M,1:N,3] = 0
plot(0:1,0:1,type="n",axes=FALSE,xlab='',ylab='', main="Channel2")
rasterImage(blueme,0,0,1,1)

greenme <- img
greenme[1:M,1:N,1] = 0
greenme[1:M,1:N,2] = 0
plot(0:1,0:1,type="n",axes=FALSE,xlab='',ylab='', main="Channel3")
rasterImage(greenme,0,0,1,1)


#plot red, blue, green images----

par(mfrow=c(2,2))

me <- load.image("ya.jpg")
plot(me,axes=FALSE,main = "me")


me_red <- load.image("ya.jpg")

#channel(me_red,1) 
channel(me_red,2) <- 0
channel(me_red,3) <- 0

plot(me_red,axes=FALSE,main = "red me")


me_green <- load.image("ya.jpg")

channel(me_green,1) <-  0
#channel(me_green,2)
channel(me_green,3) <- 0

plot(me_green,axes=FALSE, main = "green me")


me_blue <- load.image("ya.jpg")

channel(me_blue,1) <-  0
channel(me_blue,2) <-  0
#channel(me_blue,3) 

plot(me_blue,axes=FALSE,main = "blue me")


###For each channel, take the average of the columns and plot the average----
#as a line plot for each channel on a single plot. 


img <- readJPEG("ya.jpg")


ch1 <- list(colMeans(img[,,1]))
ch2 <- list(colMeans(img[,,2]))
ch3 <- list(colMeans(img[,,3]))

par(mfrow=c(2,2))
colmn <- c(1:512)
plot(colmn,ch1[[1]], col= "red", type ="l", ylab= "ch1")
plot(colmn,ch2[[1]], col= "green", type ="l" , ylab= "ch2")
plot (colmn,ch3[[1]], col= "blue", type ="l", ylab= "ch3")


plot(colmn,ch1[[1]], col= "red", type ="l", ylab= "ch", main= "Ave. of the columns for each channels")
legend(text.font=0.5,cex= 0.5,"bottomright", legend=c("Ch1", "Ch2","Ch3" ), lty= 1:3, col=c("red","green", "blue"))
points(colmn,ch2[[1]], col= "green", type ="l")
points (colmn,ch3[[1]], col= "blue",type ="l")






#subtract one half of the image from the other half ----

img <- readJPEG("ya.jpg")


plot(0:1,0:1,type="n",axes=FALSE, main="original")
rasterImage(img,0,0,1,1)

Nnew= N/2


par(mfrow=c(2,2))

sep1= img[1:M,1:Nnew,]
plot(0:1,0:1,type="n",axes=FALSE, xlab='',ylab='',main="1sthalf")
rasterImage(sep1,0,0,0.5,1)


sep2=  img[1:M,(Nnew+1):N,]
plot(0:1,0:1,type="n",axes=FALSE, xlab='',ylab='',main="2ndhalf")
rasterImage(sep2,0,0,0.5,1)

#first half-second half
newimg = img[1:M,1:Nnew,] - img[1:M,(Nnew+1):N,]


for (c in 1:3 )
{
for (i in 1:M) 
  
{
  for (j in 1:Nnew)
  {
    
    if ( newimg [i,j,c] < 0) 
    {
      newimg [i,j,c] = 0 
      
    }
    
  }
  
}

} #end c
plot(0:1,0:1,type="n",axes=FALSE,xlab='',ylab='', main="first half-second half")
rasterImage(newimg,0,0,0.5,1)

#secondhalf-firsthalf

newimg2 =  img[1:M,(Nnew+1):N,] - img[1:M,1:Nnew,] 

for (c in 1:3 )
{

for (i in 1:M) 
  
{
  for (j in 1:Nnew)
  {
    
    if ( newimg2 [i,j,c] < 0) 
    {
      newimg2 [i,j,c] = 0 
      
    }
    
  }
  
}

} #end c

plot(0:1,0:1,type="n",axes=FALSE, xlab='',ylab='',main="secondhalf-firsthalf")
rasterImage(newimg2,0,0,0.5,1)


# Plot images in each channel

par(mfrow=c(2,2))

newredme <- newimg
newredme[1:M,1:Nnew,2] = 0
redme[1:M,1:Nnew,3] = 0
plot(0:1,0:1,type="n",axes=FALSE,xlab='',ylab='', main="Half_Ch1_Red")
rasterImage(newredme,0,0,0.5,1)

newblueme <- newimg
newblueme[1:M,1:Nnew,1] = 0
newblueme[1:M,1:Nnew,3] = 0
plot(0:1,0:1,type="n",axes=FALSE, xlab='',ylab='',main="Half_Ch2_Green")
rasterImage(newblueme,0,0,0.5,1)

newgreenme <- newimg
newgreenme[1:M,1:Nnew,1] = 0
newgreenme[1:M,1:Nnew,2] = 0
plot(0:1,0:1,type="n",axes=FALSE, xlab='',ylab='',main="Half_Ch3_Blue")
rasterImage(newgreenme,0,0,0.5,1)



#add noise----

set.seed(1234567)

img <- readJPEG("ya.jpg")
par(mfrow=c(1,2))

plot(0:1,0:1,type="n",axes=FALSE,xlab='',ylab='', main="original")
rasterImage(img,0,0,1,1)

#max of each channel
N= dim(img)[1]
M=  dim(img)[2]


M1=max(img[,,1])
M2= max(img[,,2])
M3= max(img[,,3])

noisyimg <- img

noisyimg[,,1] <-  img[,,1] + runif(1,0, 0.1* M1) 
noisyimg[,,2] <-  img[,,2] + runif(1,0, 0.1* M2) 
noisyimg[,,3] <-  img[,,3] + runif(1,0, 0.1* M3) 


plot(0:1,0:1,type="n",axes=FALSE,xlab='',ylab='', main="noisy")
rasterImage(noisyimg,0,0,1,1)

#in each channel

par(mfrow=c(1,3))

nosiyredme <- noisyimg
nosiyredme[1:M,1:N,2] = 0
nosiyredme[1:M,1:N,3] = 0
plot(0:1,0:1,type="n",axes=FALSE,xlab='',ylab='', main="Noisy_Red")
rasterImage(nosiyredme,0,0,1,1)

nosiyblueme <- noisyimg
nosiyblueme[1:M,1:N,1] = 0
nosiyblueme[1:M,1:N,3] = 0
plot(0:1,0:1,type="n",axes=FALSE, xlab='',ylab='',main="Noisy_Green")
rasterImage(nosiyblueme,0,0,1,1)

nosiygreenme <- noisyimg
nosiygreenme[1:M,1:N,1] = 0
nosiygreenme[1:M,1:N,2] = 0
plot(0:1,0:1,type="n",axes=FALSE, xlab='',ylab='',main="Noisy_Blue")
rasterImage(nosiygreenme,0,0,1,1)







