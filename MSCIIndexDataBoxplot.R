# cli
# Feb. 14, 2016
# file description: output MSCI country factor data for AMS meeting slides

libraries <- c("TTR","xts")
lapply(libraries, function(lib){ library(lib, character.only=TRUE)})

# Boxplot
fields <- names(allindexData[[1]])[-1]
dates <- allindexData[[1]][,1]


for (i in 1:length(allindexData)){
  allindexData[[i]][,length(fields)+1] <- ROC(allindexData[[i]][,length(fields)+1],12)
}

titles <- c("PE Ratio 1/31/96 - 01/31/2017 mthly data", "PE 12M Forward 6/30/03 - 01/31/2017 mthly data",
                  "Dividends Yield 1/31/96 - 01/31/2017 mthly data", " PB Ratio 1/31/96 - 01/31/2017 mthly data",
                  "ROE Ratio 12/31/02 - 01/31/2017 mthly data", " EPS LT FORWARD 6/30/03 - 01/31/2017 mthly data",
                  "TRAILING 12M RETURN 1/31/96 - 01/31/2017 mthly data")
country.dm <- c(
  "FR","AT ","BE ","DE ","DK ",
  "FI ","IE ","IL ","IT ","NL ",
  "NO ","PT ","ES ","SE ","CH ",
  "GB ","AU ","HK ","JP ","NZ ","SG","US ","CA ")

emCountry <- c("MXBR Index","MXCL Index","MXCN Index","MXCO Index","MXCZ Index",
               "MXEG Index","MXHU Index","MXIN Index","MXID Index","MXKR Index",
               "MXMY Index","MXMX Index","MXMA Index","MXGR Index","MXPE Index",
               "MXPH Index","MXPL index","MXRU Index","MXZA Index","TAMSCI Index",
               "MXTH Index","MXTR Index")
country.em  <- substring(emCountry,3,4)

country4 <- c("MXUS ","MXCA ","MXEA ","MXEF ")

#All country (ex 4)

allcountry.fields <- NULL
z <- 1
for (i in fields){  
  allcountry <- NULL
  for (j in 1:length(country.dm)){ # top 23
    x<- allindexData[[j]]
    if (i == "MSCI_PX_EARN"){
      lb <- -20 
      ub <- 50
    } else if (i == "MSCI_P_12M_FWD_EARN"){
      ub <- 30
      lb <- (mean(x[,i])-1*sd(x[,i]))
    } else if (i == "MSCI_PX_BV"){
      ub <- 10
      lb <- (mean(x[,i])-1*sd(x[,i]))
    } else {  
      lb <- (mean(x[,i])-1*sd(x[,i]))
      ub <- (mean(x[,i])+1*sd(x[,i]))
    }  
    
    x[,i][x[,i]<lb] = lb
    x[,i][x[,i]>ub] = ub
    allcountry <- cbind(allcountry,x[,i])
  }
    allcountry.ts <- xts(allcountry, as.Date(dates, format = "%Y-%m-%d"))
    allcountry.ts <- na.locf(allcountry.ts, na.rm = TRUE)
    allcountry.mtx <- as.matrix(allcountry.ts)
    mscipef.curr <- tail(allcountry,1)
    png(filename= paste(i,".png",sep = ""),width = 720, height = 480,
                                           units = "px", pointsize = 12)
    bp <- boxplot(allcountry.mtx,las=2,col=rep("darkslategray1",ncol(allcountry.mtx),sep="")
                  ,at=c(1:ncol(allcountry.mtx)),names = country.dm,
                   main = titles[z] )
    xi <- 0.3 + seq(bp$n)
    #points(xi, mscipef.mean, col = "orange", pch = 19)
    #arrows(xi, mscipef.mean - mscipef.sd, xi, mscipef.mean + mscipef.sd,
    #       code = 3, col = "pink", angle = 75, length = .1)
    points(xi, mscipef.curr, col = "red", pch = 7)
    legend("topright", 
            legend=c( "recent"),
            col=c("red"), lwd=1, lty=c(1,2), 
            pch=c(7) )
   
    dev.off()
    z <- z + 1
 # allcountry.fields <- cbind(allcountry,allcountry.fields)
}
dm.country.mean <- allcountry.mtx  
#for 4 country
allcountry.fields <- NULL
z <- 1
for (i in fields){  
  allcountry <- NULL
  for (j in (length(country.dm)-2+1):(length(country.dm)+2)){ #22-25
    x<- allindexData[[j]]
    x[,i][x[,i]<lb] = lb
    x[,i][x[,i]>ub] = ub
    allcountry <- cbind(allcountry,x[,i])
  }
  allcountry.ts <- xts(allcountry, as.Date(dates, format = "%Y-%m-%d"))
  allcountry.ts <- na.locf(allcountry.ts, na.rm = TRUE)
  allcountry.mtx <- as.matrix(allcountry.ts)
  mscipef.curr <- tail(allcountry,1)
  png(filename= paste(i,"_country4.png",sep = ""),width = 720, height = 480,
      units = "px", pointsize = 12)
  bp <- boxplot(allcountry.mtx,las=2,col=rep("darkslategray1",ncol(allcountry.mtx),sep="")
                ,at=c(1:ncol(allcountry.mtx)),names = country4,
                main = titles[z] )
  xi <- 0.3 + seq(bp$n)
  #points(xi, mscipef.mean, col = "orange", pch = 19)
  #arrows(xi, mscipef.mean - mscipef.sd, xi, mscipef.mean + mscipef.sd,
  #       code = 3, col = "pink", angle = 75, length = .1)
  points(xi, mscipef.curr, col = "red", pch = 7)
  legend("topright", 
         legend=c( "recent"),
         col=c("red"), lwd=1, lty=c(1,2), 
         pch=c(7) )
  
  dev.off()
  z <- z + 1
  # allcountry.fields <- cbind(allcountry,allcountry.fields)
}


# for EM

allcountry.fields <- NULL
z <- 1
for (i in fields){  
  allcountry <- NULL
  for (j in (length(country.dm)+2+1):(length(allindexData))){ #26-47
    x<- allindexData[[j]]
    if (i == "MSCI_PX_EARN"){
      lb <- -20 
      ub <- 50
    } else if (i == "MSCI_P_12M_FWD_EARN"){
      ub <- 30
      lb <- (mean(x[,i])-1*sd(x[,i]))
    } else if (i == "MSCI_PX_BV"){
      ub <- 10
      lb <- (mean(x[,i])-1*sd(x[,i]))
    } else if (i == "MSCI_ROE"){
      ub <- (mean(x[,i])+1*sd(x[,i]))
      lb <- -30
    }else {  
      lb <- (mean(x[,i])-1*sd(x[,i]))
      ub <- (mean(x[,i])+1*sd(x[,i]))
    }  
    
    x[,i][x[,i]<lb] = lb
    x[,i][x[,i]>ub] = ub
    allcountry <- cbind(allcountry,x[,i])
  }
  allcountry.ts <- xts(allcountry, as.Date(dates, format = "%Y-%m-%d"))
  allcountry.ts <- na.locf(allcountry.ts, na.rm = TRUE)
  allcountry.mtx <- as.matrix(allcountry.ts)
  mscipef.curr <- tail(allcountry,1)
  png(filename= paste(i,"_countryEM.png",sep = ""),width = 720, height = 480,
      units = "px", pointsize = 12)
  bp <- boxplot(allcountry.mtx,las=2,col=rep("darkslategray1",ncol(allcountry.mtx),sep="")
                ,at=c(1:ncol(allcountry.mtx)),names = country.em,
                main = titles[z] )
  xi <- 0.3 + seq(bp$n)
  #points(xi, mscipef.mean, col = "orange", pch = 19)
  #arrows(xi, mscipef.mean - mscipef.sd, xi, mscipef.mean + mscipef.sd,
  #       code = 3, col = "pink", angle = 75, length = .1)
  points(xi, mscipef.curr, col = "red", pch = 7)
  legend("topright", 
         legend=c( "recent"),
         col=c("red"), lwd=1, lty=c(1,2), 
         pch=c(7) )
  
  dev.off()
  z <- z + 1
  # allcountry.fields <- cbind(allcountry,allcountry.fields)
}
em.country.mean <- allcountry.mtx
