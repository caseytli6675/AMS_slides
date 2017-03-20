# cli
# Feb. 14, 2016
# file description: retrive MSCI index data

libraries <- ( "Rbbg")
lapply(libraries, function(lib){ library(lib, character.only=TRUE)})
setwd("H:/CLi/3 AMS Slides/")
# MSCI filelds
value.fields <- c("MSCI_PX_EARN","MSCI_P_12M_FWD_EARN","MSCI_DVD_YLD","MSCI_PX_BV",
                  "MSCI_ROE","MSCI_LT_FWD_EPS_G_RATE","PX_LAST")
index  <- c("MXUS INDEX","MXCA INDEX","MXEA INDEX","MXEF INDEX")
# GET MSCI COUNTRY DATA
MSCIcountry <- c("MXFR INDEX","MXAT INDEX","MXBE INDEX",
                 "MXDE INDEX","MXDK INDEX",
                 "MXFI INDEX","MXIE INDEX","MXIL INDEX","MXIT INDEX","MXNL INDEX",
                 "MXNO INDEX","MXPT INDEX","MXES INDEX","MXSE INDEX","MXCH INDEX",
                 "MXGB INDEX","MXAU INDEX","MXHK INDEX","MXJP INDEX","MXNZ INDEX",
                 "MXSG INDEX", index)
country <- c(
  "FR","AT ","BE ","DE ","DK ",
  "FI ","IE ","IL ","IT ","NL ",
  "NO ","PT ","ES ","SE ","CH ",
  "GB ","AU ","HK ","JP ","NZ ",
  "SG","US ","CA ","EM","EAFE")

emCountry <- c("MXBR Index","MXCL Index","MXCN Index","MXCO Index","MXCZ Index",
                         "MXEG Index","MXHU Index","MXIN Index","MXID Index","MXKR Index",
                         "MXMY Index","MXMX Index","MXMA Index","MXGR Index","MXPE Index",
                         "MXPH Index","MXPL index","MXRU Index","MXZA Index","TAMSCI Index",
                         "MXTH Index","MXTR Index")
tx = substring(emCountry,3,4)
allcountry.name <- c(country,tx)

# Time Period
start.date <- as.POSIXct("1995-01-01","EST")
#end.date <- as.POSIXct("2016-05-13","EST")
end.date <- Sys.Date()

option.fields <- c("periodicitySelection", "nonTradingDayFillOption",
                   "nonTradingDayFillMethod", "periodicityAdjustment",
                   "adjustmentFollowDPDF")

option.values <- c("MONTHLY", "ALL_CALENDAR_DAYS","PREVIOUS_VALUE", "CALENDAR", "TRUE")
AllCountry <- c(MSCIcountry,emCountry)

# Bbg
conn <- blpConnect()
getindexData <- function(msci){
  value.data <- bdh(conn,msci,fields = value.fields,start.date,end.date, 
                    option_names=option.fields,option_values = option.values)
}
allindexData <- lapply(AllCountry,getindexData)

save(allindexData,file = "allCountry.Rdata")
