###########################################################################################
###########################################################################################
###########################################################################################
# graphiques à partir des données de BP statistical des évolutions de la production #######
# mondiales de pétrole  gas et charbo / consommation mondiale de pétrole gas et charbon ###
# ainsi que les prix ######################################################################
###########################################################################################
###########################################################################################
###########################################################################################

### les packages####
library("gridExtra")
library("devtools")
library("ggplot2")
library(ggthemes)
library(extrafont)
library(plyr)
library("grid")

#####création dela fonction pour récupérer la légende
library(gridExtra)
get_legend<-function(myggplot){
  tmp <- ggplot_gtable(ggplot_build(myggplot))
  leg <- which(sapply(tmp$grobs, function(x) x$name) == "guide-box")
  legend <- tmp$grobs[[leg]]
  return(legend)
}

####on importe les données 
library(readr)
world_prod_mtoe <- read_delim("~/Documents/Documents - hihihi/these/data_base/bp_world/world_prod_mtoe.csv", 
                              ";", escape_double = FALSE)

library(readr)
world_cons_mtoe <- read_delim("~/Documents/Documents - hihihi/these/data_base/bp_world/world_cons_mtoe.csv", 
                              ";", escape_double = FALSE)

prod<-world_prod_mtoe
A<-ggplot(prod,aes(x=year,y=world,colour=energy))+geom_line(size=1)+theme_bw()+ggtitle("Production mondiale \n Source: BP statistical")+xlab("year")+ylab("Mt Oil Equivalent (Mtoe)")+scale_color_manual(values=c("black","red","blue"))+theme(legend.position="bottom")+scale_fill_discrete(name="Energy")
A

cons<-world_cons_mtoe
B<-ggplot(cons,aes(x=year,y=world,colour=energy))+geom_line(size=1)+theme_bw()+ggtitle("Consommation mondiale \n Source: BP statistical")+xlab("year")+ylab("Mt Oil Equivalent (Mtoe)")+scale_color_manual(values=c("black","red","blue"))+theme(legend.position="bottom")
B



#####################################################################################
#####################################################################################
library(readr)
oil_coal_gas_price_fed_dollar_cons_1983_100 <- read_delim("~/Documents/Documents - hihihi/these/data_base/bp_world/oil_coal_gas_price_fed_dollar_cons_1983_100.csv", 
                                                          ";", escape_double = FALSE)
price<-oil_coal_gas_price_fed_dollar_cons_1983_100
price
oil_price<-price[price$energy=="oil",]

oil_p<-ggplot(oil_price,aes(x=date,y=price))+geom_line(size=1,colour="blue")+theme_bw()+ggtitle("Prix du pétrole brut dollars constants\n Source: Federal Reserve Bank of St. Louis")+xlab("year")+ylab("US dollars per barrel")
oil_p

coal_price<-price[price$energy=="coal",]
coal_price
coal_p<-ggplot(coal_price,aes(x=date,y=price))+geom_line(size=1,colour="black")+theme_bw()+ggtitle("Prix mondial du charbon (Australie) en dollars constants  \n Source: Federal Reserve Bank of St. Louis")+xlab("year")+ylab("US dollars /metric ton")
coal_p
gas_price<-price[price$energy=="gas",] 
price
gas_p<-ggplot(gas_price,aes(x=date,y=price))+geom_line(size=1,colour="red")+theme_bw()+ggtitle("Prix mondial du gaz naturel dollars constants (Henry Hub gas) \n Source: Federal Reserve Bank of St. Louis")+xlab("year")+ylab("US Dollars per million metric British Thermal Unit")
gas_p
#########################
multi<- grid.arrange(A,oil_p,coal_p,gas_p,ncol=2)
###########################
###########################
############################
### décomposition des sèries###

oil_price2<-price[price$energy=="oil",-3]
oil_price2<-oil_price2$price
oil_price2
oil_price2<-ts(oil_price2,start=1947,frequency =12)
plot(oil_price2)
plot(decompose(oil_price2))
decom<-decompose(oil_price2,type="mult")
plot(decom)
Trend<-decom$trend
Seasonal<-decom$seasonal
ts.plot(cbind(Trend,Seasonal),lty=1:2)
plot(Trend)
str(Trend)
Trend
coal_price2<-price[price$energy=="coal",-3]
coal_price2
gas_price2<-price[price$energy=="gas",-3] 
coal_price2<-coal_price2$price
coal_price2<-ts(coal_price2,start=1980,frequency = 12)
gas_price2
gas_price2<-gas_price2$price
gas_price2<-ts(gas_price2,start=1991,frequency = 12)
plot(decompose(coal_price2,type="multi"))
plot(decompose(gas_price2,type="multi"))

