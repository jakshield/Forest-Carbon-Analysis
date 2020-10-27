plot_biomass_fn<-function(x){
#Coerece DBH to numeric class(just in case)
  x$dbh<-as.numeric(x$dbh)
#Convert DBH to centimeters and filter out small/dead stems
  y<-x %>%
    rowwise()%>%
    mutate(dbh_cm = dbh*2.54)%>%
    filter(dbh_cm >= 3)%>%
    filter(inv_status %in% c("RL","IL","IG"))

#Apply Allometric Equations to Individual Tree species_code 
   acru_bm<-y%>%filter(species_code == "ACERUB")%>%
     mutate(agb = exp((-2.0470+2.3852*log(dbh_cm))))
   prse_bm<-y%>%filter(species_code == "PRUSER")%>%
     mutate(agb = exp((-2.2118+2.4133*log(dbh_cm))))
   ca_bm<-y%>%filter(species_code == "CA")%>%
     mutate(agb = exp((-2.5095+2.6175*log(dbh_cm))))
   cagl_bm<-y%>%filter(species_code == "CARGLA")%>%
     mutate(agb = exp((-2.5095+2.6175*log(dbh_cm))))
   cato_bm<-y%>%filter(species_code == "CARTOM")%>%
     mutate(agb = exp((-2.5095+2.6175*log(dbh_cm))))
   fagr_bm<-y%>%filter(species_code == "FAGGRA")%>%
     mutate(agb = exp((-2.0705+2.4410*log(dbh_cm))))
   litu_bm<-y%>%filter(species_code == "LIRTUL")%>%
     mutate(agb = exp((-2.5497+2.5011*log(dbh_cm))))
   nysy_bm<-y%>%filter(species_code == "NYSSYL")%>%
     mutate(agb = exp((-2.2118+2.4133*log(dbh_cm))))
   pist_bm<-y%>%filter(species_code == "PINSTR")%>%
     mutate(agb = exp((-2.6177+2.4638*log(dbh_cm))))
   qual_bm<-y%>%filter(species_code == "QUEALB")%>%
     mutate(agb = exp((-2.0705+2.4410*log(dbh_cm))))
   rops_bm<-y%>%filter(species_code == "ROBPSE")%>%
     mutate(agb = exp((-2.5095+2.5437*log(dbh_cm))))
   oxar_bm<-y%>%filter(species_code == "OXYARB")%>%
     mutate(agb = exp((-2.2118+2.4133*log(dbh_cm))))
   qumo_bm<-y%>%filter(species_code == "QUEMON")%>%
     mutate(agb = exp((-2.0705+2.4410*log(dbh_cm))))
   cade_bm<-y%>%filter(species_code == "CASDEN")%>%
     mutate(agb = exp((-2.0705+2.4410*log(dbh_cm))))
   fram_bm<-y%>%filter(species_code == "FRAAME")%>%
     mutate(agb = exp((-1.8384+2.3524*log(dbh_cm))))

#Combine species_code biomass into plot biomass dataframe
   aboveground_bm<-rbind(acru_bm,prse_bm,ca_bm,cagl_bm, cato_bm, fagr_bm, litu_bm,
                         nysy_bm, pist_bm,qual_bm, rops_bm, oxar_bm, qumo_bm,cade_bm,fram_bm)

#Calculate Belowground biomass and total biomass
   plot_biomass<-aboveground_bm%>%
     mutate(bgb_cr= exp((-1.4485-0.03476*log(dbh_cm))))%>%
     mutate(bgb_fr= exp((-1.8629-0.77534*log(dbh_cm))))%>%
     mutate(biomass = agb+bgb_cr+bgb_fr)

#Calculate total biomass and carbon stock
     carbon_stock<-(((((sum(plot_biomass$biomass)/1000)/400))*10000)*0.47)

   #return(plot_biomass)
   return(carbon_stock)
}

