data_biomass_fn<-function(x){
  #Coerece DBH to numeric class(just in case)
  x$dbh.cm<-as.numeric(x$dbh.cm)
  
  #Convert DBH to centimeters and filter out small/dead stems
  z<-x %>%
    filter(x$dbh.cm >= 3)
  y<-z%>%
    filter(z$inventory %in% c("RL","IL","IG"))
  
  #Apply Allometric Equations to Taxonomic Groups
  aceraceae_bm<-y%>%
    filter(y$species.code %in% c('ACERUB','ACESAC'))
  aceraceae_bm<-aceraceae_bm%>%  
    mutate(abovegroundbiomass = exp((-2.0470+2.3852*log(aceraceae_bm$dbh.cm))))
  
  cornaceae_etc_bm<-y%>%
    filter(y$species.code %in% c('AMEARB','CORFLO','NYSSYL','OXYARB','PLAOCC','PRUAVI','PRUSER','PRUVIR','RHOSPP','SASALB','ULMAME','ULMRUB','ULMSPP'))
  cornaceae_etc_bm<-cornaceae_etc_bm%>%
    mutate(abovegroundbiomass = exp((-2.2118+2.4133*log(cornaceae_etc_bm$dbh.cm))))
  
  fabaceae_carya_bm<-y%>%
    filter(y$species.code %in% c('CARCOR','CARGLA','CAROVA','CARPAL','CARTOM','CARSPP'))
  fabaceae_carya_bm<-fabaceae_carya_bm%>%  
    mutate(abovegroundbiomass = exp((-2.5095+2.6175*log(fabaceae_carya_bm$dbh.cm))))
  
  fagaceae_decidious_bm<-y%>%
    filter(y$species.code %in% c('FAGGRA','QUEALB','QUECOC','QUEMON','QUERUB','QUEVEL','CASDEN'))
  fagaceae_decidious_bm<-fagaceae_decidious_bm%>%
    mutate(abovegroundbiomass = exp((-2.0705+2.4410*log(fagaceae_decidious_bm$dbh.cm))))
    
  oleaceae_bm<-y%>%
    filter(y$species.code %in% c('FRAAME'))
  oleaceae_bm<-oleaceae_bm%>%
    mutate(abovegroundbiomass = exp((-1.8384+2.3524*log(oleaceae_bm$dbh.cm))))
  
  hamameldaceae_bm<-y%>%
    filter(y$species.code %in% c('HAMVIR'))
  hamameldaceae_bm<-hamameldaceae_bm%>%
    mutate(abovegroundbiomass = exp((-2.6390+2.5466*log(hamameldaceae_bm$dbh.cm))))
  
  fabaceae_juglandaceae_bm<-y%>%
    filter(y$species.code %in% c('JUGNIG','ROBPSE'))
  fabaceae_juglandaceae_bm<-fabaceae_juglandaceae_bm%>%
    mutate(abovegroundbiomass = exp((-2.5095+2.5437*log(fabaceae_juglandaceae_bm$dbh.cm))))
  
  magnoliaceae_bm<-y%>%
    filter(y$species.code %in% c('LIRTUL', 'MAGACU'))
  magnoliaceae_bm<-magnoliaceae_bm%>%
    mutate(abovegroundbiomass = exp((-2.5497+2.5011*log(magnoliaceae_bm$dbh.cm))))
  
  betulaceae_bm<-y%>%
    filter(y$species.code %in% c('OSTVIR'))
  betulaceae_bm<-betulaceae_bm%>%
    mutate(abovegroundbiomass = exp((-2.2652+2.5349*log(betulaceae_bm$dbh.cm))))
  
  pinus_greaterthanpointfourfive<-y%>%
    filter(y$species.code %in% c('PINECH','PINVIR'))
  pinus_greaterthanpointfourfive<-pinus_greaterthanpointfourfive%>%
    mutate(abovegroundbiomass = exp((-3.0506+2.6465*log(pinus_greaterthanpointfourfive$dbh.cm))))
  
  pinus_lessthanpointfourfive<-y%>%
    filter(y$species.code %in% c('PINSTR'))
  pinus_lessthanpointfourfive<-pinus_lessthanpointfourfive%>%
    mutate(abovegroundbiomass = exp((-2.6177+2.4638*log(pinus_lessthanpointfourfive$dbh.cm))))
  
  #Combine Taxonomic Groups 
  aboveground_biomass.df<-rbind(aceraceae_bm,cornaceae_etc_bm,fabaceae_carya_bm,fagaceae_decidious_bm,oleaceae_bm, hamameldaceae_bm, fabaceae_juglandaceae_bm, magnoliaceae_bm, betulaceae_bm,pinus_greaterthanpointfourfive, pinus_lessthanpointfourfive)
  
  #Calculate Belowground Biomass and Total Biomass
  EREN_biomass.df<-aboveground_biomass.df%>%
    mutate(course.root.biomass = exp((-1.4485-0.03476*log(aboveground_biomass.df$dbh.cm))))%>%
    mutate(fine.root.biomass = exp((-1.8629-0.77534*log(aboveground_biomass.df$dbh.cm))))%>%
    mutate(tree.biomass = abovegroundbiomass+course.root.biomass+fine.root.biomass)
  
  #Filter to desired data
  #EREN_biomass.df<-EREN_biomass.df[,c('plot name','data colle', 'tree numbe', inventory, 'stem type',species.code, dbh.cm, 'abovegroundbiomass', 'course.root.biomass','fine.root.biomass', 'tree.biomass')]
  #colnames(EREN_biomass.df)<-c('plot_name','date_collected', 'tree_number', inventory, 'stem_type','species_code', 'dbh_cm', 'aboveground_biomass', 'course_root_biomass','fine_root_biomass', 'tree_biomass')
  
  #Filter to most recent dates
  #EREN_biomass.df<-EREN_biomass.df%>%
    #group_by(plot_name)%>%
    #filter(date_collected == max(date_collected))
  #Calulate Basal Area
  EREN_biomass.df<-EREN_biomass.df%>%
    mutate(basal_area = ((dbh.cm)^2)*0.00007854)
  
  return(EREN_biomass.df)
  }