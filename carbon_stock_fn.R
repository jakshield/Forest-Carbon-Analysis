carbon_stock_analysis.fn<-function(...){
#Create list of inputs and apply biomass function to all elements
  data.list<-list(...)
  data.list<-lapply(data.list,eren_data_biomass_fn)
  data.df<-do.call(rbind, data.list)

  grouped<-data.df%>%
  group_by(plot.name)%>%
  summarise(plot_biomass_kg = sum(tree.biomass),
              plot_ba_sqm = sum(basal.area),
              carbon_stock = (((((sum(tree.biomass)/1000)/400))*10000)*0.47),
              trees_present = n())
  #Find most common tree
  common<-data.df%>%
    group_by(plot.name)%>%
    summarise(most_common_tree = names(table(species.code))[which.max(table(species.code))])
  #Find tree with greatest total biomass
  largest_tree<-data.df%>%
    group_by(plot.name, species.code)%>%
    summarise(total_tree.biomass = sum(tree.biomass))%>%
    slice(which.max(total_tree.biomass))
    
  #Find tree with greatest total volume
  volume<-data.df%>%
    group_by(plot.name, species.code)%>%
    summarise(total_ba = sum(basal.area))%>%
   slice(which.max(total_ba))


  final<-cbind.data.frame(grouped,common,volume,largest_tree)
  final<-final[,c(1,2,3,4,5,7,9,10,12,13)]
  colnames(final)<-c("plot.name", "plot.biomass.kg","plot.ba.sqm","carbon.stock_mg/ha","trees.present","most.common.tree", "greatest.ba", "total.species.ba","greatest.bm", "total.species.bm")
  return(final)
 }

