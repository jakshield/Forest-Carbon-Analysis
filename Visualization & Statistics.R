#Library
library(ggplot2)
library(ggpubr)
library(car)

### Mean and SE of plots
#Take mean and se of WWC plots
mean(plot_carbon_stock[c(1:3),3])
(sd(plot_carbon_stock[c(1:3),3]))/(sqrt(length(plot_carbon_stock[c(1:3),3])))

#Take mean and se of all plots
mean(plot_carbon_stock[,3])
(sd(plot_carbon_stock[,3]))/(sqrt(length(plot_carbon_stock[,3])))

###Graph of plot carbon
#ungrouped 
carbon.stock<-ggplot(plot_carbon_stock,aes(plot_name, carbon_stock, color = plot_name))+
  geom_point()+
  geom_hline(yintercept = mean(plot_carbon_stock$carbon_stock), linetype = "dashed", color = "red")+
  labs(x= "Plot Name", y = "Carbon Stock (Mg/ha)", title = "Plot Carbon Stock")+
  theme(axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.5), legend.position = "none")+
  scale_color_discrete(name = "Forest Type")


#Grouped by forest type
plot.carbon.stock.forest.type<-ggplot(plot_carbon_stock,aes(plot_name, carbon_stock, color = indc_spp))+
  geom_point()+
  geom_hline(yintercept = mean(plot_carbon_stock$carbon_stock), linetype = "dashed", color = "red")+
  labs(x= "Plot Name", y = "Carbon Stock (Mg/ha)", title = "Plot Carbon Stock")+
  theme(axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.5), legend.position = "none")+
  facet_grid(.~indc_spp, scales = "free", space = "free_x")+
  scale_color_discrete(name = "Forest Type")

#### Plot Carbon and Forest Type
#boxplot
plot.carbon.boxplot<-ggplot(plot_carbon_stock, aes(x = indc_spp, y = carbon_stock, fill = indc_spp))+
  geom_boxplot()+
  geom_point()+
  labs(x = "Forest Type", y = "Carbon Stock Mg/ha", title = "Average Carbon Stock of Forest Type")+
  theme(legend.position = "none")

##ANOVA
forest_group.aov<-aov(carbon_stock~indc_spp, plot_carbon_stock)


###Assumption Tests
#Normality
shapiro.test(residuals(forest_group.aov))

#Homogneity  of Variance 
leveneTest(carbon_stock~indc_spp, plot_carbon_stock)

#Summary
summary(forest_group.aov)
TukeyHSD(forest_group.aov)

###Basal Area
#Box plot
ggplot(plot_carbon_stock, aes(indc_spp,ba_sqm_ha, fill=indc_spp))+
  geom_boxplot()+
  geom_point()+
  labs(x = "Forest Type", y = "Basal Area sq. m/ha", title = "Basal Area of Forest Types")+
  theme(legend.position = "none")

#ANOVA
basal_area.aov<-aov(ba_sqm_ha~indc_spp, plot_carbon_stock)
summary(basal_area.aov)
TukeyHSD(basal_area.aov)

#regression 
f.type<-ggscatter(plot_carbon_stock, x = "ba_sqm_ha", y = "carbon_stock", color = "indc_spp", add = "reg.line")+
  stat_regline_equation(aes(label =  paste(..eq.label..,..rr.label.., sep = "~~~~"), color = indc_spp), label.x.npc = 0.75, label.y.npc = 0.51, show.legend = F)+
  labs(x = "Plot Basal Area sq. m/ha", y = " Carbon Stock Mg/ha")+
  theme(legend.position = "right")

ggpar(f.type, legend.title = "Forest Type")

#size class
ggplot(tree.summary, aes(size_class, tree_carbon_mg*forest_group_ef))+
  geom_bar(stat = "identity")+
  facet_wrap(~forest_group_spp)+
  aes(fill = forest_group_spp)+
  theme(axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.5), legend.position = "none")+
  labs(x =  "DBH (cm) Size Class", y = "Carbon (Mg/ha)", title = "Carbon Stock and Size Class")+
  scale_fill_discrete(name = "Forest Type")

#Box plot
ggplot(plot_carbon_stock, aes(indc_spp, large_trees, fill = indc_spp))+
  geom_boxplot()+
  geom_point()+
  labs(x = "Forest Type", y = "Carbon Stored In Trees DBH >= 50 cm", title = "Carbon Storage in Large Trees by Forest Type")+
  theme(legend.position = "none")
#ANOVA
summary(aov(large_prop~indc_spp, plot_carbon_stock))
TukeyHSD(aov(large_prop~indc_spp, plot_carbon_stock))


#regression
l.trees<-ggscatter(plot_carbon_stock,x = "large_prop", y="carbon_stock",add = "reg.line")+
  geom_point(aes(color = indc_spp))+
  stat_cor(aes(label = paste(..rr.label.., ..p.label.., sep = "~`,`~")),label.x = 0.17, label.y = 75)+
  stat_regline_equation(label.x = 0.17, label.y = 100)+
  theme(legend.position = "right")+
  labs(x = "Proportion of Carbon in trees >50 cm DBH", y = "Carbon Stock (Mg/ha)")

ggpar(l.trees, legend.title = "Forest Type")


### Histogram of size classes
ggplot(tree.summary,aes(dbh_cm))+
  geom_histogram()+
  facet_wrap(~forest_group_spp)