locs <- read.csv("E:/CONFIDENTIAL_for_Redmond.csv")

data<- env_vars %>% 
  left_join(plot, by=c("PLT_CN"="CN")) %>% 
  bind_cols(clim_pca$x) %>%
  left_join(locs,by="PLT_CN") 


ggplot(data %>% filter(ACTUAL_LAT >43),aes(x=PC1,y=PC2,color=ACTUAL_LAT, alpha=0.5))+
  geom_point()+
  scale_color_gradient(low="darkgreen", high="green")
ggplot(data %>% filter(ACTUAL_LAT <37),aes(x=PC1,y=PC2,color=ACTUAL_LAT, alpha=0.5))+
  geom_point()+
  scale_color_gradient(low="red", high="pink")

ggplot(data,aes(x=PC1,y=PC2, color=ACTUAL_LAT, alpha=0.5))+
  geom_point()+
  scale_color_viridis_c(option="magma")



ggplot(data,aes(x=PC1,y=PC2,color=ACTUAL_LON, alpha=0.5))+
  geom_point()+
  scale_color_viridis_c(option="magma")

summary(lm(PC1~ACTUAL_LAT, data=data))
summary(lm(PC2~ACTUAL_LAT, data=data %>% filter(ACTUAL_LAT>43)))
summary(lm(PC2~ACTUAL_LAT, data=data %>% filter(ACTUAL_LAT<37)))

summary(lm(PC2~ACTUAL_LON, data=data))
summary(lm(PC1~ACTUAL_LON, data=data))
cor.test(data$PC2, data$ACTUAL_LON)
plot(data$ACTUAL_LON, data$PC2)
plot(data$ACTUAL_LAT, data$PC2)



ggplot(median_vectors_all2, aes(x=factor(disturbance, levels=c("fire","insect.disease","none"),labels=c("fire","insect/\ndisease","none")), y=medAngle, fill=disturbance))+
  annotate("rect",xmin = -Inf, xmax = Inf, ymin=0, ymax = pi/2, fill="blue", alpha = 0.5)+
  annotate("rect",xmin = -Inf, xmax = Inf, ymin=pi/2, ymax = 3.1, fill="beige", alpha = 0.5)+
  annotate("rect",xmin = -Inf, xmax = Inf, ymin=-pi/2, ymax = 0, fill="green", alpha = 0.5)+
  annotate("rect",xmin = -Inf, xmax = Inf, ymin=-3.1, ymax = -pi/2, fill="red", alpha = 0.5)+
  geom_text(aes(x="x",y=2.745),label="hot/dry",size=5)+
  geom_text(aes(x="x",y=1.7),label="cold/dry",size=5)+
  geom_text(aes(x="x",y=0.1),label="cold/wet",size=5)+
  geom_text(aes(x="x",y=-1.7),label="hot/wet",size=5)+
  geom_text(aes(x="x",y=-2.745),label="hot/dry",size=5)+
  geom_boxplot()+
  geom_jitter(data=median_vectors_all,aes(x=factor(disturbance, levels=c("fire","insect.disease","none"),labels=c("fire","insect/\ndisease","none")), y=medAngle),width=0.1,height=0.1)+
  ylab("Median angle (radians)")+
  xlab("")+
  ylim(c(-3.14,3.14))+
  scale_fill_manual(values=c("lightgray","darkgray","white","white"))+
  geom_text(data= anova.stats.medang, aes(x=factor(disturbance, levels=c("fire","insect.disease","none"),labels=c("fire","insect/\ndisease","none")),y=y,label=letter),size=5)+
  theme_bw()+
  theme(text=element_text(size=20, color="black"),axis.text = element_text(size=20,color="black"),legend.position = "none",axis.text.x = element_text(color=c(rep("black",3),"white"),size=16))

#new PCA

set.seed(83)
clim_pca_nocmd <- prcomp(env_vars_cut_trans %>% dplyr::select(-cmd),scale.=T) #proceed from correlation matrix, which scales variables--important because our variables have different units

loadings.df<- as.data.frame(clim_pca_nocmd$rotation) %>% 
  mutate(x=c(-1,-3.4,-3.2,2,-3.2,2.3,3.2,2.5), y=c(-2,-1.5,0,-4,1.9,-1.5,1,2.5), names = c(str_wrap("mean temp coldest month", width=14),str_wrap("# frost free degree days",width=14), str_wrap("climate moisture  deficit", width=14),str_wrap("log(mean summer precip)",width=14), str_wrap( "log(summer heat moisture index)",width=14),str_wrap("log(winter precip)",width=14),str_wrap("log(precip  as snow)",width=14), str_wrap("log(degree days below 0)",width=14)))


autoplot(clim_pca_nocmd,x=1,y=2,colour="grey",loadings.colour="red", loadings=T, loadings.label=T, loadings.label.colour="black", scale=0, loadings.label.size=6, alpha=0.5)+
  geom_hline(yintercept=0,linetype="dashed")+geom_vline(xintercept=0,linetype="dashed")+
  geom_abline(intercept=0, slope = 1, linetype="dashed")+
  geom_abline(intercept=0, slope = -1, linetype="dashed")+
  # annotate("text",x=-0.4,y=3.2,label="cold/dry",size=6,angle=90)+
  # annotate(geom="text",x=5.2,y=-0.2,label="cold/wet",size=6)+
  # annotate(geom="text",x=-0.4,y=-4,label="hot/wet",size=6,angle=90)+
  # annotate(geom="text",x=-10,y=- 0.2,label="hot/dry",size=6)+
  # annotate(geom="text",x=3.2,y= 3.6,label="cold",size=6, angle=45)+
  # annotate(geom="text",x=5,y= -4.5,label="wet",size=6, angle=-45)+
  # annotate(geom="text",x=-5,y= -4.5,label="hot",size=6, angle=45)+
  # annotate(geom="text",x=-3.2,y= 3.7,label="dry",size=6, angle=-45)+
  # annotate(geom="text", x=loadings.df$x,y=loadings.df$y, label=loadings.df$names, size=4.5) +
  # theme_bw()+
  theme(axis.text = element_text(color="black",size=18),text = element_text(size=18, lineheight = 0.1),legend.text = element_text(size=18))




