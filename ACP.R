##########################################################
##################### ACP RFS ############################
##########################################################

##################### Packages ###########################


library(questionr)
library(FactoMineR)
library(factoextra)
library(explor)
library(readxl)
library(Factoshiny)
library(gtsummary)
library(GGally)
library(Hmisc)
library(dplyr)
library(corrplot)
library(ggplot2)


##################### Base d'etude ########################
Var_ACP_2021 <- read_excel("Variables pour ACP 2021.xlsx")
tab_context <- read.csv2("Table contextuelle au niveau départemental.csv")
Facto <- merge(Var_ACP_2021, tab_context, by.x = "dep_eta", by.y = "codgeo")
View(Facto)
write.csv(Facto, "Facto.csv", sep=",")


################# Stats desc et corrplot ##################

moyenne <- Facto %>%                                        
  summarise_at(vars(everything()),
               list(moyenne = mean))
View(moyenne)
copie(moyenne)

matrice_corr <- cor(Facto[1:8], use = "pairwise.complete.obs")
corrplot(matrice_corr, type = "upper", order="hclust")


#################### ACP avec recours IMG ##########################

Facto <- as.data.frame(Facto)
rownames(Facto) <- paste(Facto$dep_eta)
Facto <- Facto[,-1]
for (col in names(Facto)) {
  Facto[[col]] <- as.numeric(Facto[[col]])
}
res <- PCA(Facto, quanti.sup = 9:15)
explor(res)
PCAshiny(res)

#################### ACP sans recours IMG ##########################

res2 <- PCA(Facto, quanti.sup = 7:16)
explor(res2)
PCAshiny(res2)

#################### Plots ACP ###########################

prs <- plot.PCA(res,choix='var',select='contrib  6',unselect=0,title="Graphe des variables de l'ACP",col.quanti.sup='#0000FF')


####################### CAH ###############################

classif <- HCPC(res, graph = FALSE, nb.clust = 3) #, consol=TRUE
plot(classif)
classif$data.clust
names(classif$data.clust)
clust <- classif$data.clust
Facto <- cbind(Facto, clust)
Facto <- cbind(Facto, tab_context)
write.csv(Facto, "tab.csv", sep=",")

####################### CAH 2 ###############################

classif <- HCPC(res2, graph = FALSE, nb.clust = 4) #, consol=TRUE
plot(classif)
classif$data.clust
names(classif$data.clust)
clust <- classif$data.clust
Facto <- cbind(Facto, clust)
Facto <- cbind(Facto, tab_context)
write.csv(Facto, "tab2.csv", sep=",")

####################### Plot CAH ##########################

pclassif <- plot.HCPC(classif,choice='map',draw.tree=FALSE,title='Plan factoriel')
biplot(prs,pclassif)
fviz_pca_biplot (prs,
                 col.ind = Facto_num$clust, palette = "jco",
                 addEllipses = FALSE, label = "var",
                 col.var = "black", repel = TRUE,
                 legend.title = "Groupes")

################### stats desc CAH ########################

moyenne <- Facto_num %>%                                        
  group_by(clust) %>%                        
  summarise_at(vars(everything()),
               list(moyenne = mean))
write.csv(moyenne, "moyennes1214gne.csv", sep=",")

mediane <- Facto_num %>%                                        
  group_by(clust) %>%                        
  summarise_at(vars(everything()),
               list(médiane = median))
write.csv(mediane, "mediane.csv", sep=",")


####################### ACP IMG ###########################
Var_ACP_2021_img <- read_excel("Variables pour ACP img.xlsx")
Facto_img <- merge(Var_ACP_2021_img, tab_context, by.x = "dep", by.y = "codgeo")
#Facto_img <- as.data.frame(Facto)
rownames(Facto_img) <- paste(Facto_img$dep)
Facto_img <- Facto_img[,-1]
for (col in names(Facto_img)) {
  Facto_img[[col]] <- as.numeric(Facto_img[[col]])
}
res_img <- PCA(Facto_img, quanti.sup = 4:16)
explor(res_img)
PCAshiny(res_img)
matrice_corr <- cor(Facto_img[1:5], use = "pairwise.complete.obs")
corrplot(matrice_corr, type = "upper", order="hclust")

########### ACP avec img_horsdep et img_prive #############
matrice_corr <- cor(Facto_ivgimg[1:8], use = "pairwise.complete.obs")
corrplot(matrice_corr, type = "upper", order="hclust")
#Facto_ivgimg <- merge(Var_ACP_2021, Var_ACP_2021_img, by.x = "dep_eta", by.y = "dep")
#Facto_ivgimg <- merge(Facto_ivgimg, tab_context, by.x = "dep_eta", by.y = "codgeo")
#write.csv(Facto_ivgimg, "tab3.csv", sep=",")
Facto_ivgimg <- read.csv2("tab3.csv")
rownames(Facto_ivgimg) <- paste(Facto_ivgimg$dep)
Facto_ivgimg <- Facto_ivgimg[,-1]
for (col in names(Facto_ivgimg)) {
  Facto_ivgimg[[col]] <- as.numeric(Facto_ivgimg[[col]])
}
resivgimg<-PCA(Facto_ivgimg, quanti.sup=9:24,graph=FALSE)
plot(resivgimg)
plot.PCA(resivgimg,choix='var',unselect=0,title="Graphe des variables de l'ACP",col.quanti.sup='#0000FF')
PCAshiny(resivgimg)
classif <- HCPC(resivgimg, graph = FALSE, nb.clust = 4) #, consol=TRUE
plot(classif)
classif$data.clust
names(classif$data.clust)
clust <- classif$data.clust
Facto <- cbind(Facto, clust)
Facto <- cbind(Facto, tab_context)
write.csv(Facto, "carte.csv", sep=",")


############################### ACP eta ##################################


