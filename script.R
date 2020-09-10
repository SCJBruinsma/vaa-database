library(ggplot2)
library(readr)
library(ggrepel)
library(FactoMineR)
library(tikzDevice)
library(factoextra)
library(missMDA)
library(stargazer)
library(descr)
library(ggpubr)
library(plyr)
library(rworldmap)

database <- read_csv("database.csv")

categories <-  c("gen_name","gen_country_first","gen_year","gen_election","ques_dnk","ques_weights","type","format","pos_method","match_cityblock", "match_euclidean", "match_agreement", "match_2d","match_3d","match_6d","vis_single","vis_rank_quant","vis_rank_notquant", "vis_twodim","vis_threedim","vis_spider", "designer_uni","designer_comp","designer_media","designer_ngo")   
numerical <- c("ques_respositions", "ques_numitems", "elec_vaa", "gen_weights")
  
database_categories <- database[categories]
database_numerical <- database[numerical]
rm(database)

database_categories <- lapply(database_categories, factor)
database_categories <- as.data.frame(database_categories)

database <- cbind(database_categories, database_numerical)
rm(database_categories,database_numerical, categories, numerical)

# Weights

weights <- database$gen_weights
database$gen_weights <- NULL

# Run MCA

database_mca <- MCA(database,  quanti.sup=c(26:28), quali.sup=1:4, method = "Burt", na.method="NA",row.w = weights)

summary(database_mca)
dimdesc(database_mca)
fviz_cos2(database_mca, choice = "var", axes = 1:2)

fviz_mca_var(database_mca, col.var = "cos2",
             gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07"), 
             repel = TRUE, invisible=c("ind.sup", "quali.sup", "quanti.sup"),
             ggtheme = theme_minimal())

fviz_mca_var(database_mca, choice = "mca.cor", 
             repel = TRUE, 
             ggtheme = theme_minimal())

# Subsetting the data

mca_data_full <- data.frame(database_mca$var$coord)
mca_data_sup <- data.frame(database_mca$quali.sup$coord)

names <- c(1:93)
countries <- c(94:141)
year <- c(142:161)
elections <- c(162:168)
names <- as.numeric(names)
countries <- as.numeric(countries)
year <- as.numeric(year)
elections <- as.numeric(elections)

mca_data_names <- mca_data_sup[names,]
mca_data_countries <- mca_data_sup[countries,]
mca_data_year <- mca_data_sup[year,]
mca_data_elections <- mca_data_sup[elections,]

# Categories - Dimension 1 vs. 2

category_names <- c("", "DNK:Yes", "", "Weights:Yes","Type:Candidate","Type:Party","Response:Options", "Response:Scale", "Pos:Candidate", "Pos:Delphi", "Pos:Elite", "Pos:Expert", "Pos:Kieskompas","Pos:Manifesto", "", "Match:City:Yes","", "Match:Euclid:Yes","", "Match:Agreement:Yes","", "Match:2D:Yes", "", "Match:3D:Yes", "","Match:6D:Yes", "","Vis:Single:Yes","", "Vis:Rank-Quant:Yes","","Vis:Rank-NonQuant:Yes","","Vis:2D:Yes","", "Vis:3D:Yes", "", "Vis:Spider:Yes", "", "Des:Uni:Yes", "", "Des:Comp:Yes","","Des:Med:Yes", "", "Des:NGO:Yes")

tikz(file = "categories_12.tex")

ggplot(mca_data_full) +
 geom_point(aes(Dim.1, Dim.2), color = 'black', size=2) +
 geom_text_repel(size = 3.5, aes(Dim.1, Dim.2, label = category_names)) +
 geom_hline(yintercept=0, linetype="dashed", color = "black") +
 geom_vline(xintercept=0, linetype="dashed", color = "black") +
 ggtitle("VAA Database \n") +
 scale_x_continuous(name="Dimension 1 (38.63)",limits=c(-0.4, 0.9))+
 scale_y_continuous(name="Dimension 2 (15.81)",limits=c(-1.05, 0.88))+
 theme_classic(base_size = 16)+
 theme(plot.title = element_text(hjust=0.5))

dev.off()

# Additional Variables

tikz(file = "countries_12.tex")

ggplot(mca_data_countries) +
 geom_point(aes(Dim.1, Dim.2), color = 'black', size=2) +
 geom_text_repel(size = 3.5, aes(Dim.1, Dim.2, label = rownames(mca_data_countries))) +
 geom_hline(yintercept=0, linetype="dashed", color = "black") +
 geom_vline(xintercept=0, linetype="dashed", color = "black") +
 ggtitle("Countries \n") +
 scale_x_continuous(name="Dimension 1 (38.63)",limits=c(-0.55, 1.1))+
 scale_y_continuous(name="Dimension 2 (15.81)",limits=c(-0.55, 1.0))+
 theme_classic(base_size = 16)+
 theme(plot.title = element_text(hjust=0.5))

dev.off()


tikz(file = "elections_12.tex")

ggplot(mca_data_elections) +
 geom_point(aes(Dim.1, Dim.2), color = 'black', size=2) +
 geom_text_repel(size = 3.5, aes(Dim.1, Dim.2, label = rownames(mca_data_elections))) +
 geom_hline(yintercept=0, linetype="dashed", color = "black") +
 geom_vline(xintercept=0, linetype="dashed", color = "black") +
 ggtitle("Elections \n") +
 scale_x_continuous(name="Dimension 1 (38.63)",limits=c(-0.25, 0.20))+
 scale_y_continuous(name="Dimension 2 (15.81)",limits=c(-0.22, 0.32))+
 theme_classic(base_size = 16)+
 theme(plot.title = element_text(hjust=0.5))

dev.off()


party_names_12 <-  c("", "", "",  "", "", "",  "", "", "Choose4Greece",  "", "", "", "", "", "Election Compass",  "", "EU Profiler", "euandi",  "EUVox", "", "Glasosleditel",  "", "", "",  "", "Ikhtiar", "iSideWith",  "Israel Election Compass", "iVoter", "",  "", "", "Kandidattest",  "", "", "", "","Kend din Kandidat", "",  "Kieskompas", "KohoVolit", "",  "", "Mano Balsas", "",  "Masr bosala", "", "On the Fence",  "", "Oy Danismani", "Oy Pusulasi",  "", "ParteieNavi", "",  "", "", "Politikkabine",  "", "Rhymikone", "",  "Smartvote", "", "Stemwijzer",  "", "", "SVT Valkompassen",  "", "", "",  "Vaalikone", "", "Valasztasom",  "", "", "",  "Vokskabin", "", "Vote Compass", "Vote Match", "", "",  "", "VotingAid", "Votulmeu",  "Wahl-O-Mat", "Wahlkabine", "",  "", "", "",  "", "", "YLE Kuntavaalit") 

tikz(file = "names_12.tex")

ggplot(mca_data_names) +
 geom_point(aes(Dim.1, Dim.2), color = 'black', size=2) +
 geom_text_repel(size = 3.5, aes(Dim.1, Dim.2, label = party_names_12)) +
 geom_hline(yintercept=0, linetype="dashed", color = "black") +
 geom_vline(xintercept=0, linetype="dashed", color = "black") +
 ggtitle("Voting Advice Applications \n") +
 scale_x_continuous(name="Dimension 1 (38.63)",limits=c(-0.6, 1.1))+
 scale_y_continuous(name="Dimension 2 (15.81)",limits=c(-1.1, 1.2))+
 theme_classic(base_size = 16)+
 theme(plot.title = element_text(hjust=0.5))

dev.off()



# Contributions

contrib_dim1 <- fviz_contrib(database_mca, choice = "var", axes = 1, top = 15)
contrib_dim2 <- fviz_contrib(database_mca, choice = "var", axes = 2, top = 15)

contrib_dim1$data$name <- category_names
contrib_dim2$data$name <- category_names

contrib_dim1_data <- as.data.frame(contrib_dim1$data)
contrib_dim2_data <- as.data.frame(contrib_dim2$data)

rownames(contrib_dim1_data) <- c()
rownames(contrib_dim2_data) <- c()

contrib_dim1_data$name <- as.factor(contrib_dim1_data$name)
contrib_dim2_data$name <- as.factor(contrib_dim2_data$name)

contrib_dim1_data <- contrib_dim1_data[order(-contrib_dim1_data$contrib),] 
contrib_dim2_data <- contrib_dim2_data[order(-contrib_dim2_data$contrib),] 

contrib_dim1_data <- contrib_dim1_data[1:15,]
contrib_dim2_data <- contrib_dim2_data[1:15,]

intercept_value_dim1 <- as.numeric(contrib_dim1$layers[[2]]$data)
intercept_value_dim2 <- as.numeric(contrib_dim2$layers[[2]]$data)

plot_contrib1 <- ggplot(data=contrib_dim1_data, aes(x=reorder(name, -contrib), y=contrib)) +
  geom_bar(stat="identity", fill = "#e4e4e4")+
  scale_y_continuous(name="Contributions ()", limits = c(0,16), expand = c(0, 0), breaks=seq(0,16,2)) +
  geom_hline(yintercept = intercept_value_dim1, linetype="dashed")+
  ggtitle("Contribution of Variables to Dimension 1") +
  theme_classic()+
  theme(axis.text.x=element_text(angle=45,hjust=1), axis.title.x=element_blank())

plot_contrib2 <- ggplot(data=contrib_dim2_data, aes(x=reorder(name, -contrib), y=contrib)) +
  geom_bar(stat="identity", fill = "#e4e4e4")+
  scale_y_continuous(name="Contributions ()", limits = c(0,16), expand = c(0, 0), breaks=seq(0,16,2)) +
  geom_hline(yintercept = intercept_value_dim2, linetype="dashed")+
  ggtitle("Contribution of Variables to Dimension 2") +
  theme_classic()+
  theme(axis.text.x=element_text(angle=45,hjust=1), axis.title.x=element_blank())

tikz(file = "contribution.tex")
ggarrange(plot_contrib1, plot_contrib2, ncol = 1, nrow = 2)
dev.off()



fviz_mca_ind(database_mca, col.ind = "cos2", 
             gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07"),
             repel = TRUE, # Avoid text overlapping (slow if many points)
             ggtheme = theme_classic())


# Supplementary Variables

map <-fviz_mca_var(database_mca, choice = "quanti.sup",  ggtheme = theme_minimal())
map <- map$data

labels <- c("Number of Electors (VAA)", "Number of Items", "Number of Response Options")
labels_x <- c(0.25,0.15,-0.12)
labels_y <- c(0.08,-0.14,0.08)

tikz(file = "quantitative_12.tex")

ggplot() + 
  geom_segment(data=map, mapping=aes(x=c(0,0,0), y=c(0,0,0), xend=x, yend=y), arrow=arrow(), size=1, color="black")+
  geom_text_repel(aes(x=labels_x, y=labels_y, label=labels), size=4) + 
  scale_x_continuous(name="Dimension 1 (38.63)",limits=c(-0.3, 0.3))+
  scale_y_continuous(name="Dimension 2 (15.81)",limits=c(-0.3, 0.3))+
  geom_hline(yintercept=0, linetype="dashed", color = "black") +
  geom_vline(xintercept=0, linetype="dashed", color = "black") +
  ggtitle("Factor Map of Quantitative Variables \n") +
  theme_classic(base_size = 16)+
  theme(plot.title = element_text(hjust=0.5))

dev.off()

# Summary

summary(database)

table_election <- as.data.frame(table(database$gen_election))
table_year <- as.data.frame(table(database$gen_year))

table_election
table_year

table(database$gen_year,database$gen_election)

counts <- table(database$gen_election, database$gen_year)


# Map

freq <- count(database, 'gen_country_first')
freq$gen_country_first[freq$gen_country_first == "Czechia"] <- "Czech Republic"
freq$gen_country_first[freq$gen_country_first == "Moldavia"] <- "Moldova"

n <- joinCountryData2Map(freq, joinCode="NAME", nameJoinColumn="gen_country_first", verbose = TRUE)
par(mar = c(0,0,0,0), pty = "m",xaxs = "r", xaxt = 's', xpd = NA, yaxs = "i", yaxt = 's')

tikz(file = "map.tex")
mapCountryData(n, nameColumnToPlot="freq", mapTitle="Distribution of VAAs", addLegend = FALSE, numCats = 7, colourPalette = "white2Black", aspect=1)
dev.off()


# HCPC

res.hcpc <- HCPC(database_mca, graph = TRUE, nb.clust=-1)

tikz(file = "dendrogram.tex")

fviz_dend(res.hcpc, ggtheme = theme_classic(),  horiz = TRUE, k_colors = c("#00BA38", "#B79F00", "#F8766D", "#00BFC4", "#F564E3", "#619CFF"))

dev.off()

res.hcpc$desc.var$category 
res.hcpc$desc.ind$para
res.hcpc$desc.var$quanti

party_names_cluster <-  c("", "", "",  "", "", "",  "", "", "Choose4Greece",  "", "", "", "", "", "",  "", "EU Profiler", "",  "", "", "Glasosleditel",  "", "", "",  "", "", "iSideWith",  "", "iVoter", "",  "", "", "Kandidattest",  "", "", "", "","Kend din Kandidat", "",  "", "KohoVolit", "",  "", "Mano Balsas", "",  "Masr Bosala", "", "On the Fence",  "", "", "Oy Pusulasi",  "", "ParteieNavi", "",  "", "", "Politikkabine",  "", "Rhymikone", "",  "", "", "",  "", "", "SVT Valkompassen",  "", "", "",  "", "", "Valasztasom",  "", "", "",  "", "", "Vote Compass", "Vote Match", "", "",  "", "VotingAid", "Votulmeu",  "Wahl-O-Mat", "", "",  "", "", "",  "", "", "YLE Kuntavaalit") 

cluster <- fviz_cluster(res.hcpc, geom = "point")
cluster <- cluster$data

tikz(file = "clusters.tex")

fviz_cluster(res.hcpc, geom = "point")+
  geom_hline(yintercept=0, linetype="dashed", color = "black") +
  geom_vline(xintercept=0, linetype="dashed", color = "black") +
  geom_text_repel(data=mca_data_names, size = 3.5, aes(Dim.1, Dim.2, label = party_names_cluster))+
  ggtitle("Clusters \n") +
  theme_classic(base_size = 12)+
  scale_x_continuous(name="Dimension 1 (38.63)",limits=c(-0.5, 1.1))+
  scale_y_continuous(name="Dimension 2 (15.81)",limits=c(-1.1, 1.2))+
  annotate("text", x = -0.22705058, y = 1.06455517, label = "Vaalikone", size = 4.5, fontface =2)+
  annotate("text", x = -0.45986353, y = -0.1913475965, label = "Smartvote", size = 4.5, fontface =2)+
  annotate("text", x = -0.28312980, y = -0.2894467397, label = "Stemwijzer", size = 4.5, fontface =2)+
  annotate("text", x = 0.61652442, y = -1.09713890, label = "euandi", size = 4.5, fontface =2)+
  annotate("text", x = 0.99090771, y = -0.12278426, label = "Kieskompas", size = 4.5, fontface =2)+
  annotate("text", x = 1.05223736, y = -0.34239417, label = "EUvox" , size = 4.5, fontface =2)+
  theme(plot.title = element_text(hjust=0.5))+
  theme(legend.position="none")

dev.off()


