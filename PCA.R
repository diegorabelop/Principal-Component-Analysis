


install.packages("ggplot2")
install.packages("scales")
install.packages("psych")
install.packages("car")
install.packages("lavaan")
install.packages("foreign")
install.packages("stats")
install.packages("factoextra")

library(haven)

#Banco_INELE_01_07_PCA <- read_sav("C:/Users/natxi/OneDrive - Cruzeiro do Sul Educacional/Doutorado/Tese/Tese/Banco de dados/Banco INELE_01_07_PCA.sav")
library(haven)
Banco_INELE_01_07_PCA <- read_sav("C:/Users/diego/Downloads/Banco INELE_01_07_PCA.sav")


#View(Banco_INELE_01_07)


# View Columns Numbers

library("scales")


######## PCA dos desfechos - PRÉ INTERVENÇÃO ########

library("factoextra")

Precisao.pca <- prcomp(Banco_INELE_01_07_PCA[,c(112, 151, 153, 155)], center = TRUE, scale. = TRUE)

summary(Precisao.pca)

# By examining the principal component vectors above, we can infer the the first principal component (PC1) roughly corresponds to an overall pontuaction of "serious crimes"WORDS construct" since Words_Read_Reg_Total, Words_Irreg_T have the largest values. 
# The second component (PC2) is affected by Reading_Fluency_Total and PsWords_T 
Precisao.pca

#outputs the mean of variables
Precisao.pca$center

#outputs the standard deviation of variables
Precisao.pca$scale

#The rotation measure provides the principal component loading. Each column of rotation matrix contains the principal component loading vector. This is the most important measure we should be interested in.

Precisao.pca$rotation
#compute standard deviation of each principal component
Precisao.pca$sdev

#compute variance
pr_var <- std_dev^2
#proportion of variance explained
prop_varex <- pr_var/sum(pr_var)
prop_varex[1:4] #This shows that first principal component explains 69.4% variance. Second component explains 16.06% variance.

# A figura abaixo mostra a mesma coisa. Isso tem que estar no paper
library(ggpubr)

library(ggplot2)
fviz_eig(Precisao.pca) # Show the percentage of variances explained by each principal component.

#The plot above shows that 2 components explains around 85% variance in the data set. In order words, using PCA we have reduced 4 predictors to 1 or 2 without compromising on explained variance. 
#cumulative scree plot
plot(cumsum(prop_varex), xlab = "Principal Component",
       ylab = "Cumulative Proportion of Variance Explained",
       type = "b")
# Graph of variables. Positive correlated variables point to the same side of the plot. Negative correlated variables point to opposite sides of the graph.
fviz_pca_var(Precisao.pca,
             col.var = "contrib", # Color by contributions to the PC
             gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07"),
             repel = TRUE     # Avoid text overlapping
)


# Graph of individuals. Individuals with a similar profile are grouped together.

fviz_pca_ind(Precisao.pca,
             col.ind = "cos2", # Color by the quality of representation
             gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07"),
             repel = TRUE     # Avoid text overlapping
)
# install.packages('ggpubr', dependencies=TRUE, repos='http://cran.rstudio.com/') ## quando não quer instalar o pacote ##



install_github("vqv/ggbiplot")
library(ggbiplot)

# Results for individuals
res.ind <- get_pca_ind(Precisao.pca)
res.ind$coord        # Coordinates
res.ind$contrib        # Contributions to the PCs
res.ind$cos2 

# Eigenvalues
eig.val <- get_eigenvalue(Precisao.pca)
eig.val# We used only Dimension 1 because, using the Kaiser criterion, we use only the principal components with eigenvalues that are greater than 1.


# Results for Variables
res.var <- get_pca_var(Precisao.pca)
res.var$coord          # Coordinates
res.var$contrib        # Contributions to the PCs
res.var$cos2           # Quality of representation 
# Results for individuals
res.ind <- get_pca_ind(Precisao.pca)
res.ind$coord        # Coordinates
res.ind$contrib        # Contributions to the PCs
res.ind$cos2 
res.ind$coord <- as.data.frame(res.ind$coord)
banco_Precisao <- cbind(Banco_INELE_01_07_PCA, res.ind$coord$Dim.1)

View(banco_Precisao)


######## PCA dos desfechos - PÓS-INTERVENÇÃO  #######

PrecisaoPos.pca <- prcomp(Banco_INELE_01_07_PCA[,c(160, 162, 164, 171)], center = TRUE, scale. = TRUE)

summary(PrecisaoPos.pca)


PrecisaoPos.pca

#outputs the mean of variables
PrecisaoPos.pca$center

#outputs the standard deviation of variables
PrecisaoPos.pca$scale

#The rotation measure provides the principal component loading. Each column of rotation matrix contains the principal component loading vector. This is the most important measure we should be interested in.

PrecisaoPos.pca$rotation
#compute standard deviation of each principal component
PrecisaoPos.pca$sdev

#compute variance
pr_var2 <- PrecisaoPos.pca$sdev^2
#proportion of variance explained
prop_varex2 <- pr_var2/sum(pr_var2)
prop_varex2[1:4] #This shows that first principal component explains 65.2% variance. Second component explains 15.49% variance.


#install.packages("ggpubr")

library(ggpubr)

library(ggplot2)

fviz_eig(PrecisaoPos.pca)

# Graph of variables. Positive correlated variables point to the same side of the plot. Negative correlated variables point to opposite sides of the graph.
fviz_pca_var(PrecisaoPos.pca,
             col.var = "contrib", # Color by contributions to the PC
             gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07"),
             repel = TRUE     # Avoid text overlapping
)

#Graph of individuals. Individuals with a similar profile are grouped together.

fviz_pca_ind(PrecisaoPos.pca,
             col.ind = "cos2", # Color by the quality of representation
             gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07"),
             repel = TRUE     # Avoid text overlapping
)
#install.packages('ggpubr', dependencies=TRUE, repos='http://cran.rstudio.com/') ## quando não quer instalar o pacote ##


library(factoextra)
library(FactoMineR)
library(ade4)
pos <- PCA(Banco_INELE_01_07_PCA[,c(160, 162, 164, 171)], scale.unit = TRUE, graph = FALSE)
fviz_pca_ind(pos,
             geom.ind = "point", # show points only (nbut not "text")
            addEllipses = TRUE, # Concentration ellipses
             legend.title = "Groups"
)

#install_github("vqv/ggbiplot")
#library(ggbiplot)

# Results for individuals
res.indPos <- get_pca_ind(PrecisaoPos.pca)
res.indPos$coord        # Coordinates
res.indPos$contrib        # Contributions to the PCs
res.indPos$cos2 

# Eigenvalues
eig.valPos <- get_eigenvalue(PrecisaoPos.pca)
eig.valPos
# Results for Variables -> ESSA É A QUE EU DESCREVO NOS RESULTADOS
res.varPos <- get_pca_var(PrecisaoPos.pca)
res.varPos$coord          # Coordinates
res.varPos$contrib        # Contributions to the PCs
res.varPos$cos2           # Quality of representation 
# Results for individuals
res.indPos <- get_pca_ind(PrecisaoPos.pca)
res.indPos$coord        # Coordinates
res.indPos$contrib        # Contributions to the PCs
res.indPos$cos2 
res.indPos$coord <- as.data.frame(res.indPos$coord)
banco_PrecisaoFINAL <- cbind(banco_Precisao, res.indPos$coord$Dim.1)

View(banco_PrecisaoFINAL)

names(banco_PrecisaoFINAL)[names(banco_PrecisaoFINAL) == "res.ind$coord$Dim.1"] <- "Precisao_Pre"
names(banco_PrecisaoFINAL)[names(banco_PrecisaoFINAL) == "res.indPos$coord$Dim.1"] <- "Precisao_Pos"

######Usando o DID, podemos estimar quanto dessa diferença se deve ao tratamento.######
#https://www.econometrics-with-r.org/13-4-qe.html 

# where D i is the binary treatment indicator, P e r i o d i is a binary indicator for the after-treatment period and the P e r i o d i × D i is the interaction of both.
#prepare data for DID regression using the interaction term  https://stats.stackexchange.com/questions/171658/difference-in-differences-analysis-in-r 

####### Desfechos ########
Precisao_pre <- banco_PrecisaoFINAL$Precisao_Pre
Precisao_pos <- banco_PrecisaoFINAL$Precisao_Pos
Fluencia_pre <- banco_PrecisaoFINAL$AFLeT_Score
Fluencia_pos <- banco_PrecisaoFINAL$AFLeT_Score_A2

###### Preditores #######
Treatment <- banco_PrecisaoFINAL$Group_Intervention
Treatment2 <- banco_PrecisaoFINAL$Group_Intervention_2
Teacher <- banco_PrecisaoFINAL$Teaching
Teacher2 <- banco_PrecisaoFINAL$Teaching2
Periodo <- banco_PrecisaoFINAL$Class
Periodo2 <- banco_PrecisaoFINAL$Class2


Artigo_2 <- data.frame("Precisao" = c(Precisao_pre, Precisao_pos), "Fluencia" = c(Fluencia_pre, Fluencia_pos), 
                       "Treatment" = c(Treatment,Treatment2), 
                "Period" = c(Periodo, Periodo2),
                "Teacher" = c(Teacher, Teacher2), use.missings = TRUE)


Artigo_2$Treatment <- as.factor(Artigo_2$Treatment)
Artigo_2$Teacher <- as.factor(Artigo_2$Teacher)
Artigo_2$Period <- as.factor(Artigo_2$Period)

# estimate the model with interaction
Efeito_precisao <- lm(Precisao ~ Treatment * Period * Teacher, data = Artigo_2) #variável DID é a interação entre tratamento e periodo
summary(Efeito_precisao) ####### significativo - desenhar o modelo a partir das PCAs - te mandei uma foto 

Efeito_fluencia <- lm(Fluencia ~ Treatment * Teacher, data = Artigo_2) #variável DID é a interação entre tratamento e periodo
summary(Efeito_fluencia)  ###### não foi significativo, sem efeito



