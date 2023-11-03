#carregar bibliotecas necessárias.
library(raster)
library(sf)
library(randomForest)

#local onde foram baixados os arquivos disponibilizados na pasta 'data'.
pathToFiles <- "C:/Users/"

#processar bandas.
products <- list.files(pathToFiles, full.names = TRUE)

bands <- c(grep('B1', products, value=TRUE),
           grep('B2', products, value=TRUE),
           grep('B3', products, value=TRUE),
           grep('B4', products, value=TRUE),
           grep('B5', products, value=TRUE),
           grep('B6', products, value=TRUE),
           grep('B7', products, value=TRUE)) 

img <- stack(bands) #fundir as bandas.

shp <- read_sf("training_dataREPROJ.shp") #carregar os vetores de amostragem.

shp <- shp[c(-425),] #um polígono ficou vazio.

compareCRS(img,shp) #mesmo SRC em ambos.

#visualização.
plotRGB(img, r = 4, g = 3, b = 2, stretch = "lin", colNA = 'white')
plot(shp, col="red", add=TRUE) 

#conversão de classes em caracteres.
levels(as.factor(shp$class))

for (i in 1:length(unique(shp$class)))
  {cat(paste0(i, " ", levels(as.factor(shp$class))[i]), sep="\n")}

#renomear as bandas. melhora a visualização.
names(img)
names(img) <- c("b1", "b2", "b3", "b4", "b5", "b6", "b7")
names(img)

#extração de valores de reflectância. Leva um tempo!
smp <- raster::extract(img, shp, df = TRUE)

#combinar coluna 'ID' da extração com a coluna 'class' dos vetores.
smp$cl <- as.factor(shp$class[match(smp$ID, seq(nrow(shp)))])
smp <- smp[-1] #remover a coluna ID

#salvar o dataframe da amostragem.
save(smp, file = "smp.rda")

load(file = "smp.rda")

#visualização dos perfis espectrais das classes.
sp <- aggregate( . ~ cl, data = smp, FUN = mean, na.rm = TRUE )
graphics::plot(0,
     ylim = c(min(sp[2:ncol(sp)]), max(sp[2:ncol(sp)])), 
     xlim = c(1, ncol(smp)-1),
     type = 'n', 
     xlab = "Bandas Landsat", 
     ylab = "Reflectância [% * 100]")

mycolors <- c("#E974ED", "#af2a2a", "#0000FF",
              "#006400", "#FFEFC3", "#FFD966", "#935132")

for (i in 1:nrow(sp)){
  lines(as.numeric(sp[i, -1]), 
        lwd = 4, 
        col = mycolors[i]
  )
}

grid(col = "black")

legend(as.character(sp$cl),
       x = "topleft",
       col = mycolors,
       lwd = 5,
       bty = "n")
       
#avaliação da distribuição das amostras nas classes.
summary(smp$cl)

#sub-dimensionamento da amostra a partir da classe minoritária.
smp.size <- rep(min(summary(smp$cl)), nlevels(smp$cl))
smp.size

#modelagem.
rfmodel <- tuneRF(x = smp[-ncol(smp)],
                  y = smp$cl,
                  sampsize = smp.size,
                  strata = smp$cl,
                  ntree = 250,
                  importance = TRUE,
                  doBest = TRUE,                  #whether to run a forest using the optimal mtry found
                  plot = TRUE                     #whether to plot the OOB error as function of mtry
)

#info do modelo.
rfmodel

#importância das variáveis.
varImpPlot(rfmodel)

#plot do modelo.
plot(rfmodel, col = c("#000000", "#E974ED", "#af2a2a", "#0000FF",
                      "#006400", "#FFEFC3", "#FFD966", "#935132"), lwd = 3)

#salvar modelo.
save(rfmodel, file = "rfmodel.RData")

load("rfmodel.RData")

#rodar classificação.
result <- raster::predict(img,
                          rfmodel,
                          filename = "RF_classification.tif",
                          overwrite = TRUE,
                          progress = "text")

#plotar classificação.
plot(result, 
     axes = FALSE, 
     box = FALSE,
     col = mycolors)