library(Rcpp)
library(dtw)
library(timeSeries)
library(TSclust)
library(parallel)


no_cores <- detectCores()
cl <- makeCluster(no_cores, type = "PSOCK")

clusterEvalQ(cl=cl, library(Rcpp))
clusterEvalQ(cl=cl, library(dtw))
clusterEvalQ(cl=cl, library(timeSeries))
clusterEvalQ(cl=cl, library(TSclust))
clusterEvalQ(cl=cl, library(parallel))
clusterEvalQ(cl=cl, library(test))


main <- function(training_set, test_set, nb_voisin, code_distance, nom_dataset){
  
  ZnormalisationDeTS <- function(t){
    return (t - mean(t)) / sd(t) 
  }
  
  #EntrCB)e : 
  # - t : une sCB)rie temporelle
  #Sortie : 
  # - sCB)rie temporelle sans les valeurs maquantes
  SupprimerNA <- function(t){
    a <- data.frame(t(t))
    b <- na.omit(a)
    c <- t(b)
    c
  }
  
  
  # PrCB)dire la classe d'une sCB)rie temporelle CB  partir de celle de ses k plus proche voisins
  #EntrCB)e : 
  # - ind.TS : l'indice de la sCB)rie temporelle dont il faut prCB)dire la classe
  # - training_d : le training_set
  # - clId_training_d : classe du training set
  # - matrice.distance : matrice de distance entre les TS du test_set (en ligne) et celles du training_set (en colonne)
  #Sortie : 
  # - La classe prCB)dite
  PredirClassPAA<-function(ind.TS,training_d,k, clId_training_d, matrice.distance)
  {
    distances <- matrice.distance[ind.TS, ]
    
    s <- sort(as.vector(distances), index.return=TRUE)
    tb <- as.vector(clId_training_d[s$ix[1:(k)]])
    
    cl <- ClasseMajoritaire(tb, clId_training_d)
    
    return(cl)
  }
  
  #Calcul la somme du carrCB) des Erreurs
  #EntrCB)e : 
  # - Un vecteur de reels
  #Sortie : 
  # Somme du carrCB) des erreurs
  SSE <- function(v){
    v_numeric <- as.numeric(v)
    taille    <- length(v_numeric)
    moy       <- mean(v_numeric)
    som       <- 0
    
    for(i in 1:taille){
      som <- som + (moy - v_numeric[i])^2
    }
    return(som)
  }
  
  
  # Calculer la somme des carrCB)es des erreurs d'un segment
  #Entree : 
  # - v : un vecteur de points
  # - nbPoints : le nombre de points d'un segment
  # - ind_debut : indice de dCB)but du segment
  #Sortie : 
  #somme du carrCB) des erreurs d'un segment
  SSE_segment <- function(v, nbPoints, ind_debut){
    if((ind_debut + nbPoints - 1) > length(v)){
      print("Erreur dans la fonction SSE_segment")
      print("La longueur du segment est supCB)rieure CB  celle de la sCB)rie temporelle")
      return(-1)
    }
    else{
      
    }
    return(SSE(v[ind_debut:(ind_debut + nbPoints - 1)]))
  }
  
  
  #Calcul la somme des carrCB)s des erreurs tous les segments d'une sCB)rie temporelle
  #EntrCB)es : 
  # - v : une sCB)rie temporelle
  # - nbPoints : la longueur d'un segment
  #Sortie : 
  #Somme des erreurs carrCB)e d'un segment
  somme_SSE <- function( v, nbPoints){
    n         <- length(v)
    ind_debut <- 1
    aux_se    <- 0
    
    while((ind_debut + nbPoints - 1) <= n){
      aux_se <- aux_se + SSE_segment(v, nbPoints, ind_debut)
      ind_debut <- ind_debut + nbPoints
    }
    return(aux_se)
  }
  
  
  #Calcul de la longueur optimale de chaque segment
  # - long_min : la longueur minimale CB  considCB)rer pour un segment
  # - long_max : la longueur maximale CB  considCB)rer pour un segment
  # - v : un vecteur de points
  w_optimale <- function(long_min, long_max, v){
    len_v <- length(v)
    n     <- long_max - long_min + 1
    j     <- 1
    
    if((long_min < 1) || (long_max > len_v)){
      print("Longueur minimale ou maximale du segment non adapt?e")
      print(paste("long_min", as.character(long_min), sep = " = "))
      print(paste("long_max", as.character(long_max), sep = " = "))
      return(-1)
    }
    
    x <- matrix(nrow = n, ncol = 1)
    y <- matrix(nrow = n, ncol = 1)
    z <- matrix(nrow = n, ncol = 1)
    
    for(i in long_min:long_max){
      x[j, 1] <- i
      z[j, 1] <- -1 * (1/(len_v - (len_v %% i))) * somme_SSE(v, i)
      j <- j + 1
    }  
    aux <- 10000000 * z
    
    #plot(aux~x)
    ind_max <- indice_maximun(aux)
    ind_max <- ind_max + 1
    
    
    return(floor(len_v/x[ind_max, 1]))
    
  }
  
  
  
  
  #Calcul de la matrice de distance entre les TS de longueur w
  #Entrees: 
  #training_d : jeu de donnCB)es d'apprentissage
  #test_d : jeu de donnCB)es de test
  #w le nombre de segments de la sC)rie temporelle
  #code_distance : entier pour le choix de la distance
  #          si code_distance = 1 alors PaaDist
  #          si code_distance = 2 alors dtw
  #Sortie:
  #une matrice de distance avec dim(test_d)[1] lignes et dim(training_d)[1] colonnes
  MatriceDeDistance_w <- function(training_d, test_d, w, code_distance){
    
    #initialisation des variables
    distances          <- matrix(nrow= dim(test_d)[1],ncol=dim(training_d)[1])
    w_op_training      <- matrix(nrow= dim(training_d)[1],ncol=1)
    w_op_test          <- matrix(nrow= dim(test_d)[1],ncol=1)
    lg_min             <- 1
    lg_max             <- 2
    # W_op pour le training set
    for (i in 1:(dim(training_d)[1])){
      
      a2               <- SupprimerNA(training_d[i,])
      b2               <- as.numeric(unlist(a2))
      t2               <- ZnormalisationDeTS(b2)# a voir
      
      n                <- length(b2)
    }
    
    # W_op pour le test set
    for (i in 1:(dim(test_d)[1])){
      
      a1 <- SupprimerNA(test_d[i,])
      b1 <- as.numeric(unlist(a1))
      t1 <- ZnormalisationDeTS(b1)# a voir
      
      n            <- length(b1)
    }
    
    #plot(w_op_test)
    
    #Calcul de la distance entre les s?ries temporelles
    for (i in 1:dim(test_d)[1]){
      a1 <- SupprimerNA(test_d[i,])
      b1 <- as.numeric(unlist(a1))
      t1 <- ZnormalisationDeTS(b1)# a voir
      t1.paa    <- as.vector(PAA(t1[1:(length(t1)-length(t1)%% w)], w))
      
      for (j in 1:dim(training_d)[1]){
        a2     <- SupprimerNA(training_d[j,])
        b2     <- as.numeric(unlist(a2))
        t2     <- ZnormalisationDeTS(b2)# a voir
        t2.paa <- as.vector(PAA(t2[1 : (length(t2) - length(t2)%% w)], w))
        
        if(code_distance == 1){
          #distances[i, j] <- PaaDist(t1.paa, t2.paa, n, w_op_test[i]) # on fait l'hypoth?se que toutes les TS ont la m?me longueur d'ou n
          print("En cours de construction")
        }else{
          if(code_distance == 2){
            distances[i, j] <- dynamictw(t1.paa, t2.paa)
            # r <- dtw(t1.paa, t2.paa, dist.method = "Euclidean")
            # distances[i, j] <- r$distance
          }else{
            if(code_distance == 3){
              r <- dtw (t1.paa, t2.paa, window.type = "itakura", distance.only = TRUE)
              distances[i, j] <- r$distance
            }else{
              r <- dtw (t1.paa, t2.paa, window.type = "sakoechiba", distance.only = TRUE)
              distances[i, j] <- r$distance
            }
            
          }
          
        }
        
      }
    }
    as.matrix(distances)
  }
  
  #Tester les effets du choix d'une longueure optimale sur la classification
  #EntrCB)es : 
  # training_set : le jeux de donnCB)es d'apprentissage
  # test_set : le jeux de donnCB)es de test
  # code_distance : si code_distance  1 alors PaaDist sinon alors DTW
  #Sorties : 
  #rien en sortie
  test_w <- function(training_set, test_set, w, k, code_distance, nom_dataset){
    
    #initialisation des variables
    pClassId         <- c()
    classId_training <- training_set[,1]
    training_data    <- training_set[,-(1)]
    classId_test     <- test_set[,1]
    test_data        <- test_set[,-(1)]
    print('')
    print('******************************')
    print(nom_dataset)
    print('******************************')
    print(paste('w = ', w))
    print('******************************')
    heure1 <- Sys.time()
    
    matrice.distance <- MatriceDeDistance_w(training_data, test_data, w, code_distance)
    
    for (i in 1:dim(test_data)[1]){
      pClassId[i] <- PredirClassPAA(i,training_data,k, classId_training, matrice.distance)
    }
    
    print(table(classId_test, pClassId))
    # accuracy
    acc <- (sum(classId_test==pClassId)) / nrow(test_data)
    print(paste("Accuracy = ", acc))
    
    heure2<-Sys.time()
    
    t <- difftime(heure2, heure1, units = "mins")
    print(paste("DurC)e = ", t))
    
    return(list(accuracy = acc, temps = t))
    
  }
  
  
  #Calcul de la matrice de distance entre les TS de longueure optimale
  #Entrees: 
  #training_d : jeu de donnCB)es d'apprentissage
  #test_d : jeu de donnCB)es de test
  #code_distance : entier pour le choix de la distance
  #          si code_distance = 1 alors PaaDist
  #          si code_distance = 2 alors dtw
  #Sortie:
  #une matrice de distance avec dim(test_d)[1] lignes et dim(training_d)[1] colonnes
  MatriceDeDistance_w_opt <- function(training_d, test_d, code_distance){
    
    #initialisation des variables
    distances          <- matrix(nrow= dim(test_d)[1],ncol=dim(training_d)[1])
    w_op_training      <- matrix(nrow= dim(training_d)[1],ncol=1)
    w_op_test          <- matrix(nrow= dim(test_d)[1],ncol=1)
    lg_min             <- 2
    lg_max             <- 20
    # W_op pour le training set
    for (i in 1:(dim(training_d)[1])){
      
      a2               <- SupprimerNA(training_d[i,])
      b2               <- as.numeric(unlist(a2))
      t2               <- ZnormalisationDeTS(b2)# a voir
      
      n                <- length(b2)
      #j'ai remplacCB) floor(n/ceiling(n * 0.05)) par floor(n/2)
      #lg_max           <- floor(n/2)
      w_op_training[i] <- w_optimale(lg_min, lg_max, t2) # a voir znormalized
      
    }
    
    # W_op pour le test set
    for (i in 1:(dim(test_d)[1])){
      
      a1 <- SupprimerNA(test_d[i,])
      b1 <- as.numeric(unlist(a1))
      t1 <- ZnormalisationDeTS(b1)# a voir
      
      n            <- length(b1)
      #j'ai remplacCB) floor(n/ceiling(n * 0.05)) par floor(n/2)
      #lg_max       <- floor(n/2)
      w_op_test[i] <- w_optimale(lg_min, lg_max, t1) # a voir znormalized
      
    }
    
    #plot(w_op_test)
    
    #------- par curiositCB) -------------
    print("")
    
    print("Nombre de segments pour les donnC)es du training set = ")
    print(summary(w_op_training))
    
    print("Nombre de segments pour les donnC)es du test set= ")
    print(summary(w_op_test))
    
    
    #Calcul de la distance entre les s?ries temporelles
    for (i in 1:dim(test_d)[1]){
      a1 <- SupprimerNA(test_d[i,])
      b1 <- as.numeric(unlist(a1))
      t1 <- ZnormalisationDeTS(b1)# a voir
      t1.paa    <- as.vector(PAA(t1[1:(length(t1)-length(t1)%% w_op_test[i])], w_op_test[i]))
      
      for (j in 1:dim(training_d)[1]){
        a2     <- SupprimerNA(training_d[j,])
        b2     <- as.numeric(unlist(a2))
        t2     <- ZnormalisationDeTS(b2)# a voir
        t2.paa <- as.vector(PAA(t2[1 : (length(t2) - length(t2)%% w_op_training[j])], w_op_training[j]))
        
        if(code_distance == 1){
          #distances[i, j] <- PaaDist(t1.paa, t2.paa, n, w_op_test[i]) # on fait l'hypoth?se que toutes les TS ont la m?me longueur d'ou n
          print("En cours de construction")
        }else{
          if(code_distance == 2){
            distances[i, j] <- dynamictw(t1.paa, t2.paa)
          }else{
            if(code_distance == 3){
              r <- dtw (t1.paa, t2.paa, window.type = "itakura", distance.only = TRUE)
              distances[i, j] <- r$distance
            }else{
              r <- dtw (t1.paa, t2.paa, window.type = "sakoechiba", distance.only = TRUE)
              distances[i, j] <- r$distance
            }
            
          }
          
        }
        
      }
    }
    as.matrix(distances)
  }
  
  
  #Tester les effets du choix d'une longueure optimale sur la classification
  #EntrCB)es : 
  # training_set : le jeux de donnCB)es d'apprentissage
  # test_set : le jeux de donnCB)es de test
  # code_distance : si code_distance  1 alors PaaDist sinon alors DTW
  #Sorties : 
  #rien en sortie
  test_w_op <- function(training_set, test_set, k, code_distance, nom_dataset){
    
    #initialisation des variables
    pClassId         <- c()
    classId_training <- training_set[,1]
    training_data    <- training_set[,-(1)]
    classId_test     <- test_set[,1]
    test_data        <- test_set[,-(1)]
    
    print('******************************')
    print(nom_dataset)
    print('******************************')
    heure1 <- Sys.time()
    
    matrice.distance <- MatriceDeDistance_w_opt(training_data, test_data, code_distance)
    
    for (i in 1:dim(test_data)[1]){
      pClassId[i] <- PredirClassPAA(i,training_data,k, classId_training, matrice.distance)
    }
    
    print(table(classId_test, pClassId))
    # accuracy
    acc <- (sum(classId_test==pClassId)) / nrow(test_data)
    
    heure2<-Sys.time()
    
    t <- difftime(heure2, heure1, units = "mins")
    
    return(list(accuracy = acc, temps = t))
    
  }
  
  
  
  # Afficher un histogramme qui prC)sente l'accuracy en fonction du nombre de segments
  #Entree : 
  # - accuracy : tableau d'accuracy calculC) en fonction du nombre de segments
  # - accuracy_w_opt : accuracy obtenue aprC(s le calcul de la longueure optimale pour chaque segments
  # - nb_seg : nombre de segments
  # - nom_jeux_donnees : nom du jeux de donnC)es C  mettre sur le graphique
  #Sortie : 
  #Ne retourne rien
  graphique_accuracy_nbseg <- function(tab_accuracy, accuracy_w_opt, tab_nb_seg, nom_jeux_donnees){
    #l'accuracy en fonction du nombre de segments
    y <- tab_accuracy 
    # le nombre de segments
    x <- sapply(tab_nb_seg, as.character)
    # accuracy optimale
    l <- accuracy_w_opt 
    
    #setwd("D:/these/VerificationDesExperimentations/codeR")
    
    #sauvegarder le graphique au format pdf
    pdf(paste(nom_jeux_donnees, "_accuracy.pdf", sep = ""), height=10,width=10) 
    
    
    
    print(paste("taille(x) = ", as.character(length(x))))
    print(paste("taille(y) = ", as.character(length(y))))
    
    bp <- barplot(y, main = paste(nom_jeux_donnees,"Effets du choix du nombre de segments W \n sur la classification avec PAA et 1-PPV", sep = '\n'),
                  xlab = "Nombre de segments",
                  ylab = "Accuracy",
                  names.arg = x,
                  col = "skyblue1",
                  border = "royalblue4",
                  ylim = c(0, 1.5)
    )
    
    abline(h=l, 
           par(col = "red")
    )
    text(bp, 0, round(y, 2),cex = 0.5,pos=3) 
    text((length(x)), (l - 0.001), 
         paste(as.character(l),"\nAccuracy avec\n W optimal"),
         col = "red",
         cex = 0.75)
    
    #on ferme le graphique
    dev.off()
    
  }
  
  
  
  # Afficher le temps de calcul en fonction du nombre de segments
  #EntrC)es : 
  # - temps : tableau prC)sentant le temps de calcul en fonction du nombre de segments
  # - temps_w_opt : temps pour effectuer la classification avec la longueure optimale pour chaque segments
  # - nb_seg : tableau contenant le nombre de segments
  # - nom_jeux_donnees : nom du jeux de donnC)es C  afficher sur le graphique
  #Sorties : 
  #Pas de sorties
  
  graphique_temps_nbseg <- function(temps, temps_w_opt,sum_temps, nb_seg, nom_jeux_donnees){
    #temps en fonction du nombre de segments
    y <- temps
    # le nombre de segments
    x <- sapply(nb_seg, as.character)
    # temps avec un W optimal
    l <- temps_w_opt
    
    #setwd("D:/these/VerificationDesExperimentations/codeR")
    
    #sauvegarder le graphique au format pdf
    pdf(paste(nom_jeux_donnees, "_temps.pdf", sep = ""), height=10,width=10) 
    
    
    bp <- barplot(y, main = paste(nom_jeux_donnees, "Effets du choix du nombre de segments W sur le temps\n de la classification avec PAA et 1-PPV", sep = '\n'),
                  xlab = "Nombre de segments",
                  ylab = "Temps(minutes)",
                  names.arg = x,
                  col = "skyblue1",
                  border = "royalblue4",
                  ylim = c(0, (max(temps_w_opt, sum_temps) + 1))
    )
    abline(h=l, 
           par(col = "red")
    )
    abline(h=sum_temps, 
           par(col = "green")
    )
    
    text(bp, 0, round(y, 2),cex=0.5,pos=3) 
    
    text((length(x)), 
         (l + 0.001), 
         paste(as.character(l),"\nTemps avec\n W optimal"), 
         cex=0.75, 
         col = "red")
    
    text((length(x)), 
         (sum_temps - (0.02*sum_temps)), 
         paste(as.character(sum_temps),"\nsomme des dur?es\n pour trouver best_W"), 
         cex=0.75, 
         col = "green")
    
    #on ferme le graphique
    dev.off()
    
  }
  
  #Automatiser le test des algorithmes suivant les valeurs de w
  #Entrees : 
  #  training_set : sC)ries temporelles d'apprentissage
  #  test_set : sC)ries temporelles de test
  #  nb_voisin : le nombre de voisin de l'algorithme k-moyennes
  #  code_distance
  #             si code_distance = 1 alors PAAdist
  #             si code_distance = 2 alors DTW
  #nom_dataset : nom a utiliser pour les figures
  #Sorties : 
  #Rien en sortie
  
  
  
  #initialisation des variables
  r <- list()
  r2 <- list()
  k <- nb_voisin
  
  # w_min <- floor( (length(training_set[1,]) - 1) / 2 ) + 1
  w_min <- 1
  w_max <- length(training_set[1,]) - 1
  
  
  tab_accuracy <- c()
  tab_nb_seg   <- c()
  tab_temps    <- c()
  #w_max <- floor( (length(training_set[1,]) - 1) )
  
  # classification des s?ries temporelles avec W qui varie
  
  # r2 <- test_w_op(training_set, test_set, k, code_distance, nom_dataset)
  # 
  # accuracy_w_op <- r2[[1]]
  # temps_w_op    <- r2[[2]]
  # 
  # print(paste("Accuracy optimale = ", accuracy_w_op))
  # print(paste("temps w optimale = ", temps_w_op))
  
  heure1<-Sys.time()
  j <- 1
  
  
  maFonction <- function(i, training_set, test_set,  k, code_distance, nom_dataset){
    r <- test_w(training_set, test_set, i, k, code_distance, nom_dataset)
    c(i, (1-r[[1]]), r[[2]])
  }
  
  matrice_resultat <- t(parSapply(cl, X=w_min:w_max, FUN=maFonction, training_set, test_set,  k, code_distance, nom_dataset, simplify = TRUE))
  
  heure2<-Sys.time()
  t <- difftime(heure2, heure1, units = "mins")
  
  
  print("###################################################")
  print("###################################################")
  print(nom_dataset)
  print("###################################################")
  print(matrice_resultat)
  print("---------------------------------------------------")
  print(paste("Duree de test de tous les decoupages = ", t))
  #graphique_accuracy_nbseg(matrice_resultat[2,], accuracy_w_op, matrice_resultat[1,] ,nom_dataset)
  #graphique_temps_nbseg(matrice_resultat[3,], temps_w_op, t, matrice_resultat[1, ], nom_dataset)
}

sink(file = "brute_force_search_13-10-16.txt")



ProximalPhalanxTW_TEST <- read.csv("./UCR_TS_Archive_2015/ProximalPhalanxTW_TEST", header=FALSE)
ProximalPhalanxTW_TRAIN <- read.csv("./UCR_TS_Archive_2015/ProximalPhalanxTW_TRAIN", header=FALSE)
heure1<-Sys.time()
main(ProximalPhalanxTW_TRAIN, ProximalPhalanxTW_TEST, 1, 2, 'ProximalPhalanxTW')
heure2<-Sys.time()
t <- difftime(heure2, heure1, units = "mins")
writeLines("\n")
print(paste("Le temps (minutes) d'execution de l'heuristique est : ", t))
writeLines("\n\n\n")


InsectWingbeatSound_TEST <- read.csv("./UCR_TS_Archive_2015/InsectWingbeatSound_TEST", header=FALSE)
InsectWingbeatSound_TRAIN <- read.csv("./UCR_TS_Archive_2015/InsectWingbeatSound_TRAIN", header=FALSE)
heure1<-Sys.time()
main(InsectWingbeatSound_TRAIN, InsectWingbeatSound_TEST, 1, 2, 'InsectWingbeatSound')
heure2<-Sys.time()
t <- difftime(heure2, heure1, units = "mins")
writeLines("\n")
print(paste("Le temps (minutes) d'execution de l'heuristique est : ", t))
writeLines("\n\n\n")


Computers_TEST <- read.csv("./UCR_TS_Archive_2015/Computers_TEST", header=FALSE)
Computers_TRAIN <- read.csv("./UCR_TS_Archive_2015/Computers_TRAIN", header=FALSE)
heure1<-Sys.time()
main(Computers_TRAIN, Computers_TEST, 1, 2, 'Computers')
heure2<-Sys.time()
t <- difftime(heure2, heure1, units = "mins")
writeLines("\n")
print(paste("Le temps (minutes) d'execution de l'heuristique est : ", t))
writeLines("\n\n\n")


DistalPhalanxOutlineCorrect_TEST <- read.csv("./UCR_TS_Archive_2015/DistalPhalanxOutlineCorrect_TEST", header=FALSE)
DistalPhalanxOutlineCorrect_TRAIN <- read.csv("./UCR_TS_Archive_2015/DistalPhalanxOutlineCorrect_TRAIN", header=FALSE)
heure1<-Sys.time()
main(DistalPhalanxOutlineCorrect_TRAIN, DistalPhalanxOutlineCorrect_TEST, 1, 2, 'DistalPhalanxOutlineCorrect')
heure2<-Sys.time()
t <- difftime(heure2, heure1, units = "mins")
writeLines("\n")
print(paste("Le temps (minutes) d'execution de l'heuristique est : ", t))
writeLines("\n\n\n")


Strawberry_TEST <- read.csv("./UCR_TS_Archive_2015/Strawberry_TEST", header=FALSE)
Strawberry_TRAIN <- read.csv("./UCR_TS_Archive_2015/Strawberry_TRAIN", header=FALSE)
heure1<-Sys.time()
main(Strawberry_TRAIN, Strawberry_TEST, 1, 2, 'Strawberry')
heure2<-Sys.time()
t <- difftime(heure2, heure1, units = "mins")
writeLines("\n")
print(paste("Le temps (minutes) d'execution de l'heuristique est : ", t))
writeLines("\n\n\n")



LargeKitchenAppliances_TEST <- read.csv("./UCR_TS_Archive_2015/LargeKitchenAppliances_TEST", header=FALSE)
LargeKitchenAppliances_TRAIN <- read.csv("./UCR_TS_Archive_2015/LargeKitchenAppliances_TRAIN", header=FALSE)
heure1<-Sys.time()
main(LargeKitchenAppliances_TRAIN, LargeKitchenAppliances_TEST, 1, 2, 'LargeKitchenAppliances')
heure2<-Sys.time()
t <- difftime(heure2, heure1, units = "mins")
writeLines("\n")
print(paste("Le temps (minutes) d'execution de l'heuristique est : ", t))
writeLines("\n\n\n")


CinC_ECG_torso_TEST <- read.csv("./UCR_TS_Archive_2015/CinC_ECG_torso_TEST", header=FALSE)
CinC_ECG_torso_TRAIN <- read.csv("./UCR_TS_Archive_2015/CinC_ECG_torso_TRAIN", header=FALSE)
heure1<-Sys.time()
main(CinC_ECG_torso_TRAIN, CinC_ECG_torso_TEST, 1, 2, 'CinC_ECG_torso')
heure2<-Sys.time()
t <- difftime(heure2, heure1, units = "mins")
writeLines("\n")
print(paste("Le temps (minutes) d'execution de l'heuristique est : ", t))
writeLines("\n\n\n")

MALLAT_TEST <- read.csv("./UCR_TS_Archive_2015/MALLAT_TEST", header=FALSE)
MALLAT_TRAIN <- read.csv("./UCR_TS_Archive_2015/MALLAT_TRAIN", header=FALSE)
heure1<-Sys.time()
main(MALLAT_TRAIN, MALLAT_TEST, 1, 2, 'MALLAT')
heure2<-Sys.time()
t <- difftime(heure2, heure1, units = "mins")
writeLines("\n")
print(paste("Le temps (minutes) d'execution de l'heuristique est : ", t))
writeLines("\n\n\n")

Phoneme_TEST <- read.csv("./UCR_TS_Archive_2015/Phoneme_TEST", header=FALSE)
Phoneme_TRAIN <- read.csv("./UCR_TS_Archive_2015/Phoneme_TRAIN", header=FALSE)
heure1<-Sys.time()
main(Phoneme_TRAIN, Phoneme_TEST, 1, 2, 'Phoneme')
heure2<-Sys.time()
t <- difftime(heure2, heure1, units = "mins")
writeLines("\n")
print(paste("Le temps (minutes) d'execution de l'heuristique est : ", t))
writeLines("\n\n\n")

####################################################################################
####################################################################################

sink()

stopCluster(cl)