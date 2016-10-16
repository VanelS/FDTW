Gun_Point_TEST <- read.csv("./UCR_TS_Archive_2015/Gun_Point_TEST", header=FALSE)
Gun_Point_TRAIN <- read.csv("./UCR_TS_Archive_2015/Gun_Point_TRAIN", header=FALSE)
heure1<-Sys.time()
main(Gun_Point_TRAIN, Gun_Point_TEST, 1, 3, 'Gun_Point')
heure2<-Sys.time()
t <- difftime(heure2, heure1, units = "mins")
writeLines("\n")
print(paste("Le temps (minutes) d'execution de l'heuristique est : ", t))
writeLines("\n\n\n")





c50words_TEST <- read.csv("./UCR_TS_Archive_2015/50words_TEST", header=FALSE)
c50words_TRAIN <- read.csv("./UCR_TS_Archive_2015/50words_TRAIN", header=FALSE)
heure1<-Sys.time()
main(c50words_TRAIN, c50words_TEST, 1, 2, '50words')
heure2<-Sys.time()
t <- difftime(heure2, heure1, units = "mins")
writeLines("\n")
print(paste("Le temps (minutes) d'execution de l'heuristique est : ", t))
writeLines("\n\n\n")




Adiac_TEST <- read.csv("./UCR_TS_Archive_2015/Adiac_TEST", header=FALSE)
Adiac_TRAIN <- read.csv("./UCR_TS_Archive_2015/Adiac_TRAIN", header=FALSE)
heure1<-Sys.time()
main(Adiac_TRAIN, Adiac_TEST, 1, 2, 'AdiaAdiac_TRAIN')
heure2<-Sys.time()
t <- difftime(heure2, heure1, units = "mins")
writeLines("\n")
print(paste("Le temps (minutes) d'execution de l'heuristique est : ", t))
writeLines("\n\n\n")




Beef_TEST <- read.csv("./UCR_TS_Archive_2015/Beef_TEST", header=FALSE)
Beef_TRAIN <- read.csv("./UCR_TS_Archive_2015/Beef_TRAIN", header=FALSE)
heure1<-Sys.time()
main(Beef_TRAIN, Beef_TEST, 1, 2, 'Beef')
heure2<-Sys.time()
t <- difftime(heure2, heure1, units = "mins")
writeLines("\n")
print(paste("Le temps (minutes) d'execution de l'heuristique est : ", t))
writeLines("\n\n\n")



CBF_TEST <- read.csv("./UCR_TS_Archive_2015/CBF_TEST", header=FALSE)
CBF_TRAIN <- read.csv("./UCR_TS_Archive_2015/CBF_TRAIN", header=FALSE)
heure1<-Sys.time()
main(CBF_TRAIN, CBF_TEST, 1, 2, 'CBF')
heure2<-Sys.time()
t <- difftime(heure2, heure1, units = "mins")
writeLines("\n")
print(paste("Le temps (minutes) d'execution de l'heuristique est : ", t))
writeLines("\n\n\n")





Coffee_TEST <- read.csv("./UCR_TS_Archive_2015/Coffee_TEST", header=FALSE)
Coffee_TRAIN <- read.csv("./UCR_TS_Archive_2015/Coffee_TRAIN", header=FALSE)
heure1<-Sys.time()
main(Coffee_TRAIN, Coffee_TEST, 1, 2, 'Coffee')
heure2<-Sys.time()
t <- difftime(heure2, heure1, units = "mins")
writeLines("\n")
print(paste("Le temps (minutes) d'execution de l'heuristique est : ", t))
writeLines("\n\n\n")






Cricket_X_TEST <- read.csv("./UCR_TS_Archive_2015/Cricket_X_TEST", header=FALSE)
Cricket_X_TRAIN <- read.csv("./UCR_TS_Archive_2015/Cricket_X_TRAIN", header=FALSE)
heure1<-Sys.time()
main(Cricket_X_TRAIN, Cricket_X_TEST, 1, 2, 'Cricket_X_TEST')
heure2<-Sys.time()
t <- difftime(heure2, heure1, units = "mins")
writeLines("\n")
print(paste("Le temps (minutes) d'execution de l'heuristique est : ", t))
writeLines("\n\n\n")




Cricket_Y_TEST <- read.csv("./UCR_TS_Archive_2015/Cricket_Y_TEST", header=FALSE)
Cricket_Y_TRAIN <- read.csv("./UCR_TS_Archive_2015/Cricket_Y_TRAIN", header=FALSE)
heure1<-Sys.time()
main(Cricket_Y_TRAIN, Cricket_Y_TEST, 1, 2, 'Cricket_Y_TEST')
heure2<-Sys.time()
t <- difftime(heure2, heure1, units = "mins")
writeLines("\n")
print(paste("Le temps (minutes) d'execution de l'heuristique est : ", t))
writeLines("\n\n\n")




Cricket_Z_TEST <- read.csv("./UCR_TS_Archive_2015/Cricket_Z_TEST", header=FALSE)
Cricket_Z_TRAIN <- read.csv("./UCR_TS_Archive_2015/Cricket_Z_TRAIN", header=FALSE)
heure1<-Sys.time()
main(Cricket_Z_TRAIN, Cricket_Z_TEST, 1, 2, 'Cricket_Z_TEST')
heure2<-Sys.time()
t <- difftime(heure2, heure1, units = "mins")
writeLines("\n")
print(paste("Le temps (minutes) d'execution de l'heuristique est : ", t))
writeLines("\n\n\n")




ECG200_TEST <- read.csv("./UCR_TS_Archive_2015/ECG200_TEST", header=FALSE)
ECG200_TRAIN <- read.csv("./UCR_TS_Archive_2015/ECG200_TRAIN", header=FALSE)
heure1<-Sys.time()
main(ECG200_TRAIN, ECG200_TEST, 1, 2, 'ECG200')
heure2<-Sys.time()
t <- difftime(heure2, heure1, units = "mins")
writeLines("\n")
print(paste("Le temps (minutes) d'execution de l'heuristique est : ", t))
writeLines("\n\n\n")




ECGFiveDays_TEST <- read.csv("./UCR_TS_Archive_2015/ECGFiveDays_TEST", header=FALSE)
ECGFiveDays_TRAIN <- read.csv("./UCR_TS_Archive_2015/ECGFiveDays_TRAIN", header=FALSE)
heure1<-Sys.time()
main(ECGFiveDays_TRAIN, ECGFiveDays_TEST, 1, 2, 'ECGFiveDays_TEST')
heure2<-Sys.time()
t <- difftime(heure2, heure1, units = "mins")
writeLines("\n")
print(paste("Le temps (minutes) d'execution de l'heuristique est : ", t))
writeLines("\n\n\n")




FaceAll_TEST <- read.csv("./UCR_TS_Archive_2015/FaceAll_TEST", header=FALSE)
FaceAll_TRAIN <- read.csv("./UCR_TS_Archive_2015/FaceAll_TRAIN", header=FALSE)
heure1<-Sys.time()
main(FaceAll_TRAIN, FaceAll_TEST, 1, 2, 'FaceAll')
heure2<-Sys.time()
t <- difftime(heure2, heure1, units = "mins")
writeLines("\n")
print(paste("Le temps (minutes) d'execution de l'heuristique est : ", t))
writeLines("\n\n\n")




FaceFour_TEST <- read.csv("./UCR_TS_Archive_2015/FaceFour_TEST", header=FALSE)
FaceFour_TRAIN <- read.csv("./UCR_TS_Archive_2015/FaceFour_TRAIN", header=FALSE)
heure1<-Sys.time()
main(FaceFour_TRAIN, FaceFour_TEST, 1, 2, 'FaceFour')
heure2<-Sys.time()
t <- difftime(heure2, heure1, units = "mins")
writeLines("\n")
print(paste("Le temps (minutes) d'execution de l'heuristique est : ", t))
writeLines("\n\n\n")




FISH_TEST <- read.csv("./UCR_TS_Archive_2015/FISH_TEST", header=FALSE)
FISH_TRAIN <- read.csv("./UCR_TS_Archive_2015/FISH_TRAIN", header=FALSE)
heure1<-Sys.time()
main(FISH_TRAIN, FISH_TEST, 1, 2, 'FISH')
heure2<-Sys.time()
t <- difftime(heure2, heure1, units = "mins")
writeLines("\n")
print(paste("Le temps (minutes) d'execution de l'heuristique est : ", t))
writeLines("\n\n\n")




Haptics_TEST <- read.csv("./UCR_TS_Archive_2015/Haptics_TEST", header=FALSE)
Haptics_TRAIN <- read.csv("./UCR_TS_Archive_2015/Haptics_TRAIN", header=FALSE)
heure1<-Sys.time()
main(Haptics_TRAIN, Haptics_TEST, 1, 2, 'Haptics')
heure2<-Sys.time()
t <- difftime(heure2, heure1, units = "mins")
writeLines("\n")
print(paste("Le temps (minutes) d'execution de l'heuristique est : ", t))
writeLines("\n\n\n")




InlineSkate_TEST <- read.csv("./UCR_TS_Archive_2015/InlineSkate_TEST", header=FALSE)
InlineSkate_TRAIN <- read.csv("./UCR_TS_Archive_2015/InlineSkate_TRAIN", header=FALSE)
heure1<-Sys.time()
main(InlineSkate_TRAIN, InlineSkate_TEST, 1, 2, 'InlineSkate')
heure2<-Sys.time()
t <- difftime(heure2, heure1, units = "mins")
writeLines("\n")
print(paste("Le temps (minutes) d'execution de l'heuristique est : ", t))
writeLines("\n\n\n")





ItalyPowerDemand_TEST <- read.csv("./UCR_TS_Archive_2015/ItalyPowerDemand_TEST", header=FALSE)
ItalyPowerDemand_TRAIN <- read.csv("./UCR_TS_Archive_2015/ItalyPowerDemand_TRAIN", header=FALSE)
heure1<-Sys.time()
main(ItalyPowerDemand_TRAIN, ItalyPowerDemand_TEST, 1, 2, 'ItalyPowerDemand')
heure2<-Sys.time()
t <- difftime(heure2, heure1, units = "mins")
writeLines("\n")
print(paste("Le temps (minutes) d'execution de l'heuristique est : ", t))
writeLines("\n\n\n")




Lighting2_TEST <- read.csv("./UCR_TS_Archive_2015/Lighting2_TEST", header=FALSE)
Lighting2_TRAIN <- read.csv("./UCR_TS_Archive_2015/Lighting2_TRAIN", header=FALSE)
heure1<-Sys.time()
main(Lighting2_TRAIN, Lighting2_TEST, 1, 2, 'Lighting2')
heure2<-Sys.time()
t <- difftime(heure2, heure1, units = "mins")
writeLines("\n")
print(paste("Le temps (minutes) d'execution de l'heuristique est : ", t))
writeLines("\n\n\n")





Lighting7_TEST <- read.csv("./UCR_TS_Archive_2015/Lighting7_TEST", header=FALSE)
Lighting7_TRAIN <- read.csv("./UCR_TS_Archive_2015/Lighting7_TRAIN", header=FALSE)
heure1<-Sys.time()
main(Lighting7_TRAIN, Lighting7_TEST, 1, 2, 'Lighting7')
heure2<-Sys.time()
t <- difftime(heure2, heure1, units = "mins")
writeLines("\n")
print(paste("Le temps (minutes) d'execution de l'heuristique est : ", t))
writeLines("\n\n\n")




MedicalImages_TEST <- read.csv("./UCR_TS_Archive_2015/MedicalImages_TEST", header=FALSE)
MedicalImages_TRAIN <- read.csv("./UCR_TS_Archive_2015/MedicalImages_TRAIN", header=FALSE)
heure1<-Sys.time()
main(MedicalImages_TRAIN, MedicalImages_TEST, 1, 2, 'MedicalImages')
heure2<-Sys.time()
t <- difftime(heure2, heure1, units = "mins")
writeLines("\n")
print(paste("Le temps (minutes) d'execution de l'heuristique est : ", t))
writeLines("\n\n\n")




MoteStrain_TEST <- read.csv("./UCR_TS_Archive_2015/MoteStrain_TEST", header=FALSE)
MoteStrain_TRAIN <- read.csv("./UCR_TS_Archive_2015/MoteStrain_TRAIN", header=FALSE)
heure1<-Sys.time()
main(MoteStrain_TRAIN, MoteStrain_TEST, 1, 2, 'MoteStrain')
heure2<-Sys.time()
t <- difftime(heure2, heure1, units = "mins")
writeLines("\n")
print(paste("Le temps (minutes) d'execution de l'heuristique est : ", t))
writeLines("\n\n\n")




OliveOil_TEST <- read.csv("./UCR_TS_Archive_2015/OliveOil_TEST", header=FALSE)
OliveOil_TRAIN <- read.csv("./UCR_TS_Archive_2015/OliveOil_TRAIN", header=FALSE)
heure1<-Sys.time()
main(OliveOil_TRAIN, OliveOil_TEST, 1, 2, 'OliveOil')
heure2<-Sys.time()
t <- difftime(heure2, heure1, units = "mins")
writeLines("\n")
print(paste("Le temps (minutes) d'execution de l'heuristique est : ", t))
writeLines("\n\n\n")





OSULeaf_TEST <- read.csv("./UCR_TS_Archive_2015/OSULeaf_TEST", header=FALSE)
OSULeaf_TRAIN <- read.csv("./UCR_TS_Archive_2015/OSULeaf_TRAIN", header=FALSE)
heure1<-Sys.time()
main(OSULeaf_TRAIN, OSULeaf_TEST, 1, 2, 'OSULeaf')
heure2<-Sys.time()
t <- difftime(heure2, heure1, units = "mins")
writeLines("\n")
print(paste("Le temps (minutes) d'execution de l'heuristique est : ", t))
writeLines("\n\n\n")





SonyAIBORobotSurface_TEST <- read.csv("./UCR_TS_Archive_2015/SonyAIBORobotSurface_TEST", header=FALSE)
SonyAIBORobotSurface_TRAIN <- read.csv("./UCR_TS_Archive_2015/SonyAIBORobotSurface_TRAIN", header=FALSE)
heure1<-Sys.time()
main(SonyAIBORobotSurface_TRAIN, SonyAIBORobotSurface_TEST, 1, 2, 'SonyAIBORobotSurface')
heure2<-Sys.time()
t <- difftime(heure2, heure1, units = "mins")
writeLines("\n")
print(paste("Le temps (minutes) d'execution de l'heuristique est : ", t))
writeLines("\n\n\n")





SonyAIBORobotSurfaceII_TEST <- read.csv("./UCR_TS_Archive_2015/SonyAIBORobotSurfaceII_TEST", header=FALSE)
SonyAIBORobotSurfaceII_TRAIN <- read.csv("./UCR_TS_Archive_2015/SonyAIBORobotSurfaceII_TRAIN", header=FALSE)
heure1<-Sys.time()
main(SonyAIBORobotSurfaceII_TRAIN, SonyAIBORobotSurfaceII_TEST, 1, 2, 'SonyAIBORobotSurfaceII')
heure2<-Sys.time()
t <- difftime(heure2, heure1, units = "mins")
writeLines("\n")
print(paste("Le temps (minutes) d'execution de l'heuristique est : ", t))
writeLines("\n\n\n")



SwedishLeaf_TEST <- read.csv("./UCR_TS_Archive_2015/SwedishLeaf_TEST", header=FALSE)
SwedishLeaf_TRAIN <- read.csv("./UCR_TS_Archive_2015/SwedishLeaf_TRAIN", header=FALSE)
heure1<-Sys.time()
main(SwedishLeaf_TRAIN, SwedishLeaf_TEST, 1, 2, 'SwedishLeaf')
heure2<-Sys.time()
t <- difftime(heure2, heure1, units = "mins")
writeLines("\n")
print(paste("Le temps (minutes) d'execution de l'heuristique est : ", t))
writeLines("\n\n\n")




Symbols_TEST <- read.csv("./UCR_TS_Archive_2015/Symbols_TEST", header=FALSE)
Symbols_TRAIN <- read.csv("./UCR_TS_Archive_2015/Symbols_TRAIN", header=FALSE)
heure1<-Sys.time()
main(Symbols_TRAIN, Symbols_TEST, 1, 2, 'Symbols')
heure2<-Sys.time()
t <- difftime(heure2, heure1, units = "mins")
writeLines("\n")
print(paste("Le temps (minutes) d'execution de l'heuristique est : ", t))
writeLines("\n\n\n")




synthetic_control_TEST <- read.csv("./UCR_TS_Archive_2015/synthetic_control_TEST", header=FALSE)
synthetic_control_TRAIN <- read.csv("./UCR_TS_Archive_2015/synthetic_control_TRAIN", header=FALSE)
heure1<-Sys.time()
main(synthetic_control_TRAIN, synthetic_control_TEST, 1, 2, 'synthetic_control')
heure2<-Sys.time()
t <- difftime(heure2, heure1, units = "mins")
writeLines("\n")
print(paste("Le temps (minutes) d'execution de l'heuristique est : ", t))
writeLines("\n\n\n")




Trace_TEST <- read.csv("./UCR_TS_Archive_2015/Trace_TEST", header=FALSE)
Trace_TRAIN <- read.csv("./UCR_TS_Archive_2015/Trace_TRAIN", header=FALSE)
heure1<-Sys.time()
main(Trace_TRAIN, Trace_TEST, 1, 2, 'Trace')
heure2<-Sys.time()
t <- difftime(heure2, heure1, units = "mins")
writeLines("\n")
print(paste("Le temps (minutes) d'execution de l'heuristique est : ", t))
writeLines("\n\n\n")





Two_Patterns_TEST <- read.csv("./UCR_TS_Archive_2015/Two_Patterns_TEST", header=FALSE)
Two_Patterns_TRAIN <- read.csv("./UCR_TS_Archive_2015/Two_Patterns_TRAIN", header=FALSE)
heure1<-Sys.time()
main(Two_Patterns_TRAIN, Two_Patterns_TEST, 1, 2, 'Two_Patterns')
heure2<-Sys.time()
t <- difftime(heure2, heure1, units = "mins")
writeLines("\n")
print(paste("Le temps (minutes) d'execution de l'heuristique est : ", t))
writeLines("\n\n\n")




TwoLeadECG_TEST <- read.csv("./UCR_TS_Archive_2015/TwoLeadECG_TEST", header=FALSE)
TwoLeadECG_TRAIN <- read.csv("./UCR_TS_Archive_2015/TwoLeadECG_TRAIN", header=FALSE)
heure1<-Sys.time()
main(TwoLeadECG_TRAIN, TwoLeadECG_TEST, 1, 2, 'TwoLeadECG')
heure2<-Sys.time()
t <- difftime(heure2, heure1, units = "mins")
writeLines("\n")
print(paste("Le temps (minutes) d'execution de l'heuristique est : ", t))
writeLines("\n\n\n")




wafer_TEST <- read.csv("./UCR_TS_Archive_2015/wafer_TEST", header=FALSE)
wafer_TRAIN <- read.csv("./UCR_TS_Archive_2015/wafer_TRAIN", header=FALSE)
heure1<-Sys.time()
main(wafer_TRAIN, wafer_TEST, 1, 2, 'wafer')
heure2<-Sys.time()
t <- difftime(heure2, heure1, units = "mins")
writeLines("\n")
print(paste("Le temps (minutes) d'execution de l'heuristique est : ", t))
writeLines("\n\n\n")




WordsSynonyms_TEST <- read.csv("./UCR_TS_Archive_2015/WordsSynonyms_TEST", header=FALSE)
WordsSynonyms_TRAIN <- read.csv("./UCR_TS_Archive_2015/WordsSynonyms_TRAIN", header=FALSE)
heure1<-Sys.time()
main(WordsSynonyms_TRAIN, WordsSynonyms_TEST, 1, 2, 'WordsSynonyms')
heure2<-Sys.time()
t <- difftime(heure2, heure1, units = "mins")
writeLines("\n")
print(paste("Le temps (minutes) d'execution de l'heuristique est : ", t))
writeLines("\n\n\n")




yoga_TEST <- read.csv("./UCR_TS_Archive_2015/yoga_TEST", header=FALSE)
yoga_TRAIN <- read.csv("./UCR_TS_Archive_2015/yoga_TRAIN", header=FALSE)
heure1<-Sys.time()
main(yoga_TRAIN, yoga_TEST, 1, 2, 'yoga')
heure2<-Sys.time()
t <- difftime(heure2, heure1, units = "mins")
writeLines("\n")
print(paste("Le temps (minutes) d'execution de l'heuristique est : ", t))
writeLines("\n\n\n")


#####################################################################################
#####################################################################################

DiatomSizeReduction_TEST <- read.csv("./UCR_TS_Archive_2015/DiatomSizeReduction_TEST", header=FALSE)
DiatomSizeReduction_TRAIN <- read.csv("./UCR_TS_Archive_2015/DiatomSizeReduction_TRAIN", header=FALSE)
heure1<-Sys.time()
main(DiatomSizeReduction_TRAIN, DiatomSizeReduction_TEST, 1, 2, 'DiatomSizeReduction')
heure2<-Sys.time()
t <- difftime(heure2, heure1, units = "mins")
writeLines("\n")
print(paste("Le temps (minutes) d'execution de l'heuristique est : ", t))
writeLines("\n\n\n")



BeetleFly_TEST <- read.csv("./UCR_TS_Archive_2015/BeetleFly_TEST", header=FALSE)
BeetleFly_TRAIN <- read.csv("./UCR_TS_Archive_2015/BeetleFly_TRAIN", header=FALSE)
heure1<-Sys.time()
main(BeetleFly_TRAIN, BeetleFly_TEST, 1, 2, 'BeetleFly')
heure2<-Sys.time()
t <- difftime(heure2, heure1, units = "mins")
writeLines("\n")
print(paste("Le temps (minutes) d'execution de l'heuristique est : ", t))
writeLines("\n\n\n")



BirdChicken_TEST <- read.csv("./UCR_TS_Archive_2015/BirdChicken_TEST", header=FALSE)
BirdChicken_TRAIN <- read.csv("./UCR_TS_Archive_2015/BirdChicken_TRAIN", header=FALSE)
heure1<-Sys.time()
main(BirdChicken_TRAIN, BirdChicken_TEST, 1, 2, 'yoga')
heure2<-Sys.time()
t <- difftime(heure2, heure1, units = "mins")
writeLines("\n")
print(paste("Le temps (minutes) d'execution de l'heuristique est : ", t))
writeLines("\n\n\n")


ShapeletSim_TEST <- read.csv("./UCR_TS_Archive_2015/ShapeletSim_TEST", header=FALSE)
ShapeletSim_TRAIN <- read.csv("./UCR_TS_Archive_2015/ShapeletSim_TRAIN", header=FALSE)
heure1<-Sys.time()
main(ShapeletSim_TRAIN, ShapeletSim_TEST, 1, 2, 'ShapeletSim')
heure2<-Sys.time()
t <- difftime(heure2, heure1, units = "mins")
writeLines("\n")
print(paste("Le temps (minutes) d'execution de l'heuristique est : ", t))
writeLines("\n\n\n")


ToeSegmentation1_TEST <- read.csv("./UCR_TS_Archive_2015/ToeSegmentation1_TEST", header=FALSE)
ToeSegmentation1_TRAIN <- read.csv("./UCR_TS_Archive_2015/ToeSegmentation1_TRAIN", header=FALSE)
heure1<-Sys.time()
main(ToeSegmentation1_TRAIN, ToeSegmentation1_TEST, 1, 2, 'ToeSegmentation1_TEST')
heure2<-Sys.time()
t <- difftime(heure2, heure1, units = "mins")
writeLines("\n")
print(paste("Le temps (minutes) d'execution de l'heuristique est : ", t))
writeLines("\n\n\n")


Wine_TEST <- read.csv("./UCR_TS_Archive_2015/Wine_TEST", header=FALSE)
Wine_TRAIN <- read.csv("./UCR_TS_Archive_2015/Wine_TRAIN", header=FALSE)
heure1<-Sys.time()
main(Wine_TRAIN, Wine_TEST, 1, 2, 'Wine')
heure2<-Sys.time()
t <- difftime(heure2, heure1, units = "mins")
writeLines("\n")
print(paste("Le temps (minutes) d'execution de l'heuristique est : ", t))
writeLines("\n\n\n")


Meat_TEST <- read.csv("./UCR_TS_Archive_2015/Meat_TEST", header=FALSE)
Meat_TRAIN <- read.csv("./UCR_TS_Archive_2015/Meat_TRAIN", header=FALSE)
heure1<-Sys.time()
main(Meat_TRAIN, Meat_TEST, 1, 2, 'Meat')
heure2<-Sys.time()
t <- difftime(heure2, heure1, units = "mins")
writeLines("\n")
print(paste("Le temps (minutes) d'execution de l'heuristique est : ", t))
writeLines("\n\n\n")


Car_TEST <- read.csv("./UCR_TS_Archive_2015/Car_TEST", header=FALSE)
Car_TRAIN <- read.csv("./UCR_TS_Archive_2015/Car_TRAIN", header=FALSE)
heure1<-Sys.time()
main(Car_TRAIN, Car_TEST, 1, 2, 'Car')
heure2<-Sys.time()
t <- difftime(heure2, heure1, units = "mins")
writeLines("\n")
print(paste("Le temps (minutes) d'execution de l'heuristique est : ", t))
writeLines("\n\n\n")


Herring_TEST <- read.csv("./UCR_TS_Archive_2015/Herring_TEST", header=FALSE)
Herring_TRAIN <- read.csv("./UCR_TS_Archive_2015/Herring_TRAIN", header=FALSE)
heure1<-Sys.time()
main(Herring_TRAIN, Herring_TEST, 1, 2, 'Herring_TEST')
heure2<-Sys.time()
t <- difftime(heure2, heure1, units = "mins")
writeLines("\n")
print(paste("Le temps (minutes) d'execution de l'heuristique est : ", t))
writeLines("\n\n\n")

Worms_TEST <- read.csv("./UCR_TS_Archive_2015/Worms_TEST", header=FALSE)
Worms_TRAIN <- read.csv("./UCR_TS_Archive_2015/Worms_TRAIN", header=FALSE)
heure1<-Sys.time()
main(Worms_TRAIN, Worms_TEST, 1, 2, 'Worms')
heure2<-Sys.time()
t <- difftime(heure2, heure1, units = "mins")
writeLines("\n")
print(paste("Le temps (minutes) d'execution de l'heuristique est : ", t))
writeLines("\n\n\n")


WormsTwoClass_TEST <- read.csv("./UCR_TS_Archive_2015/WormsTwoClass_TEST", header=FALSE)
WormsTwoClass_TRAIN <- read.csv("./UCR_TS_Archive_2015/WormsTwoClass_TRAIN", header=FALSE)
heure1<-Sys.time()
main(WormsTwoClass_TRAIN, WormsTwoClass_TEST, 1, 2, 'WormsTwoClass')
heure2<-Sys.time()
t <- difftime(heure2, heure1, units = "mins")
writeLines("\n")
print(paste("Le temps (minutes) d'execution de l'heuristique est : ", t))
writeLines("\n\n\n")


Plane_TEST <- read.csv("./UCR_TS_Archive_2015/Plane_TEST", header=FALSE)
Plane_TRAIN <- read.csv("./UCR_TS_Archive_2015/Plane_TRAIN", header=FALSE)
heure1<-Sys.time()
main(Plane_TRAIN, Plane_TEST, 1, 2, 'Plane_TEST')
heure2<-Sys.time()
t <- difftime(heure2, heure1, units = "mins")
writeLines("\n")
print(paste("Le temps (minutes) d'execution de l'heuristique est : ", t))
writeLines("\n\n\n")


Ham_TEST <- read.csv("./UCR_TS_Archive_2015/Ham_TEST", header=FALSE)
Ham_TRAIN <- read.csv("./UCR_TS_Archive_2015/Ham_TRAIN", header=FALSE)
heure1<-Sys.time()
main(Ham_TRAIN, Ham_TEST, 1, 2, 'Ham_TEST')
heure2<-Sys.time()
t <- difftime(heure2, heure1, units = "mins")
writeLines("\n")
print(paste("Le temps (minutes) d'execution de l'heuristique est : ", t))
writeLines("\n\n\n")


Earthquakes_TEST <- read.csv("./UCR_TS_Archive_2015/Earthquakes_TEST", header=FALSE)
Earthquakes_TRAIN <- read.csv("./UCR_TS_Archive_2015/Earthquakes_TRAIN", header=FALSE)
heure1<-Sys.time()
main(Earthquakes_TRAIN, Earthquakes_TEST, 1, 2, 'Earthquakes')
heure2<-Sys.time()
t <- difftime(heure2, heure1, units = "mins")
writeLines("\n")
print(paste("Le temps (minutes) d'execution de l'heuristique est : ", t))
writeLines("\n\n\n")


DistalPhalanxOutlineAgeGroup_TEST <- read.csv("./UCR_TS_Archive_2015/DistalPhalanxOutlineAgeGroup_TEST", header=FALSE)
DistalPhalanxOutlineAgeGroup_TRAIN <- read.csv("./UCR_TS_Archive_2015/DistalPhalanxOutlineAgeGroup_TRAIN", header=FALSE)
heure1<-Sys.time()
main(DistalPhalanxOutlineAgeGroup_TRAIN, DistalPhalanxOutlineAgeGroup_TEST, 1, 2, 'DistalPhalanxOutlineAgeGroup')
heure2<-Sys.time()
t <- difftime(heure2, heure1, units = "mins")
writeLines("\n")
print(paste("Le temps (minutes) d'execution de l'heuristique est : ", t))
writeLines("\n\n\n")


DistalPhalanxTW_TEST <- read.csv("./UCR_TS_Archive_2015/DistalPhalanxTW_TEST", header=FALSE)
DistalPhalanxTW_TRAIN <- read.csv("./UCR_TS_Archive_2015/DistalPhalanxTW_TRAIN", header=FALSE)
heure1<-Sys.time()
main(DistalPhalanxTW_TRAIN, DistalPhalanxTW_TEST, 1, 2, 'DistalPhalanxTW')
heure2<-Sys.time()
t <- difftime(heure2, heure1, units = "mins")
writeLines("\n")
print(paste("Le temps (minutes) d'execution de l'heuristique est : ", t))
writeLines("\n\n\n")


MiddlePhalanxTW_TEST <- read.csv("./UCR_TS_Archive_2015/MiddlePhalanxTW_TEST", header=FALSE)
MiddlePhalanxTW_TRAIN <- read.csv("./UCR_TS_Archive_2015/MiddlePhalanxTW_TRAIN", header=FALSE)
heure1<-Sys.time()
main(MiddlePhalanxTW_TRAIN, MiddlePhalanxTW_TEST , 1, 2, 'MiddlePhalanxTW')
heure2<-Sys.time()
t <- difftime(heure2, heure1, units = "mins")
writeLines("\n")
print(paste("Le temps (minutes) d'execution de l'heuristique est : ", t))
writeLines("\n\n\n")


MiddlePhalanxOutlineAgeGroup_TEST <- read.csv("./UCR_TS_Archive_2015/MiddlePhalanxOutlineAgeGroup_TEST", header=FALSE)
MiddlePhalanxOutlineAgeGroup_TRAIN <- read.csv("./UCR_TS_Archive_2015/MiddlePhalanxOutlineAgeGroup_TRAIN", header=FALSE)
heure1<-Sys.time()
main(MiddlePhalanxOutlineAgeGroup_TRAIN, MiddlePhalanxOutlineAgeGroup_TEST, 1, 2, 'MiddlePhalanxOutlineAgeGroup')
heure2<-Sys.time()
t <- difftime(heure2, heure1, units = "mins")
writeLines("\n")
print(paste("Le temps (minutes) d'execution de l'heuristique est : ", t))
writeLines("\n\n\n")
