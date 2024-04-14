library(party)
require(caret)
library(caTools)
require(e1071)
require(dplyr)
library(RColorBrewer)
require(FSelectorRcpp)
library(RColorBrewer)
coul <- brewer.pal(6, "Set1") 

OGStars<-data.frame(Stars)

colorNames <- unique(OGStars$Color)
colorNames
#Pre Feature Selection colors
par(mar=c(11,4,4,4))
barplot(table(OGStars$Color),
        ylab = "Freq",
        xlab = "",
        las=2)

barplot(table(OGStars$Type),
        ylab = "Freq",
        xlab = "Type",
        col=coul)

#Feature Selection
Stars <- Stars |> 
  mutate(Color = case_when(
    Color %in% c("yellowish", "Yellowish", "Yellowish White","yellow-white", "white-Yellow", "Yellow-White", "White-Yellow" ) ~ "Yellow"
    ,TRUE ~ Color
  )
)

Stars <- Stars |> 
  mutate(Color = case_when(
    Color %in% c("Blue White", "Blue white", "Blue-White", "Blue-white") ~ "Blue-White"
    ,TRUE ~ Color
  )
)

Stars <- Stars |> 
  mutate(Color = case_when(
    Color %in% c("Pale yellow orange", "Orange-Red") ~ "Orange"
    ,TRUE ~ Color
  )
)

Stars <- Stars |> 
  mutate(Color = case_when(
    Color %in% c("White", "Whitish", "white") ~ "White"
    ,TRUE ~ Color
  )
)

colors <- c("Blue", "lightblue", "Orange", "Red", "White", "Yellow")

#New colors
barplot(table(Stars$Color),
        ylab = "Frequency",
        xlab = "Color",
        col=colors)

information_gain(formula = Type ~ ., data = Stars, type="gainratio")

#Decision Tree (It gave me different results when I switched from stars to copy?)

OGStars$Color<-as.factor(OGStars$Color)

OGStars$Spectral_Class<-as.factor(OGStars$Spectral_Class)

model<- ctree(Type ~ ., OGStars)
plot(model)

table(predict(model), Stars$Type)

#Divide dataset
starRando<-Stars[sample(nrow(Stars)),]

StarTrain<-starRando[0:192,]

StarTest<-starRando[192:240,]

#Naive Bayes (Run multiple times)
classifier_cl <- naiveBayes(Type ~ ., data = StarTrain)
classifier_cl

# Predicting on test data'
y_pred <- predict(classifier_cl, newdata = StarTest)

# Confusion Matrix
cm <- table(StarTest$Type, y_pred)
cm

# Model Evaluation
confusionMatrix(cm)
