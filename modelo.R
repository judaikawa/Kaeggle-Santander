# Kaeggle: Santander

# Libraries --------------------------------------------------------------------------
library(corrplot)
library(ggplot2)
library(plotly)
library(caret)
library(usdm)
library(ROCR)
library(MASS)
library(glmnet)

# Dataset -----------------------------------------------------------------------------
train_REAL <- read.csv('train.csv')
test_REAL <- read.csv('test.csv')
sample_submission <- read.csv('sample_submission.csv')

# Target to factor --------------------------------------------------------------------
train_REAL$target <- as.factor(train_REAL$target)
summary(train_REAL$target) # Unbaleced 

# Scaled Dataset ---------------------------------------------------------------------
scaled_train_REAL <- cbind(train_REAL[,c(1,2)], scale(train_REAL[,-c(1,2)]))
scaled_test_REAL <- cbind(test_REAL[,c(1,2)], scale(test_REAL[,-c(1,2)]))

# Use dataset ------------------------------------------------------------------------
train_USE <- scaled_train_REAL
test_USE <- scaled_test_REAL

# Getting balanced sample -------------------------------------------------------------
train_REAL_balanced <- subset(train_USE, train_USE$target==1)
train_REAL_balanced <- rbind(train_REAL_balanced, 
                             subset(train_USE, train_USE$target==0)[sample(nrow(subset(train_USE, train_USE$target==0)), 
                                                                           nrow(train_REAL_balanced)),])

# Splitting train set into train and test ---------------------------------------------
set.seed(123)
train_index <- sample(nrow(train_REAL_balanced), nrow(train_REAL_balanced)*0.7)
test_index <- setdiff(seq(1:nrow(train_REAL_balanced)), train_index)

train <- train_REAL_balanced[train_index,]
test <- train_REAL_balanced[test_index,]

# Correlations of numeric variables ---------------------------------------------------
cor_train <- as.data.frame(cor(train[,-c(1,2)]))
cor_train[cor_train == 1] <- -999

max_array <- c()

for (i in c(1:ncol(cor_train))) {
  
  max_array <- append(max_array, max(cor_train[,i]))
  
}

max(max_array) # Maximum correlation of 2.8%

# Boxplots ----------------------------------------------------------------------------

for(i in names(train)[-c(1,2)]){
  p=print(ggplot(train,aes_string(y=i,x='target'))+geom_boxplot())
  ggplotly(p)
  readline(prompt="Press [enter] to continue")
}


# Logistic Regression -----------------------------------------------------------------

# Formula inicial --------------------------------------------------------------------
formula_inicial <- target ~ var_0+var_1+var_2+var_3+var_4+var_5+  
  var_6+var_7+var_8+var_9+var_10+  +var_11+  +var_12+  +var_13+ 
  var_14+  +var_15+  +var_16+  +var_17+  +var_18+  +var_19+  +var_20+  +var_21+ 
  var_22+  +var_23+  +var_24+  +var_25+  +var_26+  +var_27+  +var_28+  +var_29+ 
  var_30+  +var_31+  +var_32+  +var_33+  +var_34+  +var_35+  +var_36+  +var_37+ 
  var_38+  +var_39+  +var_40+  +var_41+  +var_42+  +var_43+  +var_44+  +var_45+ 
  var_46+  +var_47+  +var_48+  +var_49+  +var_50+  +var_51+  +var_52+  +var_53+ 
  var_54+  +var_55+  +var_56+  +var_57+  +var_58+  +var_59+  +var_60+  +var_61+ 
  var_62+  +var_63+  +var_64+  +var_65+  +var_66+  +var_67+  +var_68+  +var_69+ 
  var_70+  +var_71+  +var_72+  +var_73+  +var_74+  +var_75+  +var_76+  +var_77+ 
  var_78+  +var_79+  +var_80+  +var_81+  +var_82+  +var_83+  +var_84+  +var_85+ 
  var_86+  +var_87+  +var_88+  +var_89+  +var_90+  +var_91+  +var_92+  +var_93+ 
  var_94+  +var_95+  +var_96+  +var_97+  +var_98+  +var_99+  +var_100+ +var_101+
  var_102+ +var_103+ +var_104+ +var_105+ +var_106+ +var_107+ +var_108+ +var_109+
  var_110+ +var_111+ +var_112+ +var_113+ +var_114+ +var_115+ +var_116+ +var_117+
  var_118+ +var_119+ +var_120+ +var_121+ +var_122+ +var_123+ +var_124+ +var_125+
  var_126+ +var_127+ +var_128+ +var_129+ +var_130+ +var_131+ +var_132+ +var_133+
  var_134+ +var_135+ +var_136+ +var_137+ +var_138+ +var_139+ +var_140+ +var_141+
  var_142+ +var_143+ +var_144+ +var_145+ +var_146+ +var_147+ +var_148+ +var_149+
  var_150+ +var_151+ +var_152+ +var_153+ +var_154+ +var_155+ +var_156+ +var_157+
  var_158+ +var_159+ +var_160+ +var_161+ +var_162+ +var_163+ +var_164+ +var_165+
  var_166+ +var_167+ +var_168+ +var_169+ +var_170+ +var_171+ +var_172+ +var_173+
  var_174+ +var_175+ +var_176+ +var_177+ +var_178+ +var_179+ +var_180+ +var_181+
  var_182+ +var_183+ +var_184+ +var_185+ +var_186+ +var_187+ +var_188+ +var_189+
  var_190+ +var_191+ +var_192+ +var_193+ +var_194+ +var_195+ +var_196+ +var_197+
  var_198+ +var_199

# Modelo inicial ---------------------------------------------------------------------

m1 <- glm(formula_step, train, family = binomial())

summary(m1)

print(paste("R quadrado: ",round( 1 - ( summary(m1)$deviance / summary(m1)$null.deviance ), 2 ))) # R squared


# Modelo 2 ---------------------------------------------------------------------------

formula_step <- target ~ 1

formula_step <- target ~ var_81 + var_139 + var_146 + var_6 + var_12 + var_110 + 
  var_53 + var_76 + var_26 + var_21 + var_99 + var_190 + var_80 + 
  var_2 + var_166 + var_174 + var_13 + var_22 + var_165 + var_1 + 
  var_133 + var_148 + var_198 + var_169 + var_44 + var_0 + 
  var_149 + var_108 + var_115 + var_92 + var_78 + var_191 + 
  var_40 + var_170 + var_94 + var_109 + var_9 + var_67 + var_179 + 
  var_33 + var_192 + var_197 + var_34 + var_164 + var_122 + 
  var_121 + var_184 + var_18 + var_127 + var_89 + var_154 + 
  var_155 + var_75 + var_173 + var_118 + var_163 + var_91 + 
  var_56 + var_86 + var_177 + var_36 + var_147 + var_123 + 
  var_48 + var_107 + var_35 + var_172 + var_43 + var_49 + var_95 + 
  var_87 + var_141 + var_32 + var_145 + var_24 + var_70 + var_188 + 
  var_137 + var_167 + var_93 + var_162 + var_5 + var_130 + 
  var_51 + var_151 + var_106 + var_131 + var_58 + var_157 + 
  var_193 + var_90 + var_71 + var_186 + var_175 + var_111 + 
  var_52

# Step --------------------------------------------------------------------------------------

step <- step(glm(formula_step, train, family = binomial()), direction = 'forward', scope=formula_inicial)
formula_step <- step$formula

glm_step <- glm(formula_step,
                train, family = binomial())
summary(glm_step)


# LASSO -----------------------------------------------------------------------------------------

x <- model.matrix(formula_inicial, train)[,-1]
y <- model.matrix(formula_inicial, test)[,-1]

m2 <- glmnet(x, train$target, family = 'binomial')
(coef(m2, s= 0.005))

# Predict -----------------------------------------------------------------------------
model = m1

pred <- predict(model, y, type = "response", s=0.05, alpha = 1)
pred <- predict(model, test, type = "response")
metrics <- ROCR::prediction(pred, test$target)

# Acuracia do modelo 
alfa <- 0.5 # Definindo treshold

perf <- performance(metrics, measure='cost')
alfa <- unname(perf@x.values[[1]][which.min(perf@y.values[[1]])])
alfa

confusionMatrix(as.factor(ifelse(pred>alfa, 1, 0)),
                as.factor(test$target)) # Acuracia: 0.7819

# Sensitivity : 0.7800         
# Specificity : 0.7838

# Score ---------------------------------------------------------------------------------
# prediction
train$prediction <- predict(model, newdata = train, type = "response" )
test$prediction  <- predict(model, newdata = test , type = "response" )

# distribution of the prediction score grouped by known outcome
ggplot(train, aes(prediction, color = target)) +
  geom_density(size = 1 ) +
  ggtitle( "Training Set's Predicted Score" ) +
  #geom_vline(xintercept = 0.47) +
  scale_color_manual(values=c("#999999", "#56B4E9")) +
  theme_minimal()

# ROC -------------------------------------------------------------------------------
perf <- performance(metrics, measure="tpr",
                    x.measure="fpr")
plot(perf)
abline(a=0,b=1)

perf <- performance(metrics, measure="auc")
print(paste0("AUC: ", perf@y.values)) # 0.85

pred_submission <- predict(model, test_USE, type = "response")
pred_submission <- as.factor(ifelse(pred_submission>alfa, 1, 0))

submission <- as.data.frame(test_USE$ID_code)
submission$target <- pred_submission

names(submission) <- names(sample_submission)

write.csv(submission, "submission.csv", quote = FALSE, row.names = FALSE)


# Model 1 scored 0.77