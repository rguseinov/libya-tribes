#install.packages('tidyverse')
#install.packages('stargazer')
#install.packages('caret')
#install.packages('bruceR')
#install.packages('pROC')
#install.packages('geojsonio')
#install.packages('ggrepel')
#install.packages('mediation')
#install.packages('DiagrammeR')
#install.packages('glue')

#library(tidyverse)
#library(stargazer)
#library(caret)
#library(bruceR)
#library(pROC)
#library(geojsonio)
#library(ggrepel)
#library(mediation)
#library(DiagrammeR)
#library(glue)

## Princtipal component analysis
# choose variables and compute  principal components
small <- regdiv %>% dplyr::select(-c('district', 'reg', 'waterperc', 'roomperc', 'lightperc', 'sewperc', 'cookperc', 'atersorceperc', 'unemployment'))
pca <- prcomp(small, center = TRUE, scale = TRUE)
small2 <- predict(pca, newdata = small)
with_pc <- cbind(regdiv[1:2], small2[, 1:2])

# getting the index
with_pc$index1 = round(-10 * with_pc$PC1, 2)
with_pc$index2 = round(10 * with_pc$PC2, 2)

# scaling the index
with_pc$index1norm = scaler(with_pc$index1, min = 0, max = 100)
with_pc$index2norm = scaler(with_pc$index2, min = 0, max = 100)


## Visualize regional divisions
ggplot(with_pc, aes(x=reg, y=index1)) + 
  geom_boxplot()+
  labs(title = 'Распределение значений первой главной\nкомпоненты по регионам Ливии',
       x = 'Регион Ливии',
       y = 'PC1')+
  theme_classic()

ggplot(with_pc, aes(x=reg, y=index2)) + 
  geom_boxplot()+
  labs(title = 'Распределение значений второй главной\nкомпоненты по регионам Ливии',
       x = 'Регион Ливии',
       y = 'PC2')+
  theme_classic()

ggplot(libya_data, aes(x = pop_tot))+
  geom_histogram()+
  geom_vline(aes(xintercept = mean(pop_tot)), libya_data, color = "red", linewidth = 1)+
  labs(title = 'Распределение оценок численности населения\nв ливийских городах',
       subtitle = 'Красней линией обозначено среднее',
       x = 'Оценка численности населения',
       y = 'Число городов')+
  scale_x_continuous(labels = scales::comma)+
  theme_classic()

## Test regional divisions
anov <- aov(with_pc$index2 ~ with_pc$reg)
xtable(summary(anov))

#get the LaTeX output
stargazer(libya_data, type = 'text')

## Test the first hypothesis
libya_data$pop_tot[is.na(libya_data$pop_tot)] <- mean(libya_data$pop_tot, na.rm = TRUE) #replace NAs with mean
libya_data$pop_tot = log(libya_data$pop_tot) #log the population
libya_data$index1norm = scaler(libya_data$index1, min = 0, max = 100)
libya_data$index2norm = scaler(libya_data$index2, min = 0, max = 100)
libya_data$longforregr = scaler(libya_data$longitude, min = 0, max = 1) #scale GPS data
libya_data$latforregr = scaler(libya_data$latitude, min = 0, max = 1) #scale GPS data

model1 <- glm(tribe_oppose_binary ~ index1norm + index2norm + foodprice, data = libya_data, family = binomial())
model2 <- glm(tribe_oppose_binary ~ index1norm + index2norm, data = libya_data, family = binomial())
model3 <- glm(tribe_oppose_binary ~ index1norm + index2norm + pop_tot, data = libya_data, family = binomial())
model4 <- glm(tribe_oppose_binary ~ index1norm + index2norm + pop_tot + longforregr, data = libya_data, family = binomial())

# without Tripoli, Benghazi and Sebha
libya_data2 = libya_data[-c(15, 56, 71), ]
model5 <- glm(tribe_oppose_binary ~ index1norm + index2norm + pop_tot + longforregr, data = libya_data2, family = binomial())

# Regression output
stargazer(model1, model2, model3, model4, model5, type = 'text',
          title="Проверка гипотезы №1",
          dep.var.labels=c("Оппозиционность племени"),
          covariate.labels=c("PC1","PC2", 'Индекс прод. цен', 'Численность населения (log)', 'Долгота'))


# Quality metrics
a <- predict(model4, libya_data, type = 'response')
libya_data$opposepred = ifelse(a >= 0.5, 1, 0)
xtab <- table(libya_data$opposepred, libya_data$tribe_oppose_binary) #get the confusion matrix
confusionMatrix(xtab, mode = "everything", positive="1") #quality metrics
# AUC-ROC
pred <- prediction(a, libya_data$tribe_oppose_binary)
auc <- performance(pred, 'auc')
auc <- unlist(slot(auc,'y.values'))


## Test the second hypothesis
model1 <- glm(uprising_control_binary ~ tribe_oppose_ord, data = libya_data, family = binomial())
model2 <- glm(uprising_control_binary ~ tribe_oppose_ord + pop_tot, data = libya_data, family = binomial())
model3 <- glm(uprising_control_binary ~ tribe_oppose_ord + pop_tot + death_toll, data = libya_data, family = binomial())
model4 <- glm(uprising_control_binary ~ tribe_oppose_ord + index1norm + index2norm, data = libya_data, family = binomial())
model5 <- glm(uprising_control_binary ~ tribe_oppose_ord + index1norm + index2norm + latforregr, data = libya_data, family = binomial())

# Regression output
stargazer(model1, model2, model3, model4, model5, type = 'latex',
          title = 'Проверка гипотезы №2',
          dep.var.labels=c("Контроль города силами Каддафи"),
          covariate.labels=c("Оппозиционность племени","Численность населения (log)", 'Людские потери', 'PC1', 'PC2', 'Широта'))

# Quality metrics
a <- predict(model5, libya_data, type = 'response')
libya_data$opposepred = ifelse(a >= 0.5, 1, 0)
xtab <- table(libya_data$opposepred, libya_data$uprising_control_binary) #get the confusion matrix
confusionMatrix(xtab, mode = "everything", positive="1") #quality metrics
# AUC-ROC
pred <- prediction(a, libya_data$uprising_control_binary)
auc <- performance(pred, 'auc')
auc <- unlist(slot(auc,'y.values'))

## mediation test
set.seed(666) #the most metal seed
X_control = mean(libya_data$index1norm) - sd(libya_data$index1norm) #set control and treatment values as treatment is continuous
X_treatment = mean(libya_data$index1norm) + sd(libya_data$index1norm)
model.0 <- glm(uprising_control_binary ~ index2norm + index1norm + foodprice, data = libya_data, family = binomial())
model.M <- glm(tribe_oppose_binary ~ index2norm + index1norm + foodprice, data = libya_data, family = binomial())
model.Y <- glm(uprising_control_binary ~ index2norm + tribe_oppose_ord + index1norm + foodprice, data = libya_data, family = binomial())
results <- mediate(model.M, model.Y, treat='index1norm', mediator='tribe_oppose_ord',
                   control.value = X_control, treat.value = X_treatment,
                   boot=TRUE, sims=500)
summary(results)

## plot mediation output
data <-
  data.frame(
    lab_x   = "Соц.-экономическое\\nблагополучие",
    lab_m   = "Оппозиционность\\nплемени",
    lab_y   = "Восстание\\nв городе",
    coef_xm = "0.47*",
    coef_my = "2.47***",
    coef_xy = "0.03*"
  )


diagram <- function(data, height = .75, width = 2, graph_label = NA, node_text_size = 12, edge_text_size = 12, color = "black", ranksep = .2, minlen = 3){
  
  require(glue)
  require(DiagrammeR)
  
  data$height  <- height   # node height
  data$width   <- width    # node width
  data$color   <- color    # node + edge border color
  data$ranksep <- ranksep  # separation btwn mediator row and x->y row
  data$minlen  <- minlen   # minimum edge length
  
  data$node_text_size  <- node_text_size
  data$edge_text_size  <- edge_text_size
  
  data$graph_label <- ifelse(is.na(graph_label), "", paste0("label = '", graph_label, "'"))
  
  diagram_out <- glue::glue_data(data,
                                 "digraph flowchart {
      fontname = Helvetica
      <<graph_label>>
      graph [ranksep = <<ranksep>>]

      # node definitions with substituted label text
      node [fontname = Helvetica, shape = rectangle, fixedsize = TRUE, width = <<width>>, height = <<height>>, fontsize = <<node_text_size>>, color = <<color>>]        
        mm [label = '<<lab_m>>']
        xx [label = '<<lab_x>>']
        yy [label = '<<lab_y>>']

      # edge definitions with the node IDs
      edge [minlen = <<minlen>>, fontname = Helvetica, fontsize = <<edge_text_size>>, color = <<color>>]
        mm -> yy [label = '<<coef_my>>'];
        xx -> mm [label = '<<coef_xm>>'];
        xx -> yy [label = '<<coef_xy>>'];
      
      { rank = same; mm }
      { rank = same; xx; yy }
      
      }

      ", .open = "<<", .close = ">>")  
  
  
  DiagrammeR::grViz(diagram_out)  
}

diagram(data)

# export diagram to png
med_diagram(med_data) %>% 
  export_svg %>% charToRaw %>% rsvg_png("graphfin.png")

## plot a map of predicted probabilities

spdf <- geojson_read('https://raw.githubusercontent.com/wmgeolab/geoBoundaries/0be9a3df1e3bff5a106104374c371de2d72c12a4/releaseData/gbOpen/LBY/ADM1/geoBoundaries-LBY-ADM1.geojson', what = "sp")
fortified <- fortify(spdf, region = "shapeISO")

ggplot() + geom_polygon(data = fortified, aes(x = long, y = lat, group = group),
                        fill = "white", color = "black") + theme_void() + coord_map() +
  geom_point(data = libya_data, aes(x = longitude, y = latitude, size = pred, col = as.factor(tribe_oppose_ord)))+
  labs(title = 'Карта предсказанных вероятностей захвата города повстанцами',
       subtitle = 'N = 80',
       caption = 'Карта административного деления на 2007 год, 22 муниципалитета (шабии)',
       col = 'Оппозиционность\nплемени',
       size = 'Предсказанная\nвероятность')

# plot a map of Libyan tribes
ggplot() + geom_polygon(data = fortified, aes(x = long, y = lat, group = group),
                        fill = "white", color = "black") + theme_void() + coord_map() +
  geom_point(data = lib_df, aes(x = newlong, y = newlat, col = tribe_name), size = 1.5)+
  geom_label_repel(data = subset(lib_df, pop_tot > 70000), aes(x = newlong, y = newlat, label = tribe_name),
                   size = 4, box.padding = 0.5, point.padding = 0, nudge_y = -0.1, nudge_x = -0.25, max.overlaps = 200)+
  labs(title = 'Доминирующие племена в ливийских городах\nиз выборки для исследования',
       subtitle = 'N = 80. В скобках указана принадлежность к племенной семье.',
       caption = 'Карта административного деления на 2007 год, 22 муниципалитета (шабии). \nОтдельно доминирующие племена отмечены в городах населением более 70000 человек на 2010 год',
       col = 'Наименование племен
на английском')+
  scale_size_continuous(breaks = seq(0, 1, 0.2))

