n

## CHURRUMAIS FLAMAS
## CHEETOS TOINGS
## CRUJITOS FLAMAS

arima_func <- function(i){
  
  df <- sorted %>% filter(Sub_marca == sub_brands[i, 1]) %>%
    group_by(date) %>%
    summarise (units = sum(Venta_Units),
               dist = mean(Dist_Vertical_Corregido),
               price = mean(Price_Sell.In, na.rm=T)) %>% arrange(date)
  
  ts_df <- ts(df$units, frequency = 52)
  
  arima_model <-
    forecast::auto.arima(y = ts_df,
                         max.order = 4,
                         xreg = df[,c('dist','price')])
  return(arima_model)

  }

arima_func(1)

df <- sorted %>% filter(Sub_marca == 'CHURRUMAIS FLAMAS') %>%
  group_by(date) %>%
  summarise (units = sum(Venta_Units),
             dist = mean(Dist_Vertical_Corregido),
             price = mean(Price_Sell.In, na.rm=T)) %>% arrange(date)

require(ggplot2)
sorted$Price_Sell.In[is.infinite(sorted$Price_Sell.In)] <- 0

avg_prices <- sorted%>%group_by(Sub_marca)%>%summarize(price = mean(Price_Sell.In, na.rm=T))



dmatx <- dist(avg_prices$price)

cluster_trees <- hclust(d = dmatx)

avg_prices$cluster_var <- cutree(tree = cluster_trees,k = 3)

##########################

size_mapping$BRAND <- gsub(pattern = '\r',replacement = '',x = size_mapping$BRAND)
size_mapping$`Type of snack` <- gsub(pattern = '\r',replacement = '',x = size_mapping$`Type of snack`)

write.csv(x = sorted, file = 'Innovation_salty_ADS_sorted.csv')
#########################
require(dplyr)

# sorted <- read.csv('Innovation_salty_ADS_sorted.csv')

sorted <- readRDS('C:\\Users\\srikar.manepalli\\Documents\\PepsiCo\\Innovation Forecasting\\CodeRep_IF\\IF_CodeRep\\innov_salty_sorted_new_29Jul.RDS')

size_mapping <- readxl::read_xlsx(path = 'Size_mapping_salty.xlsx',sheet = 1)

types <- unique(size_mapping$Class)%>%toupper()

sorted$Class <- toupper(sorted$Class)

unique_gram_combos <- paste0(sorted$Class,' ',sorted$Grams)%>%unique

sorted$size_key <- paste0(sorted$Class,' ',sorted$Grams)

grams_mapping <- sorted%>%select(Class, Grams, size_key)%>%filter(!duplicated(size_key))



size_mapping$Class <- toupper(size_mapping$Class)

final_mapping <- data.frame()
for (i in types) {
  temp_mapping <- size_mapping %>% filter(Class == i)
  
  temp_grams <- grams_mapping %>% filter(Class == i)
  
  # class(temp_grams%>%select(gr))
  
  temp_grams$Category <- cut(
    temp_grams$Grams,
    breaks = c(temp_mapping$Lower_limit_gramage,2000),
    labels = temp_mapping$Category,
    include.lowest = T
  )
  
  final_mapping <- rbind(final_mapping,temp_grams)
  
}

sorted <- sorted%>%left_join(final_mapping,by = c('size_key'))
#####
sorted$Nielsen.Price.kilo[is.na(sorted$Nielsen.Price.kilo)] <- 0
sorted$Category[is.na(sorted$Category)] <- 'Mini'
######

# 'CHEETOS POFFETS','SUNBITES PLATANITOS DULCE'

selected_sub_marcas <- c('CHURRUMAIS FLAMAS','DORITOS DINAMITA CHILE Y LIMON','DORITOS DINAMITA NACHO PICOSO',
'DORITOS INFERNO','PAKETAXO PASUMECHA','PAKETAXO QUEXO','RUFFLES MEGACRUNCH COCTELERAS','RUFFLES XTRAONDA ALITAS PICANT',
'SABRITAS ACIDULCE','SABRITAS BRAVERIA','SABRITAS CHAMPIONS','SABRITAS CHILE MANZANO','SABRITAS HABANERO LIMON',
'SABRITAS KEBAB','SABRITAS RCRUJ FH','SABRITAS ZHILLI','TOSTITOS QUESO PICANTE',
'TOSTITOS CHILE MANSO')


clean_sorted <- sorted%>%filter(Sub_marca %in% selected_sub_marcas)

clean_sorted_wlag <- clean_sorted%>%group_by(Area, Nivel_Cliente, Sub_marca)%>%mutate(lag_Venta_Units = lag(Venta_Units,default = 0))

sum(is.na(clean_sorted_wlag$lag_Venta_Units))



plot(clean_sorted_wlag$Venta_Units,clean_sorted_wlag$lag_Venta_Units)



###############################

unique(clean_sorted_wlag$Sub_marca) -> unique_innovs

test_innovs <- sample(unique_innovs,0.2*length(unique_innovs))
train_innovs <- unique_innovs[!unique_innovs %in% test_innovs]

train_data <- clean_sorted_wlag%>%filter(Sub_marca %in% train_innovs)
test_data <- clean_sorted_wlag%>%filter(!Sub_marca %in% train_innovs)

# require(lme4)

mix_model6 <-
  lmer(
    formula =  Venta_Units ~lag_Venta_Units + Dist_Vertical_Corregido**3 + Venta_Units_SABRITAS +
      Nielsen.Price.kilo  + (1 | Area:Nivel_Cliente) + (1|Category)
      +(1 | Marca) + (1 | Textura) + (1 |Familia_de_Sabor) ,#+ (1|Class.x)    ,
    data = train_data,REML = F,
    control = lmerControl(optimizer ='optimx', optCtrl=list(method='nlminb'))
    
  )


summary(mix_model6)

MuMIn::r.squaredGLMM(mix_model6)

coef(mix_model6)
###################

predicted <- predict(object = mix_model6,newdata = test_data%>%select(-Venta_Units))

results <- data.frame(predicted,test_data$Venta_Units)

predicted[is.na(predicted)] <- 0 
predicted[is.nan(predicted)] <- 0
predicted[is.infinite(predicted)] <- 0

ModelMetrics::rmse(actual = test_data$Venta_Units,predicted = predicted)/mean(test_data$Venta_Units)

###################
plot(clean_sorted_wlag$Venta_Units,clean_sorted_wlag$Venta_Units_SABRITAS)

filt_sorted_units$Dist_Vertical_Corregido_cubed <-
  filt_sorted_units$Dist_Vertical_Corregido ^ 3

mix_model7 <-
  lmer(
    formula = Venta_Units ~ Dist_Vertical_Corregido_cubed + Venta_Units_SABRITAS +
      Price_Sell.In + Grams +
      Devolucion_Units + (1 |
                            Area:Nivel_Cliente) + (1 | week_from_launch) + (1 + Area | Marca) +
      (1 |
         Textura) + (1 | Familia_de_Sabor) + (1 |
                                                Tipo) + (1 + Price_Sell.In | Marca),
    data = filt_sorted_units,
    REML = F
  )

summary(mix_model6)

# ,
# control = lmerControl(
#   optimizer ='optimx', optCtrl=list(method='nlminb'))

require(optimx)

mix_model8 <-
  lmer(
    formula = Venta_Units ~ Dist_Vertical_Corregido_cubed + Venta_Units_SABRITAS +
      Price_Sell.In + Grams +
      Devolucion_Units + (1 |
                            Area:Nivel_Cliente) + (1 | week_from_launch) + (1 + Area | Marca) +
      (1 |
         Textura) + (1 | Familia_de_Sabor) + (1 |
                                                Tipo) + (1 + Price_Sell.In | Marca),
    data = filt_sorted_units,
    REML = F,
    control = lmerControl(optimizer = 'optimx', optCtrl =
                            list(method = 'nlminb'))
  )
