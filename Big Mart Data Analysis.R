setwd(r'(C:\Users\Priti\Downloads)')
install.packages("ggplot2")
install.packages('tidyverse')
library(ggplot2)
library(tidyverse)
df<-read.csv('Train.csv')



View(df)
head(df)
str(df)

# 1. ------------------- Dimensions, summary, structure ----------------------------------- #
dim_df <- dim(df)
dim_df


summary(df)

#2.<---------------- columns treament ----------------

col<-colnames(df)


#3.<---------------- unique data ----------------

unique(df$Item_Fat_Content)
unique(df$Item_Type)
unique(df$Outlet_Establishment_Year)
unique(df$Outlet_Size)

# ----------------------------- Subset ---------------------------------# 


lf <- df[df$Item_Fat_Content=='Low Fat',c('Item_Weight','Item_MRP')]

#5.<---------------- Treatment of Null value ----------------

View(df)
df[df$Item_Fat_Content=='LF' | df$Item_Fat_Content=='low fat',] <-'Low Fat'
df[df$Item_Fat_Content=='reg',]<-'Regular'
sum(is.na(df))
colSums(is.na(df))

df$Item_Weight<-ifelse((df$Item_Weight=='Regular'|df$Item_Weight=='Low Fat'),0,df$Item_Weight)
df$Item_Weight<-as.numeric(df$Item_Weight)
df$Item_Weight<-replace(df$Item_Weight,is.na(df$Item_Weight),mean(df$Item_Weight,na.rm = T))

View(df[df$Item_Weight==0,]) # it shows that item weight column rows has all 0

df <- (df[!df$Item_Weight==0,]) #it will replace that 0

summary(df)
View(df)


table(df$Outlet_Size)
df$Outlet_Size <- ifelse(df$Outlet_Size=='','Medium',df$Outlet_Size)
View(df[df$Outlet_Size=='',])


#6.---------------- Conversion of datatype---------------
df$Item_Outlet_Sales <- as.numeric(df$Item_Outlet_Sales)
df$Outlet_Establishment_Year <- as.numeric(df$Outlet_Establishment_Year)
df$Irem_MRP <- as.numeric(df$Item_MRP)
df$Item_Visibility <- as.numeric(df$Item_Visibility)

summary(df)
View(df)

#7.-------------Univariate analysis----------------------

#1.---------------- Pie Charts -------------

install.packages('patchwork')
library(patchwork)

table(df$Item_Fat_Content)
View(table(df$Item_Type))
df_IT <- data.frame(table(df$Item_Type))
df %>% select(2:4) %>% glimpse()

View(df_IT)

View(df_IT$Freq)
P_Item_Type <-ggplot(df_IT,aes(x='',y=Freq,fill=Var1))+
  geom_col()+
  geom_text(aes(label = paste(round(Freq*100/sum(Freq)),'%')),position = position_stack(vjust = 0.8))+
  coord_polar(theta = 'y')+
  labs(title = '% of Item Type ')+
  theme_void()

P_Item_Type

View(df)
View(table(df$Outlet_Size))
df_OS <- data.frame(table(df$Outlet_Size))

P_O_Size <-ggplot(df_OS,aes(x='',y=Freq,fill=Var1))+
  geom_col()+
  geom_text(aes(label = paste(round(Freq*100/sum(Freq)),'%')),position = position_stack(vjust = 0.8))+
  coord_polar(theta = 'y')+
  labs(title = '% of Outlet Size ')+
  theme_void()
P_O_Size

View(table(df$Item_Fat_Content))
df_IF <- data.frame(table(df$Item_Fat_Content))

P_IF <-ggplot(df_IF,aes(x='',y=Freq,fill=Var1))+
  geom_col()+
  geom_text(aes(label = paste(round(Freq*100/sum(Freq)),'%')),position = position_stack(vjust = 0.8))+
  coord_polar(theta = 'y')+
  labs(title = '% of Item Fat Content ')+
  theme_void()
P_IF

P_Item_Type+P_O_Size+P_IF

#2---------------------Bar Plot----------------

B_IT<-ggplot(df, aes(x = Item_Type)) +
  geom_bar() +
  labs(title = 'Bar Plot for Item Type', x = 'Item Type', y = 'Frequency')

B_IT

B_IFC<-ggplot(df, aes(x = Item_Fat_Content)) +
  geom_bar() +
  labs(title = 'Bar Plot for Item Fat Content', x = 'Item Fat Content', y = 'Frequency')

B_IFC

B_IT / B_IFC


#3--------------- Box Plot ---------------------------

ggplot(df, aes(x = Item_Fat_Content, y = Item_Weight)) +
  geom_boxplot() +
  labs(title = 'Box Plot for Item Weight by Fat Content', x = 'Item Fat Content', y = 'Item Weight')


#4----------------Histogram---------------------

typeof(df$Item_MRP)
df$Item_MRP <- as.numeric(df$Item_MRP)


H_IM<-ggplot(df, aes(x = Item_MRP)) +
  geom_histogram(binwidth = 10, fill = 'pink', color = 'black') +
  labs(title = 'Histogram for Item MRP', xlab = 'Item MRP', ylab = 'Few')
H_IM

H_OEY<-ggplot(df, aes(x = Outlet_Establishment_Year)) +
  geom_histogram(binwidth = 10, fill = 'pink', color = 'black') +
  ggtitle('Histogram for Outlet Establishment Year')+
  xlab("Year")+
  ylab("Frequency")
H_OEY

H_IM + H_OEY

#8------------------Multivariate Analysis------------------

#1.-------------- Cooreleation----------------------

install.packages("corrplot")
library(corrplot)


df$Item_Weight <- as.numeric(df$Item_Weight)
df$Item_Outlet_Sales <- as.numeric(df$Item_Outlet_Sales)

cor_matrix <- cor(df[, c("Item_Weight", "Item_MRP", "Item_Outlet_Sales","Outlet_Establishment_Year")])

corrplot(cor_matrix, method = "shade", type = "upper", tl.col = "black")

#2.---------------- Scatter Plot -------------

ggplot(df, aes(x = Item_MRP, y = Item_Outlet_Sales)) +
  geom_point() +
  labs(title = 'Scatter Plot: Item MRP vs Item Outlet Sales', x = 'Item MRP', y = 'Item Outlet Sales')

#3.---------------- Line Plot -------------

ggplot(df, aes(x = Outlet_Establishment_Year, y = Item_Outlet_Sales, group = Outlet_Type, color = Outlet_Type)) +
  geom_line() +
  labs(title = 'Line Plot: Outlet Establishment Year vs Item Outlet Sales', x = 'Outlet Establishment Year', y = 'Item Outlet Sales') +
  theme(legend.position = "top")
