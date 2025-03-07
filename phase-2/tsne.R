require(tidyverse)
require(Rtsne)
processed_data <- read.csv("Data/processed-data.csv")

for(i in 1:15){
  print(colnames(processed_data[i]))
  print(cor(x = processed_data[,i],y=processed_data[,16]))
}


# Tsne
set.seed(142)
# Setting markers for replacing with tsne
processed_data <- processed_data %>%
  mutate(ID=row_number())

# Conducting tsne
tsne_fit <- processed_data %>%
  column_to_rownames("ID") %>%
  Rtsne()

# Extracting componenets and saving it
tsne_df <- tsne_fit$Y %>%
  as.data.frame() %>%
  rename(tSNE1="V1",
         tSNE2 = "V2") %>%
  mutate(ID=row_number())

# Using rownumbers to join back the original data
tsne_df <- tsne_df %>%
  inner_join(processed_data,by="ID")
head(tsne_df)


# Creating plots
tsne_df %>%
  ggplot(aes(x = tSNE1,
         y = tSNE2,
         shape = as.factor(Outcome),
         color = as.factor(Outcome)
         )) +
   geom_point()+
  theme(legend.position = 'bottom')





labels <- processed_data$Outcome
processed_data$Outcome <- as.factor(processed_data$Outcome)
colors = rainbow(length(unique(processed_data$Outcome)))
names(colors) = unique(processed_data$Outcome)

tsne <- Rtsne(processed_data[,-1], dims = 2, perplexity=30, verbose=TRUE, max_iter = 500)
exeTimeTsne<- system.time(Rtsne(processed_data[,-1], dims = 2, perplexity=30, verbose=TRUE, max_iter = 500))

plot(tsne$Y,t='n',main = 'tsne')
text(tsne$Y,labels = processed_data$Outcome,col = colors[processed_data$Outcome])
