library(dplyr)
library(ggplot2)
library(SPHSUgraphs)
load("Data/synth_data.rdata")
library(ggrepel)

# Under-18 birth rates ---------------------------------------------------------------------------------------


# ** dataprep ------------------------------------------------------------------------------------------------

df <- synthData_u18 %>% 
  filter(Year == 1998,!is.na(GDPperCap)) %>% 
  select(Country, GDPperCap, rate)


matrix_df <- scale(as.matrix(df[,2:3]))
row.names(matrix_df) <- df$Country


# ** k-means clustering --------------------------------------------------------------------------------------



kmeans_table <- c()
for (i in 1:15){
kmeans_table[i] <- kmeans(matrix_df, i)$tot.withinss
}

tibble(kmeans_table, k = 1:15) %>% 
  ggplot(aes(k, kmeans_table)) +
  geom_point() +
  geom_line()



df %>% 
  mutate(cluster = kmeans(scale(df[,2:3]), 4)$cluster) %>% 
  ggplot(aes(GDPperCap, rate, col = factor(cluster))) +
  geom_text_repel(aes(label = Country), size = 8) +
  theme_sphsu_light() +
  theme(panel.grid = element_blank(),
        panel.background = element_blank(),
        legend.position = "none")

# ** Hierarchical clustering ---------------------------------------------------------------------------------


hclust.out <- hclust(dist(matrix_df))
plot(hclust.out)

# Under 20 pregnancy rates  ----------------------------------------------------------------------------------


# ** k-means clustering ---------------------------------------------------------------------------------


df1 <- synthData_u20 %>% 
  filter(Year == 1998, Country != "New Zealand") %>%
  select(Country, GDPperCap, pRate)

df1[which(df1$Country %in% c("Scotland", "England and Wales")),"GDPperCap"] <- synthData %>% filter(Country == "United Kingdom", Year == 2010) %>% select(GDPperCap) %>% unique() %>% pull()

mt1 <- scale(as.matrix(df1[,2:3]))
rownames(mt1) <- df1$Country

mt1_ks <- c()

for (i in 1:15) {
  mt1_ks[i] <- kmeans(mt1, i)$tot.withinss
}

tibble(clusters = 1:15, wss = mt1_ks) %>% 
  ggplot(aes(clusters, wss)) +
  geom_point() +
  geom_line()



df1 %>% 
  mutate(cluster1 = factor(kmeans(scale(as.matrix(df1[,2:3])), 4)$cluster),
         cluster2 = factor(cutree(hclust.out1, k = 4))) %>% 
  ggplot(aes(GDPperCap, pRate, col = cluster2, label = Country)) +
  geom_text_repel(aes(label = Country), size = 8) +
  theme_sphsu_light() +
  theme(panel.grid = element_blank(),
        panel.background = element_blank(),
        legend.position = "none")


# ** Hierarchical clustering ---------------------------------------------------------------------------------


hclust.out1 <- hclust(dist(mt1))
plot(hclust.out1)



# Clustering by all predictors -------------------------------------------------------------------------------


# Under-18 ---------------------------------------------------------------------------------------------------

synthData_u18 %>% 
  filter(Year<1999) %>% 
  mutate(period = ifelse(Year<1988, 1,
                                ifelse(Year<1990, 2,
                                       ifelse(Year<1993, 3, 4)))) %>% 
  group_by(Country) %>% 
  mutate_at(vars(5:8), mean) %>% 
  group_by(Country, period) %>% 
  summarise_all(mean) %>% 
  filter_all(!anyNA(.))
