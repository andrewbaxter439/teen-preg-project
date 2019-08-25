library(dplyr)
library(ggplot2)
library(SPHSUgraphs)

df <- synthData %>% 
  filter(Year == 2010, agegrp == "Under 18", !is.na(GDPperCap)) %>% 
  select(Country, GDPperCap, rate)

df %>% 
  mutate(cluster = kmeans(scale(df[,2:3]), 4)$cluster) %>% 
  ggplot(aes(GDPperCap, rate, col = factor(cluster))) +
  geom_text(aes(label = Country))


kmeans_table <- c()
for (i in 1:15){
kmeans_table[i] <- kmeans(matrix_df, i)$tot.withinss
}

tibble(kmeans_table, k = 1:15) %>% 
  ggplot(aes(k, kmeans_table)) +
  geom_point() +
  geom_line()

matrix_df <- scale(as.matrix(df[,2:3]))
row.names(matrix_df) <- df$Country

hclust.out <- hclust(dist(matrix_df))
plot(hclust.out)


# Under 18 ---------------------------------------------------------------------------------------------------

df1 <- synthData_U20 %>% 
  filter(Year == 2010, Country != "New Zealand") %>%
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

hclust.out1 <- hclust(dist(mt1))


df1 %>% 
  mutate(cluster1 = factor(kmeans(scale(as.matrix(df1[,2:3])), 4)$cluster),
         cluster2 = factor(cutree(hclust.out1, k = 4))) %>% 
  ggplot(aes(GDPperCap, pRate, col = cluster2, label = Country)) +
  geom_text()


plot(hclust.out1)
