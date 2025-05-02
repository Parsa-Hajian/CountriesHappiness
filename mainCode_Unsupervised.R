
# ── 0) SETUP ------------------------------------------------------------------
# Change this to your local path
setwd("C:/Users/ASUS/Desktop/Term 2/Statistical Learning/Unsupervised Learning/Datasets")

# Load libraries
libs <- c(
  "readr","dplyr","tidyr","ggplot2","GGally","e1071","robustbase",
  "cluster","factoextra","ggrepel","ggforce","purrr","gridExtra","scales",
  "rnaturalearth","rnaturalearthdata","viridis","RColorBrewer"
)
invisible(lapply(libs, library, character.only=TRUE))

# ── 1) LOAD & INSPECT (2015–2019) --------------------------------------------
read_inspect <- function(f) {
  df <- read_csv(f)
  cat("\n---", f, "---\n")
  str(df); print(summary(df)); print(head(df,3))
  invisible(df)
}
data_2015 <- read_inspect("2015.csv")
data_2016 <- read_inspect("2016.csv")
data_2017 <- read_inspect("2017.csv")
data_2018 <- read_inspect("2018.csv")
data_2019 <- read_inspect("2019.csv")

# ── 2) HARMONIZE & MERGE -----------------------------------------------------
harmonize <- function(df, year) {
  if (year <= 2017) {
    df %>%
      rename(
        Score                  = matches("Happiness Score|Happiness.Score"),
        GDP_per_capita         = matches("Economy \\(GDP per Capita\\)|Economy..GDP.per.Capita."),
        Social_support         = Family,
        Healthy_life_expectancy= matches("Health \\(Life Expectancy\\)|Health..Life.Expectancy."),
        Corruption             = matches("Trust \\(Government Corruption\\)|Trust..Government.Corruption.")
      ) %>%
      mutate(Year = year) %>%
      select(Country, Year, Score, GDP_per_capita, Social_support,
             Healthy_life_expectancy, Freedom, Generosity, Corruption)
  } else {
    df %>%
      rename(
        Country                  = `Country or region`,
        Score                    = Score,
        GDP_per_capita           = `GDP per capita`,
        Social_support           = `Social support`,
        Healthy_life_expectancy  = `Healthy life expectancy`,
        Freedom                  = `Freedom to make life choices`,
        Corruption               = `Perceptions of corruption`
      ) %>%
      mutate(
        Year = year,
        Corruption = as.numeric(Corruption)
      ) %>%
      select(Country, Year, Score, GDP_per_capita, Social_support,
             Healthy_life_expectancy, Freedom, Generosity, Corruption)
  }
}

d2015 <- harmonize(data_2015, 2015)
d2016 <- harmonize(data_2016, 2016)
d2017 <- harmonize(data_2017, 2017)
d2018 <- harmonize(data_2018, 2018)
d2019 <- harmonize(data_2019, 2019)

merged_data <- bind_rows(d2015, d2016, d2017, d2018, d2019)
rm(data_2015, data_2016, data_2017, data_2018, data_2019,
   d2015, d2016, d2017, d2018, d2019)

# Inspect
str(merged_data); summary(merged_data); head(merged_data,3)

# ── 3) EDA: HISTOGRAMS & BOXPLOTS & CORRELATION ------------------------------
vars <- c("Score","GDP_per_capita","Social_support",
          "Healthy_life_expectancy","Freedom","Generosity","Corruption")

fills_hist <- c(
  Score                  = "skyblue",
  GDP_per_capita         = "lightgreen",
  Social_support         = "orange",
  Healthy_life_expectancy= "pink",
  Freedom                = "purple",
  Generosity             = "cyan",
  Corruption             = "yellow"
)

# Histograms & Boxplots
for (v in vars) {
  p_hist <- ggplot(merged_data, aes(x = .data[[v]])) +
    geom_histogram(bins = 30, fill = fills_hist[v], color = "black") +
    theme_minimal() +
    labs(title = paste("Histogram of", v), x = v, y = "Frequency")
  print(p_hist)
  
  p_box <- ggplot(merged_data, aes(y = .data[[v]])) +
    geom_boxplot(fill = fills_hist[v]) +
    theme_minimal() +
    labs(title = paste("Boxplot of", v), y = v)
  print(p_box)
}

# Correlation matrix (abbreviated names)
num_short <- merged_data %>%
  select(all_of(vars)) %>%
  rename(
    S   = Score,
    GDP = GDP_per_capita,
    SS  = Social_support,
    HL  = Healthy_life_expectancy,
    F   = Freedom,
    G   = Generosity,
    C   = Corruption
  )
p_corr <- ggcorr(num_short, label = TRUE, label_round = 2) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
  labs(title = "Correlation Matrix (Abbrev.)")
print(p_corr)

# ── 4) TIME TRENDS -----------------------------------------------------------
# 4A) One series per variable
for (v in vars) {
  df_tr <- merged_data %>%
    group_by(Year) %>%
    summarise(Mean = mean(.data[[v]], na.rm = TRUE), .groups="drop")
  p_tr <- ggplot(df_tr, aes(x = Year, y = Mean)) +
    geom_line(color = fills_hist[v]) +
    geom_point(color = fills_hist[v]) +
    theme_minimal() +
    labs(title = paste("Time Trend:", v),
         x = "Year", y = paste("Average", v))
  print(p_tr)
}

# 4B) Facetted medians & distributions
trend_long <- merged_data %>%
  pivot_longer(all_of(vars), names_to = "Var", values_to = "Val")
med_trend <- trend_long %>%
  group_by(Year, Var) %>%
  summarise(Med = median(Val, na.rm = TRUE), .groups="drop")

p_medtrend <- ggplot(med_trend, aes(x = Year, y = Med)) +
  geom_line() + geom_point() +
  facet_wrap(~Var, scales="free_y") +
  theme_minimal() +
  labs(title="Yearly Median Trend")
print(p_medtrend)

p_yearbox <- ggplot(trend_long, aes(x = factor(Year), y = Val)) +
  geom_boxplot(fill = "#99CCFF") +
  stat_summary(fun = median, geom = "line", aes(group=1),
               colour = "blue", size = 1) +
  stat_summary(fun = median, geom = "point",
               colour = "blue", size = 2) +
  facet_wrap(~Var, scales="free_y", ncol=2) +
  theme_minimal(base_size=13) +
  labs(title="Yearly Distributions & Median")
print(p_yearbox)

# ── 5) MISSING VALUES & IMPUTATION ------------------------------------------
clean_data <- merged_data %>%
  mutate(across(all_of(vars),
                ~ ifelse(is.na(.x), median(.x, na.rm=TRUE), .x)))
print(colSums(is.na(clean_data)))  # should all be zero

# ── 6) SKEWNESS CHECK & LOG TRANSFORM ----------------------------------------
skews       <- clean_data %>%
  summarise(across(all_of(vars), ~ skewness(.x, na.rm=TRUE)))
skewed_vars <- names(which(abs(skews[1,]) > 1))
if (length(skewed_vars) > 0) {
  clean_data <- clean_data %>%
    mutate(across(all_of(skewed_vars), log1p, .names="log_{.col}"))
  new_skews <- clean_data %>%
    summarise(across(starts_with("log_"), ~ skewness(.x, na.rm=TRUE)))
  print(new_skews)
} else {
  message("No highly skewed variables—skipping log transform.")
}

# ── 7) OUTLIER DETECTION -----------------------------------------------------
# 7A) Compute fences
out_stats_long <- clean_data %>%
  summarise(across(all_of(vars),
                   list(Q1  = ~quantile(.x, 0.25, na.rm=TRUE),
                        Q3  = ~quantile(.x, 0.75, na.rm=TRUE),
                        IQR = ~IQR(.x, na.rm=TRUE)),
                   .names = "{.col}_{.fn}"
  )) %>%
  pivot_longer(
    cols          = everything(),
    names_to      = c("Var","Stat"),
    names_pattern = "(.+)_(Q1|Q3|IQR)",
    values_to     = "Value"
  ) %>%
  pivot_wider(names_from = Stat, values_from = Value) %>%
  mutate(
    Lower = Q1 - 1.5 * IQR,
    Upper = Q3 + 1.5 * IQR
  )
print(out_stats_long)

# 7B) Flag outliers
flagged <- clean_data %>%
  pivot_longer(all_of(vars), names_to="Var", values_to="Val") %>%
  left_join(out_stats_long, by="Var") %>%
  filter(Val < Lower | Val > Upper)
print(flagged %>% count(Var))
print(head(flagged,20))

# ── 8) OUTLIER VISUALIZATION ------------------------------------------------
# 8A) Box + jitter for Corruption & Generosity
p_outlier_box <- clean_data %>%
  select(Country, Year, Generosity, Corruption) %>%
  pivot_longer(c(Generosity, Corruption),
               names_to="Var", values_to="Val") %>%
  ggplot(aes(x=Var, y=Val)) +
  geom_boxplot(outlier.shape=NA, fill="lightgrey") +
  geom_jitter(width=0.15, alpha=0.6) +
  theme_minimal() +
  labs(title="Outliers: Generosity & Corruption")
print(p_outlier_box)

# 8B) Scatter, highlight flagged points
plot_data <- clean_data %>%
  left_join(
    flagged %>%
      filter(Var %in% c("Generosity","Corruption")) %>%
      distinct(Country, Year) %>%
      mutate(IsOutlier=TRUE),
    by = c("Country","Year")
  ) %>%
  mutate(IsOutlier = replace_na(IsOutlier, FALSE))

p_outlier_scatter <- ggplot(plot_data, aes(x=Generosity, y=Corruption, color=IsOutlier)) +
  geom_point(alpha=0.7, size=2) +
  scale_color_manual(values = c("FALSE"="steelblue", "TRUE"="red")) +
  theme_minimal() +
  labs(
    title    = "Scatter: Corruption vs Generosity",
    subtitle = "Red = values outside 1.5 × IQR fences"
  )
print(p_outlier_scatter)

# ── 8) ROBUST SCALING --------------------------------------------------------
robust_scaled <- clean_data %>%
  mutate(across(all_of(vars),
                ~ (.x - median(.x,na.rm=TRUE))/mad(.x,na.rm=TRUE),
                .names="R_{.col}"))

p_dens <- robust_scaled %>%
  select(starts_with("R_")) %>%
  rename_with(~sub("^R_","",.x)) %>%
  pivot_longer(everything(), names_to="Var", values_to="Val") %>%
  mutate(Stage="RobustScaled") %>%
  bind_rows(
    clean_data %>% select(all_of(vars)) %>%
      pivot_longer(everything(), names_to="Var", values_to="Val") %>%
      mutate(Stage="Original")
  ) %>%
  ggplot(aes(Val,fill=Stage)) +
  geom_density(alpha=0.45) +
  facet_wrap(~Var, scales="free") +
  scale_fill_manual(values=c("Original"="#1f77b4","RobustScaled"="#ff7f0e")) +
  theme_minimal() +
  labs(title="Density: Original vs Robust-Scaled")
print(p_dens)

p_box_both <- robust_scaled %>%
  select(starts_with("R_")) %>%
  rename_with(~sub("^R_","",.x)) %>%
  pivot_longer(everything(), names_to="Var", values_to="Val") %>%
  mutate(Stage="RobustScaled") %>%
  bind_rows(
    clean_data %>% select(all_of(vars)) %>%
      pivot_longer(everything(), names_to="Var", values_to="Val") %>%
      mutate(Stage="Original")
  ) %>%
  ggplot(aes(Stage,Val,fill=Stage)) +
  geom_boxplot() +
  facet_wrap(~Var, scales="free_y") +
  scale_fill_manual(values=c("Original"="#1f77b4","RobustScaled"="#ff7f0e")) +
  theme_minimal() +
  theme(axis.text.x=element_text(angle=45,hjust=1)) +
  labs(title="Boxplot: Original vs Robust-Scaled")
print(p_box_both)

# ── 9) ROBUST MDS ------------------------------------------------------------
X <- robust_scaled %>% select(starts_with("R_")) %>% as.matrix()
mcd <- covMcd(X,alpha=0.75)
S_inv <- solve(mcd$cov)
Xw    <- sweep(X,2,mcd$center,"-") %*% chol(S_inv)
mds  <- cmdscale(dist(Xw),k=2)
mds_df <- robust_scaled %>%
  select(Country,Year) %>%
  mutate(MDS1=mds[,1], MDS2=mds[,2])

# Panel with hull
hull_cut <- quantile(sqrt(mds_df$MDS1^2+mds_df$MDS2^2),0.95)
hull_df  <- mds_df %>% filter(sqrt(MDS1^2+MDS2^2)>=hull_cut)
p_panel <- ggplot(mds_df,aes(MDS1,MDS2,colour=factor(Year))) +
  geom_point(alpha=0.55,size=2) +
  geom_mark_hull(data=hull_df,concavity=8,fill=NA,
                 colour="grey50",linetype="dashed",size=0.3) +
  scale_colour_brewer(palette="Set1",name="Year") +
  theme_minimal(base_size=12) +
  theme(legend.position="bottom") +
  labs(
    title="Classical MDS – Panel Evolution (2015–2019)",
    subtitle="dots=country-year · dashed hull=top 5% distant",
    x="MDS Dim 1",y="MDS Dim 2"
  )
print(p_panel)

# Facet by year
p_facet <- ggplot(mds_df,aes(MDS1,MDS2)) +
  geom_point(alpha=0.6,colour="#2c7fb8",size=2) +
  facet_wrap(~Year,ncol=3) +
  theme_minimal(base_size=12) +
  labs(
    title="Classical MDS by Year (Robust Distances)",
    x="MDS Dim 1",y="MDS Dim 2"
  )
print(p_facet)

# ──10) SCORE-COLOURED HIGHLIGHTS --------------------------------------------
mds_scored <- mds_df %>%
  left_join(clean_data %>% select(Country,Year,Score), by=c("Country","Year"))
labels_df <- mds_scored %>%
  group_by(Year) %>%
  arrange(desc(Score)) %>%
  mutate(rank=row_number(), total=n()) %>%
  filter(rank<=10 | rank>(total-10)) %>%
  ungroup()
mid_score <- median(mds_scored$Score,na.rm=TRUE)

p_highlight <- ggplot() +
  geom_point(data=mds_scored,
             aes(MDS1,MDS2,color=Score),
             size=1.8,alpha=0.4) +
  geom_point(data=labels_df,
             aes(MDS1,MDS2,fill=Score),
             shape=21,color="black",size=3,stroke=0.8) +
  geom_text_repel(data=labels_df,
                  aes(MDS1,MDS2,label=Country),
                  size=2.8,segment.size=0,max.overlaps=20) +
  scale_color_gradient2(
    low="red", mid="yellow", high="green",
    midpoint=mid_score, name="Score"
  ) +
  scale_fill_gradient2(
    low="red", mid="yellow", high="green",
    midpoint=mid_score, guide="none"
  ) +
  facet_wrap(~Year) +
  theme_minimal(base_size=12) +
  labs(
    title="MDS Over Time: Top/Bottom 10 Highlighted",
    x="MDS Dim 1", y="MDS Dim 2"
  )
print(p_highlight)

# ──11) ARROW TRAJECTORIES 2015→2019 ------------------------------------------
top5 <- mds_scored %>%
  filter(Year==2015) %>%
  slice_max(n=5, order_by=Score) %>%
  mutate(group="Best")
bot5 <- mds_scored %>%
  filter(Year==2015) %>%
  slice_min(n=5, order_by=Score) %>%
  mutate(group="Worst")
highlight <- bind_rows(top5, bot5) %>% select(Country,group)

segments_df <- mds_scored %>%
  inner_join(highlight, by="Country") %>%
  arrange(Country,Year) %>%
  group_by(Country) %>%
  mutate(
    xend = lead(MDS1),
    yend = lead(MDS2)
  ) %>%
  ungroup() %>%
  filter(!is.na(xend))

p_arrows <- ggplot() +
  geom_point(data=mds_scored, aes(MDS1,MDS2),
             color="grey80", size=1.2) +
  geom_segment(data=segments_df,
               aes(x=MDS1,y=MDS2,xend=xend,yend=yend,color=group),
               arrow=arrow(length=unit(0.3,"cm")), size=0.8) +
  geom_text(data=highlight %>% 
              left_join(mds_scored %>% filter(Year==2015),
                        by="Country"),
            aes(x=MDS1,y=MDS2,label=Country),
            size=3) +
  scale_color_manual(values=c("Best"="darkgreen","Worst"="darkred"),
                     guide=guide_legend(title="Group")) +
  theme_minimal(base_size=12) +
  labs(
    title="Trajectories (2015→2019) for Top-5 & Bottom-5 Countries",
    x="MDS Dim 1", y="MDS Dim 2"
  )
print(p_arrows)

# ──12) k-MEANS CLUSTERING & EVALUATION --------------------------------------
Xmat <- robust_scaled %>% select(starts_with("R_")) %>% as.matrix()

# Elbow & Silhouette
wcss <- map_dbl(1:10, ~ kmeans(Xmat,centers=.,nstart=25)$tot.withinss)
sil  <- map_dbl(2:10, ~ mean(silhouette(kmeans(Xmat,centers=.,nstart=25)$cluster, dist(Xmat))[,3]))

p_elbow <- tibble(K=1:10,WCSS=wcss) %>%
  ggplot(aes(K,WCSS)) +
  geom_line() + geom_point() +
  labs(title="Elbow: k vs WCSS") +
  theme_minimal()
p_sil   <- tibble(K=2:10,Sil=sil) %>%
  ggplot(aes(K,Sil)) +
  geom_line() + geom_point() +
  labs(title="Silhouette: k vs Avg Sil") +
  theme_minimal()

grid.arrange(p_elbow,p_sil,ncol=2)

# Final k=3
set.seed(123)
k3 <- kmeans(Xmat,centers=3,nstart=50)
clustered <- robust_scaled %>%
  mutate(cluster=factor(k3$cluster))

# MDS map by cluster
p_k3map <- mds_df %>%
  left_join(clustered %>% select(Country,Year,cluster),
            by=c("Country","Year")) %>%
  ggplot(aes(MDS1,MDS2,color=cluster)) +
  geom_point(alpha=0.7,size=2) +
  facet_wrap(~Year) +
  scale_color_brewer(palette="Set1", name="Cluster") +
  theme_minimal(base_size=12) +
  labs(
    title="MDS Over Time, Colored by k=3 Clusters",
    x="MDS Dim 1", y="MDS Dim 2"
  )
print(p_k3map)

# Cluster profiles (k=3)
vars <- c("Score","GDP_per_capita","Social_support",
          "Healthy_life_expectancy","Freedom","Generosity","Corruption")

profiles3 <- clustered %>%
  group_by(cluster) %>%
  summarise(
    n = n(),
    across(
      all_of(vars),
      list(
        mean = ~ mean(.x, na.rm = TRUE),
        med  = ~ median(.x, na.rm = TRUE)
      ),
      .names = "{.col}_{.fn}"
    )
  )

# Barplot profiles
profile_long <- profiles3 %>%
  select(cluster, ends_with("_mean")) %>%
  pivot_longer(-cluster,names_to="Indicator",values_to="Mean") %>%
  mutate(Indicator=sub("_mean$","",Indicator))

p_profiles <- ggplot(profile_long,
                     aes(x=Indicator,y=Mean,fill=cluster)) +
  geom_col(position="dodge") +
  scale_fill_brewer(palette="Set1",name="Cluster",
                    labels=c("1","2","3")) +
  theme_minimal(base_size=12) +
  theme(axis.text.x=element_text(angle=45,hjust=1)) +
  labs(title="Cluster Profiles (k=3)")
print(p_profiles)

# Yearly mix
yearly_mix <- clustered %>%
  count(Year,cluster) %>%
  group_by(Year) %>%
  mutate(pct=n/sum(n))

p_yearmix <- ggplot(yearly_mix,aes(Year,pct,color=cluster)) +
  geom_line(size=1.2) +
  scale_color_brewer(palette="Set1",name="Cluster") +
  scale_y_continuous(labels=percent_format()) +
  theme_minimal(base_size=12) +
  labs(
    title="Proportion of Country-Years by Cluster (2015–2019)",
    y="Percent", x="Year"
  )
print(p_yearmix)

# Silhouette k=3
sil3 <- silhouette(k3$cluster, dist(Xmat))
cat("Avg sil width (k=3):", round(mean(sil3[,3]),3), "\n")
p_sil3 <- fviz_silhouette(sil3,
                          palette=brewer.pal(3,"Set1"),
                          label=FALSE) +
  geom_vline(xintercept=mean(sil3[,3]),linetype="dashed") +
  theme_minimal(base_size=12) +
  labs(title="Silhouette Plot (k=3)")
print(p_sil3)

# ──13) MAP k=3 CLUSTERS FOR EVERY YEAR ────────────────────────────────────────────

library(sf)
library(rnaturalearth)
library(dplyr)
library(ggplot2)
library(RColorBrewer)

# 1) pull in the world map as an sf
world <- ne_countries(scale = "medium", returnclass = "sf") %>%
  select(name_long, geometry)

# 2) assemble your country–year–cluster table
clusters_all <- clustered %>%
  select(Country, Year, cluster)

# 3) join onto the map (repeats geometry for each year)
map_all_years <- clusters_all %>%
  left_join(world, by = c("Country" = "name_long")) %>%
  st_as_sf()

# 4) plot small multiples
ggplot(map_all_years) +
  geom_sf(aes(fill = factor(cluster)), colour = "white", size = 0.1) +
  scale_fill_brewer(
    palette = "Set1",
    na.value = "grey90",
    name     = "Cluster\n(k=3)"
  ) +
  facet_wrap(~ Year, ncol = 3) +
  theme_minimal(base_size = 13) +
  labs(
    title    = "World‐wide Happiness Clusters (2015–2019)",
    subtitle = "k=3 k-means on robust-scaled indicators",
    caption  = "Source: World Happiness Report"
  ) +
  theme(
    panel.grid.major = element_blank(),
    axis.text        = element_blank(),
    axis.ticks       = element_blank()
  )


# ──14) ────────────────────── Other clusterings ────────────────────────────────────────────
# ── A) HIERARCHICAL CLUSTERING (Ward’s method, k = 3) ────────────────────────────
# (A1) Prepare data
Xmat   <- robust_scaled %>% select(starts_with("R_")) %>% as.matrix()
base_df <- robust_scaled %>% select(Country, Year)

# (A2) Build tree & cut
hc     <- hclust(dist(Xmat), method = "ward.D2")
hc_cut <- cutree(hc, k = 3)
hc_df  <- base_df %>% mutate(hc3 = factor(hc_cut))

# (A3) Plot dendrogram (optional)
# A) Build the feature matrix (robust‐scaled)
Xmat <- robust_scaled %>%
  select(starts_with("R_")) %>%
  as.matrix()

# B) Compute the tree
hc <- hclust(dist(Xmat), method = "ward.D2")

# C) Plot with no labels and non‐collapsing leaves
par(mar = c(5,4,2,1) + 0.1)         # tighten margins
plot(
  hc,
  labels = FALSE,                  # no clutter of 780 labels
  hang   = -1,                     # force all leaves to bottom
  main   = "Hierarchical Clustering Dendrogram\n(Ward’s Method)",
  ylab   = "Height"
)

# D) Highlight the 3 clusters
rect.hclust(hc, k = 3, border = c("red","blue","darkgreen"))
# (A4) Silhouette analysis for k = 3
library(cluster)
hc_sil <- silhouette(hc_cut, dist(Xmat))
cat("Hierarchical (k = 3) avg. silhouette:",
    round(mean(hc_sil[, "sil_width"]), 3), "\n")

# (A5) Silhouette plot
library(factoextra); library(RColorBrewer)
fviz_silhouette(hc_sil,
                palette = brewer.pal(3, "Set1"),
                label   = FALSE) +
  geom_vline(xintercept = mean(hc_sil[, "sil_width"]),
             linetype      = "dashed") +
  theme_minimal(base_size = 12) +
  labs(title = "Silhouette Plot – Ward Hierarchical Clustering (k = 3)")



# ── B) DBSCAN (ε = 1.0, minPts = 5) ─────────────────────────────────────────────
# (B1) Find eps via 5-NN plot
library(dbscan)
kNNdistplot(Xmat, k = 5)
abline(h = 1.0, col = "red", lty = 2)

# (B2) Run DBSCAN
db <- dbscan(Xmat, eps = 1.0, minPts = 5)
db_df <- base_df %>%
  mutate(db = factor(db$cluster, levels = sort(unique(db$cluster))))

# (B3) Silhouette for core points only
dmat     <- dist(Xmat)
dmat_mat <- as.matrix(dmat)
core_idx <- which(db$cluster != 0)
si_db    <- silhouette(db$cluster[core_idx],
                       dmat_mat[core_idx, core_idx])
cat("DBSCAN avg. silhouette (core pts):",
    round(mean(si_db[, "sil_width"]), 3), "\n")

# (B4) Silhouette plot for DBSCAN
fviz_silhouette(si_db,
                palette = brewer.pal(length(unique(db$cluster[core_idx])), "Set1"),
                label   = FALSE) +
  geom_vline(xintercept = mean(si_db[, "sil_width"]), linetype = "dashed") +
  theme_minimal(base_size = 12) +
  labs(title = "Silhouette Plot – DBSCAN (ε = 1.0, minPts = 5)")



# ── C) MDS VISUALIZATIONS WITH CLUSTER LABELS ──────────────────────────────────
# (C1) Join HC & DBSCAN labels to MDS coords
mds_hc <- mds_df %>% left_join(hc_df, by = c("Country", "Year"))
mds_db <- mds_df %>% left_join(db_df, by = c("Country", "Year"))

# (C2) Plot MDS colored by hierarchical clusters
library(ggplot2)
p_hc <- ggplot(mds_hc, aes(MDS1, MDS2, color = hc3)) +
  geom_point(size = 2, alpha = 0.7) +
  scale_color_brewer("HC Clust.", palette = "Dark2") +
  facet_wrap(~Year) +
  theme_minimal() +
  labs(title = "MDS Map – Hierarchical Clusters (k = 3)")
print(p_hc)

# (C3) Plot MDS colored by DBSCAN clusters
library(viridis)
p_db <- ggplot(mds_db, aes(MDS1, MDS2, color = db)) +
  geom_point(size = 2, alpha = 0.7) +
  scale_color_viridis_d("DBSCAN\ncluster") +
  facet_wrap(~Year) +
  theme_minimal() +
  labs(title = "MDS Map – DBSCAN Clusters (ε = 1.0, minPts = 5)")
print(p_db)



# ── D) CLUSTER PROFILES ────────────────────────────────────────────────────────
# (D1) Hierarchical cluster profiles
profiles_hc <- hc_df %>%
  left_join(clean_data, by = c("Country", "Year")) %>%
  group_by(hc3) %>%
  summarise(across(all_of(vars),
                   mean, .names = "{.col}_mean"),
            n = n(), .groups = "drop")
print(profiles_hc)

# (D2) DBSCAN cluster profiles (exclude noise, db = 0)
profiles_db <- db_df %>%
  filter(db != 0) %>%
  left_join(clean_data, by = c("Country", "Year")) %>%
  group_by(db) %>%
  summarise(across(all_of(vars),
                   mean, .names = "{.col}_mean"),
            n = n(), .groups = "drop")
print(profiles_db)


# ── 15) Dimensionality Reduction for Visualization (t-SNE & UMAP) ───────────────────────────────────

# 1) Extract feature matrix
Xmat <- robust_scaled %>% select(starts_with("R_")) %>% as.matrix()

# 2) t-SNE
if (!requireNamespace("Rtsne", quietly=TRUE)) install.packages("Rtsne")
library(Rtsne)
set.seed(123)
tsne_out <- Rtsne(Xmat, dims = 2, perplexity = 30, verbose = TRUE, check_duplicates = FALSE)$Y

tsne_df <- robust_scaled %>%
  select(Country, Year) %>%
  mutate(
    TSNE1   = tsne_out[,1],
    TSNE2   = tsne_out[,2],
    kmeans3 = factor(k3$cluster),
    hc3     = factor(hc_cut)
  )

library(ggplot2)
# 2A) t-SNE colored by k-means
ggplot(tsne_df, aes(TSNE1, TSNE2, color = kmeans3)) +
  geom_point(size = 2, alpha = 0.7) +
  scale_color_brewer("k-means (k=3)", palette = "Set1") +
  theme_minimal() +
  labs(title = "t-SNE Map – k-means Clusters (k = 3)")

# 2B) t-SNE colored by hierarchical
ggplot(tsne_df, aes(TSNE1, TSNE2, color = hc3)) +
  geom_point(size = 2, alpha = 0.7) +
  scale_color_brewer("HC Clust. (k=3)", palette = "Dark2") +
  theme_minimal() +
  labs(title = "t-SNE Map – Ward’s Hierarchical Clusters (k = 3)")

# 3) UMAP
if (!requireNamespace("uwot", quietly=TRUE)) install.packages("uwot")
library(uwot)
set.seed(123)
umap_out <- umap(Xmat, n_neighbors = 15, min_dist = 0.1, verbose = TRUE)

umap_df <- robust_scaled %>%
  select(Country, Year) %>%
  mutate(
    UMAP1   = umap_out[,1],
    UMAP2   = umap_out[,2],
    kmeans3 = factor(k3$cluster),
    hc3     = factor(hc_cut)
  )

# 3A) UMAP colored by k-means
ggplot(umap_df, aes(UMAP1, UMAP2, color = kmeans3)) +
  geom_point(size = 2, alpha = 0.7) +
  scale_color_brewer("k-means (k=3)", palette = "Set1") +
  theme_minimal() +
  labs(title = "UMAP Map – k-means Clusters (k = 3)")

# 3B) UMAP colored by hierarchical
ggplot(umap_df, aes(UMAP1, UMAP2, color = hc3)) +
  geom_point(size = 2, alpha = 0.7) +
  scale_color_brewer("HC Clust. (k=3)", palette = "Dark2") +
  theme_minimal() +
  labs(title = "UMAP Map – Ward’s Hierarchical Clusters (k = 3)")

