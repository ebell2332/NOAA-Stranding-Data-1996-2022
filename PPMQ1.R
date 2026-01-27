library(spatstat.geom)
#library(spatstat.core)
library(spatstat.explore)
library(purrr)
library(spatstat.model)


q1 <- read_excel("/Users/ebell23/Downloads/strand_q1_allyrs.xlsx")



colnames(q1)
####Question 1 Set Up####
#make points

year_col <- "Year_num"

#add study area
library(sf)
Sys.setenv(SHAPE_RESTORE_SHX = "YES")
study <- st_read("/Users/ebell23/Downloads/Study_Area1_ExportFeatures.shp", quiet = TRUE) 
st_crs(study) #check coordinate system
st_bbox(study) #check geometry of study area


#check points and crs with study area
q1_sf <- st_as_sf(q1, coords = c("lon", "lat"), crs = 4326, remove = FALSE)
q1_sf <- st_transform(q1_sf, st_crs(study))

#filter points to window
inside <- lengths(st_intersects(q1_sf, study)) >0
q1_sf_in <- q1_sf[inside, ]
nrow(q1_sf_in) #make sure all points are fitting in the study window

#check column context
names(q1_sf_in)
"context_q1" %in% names(q1_sf_in) 
table(q1_sf_in$context_q1) 

#visual check of study area and q1 data frame
plot(st_geometry(study), border = "black")
plot(st_geometry(q1_sf_in), add = TRUE, pch = 16, cex = 0.2)

W <- as.owin(study)



#####create PPM for one year (e.x 2009)####

yr <- 2009
q1_yr <- q1_sf_in %>% filter(Year_num == yr)

q1_yr %>% st_drop_geometry() %>% count(context_q1, sort = TRUE)

#run separate ipp for each context/target group
fit_by_cnxt <- function(sf_obj, W, context_value) {
  sub <- sf_obj %>% dplyr::filter(context_q1 == context_value)
  
  if (nrow(sub) < 50) return(NULL)
  
  xy <- st_coordinates(sub)
  X <- ppp(xy[,1], xy[,2], window = W)
  
  ppm(X ~ x+y)
}

contexts <- sort(unique(q1_yr$context_q1))
fits <- setNames(lapply(contexts, \(ctx) fit_by_cnxt(q1_yr, W, ctx)), contexts)

sapply(fits, is.null)

#plot 
par(mfrow = c(2,2))
for (ctx in names(fits)) {
  if (is.null(fits[[ctx]])) next
  plot(predict(fits[[ctx]]), main = paste("Intensity:", ctx))
}
par(mfrow = c(1,1))


#results table for 2009 
results <- purrr::imap_dfr(fits, function(fit, ctx) {
  if (is.null(fit)) {
    return(tibble::tibble(context_q1 = ctx, n = NA_integer_, term = NA_character_, estimate = NA_real_))
  }
  co <- coef(fit)
  tibble::tibble(
    context_q1 = ctx,
    nrow(q1_yr %>% filter(context_q1==ctx)),
    term = names(co),
    estimate = as.numeric(co)
  )
})
results


##2. sample loop over 3 year time span (1996, 2010, 2022)####
#a. cv intensity (sd/mean) - helps understand if the patterns under different policy targets are becoming more or less concentrated over time

cv_intensity_ppm <- function(sf_data, W, year_val, context_val, min_n = 50) {
  
  sub <- sf_data %>%
    filter(Year_num == year_val,
           context_q1 == context_val)
  
  n <- nrow(sub)
  if (n < min_n) return(NULL)
  
  xy <- st_coordinates(sub)
  X  <- ppp(xy[,1], xy[,2], window = W)
  
  fit <- ppm(X ~ x + y)
  
  Z <- as.numeric(predict(fit))
  
  tibble(
    year = year_val,
    context_q1 = context_val,
    n = n,
    cv_intensity = sd(Z) / mean(Z)
  )
}
 #b. run for all 3 years
test_years <- c(1996, 2010, 2022)
contexts   <- sort(unique(q1_sf_in$context_q1))

test_results <- list()

for (yr in test_years) {
  for (ctx in contexts) {
    
    res <- cv_intensity_ppm(
      sf_data = q1_sf_in,
      W = W,
      year_val = yr,
      context_val = ctx,
      min_n = 50
    )
    
    if (!is.null(res)) {
      test_results[[length(test_results) + 1]] <- res
    }
  }
}

test_df <- bind_rows(test_results)
test_df

#test diagnostic plot of the 3 sample years with CV
ggplot(test_df,
       aes(x = year, y = cv_intensity,
           color = context_q1, group = context_q1)) +
  geom_line() +
  geom_point(size = 3) +
  labs(
    x = "Year",
    y = "CV of fitted intensity",
    color = "Policy context",
    title = "Spatial concentration of strandings by policy context"
  ) +
  theme_minimal()


ggplot(test_df, aes(x = n, y = cv_intensity, color = context_q1)) +
  geom_point(size = 3) +
  geom_smooth(method = "lm", se = FALSE) +
  labs(
    x = "Number of strandings",
    y = "CV of fitted intensity"
  ) +
  theme_minimal()



#####3. Run loop for entire study period####
cv_intensity_ppm <- function(sf_data, W, year_val, context_val, min_n = 50) {
  
  sub <- sf_data %>%
    filter(Year_num == year_val,
           context_q1 == context_val)
  
  n <- nrow(sub)
  if (n < min_n) return(NULL)
  
  xy <- st_coordinates(sub)
  X  <- ppp(xy[,1], xy[,2], window = W)
  
  fit <- ppm(X ~ x + y)
  
  Z <- as.numeric(predict(fit))
  tibble(
    year = year_val,
    context_q1 = context_val,
    n = n,
    cv_intensity = sd(Z) / mean(Z)
  )
}


years_all <- sort(unique(q1_sf_in$Year_num))
contexts  <- sort(unique(q1_sf_in$context_q1))

results <- vector("list", length = 0)

for (yr in years_all) {
  for (ctx in contexts) {
    
    res <- cv_intensity_ppm(
      sf_data = q1_sf_in,
      W = W,
      year_val = yr,
      context_val = ctx,
      min_n = 50
    )
    
    if (!is.null(res)) results[[length(results) + 1]] <- res
  }
}

cv_df <- bind_rows(results)
cv_df


#ensure that you have year and no context missing
cv_df %>% count(year) %>% arrange(year)
cv_df %>% count(context_q1) %>% arrange(desc(n))

cv_df %>% #double checking sample sizes
  group_by(context_q1) %>%
  summarise(
    years_modeled = n(),
    min_n = min(n),
    median_n = median(n),
    max_n = max(n),
    .groups = "drop"
  ) %>%
  arrange(context_q1)



#plot cv over time
cv_filt <- cv_df %>%
  filter(context_q1 %in% c("IndirectOnly", "DirectOnly")) %>%
  mutate(policy_category = ifelse(context_q1 == "DirectOnly", "Direct", "Indirect"))

ggplot(cv_filt, aes(x = year, y = cv_intensity, color = policy_category)) +
  geom_line() +
  geom_point(size = 1.8) +
  labs(
    x = "Year",
    y = "CV of spatial concentration",
    color = "Policy Target",
    title = "Yearly spatial concentration of strandings by policy target"
  ) +
  theme(
    plot.title = element_text(color = "black", size = 18),      # Title color and size
    axis.title.x = element_text(color = "black", size = 15),   # X-axis title
    axis.title.y = element_text(color = "black", size = 15),   # Y-axis title
    axis.text = element_text(size = 10), # Axis tick labels
    legend.position = "bottom",
    legend.title = element_text(size = 12,),
    legend.text = element_text(size = 10)) 

cv_filt
#plot with direct, indirect, and none
cv_filt_2 <- cv_df %>%
  filter(context_q1 %in% c("IndirectOnly", "DirectOnly", "None")) %>%
  mutate(policy_category = case_when(context_q1 == "DirectOnly" ~ "Direct", context_q1 == "IndirectOnly" ~ "Indirect", context_q1 == "None" ~ "None"))

ggplot(cv_filt_2, aes(x = year, y = cv_intensity, color = policy_category)) +
  geom_line() +
  geom_point(size = 1.8) +
  labs(
    x = "Year",
    y = "CV of spatial concentration",
    color = "Policy Target",
    title = "Yearly spatial concentration of strandings by policy target"
  ) +
  theme_minimal() #filtered line plot to only direct and indirect and none

cv_filt_2




ggplot(cv_df, aes(x = year, y = cv_intensity, color = context_q1)) +
  geom_line() +
  geom_point(size = 1.8) +
  labs(
    x = "Year",
    y = "CV of fitted intensity (inhomogeneous PPM)",
    color = "Policy Target",
    title = "Yearly spatial concentration of strandings by policy context"
  ) +
  theme_minimal() #regular line plot


#boxplot
ggplot(cv_df,
       aes(x = context_q1, y = cv_intensity, fill = context_q1)) +
  geom_boxplot(alpha = 0.7, outlier.shape = 21) +
  labs(
    x = "Policy context",
    y = "CV of fitted intensity",
    title = "Distribution of spatial concentration across years by policy context"
  ) +
  theme_minimal() +
  theme(legend.position = "none")



ggplot(cv_df, aes(x = year, y = cv_intensity, color = context_q1)) +
  geom_point(size = 1.2, alpha = 0.8) +
  geom_smooth(se = FALSE, method = "loess", span = 0.4) +
  labs(
    x = "Year",
    y = "CV of fitted intensity",
    color = "Policy Target",
    title = "Smoothed trends in spatial concentration by policy context"
  ) +
  theme_minimal() #smooth trend line

ggplot(cv_df, aes(x = n, y = cv_intensity, color = context_q1)) +
  geom_point(alpha = 0.8) +
  geom_smooth(se = FALSE, method = "lm") +
  labs(x = "Number of strandings used in fit", y = "CV of fitted intensity") +
  theme_minimal() #looking at cvs sample size

cv_mean <- cv_df %>% group_by(context_q1) %>% summarise(mean_cv = mean(cv_intensity), .groups="drop") #avg cv for each context across all years
cv_df %>% group_by(year) %>% arrange(year, context_q1) #cv by years with context

write.csv(cv_df, "q1_cv_yr_policy.csv", row.names = FALSE) #saved

#boxplot mean cv direct and indirect only
cv_filt_mn <- cv_filt %>% group_by(policy_category) %>% summarise(mean_cv = mean(cv_intensity), .groups="drop")

ggplot(cv_filt_mn, aes(x = policy_category, y = mean_cv)) +
  geom_col(fill = "black", width = 0.7) +
  labs(
    x = "Policy Category",
    y = "Avg CV of intensity",
    title = "Avg CV of intensity by policy category"
  ) +
  theme_minimal() +
  theme(
    axis.text.x = element_text(angle = 0, hjust = 1)
  )

#boxplot of direct, indirect, and none
cv_filt_2mn <- cv_filt_2 %>% group_by(policy_category) %>% summarise(mean_cv = mean(cv_intensity), .groups="drop")

ggplot(cv_filt_2mn, aes(x = policy_category, y = mean_cv)) +
  geom_col(fill = "grey", width = 0.7) +
  labs(
    x = "Policy Category",
    y = "Avg CV of intensity",
    title = "Avg CV of intensity by policy category"
  ) +
  theme_minimal() +
  theme(
    axis.text.x = element_text(angle = 0, hjust = 1)
  )

####4. Run loop for entire study period with taxonomic groups####
taxon_gp <- "tax_group"  #new name for column 

cv_intensity_ppm_tax <- function(sf_data, W,
                                 year_val, context_val, taxon_val,
                                 min_n = 50) {
  
  sub <- sf_data %>%
    dplyr::filter(
      Year_num == year_val,
      context_q1 == context_val,
      .data[[taxon_gp]] == taxon_val
    )
  
  n <- nrow(sub)
  if (n < min_n) return(NULL)
  
  xy <- st_coordinates(sub)
  X  <- ppp(xy[,1], xy[,2], window = W)
  
  fit <- ppm(X ~ x + y)
  
  Z <- as.numeric(predict(fit))
  
  tibble::tibble(
    year = year_val,
    context_q1 = context_val,
    tax_group = taxon_val,
    n = n,
    cv_intensity = sd(Z) / mean(Z)
  )
}


years_all <- sort(unique(q1_sf_in$Year_num))
contexts  <- sort(unique(q1_sf_in$context_q1))
taxa      <- sort(unique(q1_sf_in[[taxon_gp]])) #taxa = unique taxa within column

results_tax <- list()

for (yr in years_all) {
  for (ctx in contexts) {
    for (tx in taxa) {
      
      res <- cv_intensity_ppm_tax(
        sf_data = q1_sf_in,
        W = W,
        year_val = yr,
        context_val = ctx,
        taxon_val = tx,
        min_n = 40
      )
      
      if (!is.null(res)) {
        results_tax[[length(results_tax) + 1]] <- res
      }
    }
  }
}

cv_tax_df <- dplyr::bind_rows(results_tax)
cv_tax_df




cv_tax_df %>% count(tax_group) %>% arrange(desc(n)) 

cv_tax_df %>% #sample size distribution
  group_by(tax_group) %>%
  summarise(
    min_n = min(n),
    median_n = median(n),
    max_n = max(n),
    .groups = "drop"
  )

ggplot(cv_tax_df,
       aes(x = year, y = cv_intensity,
           color = context_q1, group = context_q1)) +
  geom_line() +
  geom_point(size = 1.2) +
  facet_wrap(~ tax_group, scales = "free_y") +
  labs(
    x = "Year",
    y = "CV of fitted intensity",
    color = "Policy context",
    title = "Spatial concentration of strandings by policy context and taxonomic group"
  ) +
  theme_minimal()

write.csv(cv_tax_df, "q1_cv_yr_tax&policy.csv", row.names = FALSE) #saved

#filtering only direct and indirect areas
cv_tax_filt <- cv_tax_df %>%
  filter(context_q1 %in% c("IndirectOnly", "DirectOnly"))

ggplot(cv_tax_filt,
       aes(x = year, y = cv_intensity,
           color = context_q1, group = context_q1)) +
  geom_line() +
  geom_point(size = 1.2) +
  facet_wrap(~ tax_group, scales = "free_y") +
  labs(
    x = "Year",
    y = "CV of fitted intensity",
    color = "Policy context",
    title = "Spatial concentration of strandings by policy context and taxonomic group"
  ) +
  theme_minimal()


#average overall cv by tax group
cv_tax_df2 <- cv_tax_df %>%
  mutate(policy_type = case_when(
    context_q1 %in% c("DirectOnly") ~ "Direct",
    context_q1 %in% c("IndirectOnly")  ~ "Indirect"
  ))

cv_tax_pol_mean <- cv_tax_df2 %>%
  group_by(tax_group, policy_type) %>%
  summarise(
    mean_cv = mean(cv_intensity, na.rm = TRUE),
    .groups = "drop"
  )

ggplot(cv_tax_pol_mean,
       aes(x = policy_type, y = mean_cv, fill = policy_type)) +
  geom_col(alpha = 0.7) +
  facet_wrap(~ tax_group, scales = "free_y") +
  labs(
    x = "Policy Target",
    y = "Avg CV",
    title = "Average spatial concentration by taxonomic group and policy target"
  ) +
  theme(
    strip.background = element_rect(fill = "white", color = "white"),  #facet wrap background and color
    strip.text = element_text(color = "black", size = 10,face = "italic"), #facet wrap text size and font
    plot.title = element_text(color = "black", size = 18),      # Title color and size
    axis.title.x = element_text(color = "black", size = 14),   # X-axis title
    axis.title.y = element_text(color = "black", size = 14),   # Y-axis title
    axis.text = element_text(size = 10)      # Axis tick labels
  ) +
  theme(legend.position = "none")






####extra####
#another way to loop the ppm but yields same results, just insures the intensity is compared correctly between covariates and marks
taxon_gp <- "tax_group"  #new name for column 

cv_intensity_ppm_tax2 <- function(sf_data, W,
                                 year_val, context_val, taxon_val,
                                 min_n = 50) {
  
  sub <- sf_data %>%
    dplyr::filter(
      Year_num == year_val,
      context_q1 == context_val,
      .data[[taxon_gp]] == taxon_val
    )
  
  n <- nrow(sub)
  if (n < min_n) return(NULL)
  
  xy <- st_coordinates(sub)
  X  <- ppp(xy[,1], xy[,2], window = W)
  
  fit <- ppm(X ~ x + y)
  
  Z_im <- predict(fit, type = "intensity") #im object
  z <- as.vector(Z_im$v)
  z <- z[is.finite(z)] #drop NA/Inf
  
  if (length(z) < 2 || mean(z) <= 0) reutrn (NULL)
  
  tibble::tibble(
    year = year_val,
    context_q1 = context_val,
    tax_group = taxon_val,
    n = n,
    cv_intensity = sd(z) / mean(z)
  )
}


years_all <- sort(unique(q1_sf_in$Year_num))
contexts  <- sort(unique(q1_sf_in$context_q1))
taxa      <- sort(unique(q1_sf_in[[taxon_gp]])) #taxa = unique taxa within column

results_tax1 <- list()

for (yr in years_all) {
  for (ctx in contexts) {
    for (tx in taxa) {
      
      res <- cv_intensity_ppm_tax2(
        sf_data = q1_sf_in,
        W = W,
        year_val = yr,
        context_val = ctx,
        taxon_val = tx,
        min_n = 40
      )
      
      if (!is.null(res)) results_tax1[[length(results_tax1) + 1]] <- res
      }
    }
  }


results_tax_df1 <- dplyr::bind_rows(results_tax1)
results_tax_df1











