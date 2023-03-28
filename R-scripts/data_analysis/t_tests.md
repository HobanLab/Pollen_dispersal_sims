
``` r
#Load libraries 
library(dplyr)
```

    ## Warning: package 'dplyr' was built under R version 4.0.5

    ## Warning: replacing previous import 'lifecycle::last_warnings' by
    ## 'rlang::last_warnings' when loading 'pillar'

    ## Warning: replacing previous import 'lifecycle::last_warnings' by
    ## 'rlang::last_warnings' when loading 'tibble'

    ## 
    ## Attaching package: 'dplyr'

    ## The following objects are masked from 'package:stats':
    ## 
    ##     filter, lag

    ## The following objects are masked from 'package:base':
    ## 
    ##     intersect, setdiff, setequal, union

``` r
library(tidyr)
```

    ## Warning: package 'tidyr' was built under R version 4.0.5

``` r
library(ggplot2)
```

    ## Warning: package 'ggplot2' was built under R version 4.0.5

``` r
theme_set(theme_bw())

#Load in ideal data 
load("tidy_df.Rdata")
tidy_df_ideal = tidy_df
#remove col to make same # cols 
tidy_df_ideal = subset(tidy_df_ideal, select = -c(seeds_per_tree))

#Realistic data
load("tidy_df_realistic.Rdata")
tidy_df_realistic = tidy_df

#Create new columns to identify scenarios 
tidy_df_ideal$samp_type = "ideal"
tidy_df_realistic$samp_type = "realistic"

#Combine dataframes
combined_ideal_real = rbind(tidy_df_ideal, tidy_df_realistic)
```

``` r
#Make sure variables are in the correct format 
combined_ideal_real$donor_type = factor(combined_ideal_real$donor_type)
combined_ideal_real$samp_type = factor(combined_ideal_real$samp_type)
combined_ideal_real$prop_capt = as.numeric(combined_ideal_real$prop_capt)
combined_ideal_real$maternal_trees = as.numeric(combined_ideal_real$maternal_trees)
```

``` r
#defining more descriptive labels for the facets 
mat_tree_labs = c("2 maternal trees", "5 maternal trees", "10 maternal trees", "25 maternal trees", "50 maternal trees", "100 maternal trees")
names(mat_tree_labs) = c("2", "5", "10", "25", "50", "100")

#defining a color palette to use for the plots (color-blind accessible)
cbPalette <- c("#E69F00", "#f5d58e", "#56B4E9", "#96daf2", "#CC79A7", "#fcd2e9")

#Plotting the data
combined_ideal_real %>%
    filter(total_seeds==200) %>%
    filter(as.numeric(maternal_trees)>1) %>%
    ggplot() +
    geom_boxplot(aes(x=donor_type, y=as.numeric(prop_capt), color=interaction(samp_type, donor_type))) +
    facet_wrap(vars(maternal_trees), labeller = labeller(maternal_trees = mat_tree_labs)) +
    theme(strip.background = element_rect(color="black", fill="#F2F2F2", linetype="solid")) +
    ylim(0.3,1) + 
    ggtitle("Genetic diversity capture for scenarios with 200 total seeds sampled") +
    xlab("Donor type") +
    ylab("Proportion of alleles captured") +
    labs(color = "Sampling Type") +
    scale_x_discrete(labels=c("Eligible","Same","Skewed")) + 
    scale_colour_manual(values=cbPalette, labels = c("Eligible ideal", "Eligible realistic", "Same ideal", "Same realistic", "Skewed ideal", "Skewed realistic"))
```

![](t_tests_files/figure-gfm/unnamed-chunk-3-1.png)<!-- -->

T-tests to compare equivalent scenarios between ideal and realistic:

``` r
p_vals_same = c()
ideal_avg = c()
realistic_avg = c()
mat_trees = c(2, 5, 10, 25, 50, 100)
for(i in 1:length(mat_trees)) {
    subset = combined_ideal_real %>% filter(total_seeds == 200) %>% filter(maternal_trees == mat_trees[i]) %>% filter(donor_type == 'all_same')
    ideal_avg[i] = mean((subset %>% filter(samp_type == "ideal"))$prop_capt)
    realistic_avg[i] = mean((subset %>% filter(samp_type == "realistic"))$prop_capt)
    p_vals_same[i] = t.test(subset$prop_capt ~ subset$samp_type)$p.value   
}
same_table = data.frame(donor_type = rep("same", 6), maternal_trees_samp = mat_trees, tot_seeds_sampled = rep(200, 6), prop_capt_ideal = ideal_avg, prop_capt_realistic = realistic_avg, p_value = p_vals_same)

p_vals_skewed = c()
mat_trees = c(2, 5, 10, 25, 50, 100)
for(i in 1:length(mat_trees)) {
    subset = combined_ideal_real %>% filter(total_seeds == 200) %>% filter(maternal_trees == mat_trees[i]) %>% filter(donor_type == 'skewed')
    ideal_avg[i] = mean((subset %>% filter(samp_type == "ideal"))$prop_capt)
    realistic_avg[i] = mean((subset %>% filter(samp_type == "realistic"))$prop_capt)
    p_vals_skewed[i] = t.test(subset$prop_capt ~ subset$samp_type)$p.value   
}
skewed_table = data.frame(donor_type = rep("skewed", 6), maternal_trees_samp = mat_trees, tot_seeds_sampled = rep(200, 6), prop_capt_ideal = ideal_avg, prop_capt_realistic = realistic_avg, p_value = p_vals_skewed)

p_vals_eligible = c()
mat_trees = c(2, 5, 10, 25, 50, 100)
for(i in 1:length(mat_trees)) {
    subset = combined_ideal_real %>% filter(total_seeds == 200) %>% filter(maternal_trees == mat_trees[i]) %>% filter(donor_type == 'all_eligible')
    ideal_avg[i] = mean((subset %>% filter(samp_type == "ideal"))$prop_capt)
    realistic_avg[i] = mean((subset %>% filter(samp_type == "realistic"))$prop_capt)
    p_vals_eligible[i] = t.test(subset$prop_capt ~ subset$samp_type)$p.value   
}
eligible_table = data.frame(donor_type = rep("eligible", 6), maternal_trees_samp = mat_trees, tot_seeds_sampled = rep(200, 6), prop_capt_ideal = ideal_avg, prop_capt_realistic = realistic_avg, p_value = p_vals_eligible)

p_values_combined = rbind(same_table, skewed_table, eligible_table)
p_values_combined$p_value = signif(p_values_combined$p_value, digits = 3)
p_values_combined$prop_capt_ideal = signif(p_values_combined$prop_capt_ideal)
p_values_combined$prop_capt_realistic = signif(p_values_combined$prop_capt_realistic)

p_values_combined = p_values_combined[order(p_values_combined$maternal_trees),]

write.csv(p_values_combined, file='../../R-scripts/p_values_ideal_realistic.csv')
```

-----

Calculating the average proportion of alleles captured for each pollen
type Ideal:

``` r
ideal_same = tidy_df_ideal %>% filter(donor_type == 'all_same')
mean_same = mean(ideal_same$prop_capt)
ideal_eligible = tidy_df_ideal %>% filter(donor_type == 'all_eligible')
mean_eligible = mean(ideal_eligible$prop_capt)
ideal_skewed = tidy_df_ideal %>% filter(donor_type == 'skewed')
mean_skewed = mean(ideal_skewed$prop_capt)

mean_eligible - mean_skewed
```

    ## [1] 0.215695

``` r
mean_eligible - mean_same
```

    ## [1] 0.4862354

Realistic:

``` r
realistic_same = tidy_df_realistic %>% filter(donor_type == 'all_same')
mean_same = mean(as.numeric(realistic_same$prop_capt))
realistic_eligible = tidy_df_realistic %>% filter(donor_type == 'all_eligible')
mean_eligible = mean(as.numeric(realistic_eligible$prop_capt))
realistic_skewed = tidy_df_realistic %>% filter(donor_type == 'skewed')
mean_skewed = mean(as.numeric(realistic_skewed$prop_capt))

mean_eligible - mean_skewed
```

    ## [1] 0.1469106

``` r
mean_eligible - mean_same
```

    ## [1] 0.3395776
