#Presentation figures

#Load libraries required for the whole script
library(dplyr)
library(tidyr)
library(ggplot2)
theme_set(theme_bw())

#Load in data 
load("R-scripts/Rdata/tidy_df_ideal.Rdata")

#Converting these to numeric--they already should be, but they must have been converted accidentally when making the matrix a dataframe, etc...
tidy_df$prop_capt = as.numeric(tidy_df$prop_capt)
tidy_df$total_seeds = as.numeric(tidy_df$total_seeds)
tidy_df$maternal_trees = as.numeric(tidy_df$maternal_trees)

#Subset the data by maternal tree to run two separate models 
sub_1 = tidy_df %>% subset(maternal_trees < 25)
sub_2 = tidy_df %>% subset(maternal_trees >= 25)

#Running the models
model_1 = lm(prop_capt ~ log(total_seeds) * log(maternal_trees) * donor_type, data = sub_1)
model_2 = lm(prop_capt ~ total_seeds * maternal_trees * donor_type, data = sub_2)
#Model summary! 
summary(model_1, digits = 4)
summary(model_2)

#Creating a new dataframe of values to base predictions on 
newd_1 = data.frame(maternal_trees=(rep(c(1,2,5,10), each=1500)), total_seeds=rep(seq(1,500,1),12), donor_type=factor(rep((rep(c("all_eligible", "all_same", "skewed"), each=500)), 4)))
#Predictions 
pmu_1 = predict(model_1, re.form=NA, transform = TRUE, newdata=newd_1)

newd_2 = data.frame(maternal_trees=(rep(c(25,50,100), each=1500)), total_seeds=rep(seq(1,500,1),9), donor_type=factor(rep((rep(c("all_eligible", "all_same", "skewed"), each=500)), 3)))
#Predictions 
pmu_2 = predict(model_2, re.form=NA, transform = TRUE, newdata=newd_2)

#Creating a dataframe to plot in ggplot 
preds_1 <- cbind(newd_1, pmu_1)
colnames(preds_1) = c("maternal_trees", "total_seeds", "donor_type", "pmu")
preds_2 <- cbind(newd_2, pmu_2)
colnames(preds_2) = c("maternal_trees", "total_seeds", "donor_type", "pmu")
preds = rbind(preds_1, preds_2)

#defining a color palette to use for the plots (color-blind accessible)
cbPalette <- c("#E69F00", "#56B4E9", "#CC79A7")

#defining more descriptive labels for the facets 
mat_tree_labs = c("1 maternal plant", "2 maternal plants", "5 maternal plants", "10 maternal plants", "25 maternal plants", "50 maternal plants", "100 maternal plants")
names(mat_tree_labs) = c("1", "2", "5", "10", "25", "50", "100") 

#Subset data for plotting 
sub_preds = filter(preds, maternal_trees == c(1,10,25,100))
sub_tidy_df = filter(tidy_df,  maternal_trees %in% c(1,10,25,100))

### Figure 1 

#Plotting the data
ggplot(data=sub_preds) +
    geom_point(data = sub_tidy_df, aes(x=as.numeric(total_seeds), y=as.numeric(prop_capt), color=donor_type), alpha=0.25, size=3) +
    facet_wrap(vars(maternal_trees), labeller = labeller(maternal_trees = mat_tree_labs)) +
    geom_line(data=sub_preds, mapping = aes(x=(total_seeds), y=pmu, lty=donor_type), show.legend=F) +
    ggtitle("Ex situ genetic diversity representation for ideal sampling scenarios") +
    ylab("Proportion of alleles conserved") +
    xlab("Total seeds sampled") +
    scale_colour_manual(values=cbPalette, labels = c("All eligible", "Single", "Skewed")) + 
    theme(strip.background = element_rect(color="black", fill="#F2F2F2", linetype="solid"), text = element_text(size = 30)) +
    labs(color = "Donor Type")


### Figure 2
#Load in data
load("R-scripts/Rdata/tidy_df_realistic.Rdata")
tidy_df_realistic = tidy_df
#Making sure the data is in the correct format
tidy_df_realistic$prop_capt = as.numeric(tidy_df$prop_capt)
tidy_df_realistic$total_seeds = as.numeric(tidy_df$total_seeds)
tidy_df_realistic$maternal_trees = as.numeric(tidy_df$maternal_trees)
load("R-scripts/Rdata/tidy_df_ideal.Rdata")
#Making sure the data is in the correct format
tidy_df$prop_capt = as.numeric(tidy_df$prop_capt)
tidy_df$total_seeds = as.numeric(tidy_df$total_seeds)
tidy_df$maternal_trees = as.numeric(tidy_df$maternal_trees)
#remove col to make same # cols 
tidy_df = subset(tidy_df, select = -c(seeds_per_tree))
#Create new columns to identify scenarios 
tidy_df$samp_type = "ideal"
tidy_df_realistic$samp_type = "realistic"
#Combine dataframes
combined_ideal_real = rbind(tidy_df, tidy_df_realistic)
#Make sure variables are in the correct format 
combined_ideal_real$donor_type = factor(combined_ideal_real$donor_type)
combined_ideal_real$samp_type = factor(combined_ideal_real$samp_type)
combined_ideal_real$prop_capt = as.numeric(combined_ideal_real$prop_capt)
combined_ideal_real$maternal_trees = as.numeric(combined_ideal_real$maternal_trees)

#defining more descriptive labels for the facets 
mat_tree_labs = c("2 maternal plants", "5 maternal plants", "10 maternal plants", "25 maternal plants", "50 maternal plants", "100 maternal plants")
names(mat_tree_labs) = c("2", "5", "10", "25", "50", "100")

#defining a color palette to use for the plots (color-blind accessible)
cbPalette <- c("#E69F00", "#f5d58e", "#56B4E9", "#96daf2", "#CC79A7", "#fcd2e9")

combined_ideal_real %>%
    filter(total_seeds==200) %>%
    filter(as.numeric(maternal_trees) == c(2, 10, 25, 100)) %>%
    ggplot() +
    geom_boxplot(aes(x=donor_type, y=as.numeric(prop_capt), color=interaction(samp_type, donor_type)), lwd=1) +
    facet_wrap(vars(maternal_trees), labeller = labeller(maternal_trees = mat_tree_labs)) +
    theme(strip.background = element_rect(color="black", fill="#F2F2F2", linetype="solid"), text = element_text(size = 30)) +
    ylim(0.3,1) + 
    ggtitle("Ex situ genetic diversity representation for scenarios with 200 total seeds sampled") +
    xlab("Donor type") +
    ylab("Proportion of alleles captured") +
    labs(color = "Sampling Type") +
    scale_x_discrete(labels=c("Eligible","Same","Skewed")) + 
    scale_colour_manual(values=cbPalette, labels = c("Eligible ideal", "Eligible realistic", "Single ideal", "Single realistic", "Skewed ideal", "Skewed realistic"))
