#CODE OBJECTIVE: make stacked graphs for barcode 1 and 2 combined

#installing library tidyverse, and plotly; which will be used later in making interactive graphs (.html converted)
library(tidyverse)
library(ggplot2)
library(plotly)

#gets the working directory location
getwd()

#had to do below step to change file directory as it did not recognize the folder
setwd("C:/Users/addyj/OneDrive/Documents/Taxonomic Analysis Visualisation")

#creating Dataframes to host .csv files for either barcode

df_barcode11 <- read_csv("Barcode 11 Taxonomy_without unknown.csv")
df_barcode12 <- read_csv("Barcode 12 Taxonomy_without unknown.csv")
df_barcode13 <- read_csv("Barcode 13 Taxonomy_without unknown.csv")


#hard coding the Column names for standardisation, considering serial no. for now
#assigning the new names to object (header_df_barcode1 & 2)

headers_df_barcode11 <- c('Taxa', 'Species', 'Read No.s', 'Relative Abundancies')
headers_df_barcode12 <- c('Taxa', 'Species', 'Read No.s', 'Relative Abundancies')
headers_df_barcode13 <- c('Taxa', 'Species', 'Read No.s', 'Relative Abundancies')


#applying the objs containing new column names, to function colnames() to add to the dataframes created df_barcode1 & 2

colnames(df_barcode11) <- headers_df_barcode11
colnames(df_barcode12) <- headers_df_barcode12
colnames(df_barcode13) <- headers_df_barcode13


#printing the dataframes for confirmation of new column names
#print(df_barcode1)
#print(df_barcode2)

#printing header of both to confirm dataframe
#print("Headers for Barcode 1:")
#head(df_barcode1)
#print("")

#print("Headers for Barcode 2:")
#head(df_barcode2)
#print("")

#we are using data with calculated RA and filtered for RA (0.5%) by previous python code
#
#-------------------------------------------------------------------------------------------------------
#SORTING the data from HIGHEST TO LOWEST to place it on the the barplots
#-------------------------------------------------------------------------------------------------------

#we will use 'tax' because that is the name of the file (changes in case of diff column name)


df_barcode11_tosort <- df_barcode11 %>%
  #mutate(Species = fct_reorder(Species, `Relative Abundancies`))
  arrange(desc(`Relative Abundancies`))

df_barcode12_tosort <- df_barcode12 %>%
  #mutate(Species = fct_reorder(Species, `Relative Abundancies`))
  arrange(desc(`Relative Abundancies`))

df_barcode13_tosort <- df_barcode13 %>%
  #mutate(Species = fct_reorder(Species, `Relative Abundancies`))
  arrange(desc(`Relative Abundancies`))


#We do the following steps to label each row to indicated if it originated
#from df_barcode1 or df_barcode2

df_barcode11_tocombine <- df_barcode11_tosort %>%
  mutate(Sample = "Diseased1_Run2")
df_barcode12_tocombine <- df_barcode12_tosort %>%
  mutate(Sample = "Diseased2_Run2")
df_barcode13_tocombine <- df_barcode13_tosort %>%
  mutate(Sample = "Healthy1_Run2")


#now we will bind_rows() combining the rows of each dataframe. As it has been labelled it will be 
#clearly recognised
#we also store it in a new df_combined
df_combined <- bind_rows(df_barcode11_tocombine,
                         df_barcode12_tocombine,
                         df_barcode13_tocombine)
#df_combined
#print(n = 100, df_combined)

#-------------------------------------------------------------------------------------------------------
#making a REFERENCE LIST to COMBINE with original Combined dataframe
#-------------------------------------------------------------------------------------------------------

#calculating the total RA (healthy + diseased) by 'Species' column
#Note: when referring to columns we do not use the ''. Eg, It would just be Species.
#if there is a space in the column name; we use `` (backticks)

#moving towards making a reference list to sort the original 
#Step 1: totaling RA for each species
df_RAtotaled <- df_combined %>% 
  group_by (Species) %>% 
  summarize(Total_RA = sum(`Relative Abundancies`))
print(n = 100, df_RAtotaled)
#output; 'Sample' and other columns are removed. Species and Total_RA columns only remain.

#Step 2: filtering the TOP 10 - we make a reference sheet to then compare to the original datafram
df_top10_list <- df_RAtotaled %>% 
  slice_max(order_by = Total_RA, n = 10)
df_top10_list

#reintroducing this Healthy and Diseased labels into the table
#BY combining the df_combined (original graph) to the df_top20 (which
#already contains the two columns of df_RAtotaled

#now adding to the reference list we made to the orginal dataframe (and its features)
df_combined_top10 <- df_combined %>% 
  inner_join(df_top10_list, by = "Species")

print(n = 100, df_combined_top10)
#output; 14 species were found common. As they were previously arranged;
#they are sorted by `Relative Abundancies` column (in arrange() step)

#-------------------------------------------------------------------------------------------------------
#Calculating Error and Tht
#-------------------------------------------------------------------------------------------------------

#df_error <- df_RAtotaled


#-------------------------------------------------------------------------------------------------------
#MAKING THE STACKED COMBINED PLOT
#-------------------------------------------------------------------------------------------------------
#feeding the plot the necessary dataframe
stacked_plot_combined <- df_combined_top10 %>%
  #assigning columns to each variable of the plot
  ggplot(aes(x = fct_reorder(Species, `Relative Abundancies`, .fun = sum),
             y = `Relative Abundancies`,
             fill = Sample)) + 
  #column formatting
  geom_col(position = "stack", width = 0.8) +
  
  #coord_flip() +
  #giving the y axis attributes
  scale_y_continuous(labels = function(x) paste0(x, "%")) +
  #labelling the plot
  labs (title = "Top 10 Species across Fecal Run 2",
        subtitle = "Healthy vs. Diseased",
        x = "Species",
        y = "Relative Abundancies",
        fill = "Sample Type") +
  scale_fill_manual(
    values = c(
      "Diseased1_Run2" = "tan1",
      "Diseased2_Run2" = "tomato",
      "Healthy1_Run2"  = "forestgreen"
      
    )
  )+
  #using predetermined plot format
  #theme_minimal()
  
  #using customised plot format
  theme(axis.text.x = element_text (angle = 25, hjust = 1))

#making the plot interactive
plot_stacked_interactive <- ggplotly(stacked_plot_combined)

#printing stacked plot
plot_stacked_interactive

#-------------------------------------------------------------------------------------------------------
#EXPORTING .CSV for STACKED PLOT
#-------------------------------------------------------------------------------------------------------

write_csv(df_combined_top10, "Combined Barcode Top 10 RAs_Run2_HC vs ACLD.csv")

#FOCUS ABOVE NEXT; TRY TO FINE A WAY TO REORDER ALL THE COLUMN BY RELATIVE ABUNDANCE
#AND IF WITH THE CURRENT SYNTAX ARE ALL OF THEM GETTING CHANGED?



#-------------------------------------------------------------------------------------------------------
#PART 2:
#-------------------------------------------------------------------------------------------------------
#MAKING THE STACKED COMBINED PLOT
#-------------------------------------------------------------------------------------------------------

#df_combined_top10
