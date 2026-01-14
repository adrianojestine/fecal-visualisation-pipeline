#installing library tidyverse, and plotly; which will be used later in making interactive graphs (.html converted)
library(tidyverse)
library(plotly)

#gets the working directory location
getwd()

#had to do below step to change file directory as it did not recognize the folder
setwd("C:/Users/addyj/OneDrive/Documents/Taxonomic Analysis Visualisation")

getwd()
#creating Dataframes to host .csv files for either barcode
df_barcode1 <- read_csv("Barcode 1 Taxonomy_RA Sorted_1.csv")
df_barcode2 <- read_csv("Barcode 2 Taxonomy_RA Sorted_1.csv")

#hard coding the Column names for standardisation, considering serial no. for now
#assigning the new names to object (header_df_barcode1 & 2)
#headers_df_barcode1 <- c('Taxa', 'Species', 'Healthy Read No.s', 'Relative Abundancies')
#headers_df_barcode2 <- c('Taxa', 'Species', 'Diseased (ACLF) Read No.s', 'Relative Abundancies')
#we will change the read numbers columns just to a stadardised 'Read No.s' column
#this is for ease in combining steps later on

headers_df_barcode1 <- c('Taxa', 'Species', 'Read No.s', 'Relative Abundancies')
headers_df_barcode2 <- c('Taxa', 'Species', 'Read No.s', 'Relative Abundancies')

#applying the objs containing new column names, to function colnames() to add to the dataframes created df_barcode1 & 2
colnames(df_barcode1) <- headers_df_barcode1
colnames(df_barcode2) <- headers_df_barcode2

#printing the dataframes for confirmation of new column names
print(df_barcode1)
print(df_barcode2)

#printing header of both to confirm dataframe
print("Headers for Barcode 1:")
head(df_barcode1)
print("")

print("Headers for Barcode 2:")
head(df_barcode2)
print("")

#we are using data sorted for RA (0.5%) - this was done on Python


#sorting the data from lowest to highest to place it on the the barplots
#we will use 'tax' because that is the name of the file (changes in case of diff column name)
df_barcode1_toplot <- df_barcode1 %>%
  mutate(Species = fct_reorder(Species, `Relative Abundancies`))

df_barcode2_toplot <- df_barcode2 %>%
  mutate(Species = fct_reorder(Species, `Relative Abundancies`))

#arranging df's for each barcode in descending order
df_barcode1_arranged <- df_barcode1_toplot %>%
  arrange(desc(`Relative Abundancies`))

df_barcode2_arranged <- df_barcode2_toplot %>%
  arrange(desc(`Relative Abundancies`))


#MAKING TWO BARPLOTS FOR EACH BARCODE 1 & 2:
#-------------------------------------------------------------------------------------------------------
#MAKING THE BARCODE1 PLOT
#-------------------------------------------------------------------------------------------------------
plot_barcode1 <- df_barcode1_toplot %>%
  #most of the funtions used below are from the dyplr library
  
  #initialising and making plot structure aesthetics
  ggplot(aes(x= Species, y=`Relative Abundancies`, fill= Species, desc = TRUE)) +
  
  #now defining the chracteristic of the bars in the plot we will make; width controls width of the bar
  geom_col(width = 0.8, fill = "forestgreen") +
  
  
  #focussing on x axis and determining scale. also redefining scale of x axis to percent
  #scale_y_continuous(labels = scales::percent)
  scale_y_continuous(labels = function(x) paste0(x, "%")) +
  
  #labels; setting title, x axis and y axis names
  #title, x axis, y axis, fill [for legend]
  labs(title = "Barcode 1 (Healthy Samples) Taxonomic Classification", x = "Species names", y="Relative Abundancies", fill="") +
  
  #guides formats legend, fill parameter is used to fill colours for legend
  guides(fill = "none") + 
  
  #theme_minimal(); works with general aesthetics of the thing
  #theme_minimal()
  theme(axis.text.x = element_text (angle = 25, hjust = 1))

#CONVERTING Barcode1 into html. file, so it can be interactive to the user.
plot_barcode1_interactive <- ggplotly(plot_barcode1)

#Plotting the graph
plot_barcode1_interactive

#-------------------------------------------------------------------------------------------------------
#MAKING THE BARCODE2 PLOT
#-------------------------------------------------------------------------------------------------------
plot_barcode2 <- df_barcode2_toplot %>%
  #most of the funtions used below are from the dyplr library
  
  #initialising and making plot structure aesthetics
  ggplot(aes(x= Species, y=`Relative Abundancies`, fill= Species, desc = TRUE)) +
  
  #now defining the chracteristic of the bars in the plot we will make; width controls width of the bar
  geom_col(width = 0.8, fill = "tomato") +
  
  #focussing on x axis and determining scale. also redefining scale of x axis to percent
  scale_y_continuous(labels = function(x) paste0(x, "%")) +
  
  #labels; setting title, x axis and y axis names
  #title, x axis, y axis, fill [for legend]
  labs(title = "Barcode 2 (Diseased Samples) Taxonomic Classification", x = "Species names", y="Relative Abundancies", fill="") +
  
  #guides formats legend, fill parameter is used to fill colours for legend
  guides(fill = "none") + 
  
  #theme_minimal(); works with general aesthetics of the thing
  theme(axis.text.x = element_text (angle = 25, hjust = 1))


#CONVERTING Barcode1 into html. file, so it can be interactive to the user.
plot_barcode2_interactive <- ggplotly(plot_barcode2)

plot_barcode2_interactive

#-------------------------------------------------------------------------------------------------------
#MAKING CSV FILES OF THE INDIVIDUAL BARCODES
#-------------------------------------------------------------------------------------------------------

print("Exporting Barcode 1 (Healthy) .csv file to your file loaction!")
write_csv(df_barcode1_arranged, "Barcode1_Output_csv.csv")

print("Exporting Barcode 2 (Diseased .csv file to your file loaction)")
write_csv(df_barcode2_arranged, "Barcode2_Output_csv.csv")

