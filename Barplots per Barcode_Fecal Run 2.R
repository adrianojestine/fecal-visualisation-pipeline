#installing library tidyverse, and plotly; which will be used later in making interactive graphs (.html converted)
library(tidyverse)
library(plotly)

#gets the working directory location
getwd()

#had to do below step to change file directory as it did not recognize the folder
setwd("C:/Users/addyj/OneDrive/Documents/Taxonomic Analysis Visualisation")

getwd()
#creating Dataframes to host .csv files for either barcode
df_barcode11 <- read_csv("Barcode 11 Taxonomy_without unknown.csv")
df_barcode12 <- read_csv("Barcode 12 Taxonomy_without unknown.csv")
df_barcode13 <- read.csv("Barcode 13 Taxonomy_without unknown.csv")
df_barcode14 <- read.csv("Barcode 14 Taxonomy_without unknown.csv")

#hard coding the Column names for standardisation, considering serial no. for now
#assigning the new names to object (header_df_barcode1 & 2)
#headers_df_barcode1 <- c('Taxa', 'Species', 'Healthy Read No.s', 'Relative Abundancies')
#headers_df_barcode2 <- c('Taxa', 'Species', 'Diseased (ACLF) Read No.s', 'Relative Abundancies')
#we will change the read numbers columns just to a stadardised 'Read No.s' column
#this is for ease in combining steps later on

headers_df_barcode11 <- c('Taxa', 'Species', 'Read No.s', 'Relative Abundancies')
headers_df_barcode12 <- c('Taxa', 'Species', 'Read No.s', 'Relative Abundancies')
headers_df_barcode13 <- c('Taxa', 'Species', 'Read No.s', 'Relative Abundancies')
headers_df_barcode14 <- c('Taxa', 'Species', 'Read No.s', 'Relative Abundancies')

#applying the objs containing new column names, to function colnames() to add to the dataframes created df_barcode1 & 2
colnames(df_barcode11) <- headers_df_barcode11
colnames(df_barcode12) <- headers_df_barcode12
colnames(df_barcode13) <- headers_df_barcode13
colnames(df_barcode14) <- headers_df_barcode14

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

#we are using data sorted for RA (0.5%) - this was done on Python


#sorting the data from lowest to highest to place it on the the barplots
#we will use 'tax' because that is the name of the file (changes in case of diff column name)
df_barcode11_toplot <- df_barcode11 %>%
  mutate(Species = fct_reorder(Species, `Relative Abundancies`))

df_barcode12_toplot <- df_barcode12 %>%
  mutate(Species = fct_reorder(Species, `Relative Abundancies`))

df_barcode13_toplot <- df_barcode13 %>%
  mutate(Species = fct_reorder(Species, `Relative Abundancies`))

df_barcode14_toplot <- df_barcode14 %>%
  mutate(Species = fct_reorder(Species, `Relative Abundancies`))

#arranging df's for each barcode in descending order
df_barcode11_arranged <- df_barcode11_toplot %>%
  arrange(desc(`Relative Abundancies`))

df_barcode12_arranged <- df_barcode12_toplot %>%
  arrange(desc(`Relative Abundancies`))

df_barcode13_arranged <- df_barcode13_toplot %>%
  arrange(desc(`Relative Abundancies`))

df_barcode14_arranged <- df_barcode14_toplot %>%
  arrange(desc(`Relative Abundancies`))

#FOCUS ABOVE NEXT; TRY TO FINE A WAY TO REORDER ALL THE COLUMN BY RELATIVE ABUNDANCE
#AND IF WITH THE CURRENT SYNTAX ARE ALL OF THEM GETTING CHANGED?


#MAKING BARPLOTS FOR EACH BARCODE 11, 12, 13 & 14:
#-------------------------------------------------------------------------------------------------------
#MAKING THE BARCODE11 PLOT
#-------------------------------------------------------------------------------------------------------
plot_barcode11 <- df_barcode11_toplot %>%
  #most of the funtions used below are from the dyplr library
  
  #initialising and making plot structure aesthetics
  ggplot(aes(x= Species, y=`Relative Abundancies`, fill= Species, desc = TRUE)) +
  
  #now defining the chracteristic of the bars in the plot we will make; width controls width of the bar
  geom_col(width = 0.8, fill = "tomato") +
  
  
  #focussing on x axis and determining scale. also redefining scale of x axis to percent
  #scale_y_continuous(labels = scales::percent)
  scale_y_continuous(labels = function(x) paste0(x, "%")) +
  
  #labels; setting title, x axis and y axis names
  #title, x axis, y axis, fill [for legend]
  labs(title = "Barcode 11 (Diseased Samples) Taxonomic Classification", x = "Species names", y="Relative Abundancies", fill="") +
  
  #guides formats legend, fill parameter is used to fill colours for legend
  guides(fill = "none") + 
  
  #theme_minimal(); works with general aesthetics of the thing
  #theme_minimal()
  theme(axis.text.x = element_text (angle = 25, hjust = 1))

#CONVERTING Barcode11 into html. file, so it can be interactive to the user.
plot_barcode11_interactive <- ggplotly(plot_barcode11)

#Plotting the graph
plot_barcode11_interactive

#-------------------------------------------------------------------------------------------------------
#MAKING THE BARCODE12 PLOT
#-------------------------------------------------------------------------------------------------------
plot_barcode12 <- df_barcode12_toplot %>%
  #most of the funtions used below are from the dyplr library
  
  #initialising and making plot structure aesthetics
  ggplot(aes(x= Species, y=`Relative Abundancies`, fill= Species, desc = TRUE)) +
  
  #now defining the chracteristic of the bars in the plot we will make; width controls width of the bar
  geom_col(width = 0.8, fill = "tomato") +
  
  #focussing on x axis and determining scale. also redefining scale of x axis to percent
  scale_y_continuous(labels = function(x) paste0(x, "%")) +
  
  #labels; setting title, x axis and y axis names
  #title, x axis, y axis, fill [for legend]
  labs(title = "Barcode 12 (Diseased Samples) Taxonomic Classification", x = "Species names", y="Relative Abundancies", fill="") +
  
  #guides formats legend, fill parameter is used to fill colours for legend
  guides(fill = "none") + 
  
  #theme_minimal(); works with general aesthetics of the thing
  theme(axis.text.x = element_text (angle = 25, hjust = 1))


#CONVERTING Barcode12 into html. file, so it can be interactive to the user.
plot_barcode12_interactive <- ggplotly(plot_barcode12)

plot_barcode12_interactive

#-------------------------------------------------------------------------------------------------------
#MAKING THE BARCODE13 PLOT
#-------------------------------------------------------------------------------------------------------
plot_barcode13 <- df_barcode13_toplot %>%
  #most of the funtions used below are from the dyplr library
  
  #initialising and making plot structure aesthetics
  ggplot(aes(x= Species, y=`Relative Abundancies`, fill= Species, desc = TRUE)) +
  
  #now defining the chracteristic of the bars in the plot we will make; width controls width of the bar
  geom_col(width = 0.8, fill = "forestgreen") +
  
  #focussing on x axis and determining scale. also redefining scale of x axis to percent
  scale_y_continuous(labels = function(x) paste0(x, "%")) +
  
  #labels; setting title, x axis and y axis names
  #title, x axis, y axis, fill [for legend]
  labs(title = "Barcode 13 (Healthy Samples) Taxonomic Classification", x = "Species names", y="Relative Abundancies", fill="") +
  
  #guides formats legend, fill parameter is used to fill colours for legend
  guides(fill = "none") + 
  
  #theme_minimal(); works with general aesthetics of the thing
  theme(axis.text.x = element_text (angle = 25, hjust = 1))


#CONVERTING Barcode13 into html. file, so it can be interactive to the user.
plot_barcode13_interactive <- ggplotly(plot_barcode13)

plot_barcode13_interactive

#-------------------------------------------------------------------------------------------------------
#MAKING THE BARCODE13 PLOT
#-------------------------------------------------------------------------------------------------------

plot_barcode14 <- df_barcode14_toplot %>%
  #most of the funtions used below are from the dyplr library
  
  #initialising and making plot structure aesthetics
  ggplot(aes(x= Species, y=`Relative Abundancies`, fill= Species, desc = TRUE)) +
  
  #now defining the chracteristic of the bars in the plot we will make; width controls width of the bar
  geom_col(width = 0.8, fill = "grey") +
  
  #focussing on x axis and determining scale. also redefining scale of x axis to percent
  scale_y_continuous(labels = function(x) paste0(x, "%")) +
  
  #labels; setting title, x axis and y axis names
  #title, x axis, y axis, fill [for legend]
  labs(title = "Barcode 14 (Zymo) Taxonomic Classification", x = "Species names", y="Relative Abundancies", fill="") +
  
  #guides formats legend, fill parameter is used to fill colours for legend
  guides(fill = "none") + 
  
  #theme_minimal(); works with general aesthetics of the thing
  theme(axis.text.x = element_text (angle = 25, hjust = 1))


#CONVERTING Barcode14 into html. file, so it can be interactive to the user.
plot_barcode14_interactive <- ggplotly(plot_barcode14)

plot_barcode14_interactive

#-------------------------------------------------------------------------------------------------------
#MAKING CSV FILES OF THE INDIVIDUAL BARCODES
#-------------------------------------------------------------------------------------------------------

#COMMENT IT UNLESS USING: KEEP A CHECK
print("Exporting Barcode 11 (Diseased) .csv file to your file loaction!")
write_csv(df_barcode11_arranged, "Barcode11_Output_csv.csv")

print("Exporting Barcode 12 (Diseased) .csv file to your file location!)")
write_csv(df_barcode12_arranged, "Barcode12_Output_csv.csv")

print("Exporting Barcode 13 (Healthy) .csv file to your file location!)")
write_csv(df_barcode13_arranged, "Barcode13_Output_csv.csv")

print("Exporting Barcode 14 (Zymo) .csv file to your file location!)")
write_csv(df_barcode14_arranged, "Barcode14_Output_csv.csv")

#potential NEXT STEP: add the each column's corresponding x axis values on its head



