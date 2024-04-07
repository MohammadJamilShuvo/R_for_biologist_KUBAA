#lecture: 01
#Date:16.03.2024
#Speaker: Md Tangimul Islam

#open the script
#Control+shift+N

#Save the script
#Control+S

#check the version of R studio
version  #click run or Control+enter to run the code 

#set working dicrectory
setwd('C:/Users/rifat/OneDrive/Desktop') #change / with \

#get working dicrectory
getwd() 

#End of the script


#lecture: 02
#Date:17.03.2024
#Speaker: Biplab Paul

# Variables and Function in R
avarge_length = 5000
exon1 = 2500
exon2 = 2000
intron = 3000
gene_length2 = exon1 + intron + exon2
gene1 = 2000
gene2 = 3000
gene3 = 2500
avarage_length = mean(c(gene1, gene2, gene3))

#Data types (Class)
gene_name = 'ACT1'
gc_content = 50.5
gc_content_numeric = as.numeric(gc_content)
gc_content_integer = as.integer(gc_content)
my_name = 'Biplab Paul'
genome_length = '10000'
my_name_numeric = as.numeric(my_name)
genome_length_nu = as.numeric(genome_length)
genome_length_ch = as.character(genome_length)
add_1000 = as.numeric(genome_length) + 1000

logical_false = genome_length > 15000
logical_true = gene_length > 7000

#Data structures (Vectors)
genea = 1000
geneb = 1500
gene_lengths = c(1000, 1500, 2000, 700)
promoters = 100
gene_lengths[1]

#Lists In R Programming
gene_names = c('A', 'B', 'C', 'D')
gene_names[2] = 'E'
log_vector = c('TRUE', 'FALSE', 'TRUE')
list_data = list(c(200, 100), c('A', 'B'))
list_data[[1]] + 100

#Data structures (Matrix)
vec1 = 1:12
mtx = matrix(vec1, nrow=3)
mtx[2,]

#Data structures (Data frames)
fieldtrial <- data.frame(
  plot = c( 1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12) ,
  variety = c( 1, 1, 1, 1, 1, 1, 2, 2, 2, 2, 2, 2) ,
  fertilizer = c("N","N","N","NK","NK","NK","N", "N","N","NK", "NK","NK"),
  rep = c( 1, 2, 3, 1, 2, 3, 1, 2, 3, 1, 2, 3) ,
  yield = c(80, 90, 95, 95, 100, 93, 88, 102, 92, 100, 110, 103) )
write.table(fieldtrial, file = "/Users/biplab/Documents/fieldtrial.txt")
fieldtrial2 = read.table("/Users/biplab/Documents/fieldtrial.txt")
bases = c('A', 'T', 'C', 'G')
bases_vec = sample(bases, 10000, replace=TRUE)
DNA = paste(bases_vec, collapse = "")
gc_perentage <- function(DNA){
  strsplit('atcga', split="")[[1]]
  GCcontent <- sum(bases_vec %in% c('G','C'))/length(bases_vec)
}
#Saving and loading data files


#lecture: 03
#Date:23.03.2024
#Speaker: Biplab Paul

#function
sqSum = function(x, y){
  result = x^2 + y^2
  return(result)
}

#User defined function
gc_parcentage = function(DNA_SEQ){
  nucleotides = strsplit(DNA_SEQ, split="")[[1]]
  length_dna = length(nucleotides)
  total_gc = sum(nucleotides %in% c('G', 'C'))
  gc_percentage = total_gc/length_dna*100
  return(gc_percentage)
}
student_marks = c(10, 25, 90, 30, 45, 50)
my_mean_function = function(number_vec){
  total = sum(number_vec)
  total_students = length(number_vec)
  average = total/total_students
  return(average)
}

#for loop and If-Else in R
for (m in student_marks){
  if (m > 40){
    print('Pass')
  } else {
    print('Fail')
  }}
x = 20

#while loop in R
while (x > 10) {
  print('greater than ten') 
  x = x-5
}

#lecture: 04
#Date:24.03.2024
#Speaker: Jamil Shuvo

#package looding and library calling
install.packages("tidyverse")
library("tidyverse")

#setting directory
setwd("C:/Users/ms2252/Desktop/KUBAA")

#loading data and data
data("iris")
head(iris, 10)
str(iris)

#column selections
selected <- select(iris, Sepal.Length, Petal.Length)
selected2 <- select(iris, Sepal.Length:Petal.Length)

selected <- select(iris, c(1:3))

selected3 <- select(iris, -Sepal.Length, -Petal.Length)

#filter out
filtered <- filter(iris, Species == "setosa")
filtered2 <- filter(iris, Species == "setosa", Sepal.Length < 5)


#Creates new columns
col1 <- mutate(iris, Greater.Half = Sepal.Length < 5)

table(col1$Greater.Half)

#Sort rows by variables
arranged <- arrange(col1, Sepal.Length)

arranged2 <- arrange(col1, desc(Sepal.Length))

#Group observations
group1 <- group_by(col1, Species)
group2 <- summarise(group1, Mean.Sepal.Length = mean(Sepal.Length))

#Wrap multiple functions together
iris %>%  group_by(Species) %>% summarise(Mean.Sepal.Length = mean(Sepal.Length))


#lecture: 05
#Date:30.03.2024
#Speaker: Jamil Shuvo

#loading package and library
install.packages("ggplot2")
library("ggplot2")

setwd("C:/Users/ms2252/Desktop/KUBAA")

#input data
data("iris")
head(iris, 10)
str(iris)
?iris

#starting with ggplot
ggplot (iris, aes (x=Sepal.Length, y=Petal.Length))+
  geom_point()


#add labels and title
ggplot (iris, aes (x=Sepal.Length, y=Petal.Length, color=Species))+
  geom_point()+
  labs(x = "Sepal length",
       y = "Petal length",
       title = "How sepal and petal length are related!",
       caption = "Source: Fisher, R. A. (1936)")+
  scale_color_brewer(palette = "Dark2")


?geom_point
#Modifying basic properties
ggplot (iris, aes (x=Sepal.Length, y=Petal.Length))+
  geom_point(color = "black",
             size = 3,
             alpha = .5,
             shape = "square")+
  labs(x = "Sepal length",
       y = "Petal length",
       title = "How sepal and petal length are related!",
       caption = "Source: Fisher, R. A. (1936)")+
  scale_color_brewer(palette = "Dark2")

# Adding another layer
ggplot (iris, aes (x=Sepal.Length, y=Petal.Length))+ #color=Species
  geom_point()+
  geom_smooth(method = "lm",
              se = FALSE) +
  labs(x = "Sepal length",
       y = "Petal length",
       title = "How sepal and petal length are related!",
       caption = "Source: Fisher, R. A. (1936)")+
  scale_color_brewer(palette = "Dark2")

# Other plots

ggplot(iris, aes(x = Sepal.Length)) + 
  geom_histogram(bins = 50) # one quantitative variable

ggplot(iris, aes(x = Sepal.Length)) + 
  geom_freqpoly(bins = 50)

ggplot(iris, aes(x = Species)) + 
  geom_bar(color = "black",
           fill = "lightblue")

ggplot(iris, aes(x = Species, 
                 y = Sepal.Length,
                 color = Species)) + 
  geom_boxplot(show.legend = FALSE) +
  scale_color_brewer(palette = "Dark2") +
  theme_minimal()

?theme_minimal()



# faceting

# not great:
ggplot(iris, aes(x = Sepal.Length, 
                 fill = Species)) + 
  geom_histogram(bins = 50) +
  scale_fill_brewer(palette = "Dark2")

#ok but the axis range is misleading
ggplot(iris, aes(x = Sepal.Length,
                 fill = Species)) + 
  geom_histogram(bins = 50,
                 show.legend = FALSE) + 
  facet_wrap(~Species) +
  scale_fill_brewer(palette = "Dark2")
?facet_wrap

#now look meaningful
ggplot(iris, aes(x = Sepal.Length,
                 fill = Species)) + 
  geom_histogram(bins = 50,
                 show.legend = FALSE) + 
  facet_wrap(~Species,
             ncol = 1) +
  scale_fill_brewer(palette = "Dark2") + 
  theme_minimal()

#lecture: 06
#Date:30.03.2024
#Speaker: Jamil Shuvo

## Pearson correlation between 2 variables
cor(iris$Sepal.Length, iris$Petal.Length, method = "pearson")


#Linear Regression Model
model<- lm(iris$Petal.Length~ iris$Sepal.Length, data=iris)
summary(model)


#subset data
setosa <- iris[iris$Species == "setosa", ]
versicolor <- iris[iris$Species == "versicolor", ]

# Compare Sepal Length of these two species
t.test(x = setosa$Sepal.Length, y = versicolor$Sepal.Length)

#anova (one-way)
anova <- aov(Sepal.Length ~ Species, data = iris)
summary(anova)

#pairwise one way ANOVA
t1 <- TukeyHSD(anova, conf.level=0.95)
t1
plot(t1)

#lecture: 08
#Date:06.04.2024
#Speaker: Jamil Shuvo

#uses of ChatGPT to write R code

# Generate some random data for demonstration
set.seed(123)  # Set seed for reproducibility
n <- 100  # Number of data points
sepal_length <- rnorm(n, mean = 5.5, sd = 0.8)
petal_length <- rnorm(n, mean = 3.5, sd = 0.5)

# Create a dataframe
df <- data.frame(sepal_length = sepal_length, petal_length = petal_length)

# Add a column with random class labels ("setosa", "versicolor", "virginica")
class_labels <- c("setosa", "versicolor", "virginica")
df$class <- sample(class_labels, n, replace = TRUE)

# Load ggplot2 library for plotting
library(ggplot2)

# Create scatterplot colored by class
ggplot(df, aes(x = sepal_length, y = petal_length, color = class)) +
  geom_point() +
  labs(x = "Sepal Length", y = "Petal Length", color = "Class") +
  ggtitle("Scatterplot of Sepal Length vs Petal Length by Class")

# Load the tidyverse package
library(tidyverse)

# Filter the dataframe to extract only the "Setosa" class variables and select desired columns
df2 <- df %>%
  filter(class == "setosa") %>%
  select(sepal_length, petal_length)

# View the new dataframe
print(df2)
data("iris")












