#Assignment2 Calculate following the problem
#1/a. Σ with and without loop.

#without loop
a= 5:100
sum(a^4+5*a^3)

#output: 2177844976

#with loop
a= 5:100
s <- 0
for(i in a){
  s <- s + (i^4+5*i^3)
}
s

#1/b.Calculate U40 which series Un = U(n-1)+U(n-2) 

n=40
U = 1:2
for(i in 3:n){
  U[(i)] <- U[(i-1)]+U[(i-2)]
}
U[n]

#Output: 165580141

#1/c. Write program to Calculate cos x, sin x (Taylor Series), Get the input (x)from user and n = 45

x<- as.numeric(readline(prompt=" Enter X"))
1
n=45
n <- 1:n
sin_result <- sum(((-1)^n*x^(2*n+1))/factorial(2*n+1))
sin_result
#Output: -0.158529

cos_result <- sum(((-1)^n*x^(2*n))/factorial(2*n))
cos_result
#Output: -0.4596977

#1/d. Create the vector of eX cos(x)=3,3.1,3.2, ..... , 6

x<- seq(3 , 6 , by=0.1)
x<- exp(x)*cos(x)
x

#output -19.884531 -22.178753 -24.490697 -26.773182 -28.969238 -31.011186 -32.819775
#-34.303360 -35.357194 -35.862834 -35.687732 -34.685042 -32.693695 -29.538816
# -25.032529 -18.975233 -11.157417  -1.362099  10.632038  25.046705  42.099201
#  61.996630  84.929067 111.061586 140.525075 173.405776 209.733494 249.468441
# 292.486707 338.564378 387.360340


#1/e. How many number that divisible by 2 by using Modolo %%

x <- 1:300
a= 0

for (i in x ){
  if ( i%%2==0){
    a=a+1
  }
}
a

#Output 150

#2. Solve the following system of linear equation using Gaussian elimination (Ax=y)

library(matlib)

A<- matrix(c(1,2,3,
             2,1,2,
             3,3,1),3,3, byrow = TRUE)
B<- c(9,-3,5)

ans <- gaussianElimination(A,B)

ans

print(paste("answer is x =",ans[10],' y = ', ans[11], ' z =',ans[12]))

#Output:  "answer is x = -5.08333333  y =  6.66666667  z = 0.25"

#3. Use Outer Function to create following matrix

row <- 0:4
column <- 0:4
outer(column, row, "+")

#Output:      [,1] [,2] [,3] [,4] [,5]
#[1,]    0    1    2    3    4
#[2,]    1    2    3    4    5
#[3,]    2    3    4    5    6
#[4,]    3    4    5    6    7
#[5,]    4    5    6    7    8


#4. Get the COVID-19 Dataset from the data sources. Number of observations should be more than 100.Then, report the following informations
#4/a. Data source detail
  #Dataset number of Novel Corona Virus 2019 cases in Japan
  #Link: https://www.kaggle.com/lisphilar/covid19-dataset-in-japan?select=covid_jpn_total.csv
library(readr)
covid_jpn_total <- read_csv("covid_jpn_total.csv")
covid_jpn_total

#4/.bExplain the Unit & Necessity of each variable
  #This dataset have 14 variables:
  # Date: Date of observation
  # Location: Domestic, Returnee (from Wuhan), Airport.
  # Positive:PCR-tested and positive 
  # Tested: PCR-tested
  # Symptomatic: Positive and with symptoms
  # Asymptomatic:Positive and withOUT symptoms
  # Sym-unknown:Positive. Symptoms is under confirmation.
  # Hosp_require:Requiring hospitalization.
  # Hosp_mild: Positive and hospitalized with mild symptoms.
  # Hosp_severe: Positive and hospitalized with severe symptoms.
  # Hosp_unknown: Positive and hospitalized with symptoms. Severity is not confirmed.
  # Hosp_waiting: Positive and requiring hospitalization, but not hospitalized. At hotel or home.
  # Discharged: Positve, hospitalized and discharged.
  # Fatal: Positive and fatal.
#4/c. Find the missing values(rows & columns) and replace them with mean(Tidy Dataset) 
covid_jpn_total[] <- lapply(covid_jpn_total, function(x) { 
  x[is.na(x)] <- mean(x, na.rm = TRUE)
  x
})
covid_jpn_total

#4/d. Generate the two new variables(Var1:Mean, Var2: Median from available variable) 
library(dplyr) 
covid_jpn_total %>% mutate(Mean=mean(Positive),Median=median(Positive))

#4/e. Rename the two existing variables 
covid_jpn_total %>% rename(T=Tested,P=Positive)

#4/f. Create a plot using following instructions (using 7 layers of Grammar of Graphics)
# i. Choose x and y axis(aes) 
# Choose Date as X and Positive case of Covid as Y
data <- covid_jpn_total %>% count(Date, wt = Positive)
x <- data[['Date']]
y <- data[['n']]
data

# ii. geom_point() - specify the parameters, size : 5, color: red, alpha: ⅕ 
library(ggplot2)
gp <- geom_point(alpha=1/5, color="red", size=5)
gp
p <- ggplot(x = data, mapping = aes(x = x, y = y)) + gp
p

# iii. Use Facet grid, cartesian coordinates & geom_smooth() 

# facet_grid
p + facet_grid(y)
# coord_cartesian
p + coord_cartesian(expand = FALSE)
#geom_smooth
p + geom_smooth()

# iv. Assign the title to x, y and graph 
p <- p + ggtitle("Number of Positive Covid-19 cases in Japan") +
  xlab("Date") + ylab("Number Os Positive Cases")
p

# v. Export the graph to your working directory with the title called “covid_19_ dataset.png”
ggsave(filename = "/cloud/project/covid_19_positive_cases_japan_dataset.png", units = "cm", width = 25, height = 25)

