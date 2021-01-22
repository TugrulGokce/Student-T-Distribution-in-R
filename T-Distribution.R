

# read csv file
library(readr)
X500_Person_Gender_Height_Weight_Index <- read_csv("C:/EXCEL Ornekler/500_Person_Gender_Height_Weight_Index.csv")
View(X500_Person_Gender_Height_Weight_Index)

fiveHundredPerson_Height <- head(X500_Person_Gender_Height_Weight_Index,500)
View(fiveHundredPerson_Height)

# Take the column of people's height from the dataset and assign it to an empty vector
persons_Heights <- c()

# population = 500
for(i in 1:500){
    persons_Heights[i] <- fiveHundredPerson_Height[[2]][i]
}

class(persons_Heights)
View(persons_Heights)
#---------------------------------------------------------------------------------------
# We have created an empty list for samples
personSample_list <- NULL 

# We created samples with 50 groups of 16 into the list we created.
for(i in 1:50){
    personSample_list[[i]] <- sample(persons_Heights,size = 16) # For df = 15, size = 16
}
personSample_list
length(personSample_list[[1]])

# Calculation of mean, standard deviation, t-value for the samples we created
sd_for_size_16 <-NA
mean_for_size_16 <- NA
population_mean <- mean(persons_Heights)
t1 <- NA

# function for calculate the t-value
t.value <- function(x,ort){
    t<-(mean(x)-ort)/
        (sd(x)/(sqrt(length(x))))
    return(t)
}

for(i in 1:length(personSample_list)){
    mean_for_size_16[i] <- mean(personSample_list[[i]]) # compute the mean
    sd_for_size_16[i] <- sd(personSample_list[[i]]) # compute the standart deviation
    t1[[i]] <- t.value(personSample_list[[i]], population_mean) # (x¯ - µ)/(s/sqrt(n)) --> t value
}


#-------------------------------------------------------------------------------------------------
# We have created an empty list for samples
# We created samples with 50 groups of 6 into the list we created.
personSample_list_2 <- NULL 


for(i in 1:50){
    personSample_list_2[[i]] <- sample(persons_Heights,size = 6) #  For df = 5, size = 6
}

personSample_list_2
length(personSample_list_2[[1]])


sd_for_size_6 <- NA
mean_for_size_6 <- NA
t2 <- NA

for(i in 1:length(personSample_list_2)){
    mean_for_size_6[i] <- mean(personSample_list_2[[i]]) # compute the mean
    sd_for_size_6[i] <- sd(personSample_list_2[[i]]) # compute the standart deviation
    t2[[i]] <- t.value(personSample_list_2[[i]], population_mean) # (x¯ - µ)/(s/sqrt(n)) --> t value
}

# --------------------------------------------------------------------------------------------------------



t1;t2

# take the all t-values and sort
all_T_Values <- sort(c(t1,t2))
all_T_Values <- round(all_T_Values,3)
length(all_T_Values)
all_T_Values

# random variable calculation for values ??????on the y-axis of the chart
hx <- dnorm(all_T_Values)
hx
length(hx)

# Degrees of freedom
degf <- c(5, 15 , 30) # df = n - 1

# df = 5 --> red, df = 15 --> blue, df = 30 --> black
colors <- c("red",   "blue",  "black")
labels <- c("df = 5","df = 15","normal")

# drawing their normal curves
plot(all_T_Values, hx, type = "l", lty = 2,xlim=c(-4,4), xlab = "x value",
     ylab = "Density", main = "Comparison of t Distributions")

# Plotting curves of t-values
for(i in 1:3){
    lines(all_T_Values, dt(all_T_Values,degf[i]), lwd=2, col=colors[i])
}

# Distribution adjustments
legend("topright", inset=.05, title="Distributions",
       labels, lwd=2, lty=c(1, 1, 2), col=colors)

