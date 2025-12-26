# starting fresh
rm(list = ls())

#Get the data
# link location for the data
linkGit="https://github.com/FuntamendalsRedwan/demo1/raw/refs/heads/main/FSI-2023-DOWNLOAD.xlsx"

## variable that holds the data from the file
library(rio)
fragility23=rio::import(file = linkGit)

#Exploratory Commnads
## get column names
names(x = fragility23)

## check the data types
str(object = fragility23)

## first 10 rows
head(x = fragility23,10)

## last 10 rows
tail(fragility23,10)



#transformation commands
## make a subset of these columns
##    Country, Total, S1, P1, E2

## search for the fill names of the columns and put the full names into keep
keep=grep("Country|S1|P1|E2|Total",names(fragility23),fixed = F,value = T)
keep # print out the value

## make a subset that will only have the columns we want
frag23_sub=fragility23[,keep]


## shorten the names of the columns for ease of use
names(frag23_sub)[3:5]=c("S1","E2", "P1")

## sort by "E2" and print the top 10 counties data
tail(frag23_sub[order(x=-frag23_sub$E2),],10) 
## if you want to do the above but only the country names
tail(frag23_sub[order(x=-frag23_sub$E2),'Country'],10) 


#Some Computations
## give the statistical description of "frag23_sub"
## such as mean, median, mode, quartiles
summary(object = frag23_sub)

## the value of the worst quartile of Total
q3_Total=quantile(x = frag23_sub$Total, 
                  probs = 0.75, 
                  na.rm = TRUE)
q3_Total #print out the value

## find correlations between "S1","E2", "P1"
cor(x=frag23_sub[,-c(1,2)]) # exclude coutry and total

## show correlations between "S1","E2", "P1" and their "significance"
library(corrtable)
corrtable::correlation_matrix(df = frag23_sub[,-c(1,2)])

## regress P1 and E2 on S1
##do linear regression with S1 as dependent and P1 and E2 as independent vars
lm(S1~P1+E2,data=frag23_sub)
model <- lm(S1 ~ P1 + E2, data = frag23_sub) # save info into var
summary(model) #prints the details of the regession



# some plotting
## plot for the 'P1' variable
hist(x = frag23_sub$P1) 



## visual correlation between S1 and E2
plot(x=frag23_sub$S1, y=frag23_sub$E2)

## color points if country is on the worst quartile of Total
frag23_sub$Total>=q3_Total
## adds a column for worstQT
frag23_sub$worstQt=frag23_sub$Total>=q3_Total 

## use the new column to color the plot from before
plot(frag23_sub$S1, 
     frag23_sub$E2,pch=20,
     col = as.factor(frag23_sub$worstQt))


# visual of the regression P1 and E2 on S1
library(sjPlot)
plot_models(model)

