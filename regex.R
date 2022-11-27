#1

download.file("https://d396qusza40orc.cloudfront.net/getdata%2Fdata%2Fss06hid.csv", "hid.csv", method="curl") 
df <- read.csv("hid.csv")
library(dplyr)
tb<- tbl_df(df)
agricultureLogical <- tb %>%
  mutate(row=row_number()) %>%
  select(row,ACR,AGS) %>%
  filter(ACR == 3, AGS == 6)

#2
download.file("https://d396qusza40orc.cloudfront.net/getdata%2Fjeff.jpg", "jeff.jpg", method="curl") 
install.packages("jpeg")  ## if necessary
library(jpeg)
jp <- readJPEG("jeff.jpg", native = TRUE)

q = c(.3, .8)
quantile(jp,q)


#3
download.file("https://d396qusza40orc.cloudfront.net/getdata%2Fdata%2FGDP.csv",
              "gdp.csv",method="curl")
df <- read.csv("gdp.csv")
gdp <- tbl_df(df)

download.file("https://d396qusza40orc.cloudfront.net/getdata%2Fdata%2FEDSTATS_Country.csv",
              "edu.csv",method="curl")
df <- read.csv("edu.csv")
edu <- tbl_df(df)

View(gdp)
View(edu)

a <- names(gdp)
a[1]="CountryCode"
names(gdp) <- a
df <-left_join(gdp, edu,by="CountryCode")
View(df)
df %>% arrange(desc(X.3))
df[13,]


#4
df1 <- df %>%  select(CountryCode,X.3,Income.Group, Gross.domestic.product.2012) %>% 
         filter(nchar(CountryCode)>=3) %>% 
         filter(nchar(Gross.domestic.product.2012)>=1) %>%
         filter(X.3 != "..")  %>% 
         filter(X.3 != "") %>% 
         group_by(Income.Group)
df1$X.3 <- as.numeric(gsub(",", "", df1$X.3))
df1$Gross.domestic.product.2012 <- as.numeric(gsub(",", "", df1$Gross.domestic.product.2012))
View(df1) 
summarise(df1, m=mean(as.numeric(Gross.domestic.product.2012)))


#5
names(df1) <- c("CountryCode", "GDP", "Income.Group", "Ranking")
head(df1)
probs=c(0.2, 0.4, 0.6, 0.8, 1)
a <- df1 %>% group_by(Income.Group) %>%
             summarize(quantile(Ranking, probs))

df1 %>%  filter(Ranking <= 38, Income.Group == "Lower middle income")





df <- data.frame(team=c('A', 'A', 'A', 'A', 'A', 'A', 'A', 'A',
                        'B', 'B', 'B', 'B', 'B', 'B', 'B', 'B',
                        'C', 'C', 'C', 'C', 'C', 'C', 'C', 'C'),
                 wins=c(2, 4, 4, 5, 7, 9, 13, 13, 15, 15, 14, 13,
                        11, 9, 9, 8, 8, 16, 19, 21, 24, 20, 19, 18))
q = c(.25, .5, .75)

df %>%
  group_by(team) %>%
  summarize(quant25 = quantile(wins, probs = q[1]), 
            quant50 = quantile(wins, probs = q[2]),
            quant75 = quantile(wins, probs = q[3]))


df1 <- data.frame(Q1 = c('a', 'b', 'c', 'd', 'e', 'f'),
                  Q2 = c(152, 514, 114, 218, 322, 323))
df2 <- data.frame(Q1 = c('a', 'a', 'a', 'b', 'b', 'b'),
                  Q3 = c(523, 324, 233, 134, 237, 141))
df3 <- data.frame(Q1 = c('P1', 'e', 'P2', 'g', 'P5', 'i'),
                  Q4 = c(323, 224, 333, 324, 237, 441))

df1 %>%
  left_join(df2, by='Q1') %>%  left_join(df3, by='Q1')

df1 %>%
  full_join(df2, by='Q1') %>%  full_join(df3, by='Q1')


df<-data.frame(x=c(2,13,5,36,12,50),
               y=c('a','b','c','c','c','b'))
# create groups
# calculate quantiles by group
df %>% group_by(y) %>%
  summarize(res=quantile(x,probs=0.5))

df9<-data.frame(x=c(2,95,5,36,12,95),
               y=c('a','b','c','c','c','b'))
# create groups
# find quantiles
df9 %>% group_by(y) %>%
  summarize(first=quantile(x,probs=0.2),
            second=quantile(x,probs=0.4),
            third=quantile(x,probs=0.6),
            fourth=quantile(x,probs=0.8),
            fifth=quantile(x,probs=1))

x <- c("d", "a", "c", "abba") 
grep("a", x)    
grepl("a", x) 
x[grepl("a", x)]



