setwd("C:/Kapil/School/12th Grade/Computer Science/Rprog")
mydata <- read.table("hw1_data.csv", header = TRUE, sep = ",")

# 11)
colnames(mydata)

# 12)
mydata[1:2, 1:length(mydata)]

# 13)
n <- dim(mydata)[1]
n
# We take the first element of the dim(mydata) vector

# 14)
mydata[(n - 1): n, 1:length(mydata)]

# 15)
mydata[47, "Ozone"]

# 16)
ozone_col <- mydata["Ozone"]
sum(is.na(ozone_col))
# We first run is.na on ozone_col. 
# Then we use the fact that TRUE = 1 and FALSE = 0 
# to determine the number of NA's.

# 17)
mean(ozone_col[!is.na(ozone_col)])

# 18)
temp_col <- mydata["Temp"]
extraction <- mydata[ozone_col > 31 & !is.na(ozone_col) & temp_col > 90 & !is.na(temp_col), 1:length(mydata)]
meman(t(extraction["Solar.R"]))
# We compose the extraction matrix equal from the rows of mydata 
# that satisfy the desired conditions.
# I don't know why, but R won't let me compute the mean 
# unless I transpose extraction["Solar.R"].

# 19)
month_col <- mydata["Month"]
june_matrix <- mydata[month_col == 6, 1:length(mydata)]
mean(t(june_matrix["Temp"]))
# Once again, I don't know why I have to transpose the matrix.

# 20)
may_matrix <- mydata[month_col == 5, 1:length(mydata)]
max(may_matrix["Ozone"][!is.na(may_matrix["Ozone"])])
# We extract the non-NA values in the "Ozone" column of may_matrix.
# Then we determine the maximum of these values.

