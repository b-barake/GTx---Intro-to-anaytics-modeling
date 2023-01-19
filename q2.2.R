## QUESTION 2.2


# PART 1-2

rm(list = ls()) # This code clears the environment
getwd() # checking current directory in order to add the correct filepath

data = read.table("data/credit_card_data.txt",header = FALSE)
#data = as.matrix(data) # Converting table to matrix format to run the model
install.packages('kernlab') # Installing package to use ksvm

pacman :: p_load(pacman,kernlab,kknn,caret) # Loading the needed library

# Creating list to iterate through different values of C
c_list = list(1,10,50,100,150,200,300)
score_ksvm = list()
for (i in seq_along(c_list)) {
  model_ksvm <- ksvm(as.matrix(data[,1:10]),as.factor(data[,11]),type="C-svc",kernel="vanilladot",C=c_list[i],scaled=TRUE)
  # calculate a1…am
  a <- colSums(model_ksvm@xmatrix[[1]] * model_ksvm@coef[[1]])
  a
  # calculate a0
  a0 <- model_ksvm@b
  a0
  # see what the model predicts
  pred <- predict(model_ksvm,data[,1:10])
  pred
  # see what fraction of the model’s predictions match the actual classification
  score_ksvm[i] = sum(pred == data[,11]) / nrow(data)
  }

# To easily visualize the results, we can do a basic plot
plot(unlist(c_list),unlist(score_ksvm))

# Comments about the results:
# From the plot we see that C<=200 we get the highest accurracy levels, and after that it decreases

# PART 3

#Splitting data to training and testing datasets
train_idx = createDataPartition(y=data$V11,p=0.70,list=FALSE)

#Filtering through the data using the random split 'data_split'
train_d = data[train_idx,]
test_d = data[-train_idx,]

k_list = list(1,10,20,30,40,50)
score_kknn = list()
for (i in seq_along(k_list)){
  #Creating the model
  model_kknn = kknn(V11~.,train_d,test_d,k=k_list[[i]],kernel = 'gaussian',scale=TRUE)
  #What the model predicted
  pred_kknn = as.integer(fitted(model_kknn))
  #How accurate was the model
  score_kknn[i] = sum(pred_kknn == test_d$V11)/nrow(test_d)
}

plot(unlist(k_list),unlist(score_kknn))

#Comments on plot:
# We see a very clear trend between the value of k and the accuracy of the model
# The higher the k-score the lower the accuracy

