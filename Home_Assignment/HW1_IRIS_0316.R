normalize <- function(x) {
  return ((x - min(x)) / (max(x) - min(x)))
}

library(class) # knn ?˰���?? ????�� ��??
library(gmodels) # Cross table ????�� ��??

data(iris)
View(iris)
# summary(iris)

# # ?ٺ??? ???迡 ?????? / Fisher F????
# str(iris)
# table(iris)
# summary(iris)
# round(prop.table(table(iris$Species)) * 100, digits = 1)

is.data.frame(iris[1:3, 1:3])
iris_n <- as.data.frame(lapply(iris[1:4], normalize)) # normalize?Լ??? ?????? ��??ȭ
iris_z <- as.data.frame(scale(iris[1:4])) # z-score  ?Լ??? ?????? ��??ȭ
# summary(iris_n)
# summary(iris_z)


opt_k_n <- 0 # ?????? k?? ?ʱ?ȭ (normalize)
opt_k_z <- 0 # ?????? k?? ?ʱ?ȭ (z-score)
lowest_mispredict_avg_n <- 1 # ???Ǵܷ? 100%?? ?ʱ?ȭ (normalize)
lowest_mispredict_avg_z <- 1 # ???Ǵܷ? 100%?? ?ʱ?ȭ (z-score)

N <- 100 # ???? ???Ǵܷ?�� ????��?? ?ݺ?Ƚ?? 100ȸ

# ?Ǵ? ?????? ?????? ?迭
arrname = list(c("Case1_n","Case2_n","Case3_n","Case1_z","Case2_z","Case3_z"),c("k1","k2","k3","k4","k5","k6","k7","k8","k9","k10","k11","k12","k13","k14","k15","k16","k17","k18","k19","k20","k21","k22","k23","k24","k25","k26","k27","k28","k29","k30"))
result_arr<-array(0, c(6,30),dimnames = arrname)

for(nc in 0:2){ # ��???? ?????? ?????? train??Ű?? ��?? ?ݺ???
  # nc : number change (33 33 34 ????)
  
  setosa_tnum <- 33  + (nc%%3)%/%2
  versicolor_tnum <- 33  + ((nc+1)%%3)%/%2
  virginica_tnum <- 33  + ((nc+2)%%3)%/%2
  
  log <- capture.output( # ???ʿ??? ?ܼ?â ?ݺ?????�� ???? ?ý??? ???ϸ? ?????ϱ? ��??
    for(kval in 5){ # k?? 1~30 ??ȭ 
      
      mispredict_sum_n <- 0 # ????�� ?ݺ? ???? ?ʱ?ȭ
      mispredict_sum_z <- 0 # ????�� ?ݺ? ???? ?ʱ?ȭ
      
      for(i in 1:N){ # ???? ????���� ???ϱ? ��?? ?ݺ? Ƚ??
        samp <- c(sample(c(1:50), setosa_tnum),
                  sample(c(51:100), versicolor_tnum),
                  sample(c(101:150), virginica_tnum)) 
        # ?? ��?????? ?յ??ϰ? ?????ϰ? 100?? ?̱? (33??, 33??, 34??)
        
        iris_train_n <- iris_n[-samp, ] # train data set (normalize)
        iris_test_n <- iris_n[samp, ]   # test  data set (normalize)
        
        
        iris_train_z <- iris_z[-samp, ] # train data set (z-score)
        iris_test_z <- iris_z[samp, ]   # test  data set (z-score)
        iris_train_labels <- iris[-samp, 5] # train data set?? Speices label
        iris_test_labels <- iris[samp, 5]   # test  data set?? Speices label
        
        iris_test_pred_n <- knn(train = iris_train_n, test = iris_test_n,
                              cl = iris_train_labels, k=kval)
        iris_test_pred_z <- knn(train = iris_train_z, test = iris_test_z,
                                cl = iris_train_labels, k=kval)
        
        ct_n <- CrossTable(x = iris_test_labels, y = iris_test_pred_n,
                         prop.chisq=FALSE)
        ct_z <- CrossTable(x = iris_test_labels, y = iris_test_pred_z,
                           prop.chisq=FALSE)
        
        mispredict_sum_n <- mispredict_sum_n + 
          sum(ct_n$prop.tbl[1:9])-(ct_n$prop.tbl[1,1]+ct_n$prop.tbl[2,2]+ct_n$prop.tbl[3,3])
        
        mispredict_sum_z <- mispredict_sum_z + 
          sum(ct_z$prop.tbl[1:9])-(ct_z$prop.tbl[1,1]+ct_z$prop.tbl[2,2]+ct_z$prop.tbl[3,3])
      }
      mispredict_avg_n <- mispredict_sum_n/N
      mispredict_avg_z <- mispredict_sum_z/N
      if (mispredict_avg_n < lowest_mispredict_avg_n){
        lowest_mispredict_avg_n <- mispredict_avg_n
        opt_k_n <- kval
      }
      if (mispredict_avg_z < lowest_mispredict_avg_z){
        lowest_mispredict_avg_z <- mispredict_avg_z
        opt_k_z <- kval
      }
      
      result_arr[nc+1,kval] = 1 - mispredict_avg_n
      result_arr[nc+4,kval] = 1 - mispredict_avg_z
    }
  )
  train_eachnum <- cbind(setosa_tnum,versicolor_tnum,virginica_tnum,setosa_tnum+versicolor_tnum+virginica_tnum)
  rownames(train_eachnum) <- c("train num")
  colnames(train_eachnum) <- c("setosa","versicolor","virginica","total")
  result <- rbind(c(opt_k_n, lowest_mispredict_avg_n),c(opt_k_z, lowest_mispredict_avg_z))
  rownames(result) <- c("normalize","z-score")
  colnames(result) <- c("Optimal k","mispredict average (%)")
  print("---------------------------------------------------------------------")
  print(train_eachnum)
  print(result)
}

result 

k_list = 1:30
plot(k_list,result_arr[1,1:30], type="o", col="blue")
lines(k_list,result_arr[2,1:30], type="o", col="red")
lines(k_list,result_arr[3,1:30], type="o", col="green")


iris$dis <- sqrt ((5.1-iris$Sepal.Length)^2+
                  (3.5-iris$Sepal.Width)^2+
                  (1.4-iris$Petal.Length)^2+
                  (0.2-iris$Petal.Width)^2)
