library(ggplot2)
library(dplyr)
library(lubridate) 
library(stringr)
library(tidyr)

#####データの読み込み#####
train <- read.csv("train.csv",header=T)
test <- read.csv("test.csv",header=T)



#####データの欠損値チェック#####
#トレーニングデータの欠損値チェック
any(is.na(train))

#テストデータにあるcategory_class以外の欠損値チェック
any(is.na(test[,-2]))



#####データの型変換#####
#category_classをfactor型へ変換
train$category_class <- as.factor(train$category_class)

#category_classをfactor型へ変換
train$item_id <- as.factor(train$item_id)
test$item_id <- as.factor(test$item_id)
train$item_tag_hash <- as.factor(train$item_tag_hash)
test$item_tag_hash <- as.factor(test$item_tag_hash)



#####データの中身確認、外れ値チェック#####
summary(train)
summary(test)


##########    トレーニングデータの分析・予測モデル構築   #############

#ターゲット変数の分布
train %>%
  count(category_class) %>%
  arrange(desc(n))

ggplot(train,aes(category_class)) +
  geom_bar(position="identity") +
  ggtitle("category_class")

#特徴量の分布
ggplot(train,aes(sold_price)) +
  geom_histogram(binwidth = 50) +
  ggtitle("sold_price")

ggplot(train,aes(price)) +
  geom_histogram(binwidth = 50) +
  ggtitle("price")

ggplot(train,aes(size)) +
  geom_histogram() +
  ggtitle("size")

ggplot(train,aes(area_name)) +
  geom_bar(position="identity") +
  ggtitle("area_name")

ggplot(train,aes(condition)) +
  geom_bar(position="identity") +
  ggtitle("condition")

#item_idがユニークなIDであることを確認
train %>%
  group_by(item_id) %>%
  summarize(n=n()) %>%
  arrange(desc(n))

#item_tag_hashはユニークではないことを確認
train %>%
  group_by(item_tag_hash) %>%
  summarize(n=n(),
            unique_n=n_distinct(item_id)) %>%
  arrange(desc(n))

#category_classとの相関関係
ggplot(train,aes(x=price,color=category_class,fill=category_class)) +
  geom_density(alpha=0.2) +
  ggtitle("price by category_class")

ggplot(train,aes(x=sold_price,color=category_class,fill=category_class)) +
  geom_density(alpha=0.2) +
  ggtitle("sold_price by category_class")

ggplot(train,aes(x=price,y=sold_price,shape=category_class,color=category_class)) +
  geom_point() +
  facet_wrap(~category_class) +
  ggtitle("price & sold_price by category_class")

ggplot(train,aes(x=size,color=category_class,fill=category_class)) +
  geom_density(alpha=0.2) +
  ggtitle("size by category_class")

ggplot(train,aes(x=area_name,fill=category_class)) +
  geom_bar(position="fill") +
  ggtitle("area_name by category_class")

ggplot(train,aes(x=condition,fill=category_class)) +
  geom_bar(position="fill") +
  ggtitle("condition by category_class")


#####特徴量の生成#####
#listing_atから時間の要素を抽出

train$time <- as.POSIXct(train$listing_at,orders='%Y-%m-%d %H:%M:%S')
train$time2 <- str_sub(train$listing_at,1,10)
#年
train$year <- year(train$time)
#月
train$month <- month(train$time)
#日
train$day <- days_in_month(train$time)
#曜日
train$weekday <- as.factor(wday(train$time))
#時間
train$hour <- hour(train$time)

#priceとsold_priceの差額、サイズ単位の価格を計算
train <-
  train %>%
  mutate(price_diff=sold_price - price,
         sold_price_per_size=sold_price/(size+1), #sizeに0がある為1を足す
         price_per_size=price/(size+1)) #sizeに0がある為1を足す

#新たに生成した特徴量とcategory_classの関係

train %>% 
  group_by(time2,category_class) %>%
  summarise(count=n()) %>%
  ggplot(aes(x=time2,y=count,group=category_class,color=category_class)) +
  geom_line() +
  ggtitle("listing_at by category_class")

ggplot(train,aes(as.factor(month),fill=category_class)) +
  geom_bar(position="fill") +
  ggtitle("listing_at(month) by category_class")

ggplot(train,aes(weekday,fill=category_class)) +
  geom_bar(position="fill") +
  ggtitle("listing_at(weekday) by category_class")

ggplot(train,aes(hour,fill=category_class)) +
  geom_bar(position="fill") +
  ggtitle("listing_at(hour) by category_class")

ggplot(train,aes(price_diff,fill=category_class)) +
  geom_histogram() +
  facet_wrap(~category_class) +
  ggtitle("sold_price - price by category_class")

ggplot(train,aes(sold_price_per_size,color=category_class,fill=category_class)) +
  geom_density(alpha=0.2) +
  ggtitle("sold_price per size by category_class")

ggplot(train,aes(price_per_size,color=category_class,fill=category_class)) +
  geom_density(alpha=0.2) +
  ggtitle("price per size by category_class")

train %>% 
  group_by(time2,category_class) %>%
  summarise(count=n()) %>%
  spread(category_class,count,fill=0) %>%
  View()

#item_tag_hashとcategory_classはほほ完全に対応している

hash <- train %>%
  group_by(item_tag_hash,category_class) %>%
  count(count=n()) %>%
  spread(category_class,count,fill=0) %>%
  as.data.frame() 

colnames(hash) <- c("item_tag_hash","unique_hash_num","c0","c1","c2","c3","c4")

#item_tag_hashとcategory_classが1対1に対応していないレコードを調査
#1対1に対応していないレコードは以下の6つhash値だけ
hash %>%
  filter(unique_hash_num != c0+c1+c2+c3+c4) %>%
  View()
  


#####モデル構築#####
#基本ステップ
#step0. モデル構築用データ60%と検証用データ40%に分離
#step1. モデル構築用データの中でitem_tag_hashとcategory_classの対応テーブルを構築
#       (その際に同一のitem_tag_hashで複数category_classがある場合（6レコード）は
#        最新日付のレコードにあるcategory_classを採用)

#step2. 検証用データのitem_tag_hashをstep1で作ったテーブルにマージ
#step3. step3で突合したレコードは予測値を上記テーブルから予測値（predict1）をセット
#step4. item_tag_hashを特徴量から外してモデルを構築
#step5. 検証用データに対してstep2で構築したモデルによる予測値(predict2)をセット
#step6. 最終的な予測値（predict）を以下の通り作成
#       predict1が欠損値ではない場合はpredict1をpredictにセット
#       predict1が欠損値である場合はpredict2をpredictにセット

#step0
#モデル構築用データ（60%）と検証用データ（40%）の分離
set.seed(20180619)　#幸運の数字をシード値に設定
index <- sample(nrow(train),nrow(train)*0.6)
train_training <- train[index,]
train_varidate <- train[-index,]


#step1
#ハッシュ値とcategory_classの対応テーブルを作成
hash_table_training <- train_training %>%
  arrange(item_tag_hash,desc(time)) %>% #hash値が同一なら日付(time)が最新を優先
  distinct(item_tag_hash,.keep_all=TRUE) %>%
  select(item_tag_hash,category_class) %>%
  rename(predict1=category_class)


#step2〜3
#item_tag_hashテーブルとマージ
train_varidate <- train_varidate %>%
  left_join(hash_table_training,by="item_tag_hash")

#マージ結果を確認
#マージしなかった件数は280件中の30件
summary(train_varidate$predict1)


#step4
#不要な変数を削除
train_training <- train_training %>%
  select(-item_id,-item_tag_hash,-listing_at,-time,-time2,-year)

#ランダムフォレストモデル
library(randomForest)

#最適なパラメータを探索
tuneRF(train_training[,-1],train_training[,1],doBest=T)

#モデル構築用データでモデル構築
rf.model<-randomForest(formula = category_class ~ . , data=train_training,
                       mtry=3,ntree=500)

#検証用データにモデルを適用
train_varidate$predict2 <- predict(rf.model,train_varidate,type="class")

#データの中身確認
train_varidate %>%
  select(category_class,predict1,predict2) %>%
  View()

#混合マトリクス作成
#モデル構築用サンプル
(matrix_training <- rf.model$confusion[,-6])


#step5
#最終予測結果をセット
train_varidate <- train_varidate %>%
  mutate(predict = if_else(is.na(predict1),predict2,predict1))

#実績値と予測値のconfusion matrixを作る
(matrix_varidate <- table(train_varidate$category_class,train_varidate$predict))

#Mean F1 Score算出
#F1 = 2 * (precision * recall) / (precision + recall)

precision <- rep(NA,5)
recall <- rep(NA,5)
f1_score <- rep(NA,5)

#category_classの0〜4まで5つのカテゴリ毎にF1スコアを算出する
for (i in 1:5) {
  precision[i] <- matrix_varidate[[i,i]]/sum(matrix_varidate[,i])
  recall[i] <- matrix_varidate[[i,i]]/sum(matrix_varidate[i,])
  f1_score[i] <- 2* (precision[i] * recall[i]) / (precision[i] + recall[i])
}

#category_classの0〜4まで5つのカテゴリのF1スコアの単純平均値を計算する
#以下の計算をした結果、トレーニングデータのMean F1 Scoreは0.833793と計算された
mean_f1_score <- mean(f1_score)



#################################  予測   ####################################


#####testデータの中身チェック
#####trainデータとtestデータに同一のitem_idとitem_tag_hashが含まれているか#####
test %>%
  inner_join(train,by="item_id") %>%
  count()

test %>%
  inner_join(train,by="item_tag_hash") %>%
  count()

#item_tag_hashがマッチしたデータを確認
test %>%
  inner_join(train,by="item_tag_hash") %>%
  arrange(item_tag_hash) %>%
  View()


#トレーニングデータ全量からitem_tag_hashとcategory_classのテーブルを作成
hash_table <- train %>%
  arrange(item_tag_hash,desc(time)) %>% #hash値が同一なら日付(time)が最新を優先
  distinct(item_tag_hash,.keep_all=TRUE) %>%
  select(item_tag_hash,category_class) %>%
  rename(predict1=category_class)


test <- test %>%
  left_join(hash_table_training,by="item_tag_hash")

#マージ結果を確認
#マージしなかった件数は300件中の35件
summary(test$predict1)

#特徴量を生成
#listing_atから時間の要素を抽出

test$time <- as.POSIXct(test$listing_at,orders='%Y-%m-%d %H:%M:%S')
test$year <- year(test$time)
test$month <- month(test$time)
test$day <- days_in_month(test$time)
test$weekday <- as.factor(wday(test$time))
test$hour <- hour(test$time)

#priceとsold_priceの差額、サイズ単位の価格を計算
test <-
  test %>%
  mutate(price_diff=sold_price - price,
         sold_price_per_size=sold_price/(size+1), #sizeに0がある為1を足す
         price_per_size=price/(size+1)) #sizeに0がある為1を足す

#不要な変数を削除
test <- test %>%
  select(-item_tag_hash,-listing_at,-time,-year)

#予測モデルを適用
test$predict2 <- predict(rf.model,test,type="class")

#最終予測結果をセット
test <- test %>%
  mutate(predict = if_else(is.na(predict1),predict2,predict1))

#データの中身確認
test %>%
  select(item_id,predict1,predict2,predict) %>%
  View()

#提出用ファイルに整形
ml_kadai <- test %>%
  select(item_id,predict) %>%
  rename(category_class=predict) %>% 
  write.csv("ML_Kadai.csv",row.names = F)
