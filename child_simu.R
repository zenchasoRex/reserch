##幼児の行動データを用いた強化学習
# メモリ，グラフのクリア
rm(list=ls())
graphics.off()

# 描画のためのライブラリ読み込み
library(tidyverse)
library(gridExtra)

# 乱数のシードの設定 
set.seed(141)

#パラメータ数
nParam <- 2

#excelから行動データを取得
library("openxlsx")
child_data <- read.xlsx("171214_group1.xlsx", sheet = "1_ori")

#各状態における行動と報酬
#誰も発話していない
choice0 <- c()
reward0 <- c()
#周りが関係ある話
choice1 <- c()
reward1 <- c()
#周りが関係ない話
choice2 <- c()
reward2 <- c()
#周りがどちらも
choice3 <- c()
reward3 <- c()
#本人が関係ある話
choice4 <- c()
reward4 <- c()
#本人と周りが関係ある話
choice5 <- c()
reward5 <- c()
#本人が関係ある、周りが関係ない話
choice6 <- c()
reward6 <- c()
#本人が関係ある、周りがどちらも
choice7 <- c()
reward7 <- c()
#本人が関係ない話
choice8 <- c()
reward8 <- c()
#本人が関係ない、周りが関係ある話
choice9 <- c()
reward9 <- c()
#本人も周りも関係ない話
choice10 <- c()
reward10 <- c()
#本人は関係ない、周りはどちらも
choice11 <- c()
reward11 <- c()

#データから行動を取得(t=エクセル列)
for(t in 9:1335){
  
  #周りが発話しているかどうか（発話なし0, 関係ある発話1, 関係ない発話2, どちらも3)
  label_sub <- 0
  for(i in 2:7){
    #本人（Aさん2)以外
    if(i != 2){
      #発話をしている
      if(child_data[i,t] == 1){
        if(is.na(child_data[i+15,t]) || child_data[i+15,t] == ''){
          label_sub <- label_sub
        }
        #発話内容が関係ある話
        if(child_data[i+15,t] == 3 || child_data[i+15,t] == 4 || child_data[i+15,t] == 7){
          if(label_sub == 0){
            label_sub <- 1
          } else if(label_sub == 2){
            label_sub <- 3
          }
        }
        #発話内容が関係ない話
        else if((child_data[i+15,t] == 5) || (child_data[i+15,t] == 6) || (child_data[i+15,t] == 8)){
          if(label_sub == 0){
            label_sub <- 2
          } else if(label_sub == 1){
            label_sub <- 3
          }
        }
      }
    }
  }
  
  #報酬（発話してから４時刻先までの向けられた視線の数(A)）
  reward <- 0
  for(i in 9:14){
    #本人(9)以外
    if(i != 9){
      if(child_data[i,t+1] == 'a' || child_data[i,t+2] == 'a' || child_data[i,t+3] == 'a'){
        reward <- reward + 1
      }
    }
  }
  
  #ターゲット(A=2)は発話していない
  if(child_data[2,t] == 0){
    #周りも発言していない
    if(label_sub == 0){
      choice0 <- append(choice0, child_data[2,t+1])
      reward0 <- append(reward0,0)
    }
    #周りは関係ある話をしている
    else if(label_sub == 1){
      choice1 <- append(choice1, child_data[2,t+1])
      reward1 <- append(reward1,0)
    }
    #周りは関係ない話をしている
    else if(label_sub == 2){
      choice2 <- append(choice2, child_data[2,t+1])
      reward2 <- append(reward2,0)
    }
    #周りは関係ある話とない話をしている
    else if(label_sub == 3){
      choice3 <- append(choice3, child_data[2,t+1])
      reward3 <- append(reward3,0)
    }
  }
  
  #ターゲット(A=2)は発話している
  else if(child_data[2,t] == 1){
    #ターゲットA(視線17)は関係ある話をしている
    if((child_data[17,t] == 3) || (child_data[17,t] == 4) || (child_data[17,t] == 7)){
      #周りは発言していない
      if(label_sub == 0){
        choice4 <- append(choice4, child_data[2,t+1])
        reward4 <- append(reward4, reward)
      }
      #周りは関係ある話をしている
      else if(label_sub == 1){
        choice5 <- append(choice5, child_data[2,t+1])
        reward5 <- append(reward5, reward)
      }
      #周りは関係ない話をしている
      else if(label_sub == 2){
        choice6 <- append(choice6, child_data[2,t+1])
        reward6 <- append(reward6, reward)
      }
      #周りは関係ある話とない話をしている
      else if(label_sub == 3){
        choice7 <- append(choice7, child_data[2,t+1])
        reward7 <- append(reward7, reward)
      }
    }
    #ターゲットA(視線17)は関係ない話をしている
    else if((child_data[17,t] == 5) || (child_data[17,t] == 6) || (child_data[17,t] == 8)){
      #周りは発言していない
      if(label_sub == 0){
        choice8 <- append(choice8, child_data[2,t+1])
        reward8 <- append(reward8, reward)
      }
      #周りは関係ある話をしている
      else if(label_sub == 1){
        choice9 <- append(choice9, child_data[2,t+1])
        reward9 <- append(reward9, reward)
      }
      #周りは関係ない話をしている
      else if(label_sub == 2){
        choice10 <- append(choice10, child_data[2,t+1])
        reward10 <- append(reward10, reward)
      }
      #周りは関係ある話とない話をしている
      else if(label_sub == 3){
        choice11 <- append(choice11, child_data[2,t+1])
        reward11 <- append(reward11, reward)
      }
    }
  }
}



##幼児の行動データを用いた強化学習
# メモリ，グラフのクリア
rm(list=ls())
graphics.off()

# 描画のためのライブラリ読み込み
library(tidyverse)
library(gridExtra)

# 乱数のシードの設定 
set.seed(141)

#パラメータ数
nParam <- 2

#excelから行動データを取得
library("openxlsx")
child_data <- read.xlsx("171214_group1.xlsx", sheet = "1_ori")

#各状態における行動と報酬
#誰も発話していない
choice0 <- c()
reward0 <- c()
#周りが関係ある話
choice1 <- c()
reward1 <- c()
#周りが関係ない話
choice2 <- c()
reward2 <- c()
#周りがどちらも
choice3 <- c()
reward3 <- c()
#本人が関係ある話
choice4 <- c()
reward4 <- c()
#本人と周りが関係ある話
choice5 <- c()
reward5 <- c()
#本人が関係ある、周りが関係ない話
choice6 <- c()
reward6 <- c()
#本人が関係ある、周りがどちらも
choice7 <- c()
reward7 <- c()
#本人が関係ない話
choice8 <- c()
reward8 <- c()
#本人が関係ない、周りが関係ある話
choice9 <- c()
reward9 <- c()
#本人も周りも関係ない話
choice10 <- c()
reward10 <- c()
#本人は関係ない、周りはどちらも
choice11 <- c()
reward11 <- c()

#データから行動を取得(t=エクセル列)
for(t in 9:1335){
  
  #周りが発話しているかどうか（発話なし0, 関係ある発話1, 関係ない発話2, どちらも3)
  label_sub <- 0
  for(i in 2:7){
    #本人（Aさん2)以外
    if(i != 2){
      #発話をしている
      if(child_data[i,t] == 1){
        if(is.na(child_data[i+15,t]) || child_data[i+15,t] == ''){
          label_sub <- label_sub
        }
        #発話内容が関係ある話
        if(child_data[i+15,t] == 3 || child_data[i+15,t] == 4 || child_data[i+15,t] == 7){
          if(label_sub == 0){
            label_sub <- 1
          } else if(label_sub == 2){
            label_sub <- 3
          }
        }
        #発話内容が関係ない話
        else if((child_data[i+15,t] == 5) || (child_data[i+15,t] == 6) || (child_data[i+15,t] == 8)){
          if(label_sub == 0){
            label_sub <- 2
          } else if(label_sub == 1){
            label_sub <- 3
          }
        }
      }
    }
  }
  
  #報酬（発話してから４時刻先までの向けられた視線の数(A)）
  reward <- 0
  for(i in 9:14){
    #本人(9)以外
    if(i != 9){
      if(child_data[i,t+1] == 'a' || child_data[i,t+2] == 'a' || child_data[i,t+3] == 'a'){
        reward <- reward + 1
      }
    }
  }
  
  #ターゲット(A=2)は発話していない
  if(child_data[2,t] == 0){
    #周りも発言していない
    if(label_sub == 0){
      choice0 <- append(choice0, child_data[2,t+1])
      #発話すれば報酬を、発話しなければ報酬なし
      if(child_data[2,t+1] == 1){
        reward0 <- append(reward0,reward)
      } else {
        reward0 <- append(reward0,0)
      }
    }
    #周りは関係ある話をしている
    else if(label_sub == 1){
      choice1 <- append(choice1, child_data[2,t+1])
      if(child_data[2,t+1] == 1){
        reward1 <- append(reward1,reward)
      } else {
        reward1 <- append(reward1,0)
      }
    }
    #周りは関係ない話をしている
    else if(label_sub == 2){
      choice2 <- append(choice2, child_data[2,t+1])
      if(child_data[2,t+1] == 1){
        reward2 <- append(reward2,reward)
      } else {
        reward2 <- append(reward2,0)
      }
    }
    #周りは関係ある話とない話をしている
    else if(label_sub == 3){
      choice3 <- append(choice3, child_data[2,t+1])
      if(child_data[2,t+1] == 1){
        reward3 <- append(reward3,reward)
      } else {
        reward3 <- append(reward3,0)
      }
    }
  }
  
  #ターゲット(A=2)は発話している
  else if(child_data[2,t] == 1){
    #ターゲットA(視線17)は関係ある話をしている
    if((child_data[17,t] == 3) || (child_data[17,t] == 4) || (child_data[17,t] == 7)){
      #周りは発言していない
      if(label_sub == 0){
        choice4 <- append(choice4, child_data[2,t+1])
        if(child_data[2,t+1] == 1){
          reward4 <- append(reward4,reward)
        } else {
          reward4 <- append(reward4,0)
        }
      }
      #周りは関係ある話をしている
      else if(label_sub == 1){
        choice5 <- append(choice5, child_data[2,t+1])
        if(child_data[2,t+1] == 1){
          reward5 <- append(reward5,reward)
        } else {
          reward5 <- append(reward5,0)
        }
      }
      #周りは関係ない話をしている
      else if(label_sub == 2){
        choice6 <- append(choice6, child_data[2,t+1])
        if(child_data[2,t+1] == 1){
          reward6 <- append(reward6,reward)
        } else {
          reward6 <- append(reward6,0)
        }
      }
      #周りは関係ある話とない話をしている
      else if(label_sub == 3){
        choice7 <- append(choice7, child_data[2,t+1])
        if(child_data[2,t+1] == 1){
          reward7 <- append(reward7,reward)
        } else {
          reward7 <- append(reward7,0)
        }
      }
    }
    #ターゲットA(視線17)は関係ない話をしている
    else if((child_data[17,t] == 5) || (child_data[17,t] == 6) || (child_data[17,t] == 8)){
      #周りは発言していない
      if(label_sub == 0){
        choice8 <- append(choice8, child_data[2,t+1])
        if(child_data[2,t+1] == 1){
          reward8 <- append(reward8,reward)
        } else {
          reward8 <- append(reward8,0)
        }
      }
      #周りは関係ある話をしている
      else if(label_sub == 1){
        choice9 <- append(choice9, child_data[2,t+1])
        if(child_data[2,t+1] == 1){
          reward9 <- append(reward9,reward)
        } else {
          reward9 <- append(reward9,0)
        }
      }
      #周りは関係ない話をしている
      else if(label_sub == 2){
        choice10 <- append(choice10, child_data[2,t+1])
        if(child_data[2,t+1] == 1){
          reward10 <- append(reward10,reward)
        } else {
          reward10 <- append(reward10,0)
        }
      }
      #周りは関係ある話とない話をしている
      else if(label_sub == 3){
        choice11 <- append(choice11, child_data[2,t+1])
        if(child_data[2,t+1] == 1){
          reward11 <- append(reward11,reward)
        } else {
          reward11 <- append(reward11,0)
        }
      }
    }
  }
}



#フィットするモデルの設定-------------------------------------------

#Q-learning
func_qlearning <- function(param, data, prior = NULL)
{
 
  alpha <- param[1]
  beta <- param[2]
  c <- data$choice
  r <- data$reward
  T <- length(c)
  
  pA <- numeric(T)
  
  #Q値の初期化（選択肢の数×T）
  Q <- matrix(numeric(2*T), nrow=2, ncol=T)
  
  #対数尤度を格納する変数
  ll <- 0
  
  for(t in 1:T){
    
    #ソフトマックスで選択肢（発言する）の選択確率を決定する
    pA[t] <- 1/(1+exp(-beta * (Q[1,t]-Q[2,t])))
    
    # 試行tの対数尤度は実際の選択が発言する (c=1) であれば log(pA[t]), 
    # 発言しない (c=0) であればlog(1 - pA[t]) となる
    ll <- ll + (c[t]==0) * log(pA[t]) +  (c[t]==1) * log(1-pA[t])
    
    
    # 行動価値の更新
    if (t < T) {
      
      Q[c[t]+1,t+1] <- Q[c[t]+1,t] + alpha * (r[t] - Q[c[t]+1,t]) 
      
      # 選択肢していない行動の価値はそのままの値を次の時刻に引き継ぐ
      Q[2-c[t],t+1] <- Q[2-c[t],t]
    }
  }
  
  # 対数事後確率密度
  if (is.null(prior)) {
    lprior <- 0
  } else {
    lprior <- dbeta(alpha,prior$alpha_a, prior$alpha_b,log = T) + 
      dgamma(beta,shape=prior$beta_shape, scale=prior$beta_scale,log = T) 
  }
  
  return(list(negll = -ll - lprior,Q = Q, pA = pA))
}


# パラメータ推定のための関数 -----------------------------------------------------------

#　負の対数尤度のみを返すラッパー関数
func_minimize <- function(modelfunc, param, data, prior)
{
  ret <- modelfunc(param, data, prior)
  
  # 負の対数尤度のみを返す
  return(ret$negll)
}

# 最尤推定 
paramfitML <- function(modelfunction, data, nParam)
{
  
  fvalmin <- Inf
  
  for (idx in 1:10) {
    
    initparam <- runif(nParam, 0, 1.0)
    
    res <- optim(initparam, func_minimize,
                 hessian = TRUE, modelfunc = modelfunction, 
                 data=data, prior = NULL)
    
    if (res$value < fvalmin) {
      paramest <- res$par
      fvalmin <- res$value
    }
  }
  aic <- 2*fvalmin + 2 * nParam
  bic <- 2*fvalmin + nParam * log(T)
  negll <- fvalmin
  paramlist <- paramest
  
  print(sprintf("Estimated value: %.2f", paramest))
  print(sprintf("log-likelihood: %.2f, AIC: %.2f, BIC: %.2f", 
                negll, 
                aic,
                bic))
  return(list(negll = negll, aic = aic, bic = bic, paramest = paramest))
}

choice4 <- as.numeric(choice4)
data <- list(reward=reward4, choice=choice4)

# 最尤推定 --------------------------------------------------------------------
cat("----------- ML -----------\n")
resultML <- paramfitML(func_qlearning, data, nParam)


# # 最適化により最小化する負の対数尤度を返すラッパー関数---------------
# func_minimize <- function(modelfunc, param, choice, reward)
# {
#   ret <- modelfunc(param, choice, reward)
#   
#   # 負の対数尤度のみ返す
#   return(ret$negll)
# }
# 
# 
# # 非線形最適化による最尤推定 ----------------------------------------
# 
# # 負の対数尤度の最小値を格納する変数 (最初は無限大にしておく)  
# fvalmin <- Inf
# 
# for (idx in 1:10) {
#   
#   # 初期値を一様乱数から決める
#   initparam <- runif(2, 0, 1.0)
#   
#   # 最適化の実行
#   res <- optim(initparam, func_minimize,
#                hessian = TRUE, 
#                modelfunc = func_qlearning, 
#                choice=choice0, reward=reward0)
#   
#   # 今までの解より負の対数尤度が小さかったらその結果を採用する
#   if (res$value < fvalmin) {
#     paramest <- res$par
#     fvalmin <- res$value
#   }
# }
# 
# print(sprintf("Estimated value: %.2f", paramest[1]))
# print(sprintf("Estimated value: %.2f", paramest[2]))
# print(sprintf("Model 1: log-likelihood: %.2f, AIC: %.2f", -fvalmin, 2*fvalmin + 2*2))
# 

