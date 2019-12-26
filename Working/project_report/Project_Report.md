---
title: "백화점 유인 효과 분석"
subtitle: "기상 조건을 중심으로"
author: "HAPPY BEAN"
date: '2019 12 26'
output: html_document
editor_options: 
  chunk_output_type: console
---





***
## 프로젝트 정의

- 미세먼지 비상저감 조치 발령된 2019년 3월, 롯데 백화점 매출 9.1%, 구매 고객 18.8% 증가

- 과연 미세먼지가 많은 날 사람들은 백화점을 더 많이 갈까? 많이 간다면 어느 정도 많이 가는가?

- 미세먼지와 초미세먼지에 대한 반응이 다를까?

- 먼지 이외의 다양한 기상 현상들에 대해서는 어떻게 반응할까?



***
## 수집 데이터

자료 제공처 | 수집 데이터 
------ | ------
서울열린데이터광장 | 서울 생활인구 내국인
 | 일별 평균 대기오염도
 | 강수량 및 강우일수 통계
 | 지하철 역별 승하차 인원 정보
 | 구별 용지 비율
 | 구별 인구 밀도
 | 구별 유통업체 현황
기상청 날씨마루 | 시간별 강수량, 풍량, 풍속, 적설량
통계청 | 전국 집계 구역
주요 3사 백화점 | 롯데, 현대, 신세계 백화점 주소 (25개소)


종류 | 기간
------ | ------
종점 기준 | 19년 11월 30일
생활 인구 | 17년 1월 1일부터 종점 기준까지
백화점 주소 | 2019년 11월 30일 기준
백화점 면적 | 2018년 기준
용지 면적 | 2018년 기준
인구 밀도 | 2018년 기준
지하철 역별 하차 인원 | 17년 1월 1일부터 종점 기준까지
일평균 대기 오염 | 17년 1월 1일부터 종점 기준까지
일평균 기상 현황 | 17년 1월 1일부터 종점 기준까지



***
## 데이터 설명 및 전처리
### 내국인 생활 인구 데이터

- KT와 서울시에서 제공

- 19,153개의 집계 구역의 1시간 단위 측정

- KT LTE 가입자 신호 위치 기반 **추정**

- 백화점 포함 집계 구역의 생활 인구를 유동 인구로 가정

- 출근, 거주 인구 등의 노이즈를 최소화하기 위해 주말 백화점 개점 시간만 분석



## 서울 소재 백화점

<img src="./departments.jpeg" title="plot of chunk images" alt="plot of chunk images" width="100%" />



## 분석 대상 백화점 선정
### 국내 백화점 상위 3사

- 배제 기준: 대중교통시설 포함 집계 구역 (기차, 공항, 버스)
- 하나의 집계구에 여러 백화점이 있는 경우는 총 면적 합산
- 최종 분석 대상: 16개 집계 구역 식별

사명 | 지점명
------ | ------
신세계 | 영등포
현대 | 디큐브시티, 천호, 무역센터(유플렉스), 압구정, 신촌(유플렉스), 목동(유플렉스), 미아
롯데 | 잠실, 명동/본점/에비뉴엘, 노원, 강남, 스타시티, 관악, 에비뉴엘월드타워, 미아



### 일별 평균 대기오염도

- 구별 대기 측정소에서 일별 대기 오염도 측정(이산화탄소, 오존 등의 수치는 제외)

- 백화점이 속해 있는 구를 기준으로 미세먼지 수치 정의

- 미세먼지(PM10) 수치와 초미세먼지(PM2.5) 수치 (단위: ㎍/m³)

- WHO 미세먼지/초미세먼지 8단계 기준 적용

- 미세먼지 혹은 초미세먼지 5단계 (나쁨) 이상이면 먼지 많은 날로 정의



### 강수량 및 기상 조건

- 1시간 단위로 측정된 데이터는 일평균으로 계산

- 종로구 송월동에 있는 서울기상관측소 측정값 기준

- 강수량, 풍속, 적설량 등 포함

- 일평균 강수량이 5mm 이상이면 비오는 날로 정의



### 지하철 역별 승하차 인원 정보

- 백화점 상권의 대중교통 시설 발전 정도를 통제하기 위해 하차 인원을 추가

- 백화점 홈페이지의 오시는 길에 등장하는 지하철 역 중 도보 최단 시간 기준 하차 인원



### 구별 용지 비율 및 인구 밀도

- 백화점이 속한 구의 속성 정보를 추가

- 해당 구의 주거지, 상업지, 녹지의 비율, 인구 밀도(명/㎢) 



### 구별 유통업체 현황

- 백화점 규모에 따른 유동 인구 영향을 고려하기 위해 추가

- 하나의 집계 구역에 여러 백화점이 포함되는 경우, 합산하여 처리



***
## 변수 설정

**종속변수: 백화점이 있는 집계 구역의 일평균 생활 인구(주말/개점 시간)**



**독립변수**

구분 | 설명 | 변수명
----- |------ | -----
백화점 요소 | 백화점 총 면적(m²) | size
지역 환경 요소 | 지하철역 하차 인원(명) | arrival
 | 주거지 비율(%) | residential_area
 | 상업지 비율(%) | commercial_area
 | 녹지 비율(%) | green_area
 | 인구 밀도(명/㎢) | pop_density
기상 조건 요소 | 일평균 미세먼지(㎍/m³) | fine_dust
 | 일평균 초미세먼지(㎍/m³) | hyper_dust
 | 일평균 강수량(mm) | mean_precipitation
 | 일평균 기온(°C) | mean_temperature
 | 일평균 풍속(m/s) | mean_wind
 | 일평균 적설량(cm) | mean_snow
 | 미세먼지 등급 | fine_dust_grade
 | 초미세먼지 등급 | hyper_dust_grade
 | 먼지 많은 날 | IsDustyDay
 | 비오는 날 | IsRainyDay



***

## 




```r
summary(df)
```

```
##       date                     code     
##  Min.   :20170101   1102052020001: 268  
##  1st Qu.:20170824   1105066011201: 268  
##  Median :20180666   1108068010004: 268  
##  Mean   :20180091   1109071040007: 268  
##  3rd Qu.:20190308   1111066030001: 268  
##  Max.   :20191130   1113075030010: 268  
##                     (Other)      :2680  
##     mean_pop          size       
##  Min.   :   76   Min.   : 18578  
##  1st Qu.: 3540   1st Qu.: 70475  
##  Median : 7681   Median : 90163  
##  Mean   :10442   Mean   :160502  
##  3rd Qu.:13949   3rd Qu.:147194  
##  Max.   :54512   Max.   :807686  
##                                  
##   subway_code     residential_area
##  Min.   : 202.0   Min.   :0.3290  
##  1st Qu.: 218.2   1st Qu.:0.4957  
##  Median : 283.0   Median :0.6102  
##  Mean   : 747.4   Mean   :0.5769  
##  3rd Qu.: 568.0   3rd Qu.:0.6207  
##  Max.   :2548.0   Max.   :0.8730  
##                                   
##  commercial_area     green_area     
##  Min.   :0.01144   Min.   :0.00252  
##  1st Qu.:0.01532   1st Qu.:0.25739  
##  Median :0.03688   Median :0.32457  
##  Mean   :0.05977   Mean   :0.32923  
##  3rd Qu.:0.05247   3rd Qu.:0.38527  
##  Max.   :0.39218   Max.   :0.59070  
##                                     
##   pop_density       arrival      
##  Min.   :13618   Min.   :  1024  
##  1st Qu.:16198   1st Qu.: 18762  
##  Median :18218   Median : 29984  
##  Mean   :18258   Mean   : 35142  
##  3rd Qu.:19883   3rd Qu.: 49964  
##  Max.   :26894   Max.   :143134  
##                                  
##     district      fine_dust     
##  강남구 : 804   Min.   :  3.00  
##  강북구 : 536   1st Qu.: 23.00  
##  송파구 : 536   Median : 36.00  
##  강동구 : 268   Mean   : 40.92  
##  관악구 : 268   3rd Qu.: 54.00  
##  광진구 : 268   Max.   :243.00  
##  (Other):1608                   
##    hyper_dust     fine_dust_grade
##  Min.   :  1.00   Min.   :1.000  
##  1st Qu.: 12.00   1st Qu.:2.000  
##  Median : 20.00   Median :3.000  
##  Mean   : 24.05   Mean   :3.312  
##  3rd Qu.: 31.00   3rd Qu.:5.000  
##  Max.   :106.00   Max.   :8.000  
##                                  
##  hyper_dust_grade IsDustyDay   mean_temp      
##  Min.   :1.000    0:2589     Min.   :-10.429  
##  1st Qu.:2.000    1:1699     1st Qu.:  4.749  
##  Median :3.000               Median : 15.992  
##  Mean   :3.615               Mean   : 13.896  
##  3rd Qu.:5.000               3rd Qu.: 23.309  
##  Max.   :8.000               Max.   : 31.550  
##                                               
##   mean_precipi      mean_wind     
##  Min.   : 0.000   Min.   :0.7833  
##  1st Qu.: 0.000   1st Qu.:1.4698  
##  Median : 0.000   Median :1.8104  
##  Mean   : 0.394   Mean   :1.9320  
##  3rd Qu.: 0.100   3rd Qu.:2.3021  
##  Max.   :12.136   Max.   :3.8500  
##                                   
##    mean_snow       IsRainyDay
##  Min.   :0.00000   0:3840    
##  1st Qu.:0.00000   1: 448    
##  Median :0.00000             
##  Mean   :0.08191             
##  3rd Qu.:0.00000             
##  Max.   :4.62941             
## 
```



***

## 모델 정의 및 검증
### 일평균 유동 인구

정규화를 위해 log 변환을 실시

![plot of chunk histogram](figure/histogram-1.png)



### 모델 정의

백화점 요소, 지역 환경 요소, 기상 조건 요소를 포함한 선형 모델 생성
해당 모델의 유의 변수 추출(step function: stepwise method)


```r
model = lm(log(mean_pop) ~ size + residential_area + commercial_area + green_area + pop_density + arrival + fine_dust * hyper_dust + mean_precipi + mean_temp + mean_snow + mean_wind, data = df)

step(model, direction = "both")
```

```
## Start:  AIC=-1393.48
## log(mean_pop) ~ size + residential_area + commercial_area + green_area + 
##     pop_density + arrival + fine_dust * hyper_dust + mean_precipi + 
##     mean_temp + mean_snow + mean_wind
## 
##                        Df Sum of Sq    RSS
## - mean_wind             1      0.06 3078.2
## - mean_temp             1      0.16 3078.3
## - fine_dust:hyper_dust  1      0.16 3078.3
## - mean_snow             1      0.33 3078.4
## - mean_precipi          1      0.37 3078.5
## <none>                              3078.1
## - commercial_area       1     15.31 3093.4
## - arrival               1     36.31 3114.4
## - green_area            1     43.47 3121.6
## - residential_area      1     90.66 3168.8
## - pop_density           1     94.86 3173.0
## - size                  1    579.66 3657.8
##                             AIC
## - mean_wind            -1395.39
## - mean_temp            -1395.26
## - fine_dust:hyper_dust -1395.25
## - mean_snow            -1395.03
## - mean_precipi         -1394.97
## <none>                 -1393.48
## - commercial_area      -1374.21
## - arrival              -1345.19
## - green_area           -1335.35
## - residential_area     -1271.01
## - pop_density          -1265.33
## - size                  -655.64
## 
## Step:  AIC=-1395.39
## log(mean_pop) ~ size + residential_area + commercial_area + green_area + 
##     pop_density + arrival + fine_dust + hyper_dust + mean_precipi + 
##     mean_temp + mean_snow + fine_dust:hyper_dust
## 
##                        Df Sum of Sq    RSS
## - mean_temp             1      0.13 3078.3
## - fine_dust:hyper_dust  1      0.15 3078.3
## - mean_snow             1      0.32 3078.5
## - mean_precipi          1      0.36 3078.5
## <none>                              3078.2
## + mean_wind             1      0.06 3078.1
## - commercial_area       1     15.36 3093.5
## - arrival               1     36.27 3114.4
## - green_area            1     43.42 3121.6
## - residential_area      1     90.61 3168.8
## - pop_density           1     94.93 3173.1
## - size                  1    579.82 3658.0
##                             AIC
## - mean_temp            -1397.21
## - fine_dust:hyper_dust -1397.18
## - mean_snow            -1396.94
## - mean_precipi         -1396.90
## <none>                 -1395.39
## + mean_wind            -1393.48
## - commercial_area      -1376.05
## - arrival              -1347.17
## - green_area           -1337.33
## - residential_area     -1272.99
## - pop_density          -1267.16
## - size                  -657.38
## 
## Step:  AIC=-1397.21
## log(mean_pop) ~ size + residential_area + commercial_area + green_area + 
##     pop_density + arrival + fine_dust + hyper_dust + mean_precipi + 
##     mean_snow + fine_dust:hyper_dust
## 
##                        Df Sum of Sq    RSS
## - fine_dust:hyper_dust  1      0.19 3078.5
## - mean_snow             1      0.25 3078.6
## - mean_precipi          1      0.32 3078.6
## <none>                              3078.3
## + mean_temp             1      0.13 3078.2
## + mean_wind             1      0.04 3078.3
## - commercial_area       1     15.33 3093.6
## - arrival               1     36.23 3114.5
## - green_area            1     43.51 3121.8
## - residential_area      1     90.75 3169.1
## - pop_density           1     94.96 3173.3
## - size                  1    579.73 3658.0
##                             AIC
## - fine_dust:hyper_dust -1398.94
## - mean_snow            -1398.87
## - mean_precipi         -1398.77
## <none>                 -1397.21
## + mean_temp            -1395.39
## + mean_wind            -1395.26
## - commercial_area      -1377.91
## - arrival              -1349.03
## - green_area           -1339.03
## - residential_area     -1274.62
## - pop_density          -1268.94
## - size                  -659.33
## 
## Step:  AIC=-1398.94
## log(mean_pop) ~ size + residential_area + commercial_area + green_area + 
##     pop_density + arrival + fine_dust + hyper_dust + mean_precipi + 
##     mean_snow
## 
##                        Df Sum of Sq    RSS
## - mean_snow             1      0.22 3078.7
## - mean_precipi          1      0.33 3078.8
## <none>                              3078.5
## - fine_dust             1      1.96 3080.5
## - hyper_dust            1      2.67 3081.2
## + fine_dust:hyper_dust  1      0.19 3078.3
## + mean_temp             1      0.17 3078.3
## + mean_wind             1      0.02 3078.5
## - commercial_area       1     15.26 3093.8
## - arrival               1     36.32 3114.8
## - green_area            1     43.58 3122.1
## - residential_area      1     90.83 3169.3
## - pop_density           1     94.92 3173.4
## - size                  1    580.32 3658.8
##                             AIC
## - mean_snow            -1400.64
## - mean_precipi         -1400.49
## <none>                 -1398.94
## - fine_dust            -1398.21
## - hyper_dust           -1397.22
## + fine_dust:hyper_dust -1397.21
## + mean_temp            -1397.18
## + mean_wind            -1396.97
## - commercial_area      -1379.74
## - arrival              -1350.65
## - green_area           -1340.66
## - residential_area     -1276.25
## - pop_density          -1270.73
## - size                  -660.41
## 
## Step:  AIC=-1400.64
## log(mean_pop) ~ size + residential_area + commercial_area + green_area + 
##     pop_density + arrival + fine_dust + hyper_dust + mean_precipi
## 
##                        Df Sum of Sq    RSS
## - mean_precipi          1      0.28 3079.0
## <none>                              3078.7
## - fine_dust             1      2.02 3080.7
## + mean_snow             1      0.22 3078.5
## - hyper_dust            1      2.70 3081.4
## + fine_dust:hyper_dust  1      0.16 3078.6
## + mean_temp             1      0.09 3078.6
## + mean_wind             1      0.03 3078.7
## - commercial_area       1     15.28 3094.0
## - arrival               1     36.26 3115.0
## - green_area            1     43.56 3122.3
## - residential_area      1     90.82 3169.5
## - pop_density           1     94.92 3173.6
## - size                  1    580.19 3658.9
##                             AIC
## - mean_precipi         -1402.24
## <none>                 -1400.64
## - fine_dust            -1399.83
## + mean_snow            -1398.94
## - hyper_dust           -1398.88
## + fine_dust:hyper_dust -1398.87
## + mean_temp            -1398.76
## + mean_wind            -1398.68
## - commercial_area      -1381.41
## - arrival              -1352.43
## - green_area           -1342.39
## - residential_area     -1277.98
## - pop_density          -1272.43
## - size                  -662.31
## 
## Step:  AIC=-1402.24
## log(mean_pop) ~ size + residential_area + commercial_area + green_area + 
##     pop_density + arrival + fine_dust + hyper_dust
## 
##                        Df Sum of Sq    RSS
## <none>                              3079.0
## - fine_dust             1      2.18 3081.2
## + mean_precipi          1      0.28 3078.7
## + mean_snow             1      0.17 3078.8
## + fine_dust:hyper_dust  1      0.17 3078.8
## - hyper_dust            1      2.80 3081.8
## + mean_temp             1      0.06 3078.9
## + mean_wind             1      0.02 3079.0
## - commercial_area       1     15.33 3094.3
## - arrival               1     36.02 3115.0
## - green_area            1     43.52 3122.5
## - residential_area      1     90.81 3169.8
## - pop_density           1     94.93 3173.9
## - size                  1    579.98 3659.0
##                             AIC
## <none>                 -1402.24
## - fine_dust            -1401.21
## + mean_precipi         -1400.64
## + mean_snow            -1400.49
## + fine_dust:hyper_dust -1400.48
## - hyper_dust           -1400.35
## + mean_temp            -1400.33
## + mean_wind            -1400.27
## - commercial_area      -1382.94
## - arrival              -1354.37
## - green_area           -1344.06
## - residential_area     -1279.60
## - pop_density          -1274.04
## - size                  -664.22
```

```
## 
## Call:
## lm(formula = log(mean_pop) ~ size + residential_area + commercial_area + 
##     green_area + pop_density + arrival + fine_dust + hyper_dust, 
##     data = df)
## 
## Coefficients:
##      (Intercept)              size  
##        9.226e+00         2.723e-06  
## residential_area   commercial_area  
##       -2.148e+00         1.367e+00  
##       green_area       pop_density  
##       -1.657e+00         5.744e-05  
##          arrival         fine_dust  
##       -5.604e-06         1.596e-03  
##       hyper_dust  
##       -2.727e-03
```

아래와 같이 최종 모델 도출


```r
model_step = lm(formula = log(mean_pop) ~ size + residential_area + commercial_area + green_area + pop_density + arrival + fine_dust + hyper_dust, data = df)

summary(model_step)
```

```
## 
## Call:
## lm(formula = log(mean_pop) ~ size + residential_area + commercial_area + 
##     green_area + pop_density + arrival + fine_dust + hyper_dust, 
##     data = df)
## 
## Residuals:
##     Min      1Q  Median      3Q     Max 
## -4.1327 -0.4636 -0.0263  0.4953  2.5366 
## 
## Coefficients:
##                    Estimate Std. Error t value
## (Intercept)       9.226e+00  2.237e-01  41.236
## size              2.723e-06  9.592e-08  28.391
## residential_area -2.148e+00  1.912e-01 -11.234
## commercial_area   1.367e+00  2.960e-01   4.616
## green_area       -1.657e+00  2.131e-01  -7.777
## pop_density       5.744e-05  5.001e-06  11.486
## arrival          -5.604e-06  7.921e-07  -7.075
## fine_dust         1.596e-03  9.164e-04   1.741
## hyper_dust       -2.727e-03  1.384e-03  -1.971
##                  Pr(>|t|)    
## (Intercept)       < 2e-16 ***
## size              < 2e-16 ***
## residential_area  < 2e-16 ***
## commercial_area  4.02e-06 ***
## green_area       9.25e-15 ***
## pop_density       < 2e-16 ***
## arrival          1.73e-12 ***
## fine_dust          0.0817 .  
## hyper_dust         0.0488 *  
## ---
## Signif. codes:  
## 0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
## 
## Residual standard error: 0.8483 on 4279 degrees of freedom
## Multiple R-squared:  0.3461,	Adjusted R-squared:  0.3449 
## F-statistic: 283.1 on 8 and 4279 DF,  p-value: < 2.2e-16
```

해당 모델은 데이터의 약 34.5% (Adj. R square)를 설명하고 있으며 미세먼지, 초미세먼지 변수가 모두 유의

종속 변수에 log 변환을 실시하였기 때문에 모델은 **독립 변수가 1단위 증가할 때의 종속 변수의 변화량**으로 해석

미세먼지의 경우, 단위가 1 증가할 때 백화점을 방문한 유동인구는 0.15% 증가하는 반면 초미세먼지의 경우, -0.27% 감소



![plot of chunk unnamed-chunk-2](figure/unnamed-chunk-2-1.png)



잔차의 QQ plot을 확인하였을 때, 잔차가 정규성을 잘 따르고 있음



### 모델 유의성 검증
모델의 유의성을 검증하기 위해 10겹 교차 검증(10-fold cross validation)을 진행


```r
cal_mape = function(model, test_data, mape_res){
  
  # calculate distance
  distPred <- predict(model, test_data)
  actuals_preds <- data.frame(cbind(actuals = log(test_data$mean_pop), predicteds = distPred))
  correlation_accuracy <- cor(actuals_preds)
  
  # calculate MAPE 
  mape <- mean(abs((actuals_preds$predicteds - actuals_preds$actuals))/actuals_preds$actuals) * 100
  mape_res = rbind(mape_res, as.data.frame(mape))
  
  return(mape_res)
}

data_num = nrow(df) %/% 10
mape_res = NULL

for (row_idx in 1:10){
  start = 1 + (row_idx - 1) * data_num
  end   = row_idx * data_num
  
  test_data = df[start:end, ]
  train_data = df[-c(start:end), ]
  
  model = lm(formula = log(mean_pop) ~ size + residential_area + commercial_area +
               green_area + pop_density + arrival + fine_dust + hyper_dust,
             data = train_data)
  
  mape_res = cal_mape(model, test_data, mape_res)
}

mean_error = mean(mape_res$mape)
print(mean_error)
```

```
## [1] 7.749732
```

MAPE는 비율 에러를 측정하는 방법으로  $A_t$는 실제 값, $F_t$는 예측 값을 의미



$$
MAPE = \frac{100}{n} \sum_{t=1}^{n}|\frac{A_t - F_t}{A_t}|
$$



해당 모델의 MAPE는 7.75%로 상당 부분 예측을 잘 수행하고 있음



***
## 논의

본 프로젝트의 목표는 서울시에서 제공하는 오픈 데이터인 생활 인구 데이터를 바탕으로 기상 조건이 백화점 방문 고객 수에 미치는 영향을 분석하기 위해서 진행하였다. 기온, 강수량, 적설량, 풍속 등의 기상 조건은 백화점 유동 인구에 별다른 영향을 주지 않은 반면, 미세먼지와 초미세먼지는 유의한 영향을 미친다는 점을 확인하였다.



그러나 백화점 유동 인구에 미세먼지와 초미세먼지가 미치는 영향은 반대로 나타났다. 미세먼지와 백화점 유동 인구는 양의 관계가 있는 반면 초미세먼지와 백화점 유동 인구는 음의 관계가 있었다. *[미세먼지 1단위 증가당 유동 인구 0.15% 증가 / 초미세먼지 1단위 증가당 유동 인구 0.27% 감소]* 본 프로젝트에서 수립한 모델은 10-fold 교차 검증에서 MAPE 7.75% 정도로 우수한 것으로 검증하였으며, 실제 백화점 유동 인구 예측에서도 사용이 가능할 것으로 보인다. 



그러나 본 프로젝트에서 핵심적으로 사용한 KT 생활 인구 데이터셋은 KT의 LTE 통신망 사용자의 수를 바탕으로 추정한 결과이기 때문에 실제와 차이가 발생할 가능성이 농후하다. 해당 데이터셋의 신뢰성을 검증하기 위해 지난 12월 9일 공개된 SKT 데이터 허브의 유동인구 데이터와의 비교 검증을 진행할 예정이다. 또한 백화점이 포함된 집계 구역의 생활 인구 발생 정도를 유동 인구로 가정한 것도 문제의 여지가 있다. 최초에 거주 인구를 정의하기 위해 평일 새벽 시간대 인구를 계산하였다. 그러나 몇몇 집계 구역에서 갑작스러운 생활 인구 증가가 발견되었다. 이 경향은 몇 달간 유지된 후 급작스럽게 떨어졌다. 때문에 해당 기간동안 "낮 시간의 생활 인구 - 심야 시간 생활 인구"를 계산하면 오히려 거주 인구가 더 많은 것처럼 보이는 현상이 발생하여 낮 시간대의 생활 인구를 유동 인구로 가정하였다. 



본 프로젝트의 결과는 미세먼지의 영향이 백화점을 방문하는 것에 유의한 영향을 미친다는 것을 확인했다는 점, 미세먼지와 초미세먼지의 수치에 반응하는 경향이 다르다는 점을 밝혔다는 것에 의의가 있다. 향후 미세먼지와 초미세먼지에 따라 다르게 반응하는 원인을 탐색하고 이를 검증하는 작업이 필요할 것이다. 



***
## 참고 문헌
[미세먼지에 의한 대기질 악화가 서울시 상권 매출액에 미치는 영향 분석](https://bigdata.seoul.go.kr/noti/selectNoti.do?r_id=P440&bbs_seq=170&sch_type=&sch_text=&currentPage=1)

***
## 프로젝트 github 링크
[Github Repository](https://github.com/jonghyunlee1993/Multicampus_semi)
