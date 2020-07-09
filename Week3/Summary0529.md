ESP

Step-wise multivariate linear regression

* Outcome: binary health status
* covariates: education, ethincity, income, race
* predictor: numeracy score

| Outcome                | Health status 1 (split at 4, Excellent VS. Not-Excellent) | Health status 2 (split at 3, Good VS. Not-Good) |
| ---------------------- | --------------------------------------------------------- | ----------------------------------------------- |
| significant predictor  | numeracy score                                            |                                                 |
| significant covariates | Education, ethinicity                                     | income, education                               |
| AUC                    | 0.8123                                                    | 0.7024                                          |

PIAAC

* Outcome: Binary health status
* predictor: numeracy score
* covariates: income, race, education

| Outcome                | Health status 1 (split at 5, Good VS. Not-Good) | Health status 2 (split at 4, Not-Poor VS. Poor) |
| ---------------------- | ----------------------------------------------- | ----------------------------------------------- |
| significant predictor  | numeracy score                                  | numeracy score                                  |
| significant covariates | income, race                                    | income, race, education                         |
| AUC                    | 0.7524                                          | 0.7114                                          |



Considering additional covariates:

* Outcome: Binary health status
* predictor: numeracy score
* covariates: income, race, education

| Outcome                | Health status 1 (split at 5, Good VS. Not-Good) | Health status 2 (split at 4, Not-Poor VS. Poor) |
| ---------------------- | ----------------------------------------------- | ----------------------------------------------- |
| significant predictor  | numeracy score                                  | numeracy score                                  |
| significant covariates | income, race, education, problem solving        | income, race, education, problem solving        |
| AUC                    | 0.8196                                          | 0.6920                                          |



* AUC: 0.7524

  

* Outcome: health status 2

* predictor & covariates: numeracy score & income, race, **education**

* AUC: 0.7114

  

* Outcome: health status 1

* predictor & covariates: numeracy score & income, race, **education**, problem_solving, (literacy score)

* AUC: 0.8196



* Outcome: health status 2
* predictor & covariates: numeracy score & income, race, **education**, problem solving, (literacy score).
* AUC: 0.6920



Pregression comparision

| Outcome                   | Health status 1 (divide at 5)            | Health status 2 (divide at 4)            |
| ------------------------- | ---------------------------------------- | ---------------------------------------- |
|                           |                                          |                                          |
| significant predictor     | numeracy score                           | numeracy score                           |
| significant covariates    | income, race                             | income, race, education                  |
| Non-significant covariate | Education                                |                                          |
| AUC                       | 0.7524                                   | 0.7114                                   |
|                           |                                          |                                          |
| significant predictor     | numeracy score                           | numeracy score                           |
| significant covariates    | income, race, education, problem solving | income, race, education, problem solving |
| Non-significant covariate | literacy score                           | literacy score                           |
| AUC                       | 0.8196                                   | 0.6920                                   |



Overall, numeracy score is a significant predictor, income and race are significant covariates in all models; education is a significant covariates in all the other models except for the first one (the model with 4 independent variabes and set the health score cutpoint at 5)

As for additional predictors, problem solving score is significant while literacy score is not significant no matter which cut point to set.



G_Q03C 

Label: Skill use work - Numeracy - How often - Use or calculate fractions or percentages

Question: In your job, how often do/did you usually use or calculate fractions, decimals or percentages?



H_Q03C 

Label: Skill use everyday life - Numeracy - How often - Use or calculate fractions or percentages 

Question: In everyday life, how often do you usually use or calculate fractions, decimals or percentages?







Proficiency Estimation - 

**Plausible Values** ：

PIAAC cannot provide individual-level results, because each adult answers only a small number of assessment questions. PIAAC provides reliable estimates of proficiency only at the national level, or at the level of large subgroups (e.g., females, employed, or college educated) • Plausible Values (PVs) allows PIAAC dataset information to be saved at the case level to estimate proficiency at the national or subgroup level o Each case's PVs reflect not only that individual's performance on the small number of items s/he answered, but also the performance of similar respondents on the rest of the PIAAC assessment



Each individual case in the PIAAC dataset has a randomly chosen set of ten plausible values (PVs) o All ten PVs must be used together to estimate proficiency, or else one understates the variability in the predicted outcomes o The randomly chosen set of PVs best represents the score distribution for a subgroup of adults




```{r}
​```{r}
data(api)
?svydesign
?weights
dstrat<-svydesign(id=~1,strata=~stype, weights=~pw, data=apistrat, fpc=~fpc)
  dclus2<-svydesign(id=~dnum+snum, weights=~pw, data=apiclus2)
  rstrat<-as.svrepdesign(dstrat)
  rclus2<-as.svrepdesign(dclus2)

  summary(svyglm(api00~ell+meals+mobility, design=dstrat))
  summary(svyglm(api00~ell+meals+mobility, design=dclus2))
  summary(svyglm(api00~ell+meals+mobility, design=rstrat))
  summary(svyglm(api00~ell+meals+mobility, design=rclus2))

  ## use quasibinomial, quasipoisson to avoid warning messages
  summary(svyglm(sch.wide~ell+meals+mobility, design=dstrat,
        family=quasibinomial()))

  ## Compare regression and ratio estimation of totals
  api.ratio <- svyratio(~api.stu,~enroll, design=dstrat)
  pop<-data.frame(enroll=sum(apipop$enroll, na.rm=TRUE))
  npop <- nrow(apipop)
  predict(api.ratio, pop$enroll)

  ## regression estimator is less efficient
  api.reg <- svyglm(api.stu~enroll, design=dstrat)
  predict(api.reg, newdata=pop, total=npop)
  ## same as calibration estimator
  svytotal(~api.stu, calibrate(dstrat, ~enroll, pop=c(npop, pop$enroll)))

  ## svyglm can also reproduce the ratio estimator
  api.reg2 <- svyglm(api.stu~enroll-1, design=dstrat,
                    family=quasi(link="identity",var="mu"))
  predict(api.reg2, newdata=pop, total=npop)

  ## higher efficiency by modelling variance better
  api.reg3 <- svyglm(api.stu~enroll-1, design=dstrat,
                    family=quasi(link="identity",var="mu^3"))
  predict(api.reg3, newdata=pop, total=npop)
  ## true value
  sum(apipop$api.stu)

 

​```
```



plausible value我大概搞明白了，稍微解释下：

1. 每个参加PIAAC问卷的人仅仅回答一小部分评估问题，所以并不能准确估计出每个人的proficiency score
2. PIAAC在national level上提供proficiency的估计，也就是plausible value1- plausible value 10，共计10个plausible value
3. plausible value 在case level上保存，该plausible value不仅反应那个individual对回答的小部分问题的proficiency，还保存了相似的人对未回答问题的估计

另外，如果想要确切的proficiency，不能只看一个plausible value，需要综合10个一起看==



```{r}
piaac.reg.pv(x = c("educ", "income.cat", "race"), 
             pvlabel = "NUM", by = "CNTRYID",
             data = PIAAC.pv)

piaac.reg.pv(pvlabel="NUM", x=c("B_Q01A", "J_Q09USX", "RACETHN_5CAT"), by = "I_Q08", data=raw_PIAAC)
raw_PIAAC$PVNUM1
```

我把我做的上传在week3 analysis文件夹下了，主要加了4部分内容

1. 总结之前的回归结果 （education & numeracy interaction 不显著，后续就没有再加这个或者其它的interaction）
2. ESP的weighted regression （目前只对education level做了weights，function已经写好了，可以给其它covariates加weights）
3. 给PIAAC加了ethnicity项，和ESP做对比（这里加ethnicity项应该没问题把==你们可以看下）
4. 解释了一下PIAAC 的plausible value，用PIAAC已经做好的包看了一下health status和numeracy的联系，two-sample t-test显示联系显著（因为sample size实在太大了，literacy performance & problem solving和healthstatus的association也是显著的）