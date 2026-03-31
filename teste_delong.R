library(pROC)

mod1 <- glm(
  eleita ~ partpsb + corraçabrc + gini_rec + gini_gvt,
  family = binomial,
  data = database_candidatas_gini
)

mod2 <- glm(
  eleita ~ partpsb + corraçabrc + ecicas + escsupc + id_gen_cis + ori_n_dec + gini_rec + gini_gvt,
  family = binomial,
  data = database_candidatas_gini
)

prob_modelo1 <- predict(mod1, type = "response")
prob_modelo2 <- predict(mod2, type = "response")

# prob_modelo1 e prob_modelo2: probabilidades previstas pelos modelos #consturir curvas

roc_m1 <- roc(database_candidatas_gini$eleita, prob_modelo1)
roc_m2 <- roc(database_candidatas_gini$eleita, prob_modelo2)

# Teste de DeLong
teste_delong <- roc.test(roc_m1, roc_m2, method = "delong")

print(teste_delong)
