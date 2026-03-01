
# 1. Rodar o modelo logit

modelo_logit <- glm(
  eleita ~ partpsb + corraçabrc + gini_rec + gini_gvt,
  family = binomial,
  data = database_candidatas_gini
)

# 2. Resumo do modelo

summary(modelo_logit)

# 3. Odds Ratios (OR) e Intervalos de Confiança para o OR (a chance de a candidata ser eleita)

# OR
odds_ratios <- exp(coef(modelo_logit))
odds_ratios

# IC para OR
ic_odds <- exp(confint(modelo_logit))
ic_odds

# 4. Interpretar cada OR com frase automática

OR <- exp(coef(modelo_logit))

interpretar_or <- function(nome, or){
  if(or > 1){
    aumento <- round((or - 1) * 100, 1)
    cat(nome, "→", round(or, 3), "\n",
        "Aumenta as chances em", aumento, "%.\n\n")
  } else {
    reducao <- round((1 - or) * 100, 1)
    cat(nome, "→", round(or, 3), "\n",
        "Reduz as chances em", reducao, "%.\n\n")
  }
}

interpretar_or("partpsb", OR["partpsb"])
interpretar_or("corraçabrc", OR["corraçabrc"])
interpretar_or("gini_rec", OR["gini_rec"])
interpretar_or("gini_gvt", OR["gini_gvt"])


# 5. Teste de Hosmer–Lemeshow

library(ResourceSelection)

hoslem.test(
  database_candidatas_gini$eleita,
  fitted(modelo_logit)
)

# 6. Curva ROC e AUC

library(pROC)

prob <- predict(modelo_logit, type = "response")

roc_obj <- roc(database_candidatas_gini$eleita, prob)
auc(roc_obj)




