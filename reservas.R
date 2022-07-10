



## MMQ:

El instrumento cuenta con 6 subescalas, compuestas de la siguiente manera:
  
  **Escalas positivas:**
  
  F1 (reflexión): 1,6,8,10,16,17,18,31,32

F2 (ego-strength): 11,22,24,25,26,30

F3 (relational attunement): 4,5,14,21,28

**Escalas negativas:**
  
  F4 (relational discomfort): 9,12,15,27,33

F5 (distrust): 13,19,20,29

F6 (emotional discontrol):2,3,7,23



### Escalas con complicaciones:

Las variables más complicadas son:
  
  Relational Attunement: originalmente ítems 4,5,14,21,28; propuesta: 4,5,28. Ítem 14 y 21 asociados a relational discomfort. Hipótesis es predominancia de empatía cognitiva, y los ítems más afectivos podrían tener que ver con relaciones sociales concretas.


Relational Discomfort: originalmente ítems 9,12,15,27,33; propuesta: 9,12,27,33 6,14,21,31. Hipótesis es predominancia de ítems sobre empatía emocional.


### Relational Attunement:

Model1 (original)

```{r}
library(lavaan)


#f_ra1~~suma_aq_af
#f_ra1~~suma_aq_av
#f_ra1~~suma_aq_r
#f_ra1~~suma_aq_h
#f_ra1~~suma_eq_hs
#f_ra1~~suma_eq_re
#f_ra1~~suma_iri_fs
#f_ra1~~suma_iri_pd
#f_ra1~~suma_iri_pt


cod_ra_cov1 = "f_ra1 =~ q0007_0004+q0007_0005+q0007_0028+q0007_0014+q0007_0021
f_ra1~~suma_eq_ec
f_ra1~~suma_iri_ec
"

# ordered = names(datos_fa[,17:77]) agregar en caso de:

mod_ra_cov1 = sem(model=cod_ra_cov1, data = datos_fa, estimator = "MLR")

summary(mod_ra_cov1, fit.measures = T, standardized = TRUE)
```


Model2:
  
  ```{r}
cod_ra_cov2 = "f_ra1 =~ q0007_0004+q0007_0005+q0007_0028
f_ra1~~suma_eq_ec
f_ra1~~suma_iri_ec"

# ordered = names(datos_fa[,17:77]) agregar en caso de:

mod_ra_cov2 = sem(model=cod_ra_cov2, data = datos_fa,
                  estimator = "MLR")

summary(mod_ra_cov2, fit.measures = T, standardized = TRUE)
```


Conclusión: los ítems 14 y 21 tienen un efecto en el factor que lo hace tener más que ver con la vinculación emocional (relación con EC), y disminuye levemente su asociación con la empatía cognitiva. El ajuste general del modelo disminuye. Se propone que estos ítems podrían no pertenecer al mismo constructo.



### Relational discomfort:


Model1: (original)

```{r}
cod_rd_cov1 = "f_rd1 =~ q0007_0009+q0007_0012+q0007_0015+q0007_0027+q0007_0033
f_rd1~~suma_eq_ec
f_rd1~~suma_iri_ec
f_rd1~~suma_aq_h"

# ordered = names(datos_fa[,17:77]) agregar en caso de:

mod_rd_cov1 = sem(model=cod_rd_cov1, data = datos_fa,
                  estimator = "MLR")

summary(mod_rd_cov1, fit.measures = T, standardized = TRUE)
```


Model2: (propuesta)

```{r}
datos_fa$q0007_0006r = 6 - datos_fa$q0007_0006
datos_fa$q0007_0014r = 6 - datos_fa$q0007_0014
datos_fa$q0007_0021r = 6 - datos_fa$q0007_0021
datos_fa$q0007_0031r = 6 - datos_fa$q0007_0031


cod_rd_cov2 = "f_rd1 =~ q0007_0009+q0007_0012+q0007_0027+q0007_0033+q0007_0006r+q0007_0014r+q0007_0021r+q0007_0031r
f_rd1~~suma_eq_ec
f_rd1~~suma_iri_ec
f_rd1~~suma_aq_h"

# ordered = names(datos_fa[,17:77]) agregar en caso de:

mod_rd_cov2 = sem(model=cod_rd_cov2, data = datos_fa,
                  estimator = "MLR")

summary(mod_rd_cov2, fit.measures = T, standardized = TRUE)
```


Conclusión: los ítems que se agregan no contribuyen a definir el factor. La propuesta es dejar la hostilidad como criterio y mantener la escala original.


### Reflexibility:

Modelo 1: escala original: 1,6,8,10,16,17,18,31,32

```{r}
cod_rf_cov1 = "f_rf1 =~ q0007_0001+q0007_0006+q0007_0008+q0007_0010+q0007_0016+q0007_0017+q0007_0018+q0007_0031+
q0007_0032
f_rf1~~suma_eq_ec
f_rf1~~suma_iri_ec
f_rf1~~suma_iri_pt"

# ordered = names(datos_fa[,17:77]) agregar en caso de:

mod_rf_cov1 = sem(model=cod_rf_cov1, data = datos_fa,
                  estimator = "MLR")

summary(mod_rf_cov1, fit.measures = T, standardized = TRUE)
```


Modelo 2: escala propuesta

```{r}
cod_rf_cov2 = "f_rf1 =~ q0007_0001+q0007_0008+q0007_0010+q0007_0016+q0007_0017+q0007_0018+
q0007_0032
f_rf1~~suma_eq_ec
f_rf1~~suma_iri_ec
f_rf1~~suma_iri_pt"

# ordered = names(datos_fa[,17:77]) agregar en caso de:

mod_rf_cov2 = sem(model=cod_rf_cov2, data = datos_fa, estimator = "MLR")

summary(mod_rf_cov2, fit.measures = T, standardized = TRUE)
```











### Propuesta del nuevo factor (Empatic concern):

Se propone un factor de empatic concern in close relationships con los siguientes ítems. 


6.- Para entender las acciones de los demás, es fundamental comprender lo que sienten.


14.- Soy capaz de empatizar con otros cuando me cuentan algo.


21.- Soy sensible a lo que le pasa a los demás.


31.- Soy una persona que piensa en los demás.


```{r}
cod_ec_cov1 = "f_ec1 =~ q0007_0006+q0007_0014+q0007_0021+q0007_0031
f_ec1~~suma_eq_ec
f_ec1~~suma_iri_ec"

mod_ec_cov1 = sem(model=cod_ec_cov1, data = datos_fa,
                  estimator = "MLR")

summary(mod_ec_cov1, fit.measures = T, standardized = TRUE)
```



```{r}
cod_ec_cov2 = "f_ec1 =~ q0007_0014+q0007_0021+q0007_0031
f_ec1~~suma_eq_ec
f_ec1~~suma_iri_ec"

mod_ec_cov2 = sem(model=cod_ec_cov2, data = datos_fa,
                  estimator = "MLR")

summary(mod_ec_cov2, fit.measures = T, standardized = TRUE)
```



## Reflexibility:


```{r}
cod_ec_cov1 = "f_ec1 =~ q0007_0006+q0007_0014+q0007_0021+q0007_0031
f_ec1~~suma_eq_ec
f_ec1~~suma_iri_ec"

mod_ec_cov1 = sem(model=cod_ec_cov1, data = datos_fa,
                  estimator = "MLR")

summary(mod_ec_cov1, fit.measures = T, standardized = TRUE)
```




```{r}
cod_ec_cov1 = "f_ec1 =~ q0007_0006+q0007_0014+q0007_0021+q0007_0031
f_ec1~~suma_eq_ec
f_ec1~~suma_iri_ec"

mod_ec_cov1 = sem(model=cod_ec_cov1, data = datos_fa,
                  estimator = "MLR")

summary(mod_ec_cov1, fit.measures = T, standardized = TRUE)
```



### F2 (ego-strength)

Escala original: 11,22,24,25,26,30

```{r}
cod_es_cov1 = "f_es1 =~ q0007_0011+q0007_0022+q0007_0024+q0007_0025+q0007_0026+q0007_0030
f_es1~~suma_eq_ec
f_es1~~suma_iri_ec
f_es1~~suma_iri_pd"

mod_es_cov1 = sem(model=cod_es_cov1, data = datos_fa,
                  estimator = "MLR")

summary(mod_es_cov1, fit.measures = T, standardized = TRUE)
```


### F5 (distrust)

Escala original: 13,19,20,29



```{r}
cod_dt_cov1 = "f_dt1 =~ q0007_0013+q0007_0019+q0007_0020+q0007_0029
f_dt1~~suma_iri_ec
f_dt1~~suma_aq_h"

# f_dt1~~suma_eq_ec

mod_dt_cov1 = sem(model=cod_dt_cov1, data = datos_fa,
                  estimator = "MLR")

summary(mod_dt_cov1, fit.measures = T, standardized = TRUE)
```


Escala propuesta: 13,15,20,29


```{r}
cod_dt_cov2 = "f_dt1 =~ q0007_0013+q0007_0015+q0007_0020+q0007_0029
f_dt1~~suma_iri_ec
f_dt1~~suma_aq_h"

mod_dt_cov2 = sem(model=cod_dt_cov2, data = datos_fa,
                  estimator = "MLR")

summary(mod_dt_cov2, fit.measures = T, standardized = TRUE)
```

Conclusión: nos quedamos con el modelo alternativo.


### Emotional discontrol

Escala original: 2,3,7,23

```{r}
cod_ed_cov1 = "f_ed1 =~ q0007_0002+q0007_0003+q0007_0007+q0007_0023
f_ed1~~suma_aq_r
f_ed1~~suma_iri_pd
"

# f_ed1~~suma_iri_pd; f_ed1~~suma_aq_h

mod_ed_COV1 = sem(model=cod_ed_cov1, data = datos_fa,
                  estimator = "MLR")

summary(mod_ed_COV1, fit.measures = T, standardized = TRUE)
```

Escala propuesta: 2,3,7,19,23

```{r}
cod_ed_cov2 = "f_ed1 =~ q0007_0002+q0007_0003+q0007_0007+q0007_0023+q0007_0019
f_ed1~~suma_aq_r
f_ed1~~suma_iri_pd
"

mod_ed_COV2 = sem(model=cod_ed_cov2, data = datos_fa,
                  estimator = "MLR")

summary(mod_ed_COV2, fit.measures = T, standardized = TRUE)
```

Conclusión: nos quedamos con el modelo original