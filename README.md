Análisis instrumentos
================

# Método:

Este estudio contó con 3 etapas: traducción y adaptación, entrevistas
cognitivas y finalmente el análisis psicométricos.

## Adaptación al español:

El proceso de traducción y adaptación de los ítems tuvo como enfoque
reducir características irrelevantes del constructo a partir de una
adpatación que diera cuenta de las características culturales de la
población objetivo (APA, 2014). Por ello, los todos los ítems fueron
fueron traducidos de dos formas: una forma literal, y una forma
adaptada, buscando que esta última abordara los modos expresivos propios
de la cultura Chilena. Ambas traducciones, junto con los ítems en
inglés, fueron expuestos a un panel de 4 personas, compuesto por 2
psicolingüistas y 2 lingüistas, quienes analizaron y evaluaron las
traducciones siguiendo un proceso adaptado de Solano et al.(2012), en el
cual se analizan 10 dimensiones en que pueden haber errores de
adaptación en el proceso de traducción: estilo, formato, convenciones,
información, gramática, semántica, registro, cultura y origen (error en
el ítem en su versión original). Los ítems en los que se detectó algún
error fueron discutidos hasta que se llegó a un consenso respecto a una
versión final del ítem que no tuviera errores.

En el caso del MMQ, se consensuaron 15 ítems con errores, de los cuales
11 fueron en el área de registro (La traducción no refleja las formas
discursivas o expresivas locales (ej. la traducción suena poco natural
en el uso del lenguaje), 5 fueron en el área semántica (la traducción
altera el significado original), y 2 fueron en el área de información
(la traducción altera la cantidad de información).

En el caso del MentS se consensuaron 21 ítems con errores, de los cuales
19 fueron en el área del registro, 7 en el área semántica, 1 en el área
de información.

## Entrevistas cognitivas:

Se realizaron 5 entrevistas cognitivas (3 mujeres) a personas no
relacinadas con la psicología. El procedimiento consistió en 2 etapas:
en la primera etapa los participantes contestaron el instrumento MMQ y
el MentS; en la segunda etapa frente a cada ítem se le hicieron 4
preguntas: 1) ¿qué te está pidiendo que contestes el ítem?, 2) ¿qué
contestaste?, 3) ¿por qué elegiste esta categoría?, 4) ¿hay algo del
ítem que te pareció poco claro o confuso?

# Descripción de los datos:

## Sociodemográficos

### Datos perdidos:

Análisis inicial va a partir por los datos perdidos:

``` r
# Descripción datos perdidos ----------------------------------------------

datos = datos %>% rename(acuerdo = q0001, edad = q0002, genero = q0003, educacion = q0004,
                 estudias = q0005, estudio_actual = q0006)


## Nivel sociodemográfico:
naniar::miss_var_summary(datos[,11:16])
```

    ## # A tibble: 6 x 3
    ##   variable       n_miss pct_miss
    ##   <chr>           <int>    <dbl>
    ## 1 estudio_actual    141    33.4 
    ## 2 genero             14     3.32
    ## 3 educacion          14     3.32
    ## 4 estudias           14     3.32
    ## 5 acuerdo             0     0   
    ## 6 edad                0     0

``` r
vis_miss(datos[,11:16])
```

    ## Warning: `gather_()` was deprecated in tidyr 1.2.0.
    ## Please use `gather()` instead.
    ## This warning is displayed once every 8 hours.
    ## Call `lifecycle::last_lifecycle_warnings()` to see where this warning was generated.

![](README_files/figure-gfm/unnamed-chunk-1-1.png)<!-- -->

Se observa que quienes omiten en género, educación y estudio, son las
mismas personas. Quienes omiten en estudio actual son quienes no están
estudiando actualmente.

### Descripción de la muestra:

La variable de estudio actual permite construir una nueva variable
dummy, de estudiante universitario (Sí/No), dada la cantidad de personas
en la muestra que estudian en la universidad.

``` r
datos = datos %>% mutate(universitatio = case_when(
  educacion == "Educación media completa" &
    estudias == "Sí" &
    estudio_actual == "Universitaria"~"Sí",
  estudias == "No"~"No"))

datos = datos %>% mutate(estudio_actual = ifelse(estudias == "No","No",estudio_actual))
datos = datos %>% mutate(estudio_actual = factor(estudio_actual, levels = c("No",3,4), labels = c("No","Universitaria", "Postgrado")))

datos$edad = as.numeric(as.character(datos$edad))
```

``` r
summary(tableby(~edad+genero+educacion+estudias+estudio_actual, data = datos))
```

|                                                               | Overall (N=422) |
|:--------------------------------------------------------------|:---------------:|
| **edad**                                                      |                 |
|    N-Miss                                                     |       14        |
|    Mean (SD)                                                  | 29.113 (11.764) |
|    Range                                                      | 0.000 - 85.000  |
| **¿Con qué género te identificas?**                           |                 |
|    N-Miss                                                     |       14        |
|    Masculino                                                  |   126 (30.9%)   |
|    Femenino                                                   |   270 (66.2%)   |
|    Otro                                                       |    7 (1.7%)     |
|    Prefiero no decirlo                                        |    5 (1.2%)     |
| **¿Cuál es el nivel educacional más alto que has alcanzado?** |                 |
|    N-Miss                                                     |       14        |
|    Educación básica completa                                  |    0 (0.0%)     |
|    Educación media completa                                   |   208 (51.0%)   |
|    Educación técnico profesional completa                     |    25 (6.1%)    |
|    Educación universitaria completa                           |   100 (24.5%)   |
|    Educación de postgrado completa                            |   75 (18.4%)    |
| **¿Actualmente te encuentras estudiando?**                    |                 |
|    N-Miss                                                     |       14        |
|    Sí                                                         |   277 (67.9%)   |
|    No                                                         |   131 (32.1%)   |
| **estudio_actual**                                            |                 |
|    N-Miss                                                     |       18        |
|    No                                                         |   131 (32.4%)   |
|    Universitaria                                              |   225 (55.7%)   |
|    Postgrado                                                  |   48 (11.9%)    |

Diferencias entre personas según están estudiando actualmente:

``` r
summary(tableby(estudio_actual~edad+genero+educacion+estudias, data = datos))
```

|                                                               |   No (N=131)    | Universitaria (N=225) | Postgrado (N=48) |  Total (N=404)  |  p value |
|:--------------------------------------------------------------|:---------------:|:---------------------:|:----------------:|:---------------:|---------:|
| **edad**                                                      |                 |                       |                  |                 | \< 0.001 |
|    Mean (SD)                                                  | 39.420 (12.565) |    21.840 (4.266)     |  34.250 (8.494)  | 29.015 (11.690) |          |
|    Range                                                      | 0.000 - 85.000  |    17.000 - 49.000    | 23.000 - 70.000  | 0.000 - 85.000  |          |
| **¿Con qué género te identificas?**                           |                 |                       |                  |                 |    0.017 |
|    Masculino                                                  |   47 (35.9%)    |      56 (24.9%)       |    22 (45.8%)    |   125 (30.9%)   |          |
|    Femenino                                                   |   83 (63.4%)    |      159 (70.7%)      |    25 (52.1%)    |   267 (66.1%)   |          |
|    Otro                                                       |    0 (0.0%)     |       7 (3.1%)        |     0 (0.0%)     |    7 (1.7%)     |          |
|    Prefiero no decirlo                                        |    1 (0.8%)     |       3 (1.3%)        |     1 (2.1%)     |    5 (1.2%)     |          |
| **¿Cuál es el nivel educacional más alto que has alcanzado?** |                 |                       |                  |                 |          |
|    Educación básica completa                                  |    0 (0.0%)     |       0 (0.0%)        |     0 (0.0%)     |    0 (0.0%)     |          |
|    Educación media completa                                   |    2 (1.5%)     |      205 (91.1%)      |     1 (2.1%)     |   208 (51.5%)   |          |
|    Educación técnico profesional completa                     |   15 (11.5%)    |       8 (3.6%)        |     1 (2.1%)     |    24 (5.9%)    |          |
|    Educación universitaria completa                           |   55 (42.0%)    |       11 (4.9%)       |    32 (66.7%)    |   98 (24.3%)    |          |
|    Educación de postgrado completa                            |   59 (45.0%)    |       1 (0.4%)        |    14 (29.2%)    |   74 (18.3%)    |          |
| **¿Actualmente te encuentras estudiando?**                    |                 |                       |                  |                 | \< 0.001 |
|    Sí                                                         |    0 (0.0%)     |     225 (100.0%)      |   48 (100.0%)    |   273 (67.6%)   |          |
|    No                                                         |  131 (100.0%)   |       0 (0.0%)        |     0 (0.0%)     |   131 (32.4%)   |          |

# Instrumentos de mentalización:

## Datos perdidos:

### Omisiones en MMQ

``` r
naniar::miss_var_summary(datos[,17:49])
```

    ## # A tibble: 33 x 3
    ##    variable   n_miss pct_miss
    ##    <chr>       <int>    <dbl>
    ##  1 q0007_0001     55     13.0
    ##  2 q0007_0002     55     13.0
    ##  3 q0007_0003     55     13.0
    ##  4 q0007_0004     55     13.0
    ##  5 q0007_0005     55     13.0
    ##  6 q0007_0006     55     13.0
    ##  7 q0007_0007     55     13.0
    ##  8 q0007_0008     55     13.0
    ##  9 q0007_0009     55     13.0
    ## 10 q0007_0010     55     13.0
    ## # ... with 23 more rows

``` r
vis_miss(datos[,17:49])
```

![](README_files/figure-gfm/unnamed-chunk-5-1.png)<!-- -->

Las omisiones son estables en todo el instrumento.

### Omisiones en MentS

``` r
naniar::miss_var_summary(datos[,50:77])
```

    ## # A tibble: 28 x 3
    ##    variable   n_miss pct_miss
    ##    <chr>       <int>    <dbl>
    ##  1 q0008_0001     74     17.5
    ##  2 q0008_0002     74     17.5
    ##  3 q0008_0003     74     17.5
    ##  4 q0008_0004     74     17.5
    ##  5 q0008_0005     74     17.5
    ##  6 q0008_0006     74     17.5
    ##  7 q0008_0007     74     17.5
    ##  8 q0008_0008     74     17.5
    ##  9 q0008_0009     74     17.5
    ## 10 q0008_0010     74     17.5
    ## # ... with 18 more rows

``` r
vis_miss(datos[,50:77])
```

![](README_files/figure-gfm/unnamed-chunk-6-1.png)<!-- -->

### Omisiones en ambos instrumentos:

``` r
vis_miss(datos[,17:77])
```

![](README_files/figure-gfm/unnamed-chunk-7-1.png)<!-- -->

Las omisiones son estables en todo el instrumento.

### Caracterización de las omisiones:

``` r
datos = datos %>% mutate(omision_mentalizacion = ifelse(
  is.na(q0008_0001)==T,"sí","no"
))
```

``` r
summary(tableby(omision_mentalizacion~edad+genero+educacion+estudias+estudio_actual, data = datos))
```

|                                                               |   no (N=348)    |    sí (N=74)    |  Total (N=422)  | p value |
|:--------------------------------------------------------------|:---------------:|:---------------:|:---------------:|--------:|
| **edad**                                                      |                 |                 |                 |   0.724 |
|    N-Miss                                                     |        0        |       14        |       14        |         |
|    Mean (SD)                                                  | 29.198 (11.870) | 28.617 (11.210) | 29.113 (11.764) |         |
|    Range                                                      | 0.000 - 85.000  | 18.000 - 67.000 | 0.000 - 85.000  |         |
| **¿Con qué género te identificas?**                           |                 |                 |                 |   0.469 |
|    N-Miss                                                     |        0        |       14        |       14        |         |
|    Masculino                                                  |   103 (29.6%)   |   23 (38.3%)    |   126 (30.9%)   |         |
|    Femenino                                                   |   234 (67.2%)   |   36 (60.0%)    |   270 (66.2%)   |         |
|    Otro                                                       |    6 (1.7%)     |    1 (1.7%)     |    7 (1.7%)     |         |
|    Prefiero no decirlo                                        |    5 (1.4%)     |    0 (0.0%)     |    5 (1.2%)     |         |
| **¿Cuál es el nivel educacional más alto que has alcanzado?** |                 |                 |                 |         |
|    N-Miss                                                     |        0        |       14        |       14        |         |
|    Educación básica completa                                  |    0 (0.0%)     |    0 (0.0%)     |    0 (0.0%)     |         |
|    Educación media completa                                   |   176 (50.6%)   |   32 (53.3%)    |   208 (51.0%)   |         |
|    Educación técnico profesional completa                     |    18 (5.2%)    |    7 (11.7%)    |    25 (6.1%)    |         |
|    Educación universitaria completa                           |   83 (23.9%)    |   17 (28.3%)    |   100 (24.5%)   |         |
|    Educación de postgrado completa                            |   71 (20.4%)    |    4 (6.7%)     |   75 (18.4%)    |         |
| **¿Actualmente te encuentras estudiando?**                    |                 |                 |                 |   0.937 |
|    N-Miss                                                     |        0        |       14        |       14        |         |
|    Sí                                                         |   236 (67.8%)   |   41 (68.3%)    |   277 (67.9%)   |         |
|    No                                                         |   112 (32.2%)   |   19 (31.7%)    |   131 (32.1%)   |         |
| **estudio_actual**                                            |                 |                 |                 |   0.700 |
|    N-Miss                                                     |        2        |       16        |       18        |         |
|    No                                                         |   112 (32.4%)   |   19 (32.8%)    |   131 (32.4%)   |         |
|    Universitaria                                              |   191 (55.2%)   |   34 (58.6%)    |   225 (55.7%)   |         |
|    Postgrado                                                  |   43 (12.4%)    |    5 (8.6%)     |   48 (11.9%)    |         |

Hasta el momento los grupos son similares.

## Descripción de los instrumentos:

### Descripción y depuración del MMQ:

El MMQ cuenta con 6 factores, 3 de los cuales dan cuenta de aspectos
positivos y 3 de aspectos negativos. No cuenta con ítems invertidos,
aunque el contenido es negativo para el caso de los factores negativos
(ej. A veces experimento cambios de ánimo que no puedo controlar),

``` r
nombres_MMQ = labelled::var_label(datos_un[,17:49])
nombres_MMQ = unlist(nombres_MMQ)
items_mmq = datos_un %>% select(17:49)
items_mmq = na.omit(items_mmq)
```

``` r
colnames(items_mmq) = nombres_MMQ
```

``` r
items_mmq %>% gather(key=item,value=respuesta,1:33) %>%
  filter(is.na(respuesta)==F) %>% 
  mutate(respuesta = factor(respuesta, levels = c("Totalmente en desacuerdo",
                            "En desacuerdo",
                            "Ni de acuerdo ni en desacuerdo",
                            "De acuerdo",
                            "Muy de acuerdo"))) %>%
  count(item,respuesta) %>% group_by(item) %>% 
  mutate(porc = round(n/sum(n),3)*100) %>% 
  ggplot(aes(x=item, y=porc, fill=respuesta)) +
  geom_col() +
  coord_flip() +
  theme_bw() +
  theme(legend.position = "bottom", axis.text.y = element_text(size = 6.5, color = "black")) +
  guides(fill=guide_legend(nrow=3,byrow=TRUE)) +
  geom_text(aes(label=str_c(porc,"%")), position = "stack", size=2)
```

    ## Warning: attributes are not identical across measure variables;
    ## they will be dropped

![](README_files/figure-gfm/unnamed-chunk-12-1.png)<!-- -->

### Análisis de subescalas:

El instrumento cuenta con 6 subescalas, compuestas de la siguiente
manera:

**Escalas positivas:**

F1 (reflexión): 1,6,8,10,16,17,18,31,32

F2 (ego-strength): 11,22,24,25,26,30

F3 (relational attunement): 4,5,14,21,28

**Escalas negativas:**

F4 (relational discomfort): 9,12,15,27,33

F5 (distrust): 13,19,20,29

F6 (emotional discontrol):2,3,7,23

¿Cuántos factores extraer?

``` r
psych::fa.parallel((datos[,17:49]))
```

![](README_files/figure-gfm/unnamed-chunk-13-1.png)<!-- -->

    ## Parallel analysis suggests that the number of factors =  6  and the number of components =  5

Al parecer es una buena solución 6 factores

``` r
print(fa(datos[,17:49], nfactors = 6,rotate = "oblimin", cor = "poly"),cut = .2)
```

    ## Loading required namespace: GPArotation

    ## Factor Analysis using method =  minres
    ## Call: fa(r = datos[, 17:49], nfactors = 6, rotate = "oblimin", cor = "poly")
    ## Standardized loadings (pattern matrix) based upon correlation matrix
    ##              MR1   MR2   MR5   MR3   MR4   MR6   h2   u2 com
    ## q0007_0001        0.28       -0.23             0.16 0.84 2.9
    ## q0007_0002              0.68                   0.40 0.60 1.1
    ## q0007_0003              0.64                   0.60 0.40 1.3
    ## q0007_0004                          0.75       0.63 0.37 1.1
    ## q0007_0005                          0.80       0.63 0.37 1.1
    ## q0007_0006 -0.25                    0.20  0.45 0.36 0.64 3.2
    ## q0007_0007              0.68                   0.66 0.34 1.2
    ## q0007_0008  0.24  0.56                         0.40 0.60 1.5
    ## q0007_0009 -0.29              0.22       -0.36 0.36 0.64 2.8
    ## q0007_0010        0.69                         0.55 0.45 1.2
    ## q0007_0011  0.51       -0.24                   0.47 0.53 1.6
    ## q0007_0012              0.27  0.22       -0.39 0.47 0.53 3.1
    ## q0007_0013                    0.81             0.66 0.34 1.0
    ## q0007_0014              0.20        0.29  0.43 0.52 0.48 2.9
    ## q0007_0015                    0.40             0.28 0.72 1.6
    ## q0007_0016        0.85                         0.74 0.26 1.1
    ## q0007_0017        0.73                         0.59 0.41 1.0
    ## q0007_0018        0.64                         0.46 0.54 1.1
    ## q0007_0019              0.50             -0.23 0.29 0.71 1.8
    ## q0007_0020                    0.60       -0.24 0.51 0.49 1.4
    ## q0007_0021                          0.23  0.38 0.40 0.60 3.3
    ## q0007_0022  0.56                               0.37 0.63 1.2
    ## q0007_0023        0.20  0.57                   0.43 0.57 1.4
    ## q0007_0024  0.83                               0.78 0.22 1.1
    ## q0007_0025  0.82                               0.74 0.26 1.1
    ## q0007_0026  0.61       -0.32                   0.60 0.40 1.5
    ## q0007_0027              0.25  0.20       -0.47 0.53 0.47 2.7
    ## q0007_0028                          0.72       0.58 0.42 1.2
    ## q0007_0029                    0.81             0.62 0.38 1.1
    ## q0007_0030  0.89                               0.83 0.17 1.1
    ## q0007_0031              0.20        0.24  0.47 0.49 0.51 2.2
    ## q0007_0032        0.37                         0.28 0.72 2.1
    ## q0007_0033              0.33  0.21       -0.23 0.33 0.67 3.2
    ## 
    ##                        MR1  MR2  MR5  MR3  MR4  MR6
    ## SS loadings           4.01 3.09 2.90 2.53 2.40 1.80
    ## Proportion Var        0.12 0.09 0.09 0.08 0.07 0.05
    ## Cumulative Var        0.12 0.22 0.30 0.38 0.45 0.51
    ## Proportion Explained  0.24 0.18 0.17 0.15 0.14 0.11
    ## Cumulative Proportion 0.24 0.42 0.60 0.75 0.89 1.00
    ## 
    ##  With factor correlations of 
    ##       MR1  MR2   MR5   MR3  MR4   MR6
    ## MR1  1.00 0.23 -0.35 -0.24 0.15  0.16
    ## MR2  0.23 1.00  0.14  0.12 0.37  0.21
    ## MR5 -0.35 0.14  1.00  0.32 0.24 -0.06
    ## MR3 -0.24 0.12  0.32  1.00 0.00 -0.24
    ## MR4  0.15 0.37  0.24  0.00 1.00  0.22
    ## MR6  0.16 0.21 -0.06 -0.24 0.22  1.00
    ## 
    ## Mean item complexity =  1.7
    ## Test of the hypothesis that 6 factors are sufficient.
    ## 
    ## The degrees of freedom for the null model are  528  and the objective function was  17.88 with Chi Square of  7315.51
    ## The degrees of freedom for the model are 345  and the objective function was  3.61 
    ## 
    ## The root mean square of the residuals (RMSR) is  0.04 
    ## The df corrected root mean square of the residuals is  0.05 
    ## 
    ## The harmonic number of observations is  367 with the empirical chi square  586  with prob <  9.7e-15 
    ## The total number of observations was  422  with Likelihood Chi Square =  1464.67  with prob <  1.4e-137 
    ## 
    ## Tucker Lewis Index of factoring reliability =  0.745
    ## RMSEA index =  0.088  and the 90 % confidence intervals are  0.083 0.092
    ## BIC =  -620.85
    ## Fit based upon off diagonal values = 0.98
    ## Measures of factor score adequacy             
    ##                                                    MR1  MR2  MR5  MR3  MR4  MR6
    ## Correlation of (regression) scores with factors   0.96 0.94 0.92 0.92 0.92 0.87
    ## Multiple R square of scores with factors          0.93 0.89 0.85 0.85 0.85 0.76
    ## Minimum correlation of possible factor scores     0.86 0.78 0.70 0.70 0.70 0.51

### Estructura interna:

**Respecto a reflexibility:** la estructura general se mantiene de forma
parcial, el ítem 6 y el 31 no están asociados al factor:

6.- Para entender las acciones de los demás, es fundamental comprender
lo que sienten

31.- Soy una persona que piensa en los demás.

Ambos ítems cargan al mismo factor, relational discomfort (ej. 27.- La
gente me abandona)

**Respecto a ego-strength:** la estructura general se mantiene.

**Respecto a relational attunement:** (ej. 5.- Me puedo conectar con el
estado mental de los demás). La estructura general se mantiene, aunque
hay dos ítems con cargas factoriales notablemente más bajas.

14.- Soy capaz de empatizar con otros cuando me cuentan algo.

21.- Soy sensible a lo que le pasa a los demás.

Estos ítems parecen estar más asociados a relational discomfort.

**Respecto a relational discomfort:** la estructura se mantiene de forma
parcial, el ítem 15 no se asocia al factor.

15.- Me asusta abrirme con los demás.

Este ítem está más asociado a distrust (ej. 13.- Es mejor tener cuidado
con los demás)

También el ítem 33 está muy poco asociado a este factor:

33.- Algunas personas son la causa de mis problemas

y está más asociado al factor emotional discontrol (ej. 7.- A veces
siento que estoy perdiendo el control de mis emociones)

Por último, este factor recibe cargas factoriales de los ítems 6, 14, 21
y 31:

6.- Para entender las acciones de los demás, es fundamental comprender
lo que sienten.

14.- Soy capaz de empatizar con otros cuando me cuentan algo.

21.- Soy sensible a lo que le pasa a los demás.

31.- Soy una persona que piensa en los demás.

Las cargas factoriales promedios son bajas, por lo que no queda claro el
contenido de este factor.

**Respecto a distrust:** la estructura se mantiene de forma parcial, el
ítem 19 no se asocia al mismo factor:

19.- Para mí las cosas son blancas o son negras.

Este ítem carga al factor emotional discontrol (ej. 2.- Soy una persona
impulsiva)

El ítem 15 (15.- Me asusta abrirme con los demás) si bien teóricamente
se lo ubicaba en relational discomfort, ahora se lo ubica en distrust
(ej. Es mejor tener cuidado con los desconocidos).

**Respecto a emotional discontrol:** la estructura se mantiene en su
totalidad, aunque habría que agregar el 19:

19.- Para mí las cosas son blancas o son negras

**Notas generales:**

El factor menos claro en cuanto a su contenido es emotional discomfort,
ya que las cargas de los ítems teóricamente asociados es baja (menores a
.50) y además tienden a cargar al mismo factor que otros 4 ítems,
teóricamente asociados a factores distintos:

6.- Para entender las acciones de los demás, es fundamental comprender
lo que sienten.

14.- Soy capaz de empatizar con otros cuando me cuentan algo.

21.- Soy sensible a lo que le pasa a los demás.

31.- Soy una persona que piensa en los demás.

Todos estos ítems tienen cargas bajas en general, y muestran su mayor
asociación con los ítems de emotional discomfort. Respecto al ítem 14 y
21 (originalmente de relational attunement), el 6 y el 31
(reflexibility), una hipótesis es que comparten en su contenido la
sensibilidad por los demás, de manera afectiva. En particular, el factor
de relational attunement agrupa ítems centrados en la empatía cognitiva,
y los ítems 14 y 21 tienen que ver con empatía afectiva. Por otro lado,
en reflexibility los ítems se centran en analizar el propio
comportamiento, y los ítems 6 y 31 se centran en la preocupación por los
demás.

Por su parte, relational attunement prácticamente perdió la mitad de sus
ítems asociados, ya que solo cargan fuertemente al mismo factor:

4.- Soy capaz de captar los aspectos más profundos de las personas que
me rodean.

5.- Me puedo conectar con el estado mental de los demás.

28.- Puedo conectarme fácilmente con lo que piensan las otras personas.

A su vez el ítem 15 (Me asusta abrirme con los demás) se asocia al
factor distrust.

El ítem 19 (Para mí las cosas son blancas o son negras) se asocia a
emotional discontrol

### Descripción y depuración del MentS:

El instrumento cuenta con 3 factores (motivation, self y others), y
algunos ítems están reversados.

``` r
nombres_MentS = labelled::var_label(datos_un[,50:77])
nombres_MentS = unlist(nombres_MentS)
items_ments = datos_un %>% select(50:77)
items_ments = na.omit(items_ments)
```

Descriptivos:

``` r
nombres_MentS = str_sub(nombres_MentS, start = 1, end = 100)
colnames(items_ments) = nombres_MentS
items_ments %>% gather(key=item, value = respuesta) %>% mutate(respuesta = factor(respuesta, levels = c("Totalmente en desacuerdo",
                            "En desacuerdo",
                            "Ni de acuerdo ni en desacuerdo",
                            "De acuerdo",
                            "Muy de acuerdo"))) %>%
  count(item,respuesta) %>% group_by(item) %>% 
  mutate(porc = round(n/sum(n),3)*100) %>% 
  ggplot(aes(x=item, y=porc, fill=respuesta)) +
  geom_col() +
  coord_flip() +
  theme_bw() +
  theme(legend.position = "bottom", axis.text.y = element_text(size = 6.5, color = "black")) +
  guides(fill=guide_legend(nrow=3,byrow=TRUE)) +
  geom_text(aes(label=str_c(porc,"%")), position = "stack", size=2)
```

    ## Warning: attributes are not identical across measure variables;
    ## they will be dropped

![](README_files/figure-gfm/unnamed-chunk-16-1.png)<!-- -->

Ahora hay que reversal los ítems en negativo:

8, 9, 11, 14, 18, 19, 21, 22, 26, 27

``` r
ments = datos[,50:77]
ments = na.omit(ments)
ments = ments %>% mutate(across(.cols = c(8,9,11,14,18,19,21,22,26,27), .fns = function(x){
  x = 6-x
}))
```

``` r
library(corrplot)
```

    ## corrplot 0.92 loaded

``` r
corrplot(cor(ments),order="hclust",addrect =3)
```

![](README_files/figure-gfm/unnamed-chunk-18-1.png)<!-- -->

``` r
fa.parallel(ments)
```

![](README_files/figure-gfm/unnamed-chunk-19-1.png)<!-- -->

    ## Parallel analysis suggests that the number of factors =  5  and the number of components =  4

``` r
fa_ments = fa(ments, nfactors = 3, rotate = "oblimin", cor = "poly")
print(fa_ments, cut = 0.3)
```

    ## Factor Analysis using method =  minres
    ## Call: fa(r = ments, nfactors = 3, rotate = "oblimin", cor = "poly")
    ## Standardized loadings (pattern matrix) based upon correlation matrix
    ##              MR2   MR1   MR3   h2   u2 com
    ## q0008_0001        0.73       0.46 0.54 1.1
    ## q0008_0002        0.35       0.22 0.78 1.6
    ## q0008_0003              0.67 0.54 0.46 1.1
    ## q0008_0004        0.49  0.31 0.47 0.53 2.3
    ## q0008_0005              0.61 0.43 0.57 1.1
    ## q0008_0006              0.43 0.35 0.65 1.7
    ## q0008_0007        0.47       0.23 0.77 1.0
    ## q0008_0008  0.69             0.48 0.52 1.0
    ## q0008_0009        0.47       0.36 0.64 1.4
    ## q0008_0010              0.47 0.22 0.78 1.0
    ## q0008_0011  0.66             0.46 0.54 1.0
    ## q0008_0012              0.65 0.44 0.56 1.0
    ## q0008_0013        0.60       0.41 0.59 1.1
    ## q0008_0014  0.41  0.42 -0.31 0.35 0.65 2.8
    ## q0008_0015        0.57       0.34 0.66 1.0
    ## q0008_0016                   0.21 0.79 2.4
    ## q0008_0017        0.43       0.24 0.76 1.1
    ## q0008_0018  0.58             0.35 0.65 1.1
    ## q0008_0019  0.47             0.25 0.75 1.4
    ## q0008_0020              0.67 0.46 0.54 1.0
    ## q0008_0021  0.88             0.76 0.24 1.0
    ## q0008_0022  0.74             0.59 0.41 1.1
    ## q0008_0023              0.52 0.37 0.63 1.2
    ## q0008_0024        0.67       0.58 0.42 1.2
    ## q0008_0025  0.68             0.55 0.45 1.2
    ## q0008_0026                   0.30 0.70 2.8
    ## q0008_0027        0.43       0.26 0.74 1.3
    ## q0008_0028        0.43       0.25 0.75 1.2
    ## 
    ##                        MR2  MR1  MR3
    ## SS loadings           3.83 3.84 3.24
    ## Proportion Var        0.14 0.14 0.12
    ## Cumulative Var        0.14 0.27 0.39
    ## Proportion Explained  0.35 0.35 0.30
    ## Cumulative Proportion 0.35 0.70 1.00
    ## 
    ##  With factor correlations of 
    ##      MR2  MR1  MR3
    ## MR2 1.00 0.15 0.11
    ## MR1 0.15 1.00 0.42
    ## MR3 0.11 0.42 1.00
    ## 
    ## Mean item complexity =  1.4
    ## Test of the hypothesis that 3 factors are sufficient.
    ## 
    ## The degrees of freedom for the null model are  378  and the objective function was  12.16 with Chi Square of  4094.23
    ## The degrees of freedom for the model are 297  and the objective function was  3.38 
    ## 
    ## The root mean square of the residuals (RMSR) is  0.06 
    ## The df corrected root mean square of the residuals is  0.07 
    ## 
    ## The harmonic number of observations is  348 with the empirical chi square  882.51  with prob <  2e-59 
    ## The total number of observations was  348  with Likelihood Chi Square =  1132.35  with prob <  9.6e-98 
    ## 
    ## Tucker Lewis Index of factoring reliability =  0.712
    ## RMSEA index =  0.09  and the 90 % confidence intervals are  0.085 0.096
    ## BIC =  -605.75
    ## Fit based upon off diagonal values = 0.95
    ## Measures of factor score adequacy             
    ##                                                    MR2  MR1  MR3
    ## Correlation of (regression) scores with factors   0.95 0.93 0.92
    ## Multiple R square of scores with factors          0.90 0.86 0.84
    ## Minimum correlation of possible factor scores     0.81 0.72 0.69

**Respecto al factor de Motivation:**

Los ítems que cargan al mismo factor teórico son: 1,4,7,9,13,15,17,24,27

1.- Encuentro importante entender las razones de mis acciones.

4.- Suelo pensar en otras personas y su comportamiento

7.- Cuando alguien me cae mal, trato de entender por qué siento eso

9.- No me gusta perder el tiempo tratando de entender el comportamiento
de los demás.

13.- Encuentro importante entender lo que pasa en mis relaciones con
personas cercanas

15.- Para entender por qué alguien hace algo, necesitamos saber lo que
piensa, quiere y siente

17.- Me gusta leer sobre temas relacionados con psicología.

24.- Siempre me ha interesado saber por qué las personas se comportan de
la manera en que lo hacen

27.- No tiene sentido pensar en las intenciones y deseos de los demás
porque todos dependemos de las circunstancias de la vida

A eso habría que sumarle el ítem 2,14 y 28, aunque con cargas más bajas:

2.- Para sacar conclusiones sobre la personalidad de los demás, me fijo
en lo que dicen y hacen

14.- Prefiero no saber algo de mí si no me va a gustar

28.- Una de las cosas más importantes que deben aprender los/as niños/as
es a expresar sus sentimientos y deseos

El ítem 16, por su parte, no se asocia al factor de motivación, ni
tampoco a ningún otro claramente.

16.- Suelo hablar de emociones con mis cercanos

***Resumen del factor Motivation:***

El factor está centrado en buscar entender la conducta y emociones
propias y de los demás, es decir, de mentalizar a las personas y a sí
mismo. Esto también vinculado con actitudes en entender el mundo
psíquico. El ítem 16, no necesariamente tiene que ver con interés en
buscar entender el mundo psíquico propio y de los demás, y apunta a la
frecuencia de una conducta, la cual puede ocurrir o no ocurrir por
variadas circunstancias.

**Respecto al factor de Others:**

Los ítems que cargan al mismo factor teórico son: 3, 5, 6, 12, 20, 23.

3.- Puedo reconocer los sentimientos de los demás

5.- Suelo darme cuenta de lo que hace sentir incómodas a las personas

6.- Puedo empatizar con los sentimientos de los demás. (marginalmente)

10.- Puedo predecir lo que van a hacer los demás si conozco sus
creencias y sentimientos (marginalmente).

12.- A veces, puedo comprender lo que el otro siente antes de que me lo
diga

20.- Puedo describir con precisión y detalle la personalidad de mis
cercanos

23.- Las personas me dicen que las entiendo y que les doy buenos
consejos

Los ítems que debieran cargar y no lo hacen son: 2, 25 y 28

2.- Para sacar conclusiones sobre la personalidad de los demás, me fijo
en lo que dicen y hacen.

25.- Puedo describir fácilmente lo que siento

28.- Una de las cosas más importantes que deben aprender los/as niños/as
es a expresar sus sentimientos y deseos.

***Descripción del factor otros:***

Está vinculado a la percepción de la propia capacidad para comprender a
los demás (sus sentimientos o lo que piensan) y empatizar con lo que
sienten. El ítem 2, 25, 28 no están asociados al factor debido a que no
aluden a la capacidad de comprender a otros.

**Respecto al factor Self:**

Los ítems que cargan al factor tórico son los 8, 11, 14, 18, 19, 21, 22,
26:

8.- Cuando estoy mal de ánimo, no estoy seguro/a si es tristeza, miedo o
rabia

11.- Con frecuencia, ni a mí me puedo explicar por qué hice algo

14.- Prefiero no saber algo de mí si no me va a gustar (carga menos que
para el factor motivation)

18.- Me cuesta reconocer ante mí mismo que estoy triste, dolido o
asustado

19.- No me gusta pensar en mis problemas

21.- Suelo confundirme acerca de cuáles son mis sentimientos

22.- Me cuesta encontrar las palabras adecuadas para expresar mis
sentimientos

26.- Suelo distraerme cuando la gente me habla de sus sentimientos y
necesidades (marginalmente)

A estos ítems se le suma el ítem 25:

25.- Puedo describir fácilmente lo que siento

***Descripción del factor Self:***

A nivel central, refiere a la percepción del grado de claridad/confusión
que tiene una persona respecto a sus propios pensamientos, emociones y
acciones.

# Relación con otras variables:

### EQ:

El Empathy Quotient \[EQ\] fue adaptado a la población chilena y cuenta
con evidencia de validez en esta población (revisar paper). Este
instrumento cuenta con 3 subdimensiones: empatía cognitiva, reactividad
emocional y habilidades sociales.

``` r
items_eq = datos %>% dplyr::select(starts_with("q0009"))
```

``` r
naniar::miss_var_summary(items_eq)
```

    ## # A tibble: 60 x 3
    ##    variable   n_miss pct_miss
    ##    <chr>       <int>    <dbl>
    ##  1 q0009_0001    112     26.5
    ##  2 q0009_0002    112     26.5
    ##  3 q0009_0003    112     26.5
    ##  4 q0009_0004    112     26.5
    ##  5 q0009_0005    112     26.5
    ##  6 q0009_0006    112     26.5
    ##  7 q0009_0007    112     26.5
    ##  8 q0009_0008    112     26.5
    ##  9 q0009_0009    112     26.5
    ## 10 q0009_0010    112     26.5
    ## # ... with 50 more rows

``` r
vis_miss(items_eq)
```

![](README_files/figure-gfm/unnamed-chunk-22-1.png)<!-- -->

``` r
items_eq=items_eq %>% mutate(across(.cols = everything(), .fns = function(x){
  x=5-x
}))
```

``` r
items_eq$eq_ec = rowSums(items_eq[,c(1,19,25,26,36,41,43,44,52,54,55,58,60)])
items_eq$eq_hs = rowSums(items_eq[,c(4,8,12,14,21,35,57)])
items_eq$eq_re = rowSums(items_eq[,c(15,27,32,42,46,48,49,50,59)])
```

### IRI:

El Interpersonal reactivity index (IRI) cuenta con 4 factores:

Fantasy Scale (FS); Perspective taking (PT); Empathic Concern (EC) and
Personal Distress (PD) scales

``` r
items_iri = datos %>% select(starts_with("q0010"))
```

``` r
naniar::miss_var_summary(items_iri)
```

    ## # A tibble: 28 x 3
    ##    variable   n_miss pct_miss
    ##    <chr>       <int>    <dbl>
    ##  1 q0010_0001    125     29.6
    ##  2 q0010_0002    125     29.6
    ##  3 q0010_0003    125     29.6
    ##  4 q0010_0004    125     29.6
    ##  5 q0010_0005    125     29.6
    ##  6 q0010_0006    125     29.6
    ##  7 q0010_0007    125     29.6
    ##  8 q0010_0008    125     29.6
    ##  9 q0010_0009    125     29.6
    ## 10 q0010_0010    125     29.6
    ## # ... with 18 more rows

``` r
vis_miss(items_iri)
```

![](README_files/figure-gfm/unnamed-chunk-26-1.png)<!-- -->

Reversar ítems:

``` r
items_iri$fs = NULL

items_iri = items_iri %>% mutate(across(.cols = c(3,4,7,12,13,14,15,18,19), .fns = function(x){6-x}))

corrplot(cor(items_iri,use="complete.obs"), order = "hclust", addrect = 4)
```

![](README_files/figure-gfm/unnamed-chunk-27-1.png)<!-- -->

``` r
items_iri$fs = rowSums(items_iri[,c(1,5,7,12,16,20,26)])

items_iri$pd = rowSums(items_iri[,c(6,10,13,17,19,24,27)])

items_iri$pt = rowSums(items_iri[,c(3,8,11,15,21,25,28)])

items_iri$ec = rowSums(items_iri[,c(2,4,9,14,18,20,22)])
```

## Buss Perry Aggression Questionnaire (BPAQ)

Este instrumento está compuesto por 4 factores: agresión física,
agresión verbal, rabia y hostilidad.

Agresión física: 1,5,9,17,21,27,29

Agresión verbal: 2,6,14,18

Anger: 3,7,11,22

Hostilidad: 4,16,20,26,28

``` r
items_aq = datos %>% select(starts_with("q0011"))
```

``` r
naniar::miss_var_summary(items_aq)
```

    ## # A tibble: 29 x 3
    ##    variable   n_miss pct_miss
    ##    <chr>       <int>    <dbl>
    ##  1 q0011_0001    132     31.3
    ##  2 q0011_0002    132     31.3
    ##  3 q0011_0003    132     31.3
    ##  4 q0011_0004    132     31.3
    ##  5 q0011_0005    132     31.3
    ##  6 q0011_0006    132     31.3
    ##  7 q0011_0007    132     31.3
    ##  8 q0011_0008    132     31.3
    ##  9 q0011_0009    132     31.3
    ## 10 q0011_0010    132     31.3
    ## # ... with 19 more rows

``` r
vis_miss(items_aq)
```

![](README_files/figure-gfm/unnamed-chunk-29-1.png)<!-- -->

``` r
items_aq$af = rowSums(items_aq[,c(1,5,9,17,21,27,29)])
items_aq$av = rowSums(items_aq[,c(2,6,14,18)])
items_aq$ang = rowSums(items_aq[,c(3,7,11,22)])
items_aq$host = rowSums(items_aq[,c(4,16,20,26,28)])
```

# Asociación con otras variables:

EQ: empatía cognitiva (EC), reactividad emocional (RE) y habilidades
sociales (HS).

IRI: Fantasy Scale (FS); Perspective taking (PT); Empathic Concern (EC)
and Personal Distress (PD) scales

BPAQ: agresión física (AF), agresión verbal (AV), rabia (R) y hostilidad
(H).

``` r
datos_fa = datos
datos_fa$suma_eq_ec = items_eq$eq_ec
datos_fa$suma_eq_hs = items_eq$eq_hs
datos_fa$suma_eq_re = items_eq$eq_re 

## iri: 

datos_fa$suma_iri_fs = items_iri$fs
datos_fa$suma_iri_pd = items_iri$pd
datos_fa$suma_iri_pt = items_iri$pt 
datos_fa$suma_iri_ec = items_iri$ec

## BPAQ:


datos_fa$suma_aq_af = items_aq$af
datos_fa$suma_aq_av = items_aq$av
datos_fa$suma_aq_r = items_aq$ang
datos_fa$suma_aq_h = items_aq$host
```

## MMQ:

El instrumento cuenta con 6 subescalas, compuestas de la siguiente
manera:

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

Relational Attunement: originalmente ítems 4,5,14,21,28; propuesta:
4,5,28. Ítem 14 y 21 asociados a relational discomfort. Hipótesis es
predominancia de empatía cognitiva, y los ítems más afectivos podrían
tener que ver con relaciones sociales concretas.

Relational Discomfort: originalmente ítems 9,12,15,27,33; propuesta:
9,12,27,33 6,14,21,31. Hipótesis es predominancia de ítems sobre empatía
emocional.

### Relational Attunement:

Model1 (original)

``` r
library(lavaan)
```

    ## This is lavaan 0.6-10
    ## lavaan is FREE software! Please report any bugs.

    ## 
    ## Attaching package: 'lavaan'

    ## The following object is masked from 'package:psych':
    ## 
    ##     cor2cov

``` r
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

    ## lavaan 0.6-10 ended normally after 36 iterations
    ## 
    ##   Estimator                                         ML
    ##   Optimization method                           NLMINB
    ##   Number of model parameters                        14
    ##                                                       
    ##                                                   Used       Total
    ##   Number of observations                           297         422
    ##                                                                   
    ## Model Test User Model:
    ##                                                Standard      Robust
    ##   Test Statistic                                146.426     105.721
    ##   Degrees of freedom                                 14          14
    ##   P-value (Chi-square)                            0.000       0.000
    ##   Scaling correction factor                                   1.385
    ##        Yuan-Bentler correction (Mplus variant)                     
    ## 
    ## Model Test Baseline Model:
    ## 
    ##   Test statistic                               626.530     423.309
    ##   Degrees of freedom                                21          21
    ##   P-value                                        0.000       0.000
    ##   Scaling correction factor                                  1.480
    ## 
    ## User Model versus Baseline Model:
    ## 
    ##   Comparative Fit Index (CFI)                    0.781       0.772
    ##   Tucker-Lewis Index (TLI)                       0.672       0.658
    ##                                                                   
    ##   Robust Comparative Fit Index (CFI)                         0.787
    ##   Robust Tucker-Lewis Index (TLI)                            0.680
    ## 
    ## Loglikelihood and Information Criteria:
    ## 
    ##   Loglikelihood user model (H0)              -3267.730   -3267.730
    ##   Scaling correction factor                                  1.550
    ##       for the MLR correction                                      
    ##   Loglikelihood unrestricted model (H1)             NA          NA
    ##   Scaling correction factor                                  1.467
    ##       for the MLR correction                                      
    ##                                                                   
    ##   Akaike (AIC)                                6563.460    6563.460
    ##   Bayesian (BIC)                              6615.173    6615.173
    ##   Sample-size adjusted Bayesian (BIC)         6570.774    6570.774
    ## 
    ## Root Mean Square Error of Approximation:
    ## 
    ##   RMSEA                                          0.178       0.149
    ##   90 Percent confidence interval - lower         0.153       0.127
    ##   90 Percent confidence interval - upper         0.205       0.171
    ##   P-value RMSEA <= 0.05                          0.000       0.000
    ##                                                                   
    ##   Robust RMSEA                                               0.175
    ##   90 Percent confidence interval - lower                     0.145
    ##   90 Percent confidence interval - upper                     0.207
    ## 
    ## Standardized Root Mean Square Residual:
    ## 
    ##   SRMR                                           0.119       0.119
    ## 
    ## Parameter Estimates:
    ## 
    ##   Standard errors                             Sandwich
    ##   Information bread                           Observed
    ##   Observed information based on                Hessian
    ## 
    ## Latent Variables:
    ##                    Estimate  Std.Err  z-value  P(>|z|)   Std.lv  Std.all
    ##   f_ra1 =~                                                              
    ##     q0007_0004        1.000                               0.484    0.618
    ##     q0007_0005        1.049    0.111    9.430    0.000    0.508    0.611
    ##     q0007_0028        0.872    0.096    9.102    0.000    0.422    0.632
    ##     q0007_0014        0.660    0.125    5.282    0.000    0.319    0.549
    ##     q0007_0021        0.819    0.211    3.889    0.000    0.396    0.492
    ## 
    ## Covariances:
    ##                    Estimate  Std.Err  z-value  P(>|z|)   Std.lv  Std.all
    ##   f_ra1 ~~                                                              
    ##     suma_eq_ec        1.878    0.369    5.089    0.000    3.882    0.686
    ##     suma_iri_ec       0.949    0.217    4.376    0.000    1.961    0.385
    ## 
    ## Variances:
    ##                    Estimate  Std.Err  z-value  P(>|z|)   Std.lv  Std.all
    ##    .q0007_0004        0.378    0.054    7.000    0.000    0.378    0.618
    ##    .q0007_0005        0.432    0.057    7.646    0.000    0.432    0.627
    ##    .q0007_0028        0.268    0.038    7.040    0.000    0.268    0.601
    ##    .q0007_0014        0.236    0.032    7.328    0.000    0.236    0.698
    ##    .q0007_0021        0.493    0.064    7.699    0.000    0.493    0.758
    ##     suma_eq_ec       32.000    2.863   11.178    0.000   32.000    1.000
    ##     suma_iri_ec      25.977    2.657    9.777    0.000   25.977    1.000
    ##     f_ra1             0.234    0.059    3.982    0.000    1.000    1.000

Model2:

``` r
cod_ra_cov2 = "f_ra1 =~ q0007_0004+q0007_0005+q0007_0028
f_ra1~~suma_eq_ec
f_ra1~~suma_iri_ec"

# ordered = names(datos_fa[,17:77]) agregar en caso de:

mod_ra_cov2 = sem(model=cod_ra_cov2, data = datos_fa,
                  estimator = "MLR")

summary(mod_ra_cov2, fit.measures = T, standardized = TRUE)
```

    ## lavaan 0.6-10 ended normally after 34 iterations
    ## 
    ##   Estimator                                         ML
    ##   Optimization method                           NLMINB
    ##   Number of model parameters                        10
    ##                                                       
    ##                                                   Used       Total
    ##   Number of observations                           297         422
    ##                                                                   
    ## Model Test User Model:
    ##                                                Standard      Robust
    ##   Test Statistic                                 28.995      19.152
    ##   Degrees of freedom                                  5           5
    ##   P-value (Chi-square)                            0.000       0.002
    ##   Scaling correction factor                                   1.514
    ##        Yuan-Bentler correction (Mplus variant)                     
    ## 
    ## Model Test Baseline Model:
    ## 
    ##   Test statistic                               375.475     260.468
    ##   Degrees of freedom                                10          10
    ##   P-value                                        0.000       0.000
    ##   Scaling correction factor                                  1.442
    ## 
    ## User Model versus Baseline Model:
    ## 
    ##   Comparative Fit Index (CFI)                    0.934       0.943
    ##   Tucker-Lewis Index (TLI)                       0.869       0.887
    ##                                                                   
    ##   Robust Comparative Fit Index (CFI)                         0.941
    ##   Robust Tucker-Lewis Index (TLI)                            0.881
    ## 
    ## Loglikelihood and Information Criteria:
    ## 
    ##   Loglikelihood user model (H0)              -2706.323   -2706.323
    ##   Scaling correction factor                                  1.285
    ##       for the MLR correction                                      
    ##   Loglikelihood unrestricted model (H1)             NA          NA
    ##   Scaling correction factor                                  1.361
    ##       for the MLR correction                                      
    ##                                                                   
    ##   Akaike (AIC)                                5432.647    5432.647
    ##   Bayesian (BIC)                              5469.584    5469.584
    ##   Sample-size adjusted Bayesian (BIC)         5437.871    5437.871
    ## 
    ## Root Mean Square Error of Approximation:
    ## 
    ##   RMSEA                                          0.127       0.098
    ##   90 Percent confidence interval - lower         0.085       0.062
    ##   90 Percent confidence interval - upper         0.174       0.136
    ##   P-value RMSEA <= 0.05                          0.002       0.017
    ##                                                                   
    ##   Robust RMSEA                                               0.120
    ##   90 Percent confidence interval - lower                     0.066
    ##   90 Percent confidence interval - upper                     0.179
    ## 
    ## Standardized Root Mean Square Residual:
    ## 
    ##   SRMR                                           0.090       0.090
    ## 
    ## Parameter Estimates:
    ## 
    ##   Standard errors                             Sandwich
    ##   Information bread                           Observed
    ##   Observed information based on                Hessian
    ## 
    ## Latent Variables:
    ##                    Estimate  Std.Err  z-value  P(>|z|)   Std.lv  Std.all
    ##   f_ra1 =~                                                              
    ##     q0007_0004        1.000                               0.552    0.697
    ##     q0007_0005        1.008    0.096   10.549    0.000    0.556    0.661
    ##     q0007_0028        0.849    0.098    8.628    0.000    0.468    0.692
    ## 
    ## Covariances:
    ##                    Estimate  Std.Err  z-value  P(>|z|)   Std.lv  Std.all
    ##   f_ra1 ~~                                                              
    ##     suma_eq_ec        2.224    0.289    7.684    0.000    4.033    0.713
    ##     suma_iri_ec       0.445    0.151    2.952    0.003    0.807    0.158
    ## 
    ## Variances:
    ##                    Estimate  Std.Err  z-value  P(>|z|)   Std.lv  Std.all
    ##    .q0007_0004        0.322    0.042    7.603    0.000    0.322    0.514
    ##    .q0007_0005        0.398    0.064    6.236    0.000    0.398    0.563
    ##    .q0007_0028        0.238    0.030    7.911    0.000    0.238    0.521
    ##     suma_eq_ec       32.000    2.863   11.178    0.000   32.000    1.000
    ##     suma_iri_ec      25.977    2.657    9.777    0.000   25.977    1.000
    ##     f_ra1             0.304    0.051    5.966    0.000    1.000    1.000

Conclusión: los ítems 14 y 21 tienen un efecto en el factor que lo hace
tener más que ver con la vinculación emocional (relación con EC), y
disminuye levemente su asociación con la empatía cognitiva. El ajuste
general del modelo disminuye. Se propone que estos ítems podrían no
pertenecer al mismo constructo.

### Relational discomfort:

Model1: (original)

``` r
cod_rd_cov1 = "f_rd1 =~ q0007_0009+q0007_0012+q0007_0015+q0007_0027+q0007_0033
f_rd1~~suma_eq_ec
f_rd1~~suma_iri_ec
f_rd1~~suma_aq_h"

# ordered = names(datos_fa[,17:77]) agregar en caso de:

mod_rd_cov1 = sem(model=cod_rd_cov1, data = datos_fa,
                  estimator = "MLR")

summary(mod_rd_cov1, fit.measures = T, standardized = TRUE)
```

    ## lavaan 0.6-10 ended normally after 37 iterations
    ## 
    ##   Estimator                                         ML
    ##   Optimization method                           NLMINB
    ##   Number of model parameters                        16
    ##                                                       
    ##                                                   Used       Total
    ##   Number of observations                           290         422
    ##                                                                   
    ## Model Test User Model:
    ##                                                Standard      Robust
    ##   Test Statistic                                 57.884      48.731
    ##   Degrees of freedom                                 20          20
    ##   P-value (Chi-square)                            0.000       0.000
    ##   Scaling correction factor                                   1.188
    ##        Yuan-Bentler correction (Mplus variant)                     
    ## 
    ## Model Test Baseline Model:
    ## 
    ##   Test statistic                               451.496     383.499
    ##   Degrees of freedom                                28          28
    ##   P-value                                        0.000       0.000
    ##   Scaling correction factor                                  1.177
    ## 
    ## User Model versus Baseline Model:
    ## 
    ##   Comparative Fit Index (CFI)                    0.911       0.919
    ##   Tucker-Lewis Index (TLI)                       0.875       0.887
    ##                                                                   
    ##   Robust Comparative Fit Index (CFI)                         0.918
    ##   Robust Tucker-Lewis Index (TLI)                            0.886
    ## 
    ## Loglikelihood and Information Criteria:
    ## 
    ##   Loglikelihood user model (H0)              -4521.504   -4521.504
    ##   Scaling correction factor                                  1.072
    ##       for the MLR correction                                      
    ##   Loglikelihood unrestricted model (H1)             NA          NA
    ##   Scaling correction factor                                  1.136
    ##       for the MLR correction                                      
    ##                                                                   
    ##   Akaike (AIC)                                9075.009    9075.009
    ##   Bayesian (BIC)                              9133.727    9133.727
    ##   Sample-size adjusted Bayesian (BIC)         9082.988    9082.988
    ## 
    ## Root Mean Square Error of Approximation:
    ## 
    ##   RMSEA                                          0.081       0.070
    ##   90 Percent confidence interval - lower         0.057       0.048
    ##   90 Percent confidence interval - upper         0.106       0.094
    ##   P-value RMSEA <= 0.05                          0.019       0.069
    ##                                                                   
    ##   Robust RMSEA                                               0.077
    ##   90 Percent confidence interval - lower                     0.050
    ##   90 Percent confidence interval - upper                     0.104
    ## 
    ## Standardized Root Mean Square Residual:
    ## 
    ##   SRMR                                           0.077       0.077
    ## 
    ## Parameter Estimates:
    ## 
    ##   Standard errors                             Sandwich
    ##   Information bread                           Observed
    ##   Observed information based on                Hessian
    ## 
    ## Latent Variables:
    ##                    Estimate  Std.Err  z-value  P(>|z|)   Std.lv  Std.all
    ##   f_rd1 =~                                                              
    ##     q0007_0009        1.000                               0.578    0.579
    ##     q0007_0012        1.095    0.146    7.516    0.000    0.633    0.660
    ##     q0007_0015        0.906    0.162    5.599    0.000    0.524    0.444
    ##     q0007_0027        1.201    0.148    8.117    0.000    0.694    0.652
    ##     q0007_0033        1.024    0.152    6.742    0.000    0.592    0.550
    ## 
    ## Covariances:
    ##                    Estimate  Std.Err  z-value  P(>|z|)   Std.lv  Std.all
    ##   f_rd1 ~~                                                              
    ##     suma_eq_ec       -0.155    0.190   -0.816    0.415   -0.268   -0.047
    ##     suma_iri_ec       0.032    0.161    0.197    0.844    0.055    0.011
    ##     suma_aq_h         1.566    0.257    6.095    0.000    2.709    0.751
    ## 
    ## Variances:
    ##                    Estimate  Std.Err  z-value  P(>|z|)   Std.lv  Std.all
    ##    .q0007_0009        0.661    0.073    9.039    0.000    0.661    0.664
    ##    .q0007_0012        0.520    0.061    8.470    0.000    0.520    0.565
    ##    .q0007_0015        1.116    0.097   11.496    0.000    1.116    0.803
    ##    .q0007_0027        0.652    0.069    9.475    0.000    0.652    0.575
    ##    .q0007_0033        0.809    0.071   11.469    0.000    0.809    0.697
    ##     suma_eq_ec       32.559    2.919   11.154    0.000   32.559    1.000
    ##     suma_iri_ec      25.911    2.728    9.497    0.000   25.911    1.000
    ##     suma_aq_h        13.007    1.165   11.165    0.000   13.007    1.000
    ##     f_rd1             0.334    0.076    4.415    0.000    1.000    1.000

Model2: (propuesta)

``` r
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

    ## lavaan 0.6-10 ended normally after 51 iterations
    ## 
    ##   Estimator                                         ML
    ##   Optimization method                           NLMINB
    ##   Number of model parameters                        22
    ##                                                       
    ##                                                   Used       Total
    ##   Number of observations                           290         422
    ##                                                                   
    ## Model Test User Model:
    ##                                                Standard      Robust
    ##   Test Statistic                                391.043     379.388
    ##   Degrees of freedom                                 44          44
    ##   P-value (Chi-square)                            0.000       0.000
    ##   Scaling correction factor                                   1.031
    ##        Yuan-Bentler correction (Mplus variant)                     
    ## 
    ## Model Test Baseline Model:
    ## 
    ##   Test statistic                               805.840     641.055
    ##   Degrees of freedom                                55          55
    ##   P-value                                        0.000       0.000
    ##   Scaling correction factor                                  1.257
    ## 
    ## User Model versus Baseline Model:
    ## 
    ##   Comparative Fit Index (CFI)                    0.538       0.428
    ##   Tucker-Lewis Index (TLI)                       0.422       0.285
    ##                                                                   
    ##   Robust Comparative Fit Index (CFI)                         0.531
    ##   Robust Tucker-Lewis Index (TLI)                            0.413
    ## 
    ## Loglikelihood and Information Criteria:
    ## 
    ##   Loglikelihood user model (H0)              -5266.717   -5266.717
    ##   Scaling correction factor                                  1.813
    ##       for the MLR correction                                      
    ##   Loglikelihood unrestricted model (H1)             NA          NA
    ##   Scaling correction factor                                  1.292
    ##       for the MLR correction                                      
    ##                                                                   
    ##   Akaike (AIC)                               10577.433   10577.433
    ##   Bayesian (BIC)                             10658.170   10658.170
    ##   Sample-size adjusted Bayesian (BIC)        10588.404   10588.404
    ## 
    ## Root Mean Square Error of Approximation:
    ## 
    ##   RMSEA                                          0.165       0.162
    ##   90 Percent confidence interval - lower         0.150       0.148
    ##   90 Percent confidence interval - upper         0.180       0.177
    ##   P-value RMSEA <= 0.05                          0.000       0.000
    ##                                                                   
    ##   Robust RMSEA                                               0.165
    ##   90 Percent confidence interval - lower                     0.150
    ##   90 Percent confidence interval - upper                     0.180
    ## 
    ## Standardized Root Mean Square Residual:
    ## 
    ##   SRMR                                           0.155       0.155
    ## 
    ## Parameter Estimates:
    ## 
    ##   Standard errors                             Sandwich
    ##   Information bread                           Observed
    ##   Observed information based on                Hessian
    ## 
    ## Latent Variables:
    ##                    Estimate  Std.Err  z-value  P(>|z|)   Std.lv  Std.all
    ##   f_rd1 =~                                                              
    ##     q0007_0009        1.000                               0.296    0.298
    ##     q0007_0012        0.986    0.220    4.478    0.000    0.292    0.306
    ##     q0007_0027        0.974    0.206    4.737    0.000    0.288    0.272
    ##     q0007_0033        0.789    0.269    2.938    0.003    0.233    0.218
    ##     q0007_0006r       0.592    0.370    1.598    0.110    0.175    0.251
    ##     q0007_0014r       1.161    0.709    1.638    0.101    0.343    0.597
    ##     q0007_0021r       1.707    1.125    1.517    0.129    0.505    0.640
    ##     q0007_0031r       1.226    0.826    1.485    0.138    0.363    0.565
    ## 
    ## Covariances:
    ##                    Estimate  Std.Err  z-value  P(>|z|)   Std.lv  Std.all
    ##   f_rd1 ~~                                                              
    ##     suma_eq_ec       -0.510    0.273   -1.869    0.062   -1.725   -0.302
    ##     suma_iri_ec      -0.993    0.394   -2.521    0.012   -3.357   -0.660
    ##     suma_aq_h         0.336    0.421    0.798    0.425    1.137    0.315
    ## 
    ## Variances:
    ##                    Estimate  Std.Err  z-value  P(>|z|)   Std.lv  Std.all
    ##    .q0007_0009        0.894    0.110    8.139    0.000    0.894    0.911
    ##    .q0007_0012        0.823    0.114    7.242    0.000    0.823    0.906
    ##    .q0007_0027        1.039    0.121    8.555    0.000    1.039    0.926
    ##    .q0007_0033        1.098    0.104   10.560    0.000    1.098    0.953
    ##    .q0007_0006r       0.454    0.072    6.302    0.000    0.454    0.937
    ##    .q0007_0014r       0.213    0.036    5.940    0.000    0.213    0.644
    ##    .q0007_0021r       0.368    0.093    3.952    0.000    0.368    0.590
    ##    .q0007_0031r       0.281    0.054    5.191    0.000    0.281    0.681
    ##     suma_eq_ec       32.558    2.919   11.154    0.000   32.558    1.000
    ##     suma_iri_ec      25.911    2.728    9.497    0.000   25.911    1.000
    ##     suma_aq_h        13.007    1.165   11.165    0.000   13.007    1.000
    ##     f_rd1             0.087    0.087    1.009    0.313    1.000    1.000

Conclusión: los ítems que se agregan no contribuyen a definir el factor.
La propuesta es dejar la hostilidad como criterio y mantener la escala
original.

### Reflexibility:

Modelo 1: escala original: 1,6,8,10,16,17,18,31,32

``` r
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

    ## lavaan 0.6-10 ended normally after 42 iterations
    ## 
    ##   Estimator                                         ML
    ##   Optimization method                           NLMINB
    ##   Number of model parameters                        24
    ##                                                       
    ##                                                   Used       Total
    ##   Number of observations                           297         422
    ##                                                                   
    ## Model Test User Model:
    ##                                                Standard      Robust
    ##   Test Statistic                                259.958     213.740
    ##   Degrees of freedom                                 54          54
    ##   P-value (Chi-square)                            0.000       0.000
    ##   Scaling correction factor                                   1.216
    ##        Yuan-Bentler correction (Mplus variant)                     
    ## 
    ## Model Test Baseline Model:
    ## 
    ##   Test statistic                               657.395     500.748
    ##   Degrees of freedom                                66          66
    ##   P-value                                        0.000       0.000
    ##   Scaling correction factor                                  1.313
    ## 
    ## User Model versus Baseline Model:
    ## 
    ##   Comparative Fit Index (CFI)                    0.652       0.633
    ##   Tucker-Lewis Index (TLI)                       0.574       0.551
    ##                                                                   
    ##   Robust Comparative Fit Index (CFI)                         0.660
    ##   Robust Tucker-Lewis Index (TLI)                            0.584
    ## 
    ## Loglikelihood and Information Criteria:
    ## 
    ##   Loglikelihood user model (H0)              -5435.763   -5435.763
    ##   Scaling correction factor                                  1.922
    ##       for the MLR correction                                      
    ##   Loglikelihood unrestricted model (H1)             NA          NA
    ##   Scaling correction factor                                  1.434
    ##       for the MLR correction                                      
    ##                                                                   
    ##   Akaike (AIC)                               10919.527   10919.527
    ##   Bayesian (BIC)                             11008.176   11008.176
    ##   Sample-size adjusted Bayesian (BIC)        10932.064   10932.064
    ## 
    ## Root Mean Square Error of Approximation:
    ## 
    ##   RMSEA                                          0.113       0.100
    ##   90 Percent confidence interval - lower         0.100       0.087
    ##   90 Percent confidence interval - upper         0.127       0.113
    ##   P-value RMSEA <= 0.05                          0.000       0.000
    ##                                                                   
    ##   Robust RMSEA                                               0.110
    ##   90 Percent confidence interval - lower                     0.095
    ##   90 Percent confidence interval - upper                     0.126
    ## 
    ## Standardized Root Mean Square Residual:
    ## 
    ##   SRMR                                           0.105       0.105
    ## 
    ## Parameter Estimates:
    ## 
    ##   Standard errors                             Sandwich
    ##   Information bread                           Observed
    ##   Observed information based on                Hessian
    ## 
    ## Latent Variables:
    ##                    Estimate  Std.Err  z-value  P(>|z|)   Std.lv  Std.all
    ##   f_rf1 =~                                                              
    ##     q0007_0001        1.000                               0.316    0.309
    ##     q0007_0006        0.562    0.270    2.077    0.038    0.178    0.256
    ##     q0007_0008        0.903    0.219    4.117    0.000    0.286    0.471
    ##     q0007_0010        1.007    0.296    3.403    0.001    0.319    0.546
    ##     q0007_0016        1.323    0.289    4.571    0.000    0.419    0.688
    ##     q0007_0017        1.250    0.303    4.123    0.000    0.396    0.637
    ##     q0007_0018        1.327    0.398    3.335    0.001    0.420    0.492
    ##     q0007_0031        0.757    0.249    3.036    0.002    0.240    0.363
    ##     q0007_0032        0.961    0.307    3.132    0.002    0.304    0.340
    ## 
    ## Covariances:
    ##                    Estimate  Std.Err  z-value  P(>|z|)   Std.lv  Std.all
    ##   f_rf1 ~~                                                              
    ##     suma_eq_ec        0.458    0.175    2.619    0.009    1.447    0.256
    ##     suma_iri_ec       0.312    0.152    2.048    0.041    0.986    0.193
    ##     suma_iri_pt       0.274    0.120    2.289    0.022    0.867    0.186
    ## 
    ## Variances:
    ##                    Estimate  Std.Err  z-value  P(>|z|)   Std.lv  Std.all
    ##    .q0007_0001        0.949    0.097    9.819    0.000    0.949    0.905
    ##    .q0007_0006        0.452    0.067    6.767    0.000    0.452    0.935
    ##    .q0007_0008        0.287    0.050    5.694    0.000    0.287    0.779
    ##    .q0007_0010        0.239    0.052    4.564    0.000    0.239    0.702
    ##    .q0007_0016        0.195    0.038    5.120    0.000    0.195    0.527
    ##    .q0007_0017        0.229    0.056    4.084    0.000    0.229    0.594
    ##    .q0007_0018        0.553    0.062    8.955    0.000    0.553    0.758
    ##    .q0007_0031        0.378    0.038   10.006    0.000    0.378    0.868
    ##    .q0007_0032        0.706    0.078    9.047    0.000    0.706    0.884
    ##     suma_eq_ec       32.000    2.863   11.178    0.000   32.000    1.000
    ##     suma_iri_ec      25.977    2.657    9.777    0.000   25.977    1.000
    ##     suma_iri_pt      21.812    2.149   10.151    0.000   21.812    1.000
    ##     f_rf1             0.100    0.048    2.080    0.038    1.000    1.000

Modelo 2: escala propuesta

``` r
cod_rf_cov2 = "f_rf1 =~ q0007_0001+q0007_0008+q0007_0010+q0007_0016+q0007_0017+q0007_0018+
q0007_0032
f_rf1~~suma_eq_ec
f_rf1~~suma_iri_ec
f_rf1~~suma_iri_pt"

# ordered = names(datos_fa[,17:77]) agregar en caso de:

mod_rf_cov2 = sem(model=cod_rf_cov2, data = datos_fa, estimator = "MLR")

summary(mod_rf_cov2, fit.measures = T, standardized = TRUE)
```

    ## lavaan 0.6-10 ended normally after 39 iterations
    ## 
    ##   Estimator                                         ML
    ##   Optimization method                           NLMINB
    ##   Number of model parameters                        20
    ##                                                       
    ##                                                   Used       Total
    ##   Number of observations                           297         422
    ##                                                                   
    ## Model Test User Model:
    ##                                                Standard      Robust
    ##   Test Statistic                                174.391     155.075
    ##   Degrees of freedom                                 35          35
    ##   P-value (Chi-square)                            0.000       0.000
    ##   Scaling correction factor                                   1.125
    ##        Yuan-Bentler correction (Mplus variant)                     
    ## 
    ## Model Test Baseline Model:
    ## 
    ##   Test statistic                               526.032     402.822
    ##   Degrees of freedom                                45          45
    ##   P-value                                        0.000       0.000
    ##   Scaling correction factor                                  1.306
    ## 
    ## User Model versus Baseline Model:
    ## 
    ##   Comparative Fit Index (CFI)                    0.710       0.664
    ##   Tucker-Lewis Index (TLI)                       0.627       0.569
    ##                                                                   
    ##   Robust Comparative Fit Index (CFI)                         0.711
    ##   Robust Tucker-Lewis Index (TLI)                            0.628
    ## 
    ## Loglikelihood and Information Criteria:
    ## 
    ##   Loglikelihood user model (H0)              -4845.146   -4845.146
    ##   Scaling correction factor                                  1.997
    ##       for the MLR correction                                      
    ##   Loglikelihood unrestricted model (H1)             NA          NA
    ##   Scaling correction factor                                  1.442
    ##       for the MLR correction                                      
    ##                                                                   
    ##   Akaike (AIC)                                9730.292    9730.292
    ##   Bayesian (BIC)                              9804.167    9804.167
    ##   Sample-size adjusted Bayesian (BIC)         9740.740    9740.740
    ## 
    ## Root Mean Square Error of Approximation:
    ## 
    ##   RMSEA                                          0.116       0.107
    ##   90 Percent confidence interval - lower         0.099       0.091
    ##   90 Percent confidence interval - upper         0.133       0.124
    ##   P-value RMSEA <= 0.05                          0.000       0.000
    ##                                                                   
    ##   Robust RMSEA                                               0.114
    ##   90 Percent confidence interval - lower                     0.096
    ##   90 Percent confidence interval - upper                     0.133
    ## 
    ## Standardized Root Mean Square Residual:
    ## 
    ##   SRMR                                           0.100       0.100
    ## 
    ## Parameter Estimates:
    ## 
    ##   Standard errors                             Sandwich
    ##   Information bread                           Observed
    ##   Observed information based on                Hessian
    ## 
    ## Latent Variables:
    ##                    Estimate  Std.Err  z-value  P(>|z|)   Std.lv  Std.all
    ##   f_rf1 =~                                                              
    ##     q0007_0001        1.000                               0.324    0.316
    ##     q0007_0008        0.948    0.237    4.007    0.000    0.307    0.505
    ##     q0007_0010        0.982    0.286    3.437    0.001    0.318    0.543
    ##     q0007_0016        1.395    0.300    4.646    0.000    0.451    0.739
    ##     q0007_0017        1.246    0.295    4.222    0.000    0.403    0.646
    ##     q0007_0018        1.264    0.372    3.397    0.001    0.409    0.478
    ##     q0007_0032        0.856    0.277    3.086    0.002    0.277    0.309
    ## 
    ## Covariances:
    ##                    Estimate  Std.Err  z-value  P(>|z|)   Std.lv  Std.all
    ##   f_rf1 ~~                                                              
    ##     suma_eq_ec        0.415    0.166    2.494    0.013    1.283    0.227
    ##     suma_iri_ec       0.211    0.143    1.478    0.139    0.653    0.128
    ##     suma_iri_pt       0.267    0.120    2.226    0.026    0.826    0.177
    ## 
    ## Variances:
    ##                    Estimate  Std.Err  z-value  P(>|z|)   Std.lv  Std.all
    ##    .q0007_0001        0.947    0.096    9.872    0.000    0.947    0.900
    ##    .q0007_0008        0.276    0.054    5.128    0.000    0.276    0.745
    ##    .q0007_0010        0.242    0.055    4.396    0.000    0.242    0.706
    ##    .q0007_0016        0.170    0.040    4.280    0.000    0.170    0.454
    ##    .q0007_0017        0.226    0.061    3.694    0.000    0.226    0.582
    ##    .q0007_0018        0.566    0.068    8.343    0.000    0.566    0.772
    ##    .q0007_0032        0.725    0.080    9.113    0.000    0.725    0.904
    ##     suma_eq_ec       32.000    2.863   11.178    0.000   32.000    1.000
    ##     suma_iri_ec      25.977    2.657    9.777    0.000   25.977    1.000
    ##     suma_iri_pt      21.812    2.149   10.151    0.000   21.812    1.000
    ##     f_rf1             0.105    0.047    2.214    0.027    1.000    1.000

### Propuesta del nuevo factor (Empatic concern):

Se propone un factor de empatic concern in close relationships con los
siguientes ítems.

6.- Para entender las acciones de los demás, es fundamental comprender
lo que sienten.

14.- Soy capaz de empatizar con otros cuando me cuentan algo.

21.- Soy sensible a lo que le pasa a los demás.

31.- Soy una persona que piensa en los demás.

``` r
cod_ec_cov1 = "f_ec1 =~ q0007_0006+q0007_0014+q0007_0021+q0007_0031
f_ec1~~suma_eq_ec
f_ec1~~suma_iri_ec"

mod_ec_cov1 = sem(model=cod_ec_cov1, data = datos_fa,
                  estimator = "MLR")

summary(mod_ec_cov1, fit.measures = T, standardized = TRUE)
```

    ## lavaan 0.6-10 ended normally after 36 iterations
    ## 
    ##   Estimator                                         ML
    ##   Optimization method                           NLMINB
    ##   Number of model parameters                        12
    ##                                                       
    ##                                                   Used       Total
    ##   Number of observations                           297         422
    ##                                                                   
    ## Model Test User Model:
    ##                                                Standard      Robust
    ##   Test Statistic                                 31.615      18.645
    ##   Degrees of freedom                                  9           9
    ##   P-value (Chi-square)                            0.000       0.028
    ##   Scaling correction factor                                   1.696
    ##        Yuan-Bentler correction (Mplus variant)                     
    ## 
    ## Model Test Baseline Model:
    ## 
    ##   Test statistic                               393.504     232.529
    ##   Degrees of freedom                                15          15
    ##   P-value                                        0.000       0.000
    ##   Scaling correction factor                                  1.692
    ## 
    ## User Model versus Baseline Model:
    ## 
    ##   Comparative Fit Index (CFI)                    0.940       0.956
    ##   Tucker-Lewis Index (TLI)                       0.900       0.926
    ##                                                                   
    ##   Robust Comparative Fit Index (CFI)                         0.956
    ##   Robust Tucker-Lewis Index (TLI)                            0.926
    ## 
    ## Loglikelihood and Information Criteria:
    ## 
    ##   Loglikelihood user model (H0)              -2901.995   -2901.995
    ##   Scaling correction factor                                  1.792
    ##       for the MLR correction                                      
    ##   Loglikelihood unrestricted model (H1)             NA          NA
    ##   Scaling correction factor                                  1.751
    ##       for the MLR correction                                      
    ##                                                                   
    ##   Akaike (AIC)                                5827.990    5827.990
    ##   Bayesian (BIC)                              5872.315    5872.315
    ##   Sample-size adjusted Bayesian (BIC)         5834.259    5834.259
    ## 
    ## Root Mean Square Error of Approximation:
    ## 
    ##   RMSEA                                          0.092       0.060
    ##   90 Percent confidence interval - lower         0.058       0.030
    ##   90 Percent confidence interval - upper         0.128       0.090
    ##   P-value RMSEA <= 0.05                          0.022       0.260
    ##                                                                   
    ##   Robust RMSEA                                               0.078
    ##   90 Percent confidence interval - lower                     0.024
    ##   90 Percent confidence interval - upper                     0.129
    ## 
    ## Standardized Root Mean Square Residual:
    ## 
    ##   SRMR                                           0.082       0.082
    ## 
    ## Parameter Estimates:
    ## 
    ##   Standard errors                             Sandwich
    ##   Information bread                           Observed
    ##   Observed information based on                Hessian
    ## 
    ## Latent Variables:
    ##                    Estimate  Std.Err  z-value  P(>|z|)   Std.lv  Std.all
    ##   f_ec1 =~                                                              
    ##     q0007_0006        1.000                               0.183    0.264
    ##     q0007_0014        1.962    0.608    3.229    0.001    0.360    0.620
    ##     q0007_0021        3.011    0.803    3.751    0.000    0.552    0.692
    ##     q0007_0031        2.203    0.582    3.786    0.000    0.404    0.622
    ## 
    ## Covariances:
    ##                    Estimate  Std.Err  z-value  P(>|z|)   Std.lv  Std.all
    ##   f_ec1 ~~                                                              
    ##     suma_eq_ec        0.332    0.113    2.926    0.003    1.809    0.320
    ##     suma_iri_ec       0.668    0.201    3.314    0.001    3.642    0.715
    ## 
    ## Variances:
    ##                    Estimate  Std.Err  z-value  P(>|z|)   Std.lv  Std.all
    ##    .q0007_0006        0.448    0.070    6.406    0.000    0.448    0.930
    ##    .q0007_0014        0.208    0.027    7.583    0.000    0.208    0.616
    ##    .q0007_0021        0.331    0.041    8.058    0.000    0.331    0.521
    ##    .q0007_0031        0.258    0.042    6.214    0.000    0.258    0.613
    ##     suma_eq_ec       32.000    2.863   11.178    0.000   32.000    1.000
    ##     suma_iri_ec      25.977    2.657    9.777    0.000   25.977    1.000
    ##     f_ec1             0.034    0.018    1.919    0.055    1.000    1.000

``` r
cod_ec_cov2 = "f_ec1 =~ q0007_0014+q0007_0021+q0007_0031
f_ec1~~suma_eq_ec
f_ec1~~suma_iri_ec"

mod_ec_cov2 = sem(model=cod_ec_cov2, data = datos_fa,
                  estimator = "MLR")

summary(mod_ec_cov2, fit.measures = T, standardized = TRUE)
```

    ## lavaan 0.6-10 ended normally after 27 iterations
    ## 
    ##   Estimator                                         ML
    ##   Optimization method                           NLMINB
    ##   Number of model parameters                        10
    ##                                                       
    ##                                                   Used       Total
    ##   Number of observations                           297         422
    ##                                                                   
    ## Model Test User Model:
    ##                                                Standard      Robust
    ##   Test Statistic                                 30.137      14.151
    ##   Degrees of freedom                                  5           5
    ##   P-value (Chi-square)                            0.000       0.015
    ##   Scaling correction factor                                   2.130
    ##        Yuan-Bentler correction (Mplus variant)                     
    ## 
    ## Model Test Baseline Model:
    ## 
    ##   Test statistic                               372.877     192.178
    ##   Degrees of freedom                                10          10
    ##   P-value                                        0.000       0.000
    ##   Scaling correction factor                                  1.940
    ## 
    ## User Model versus Baseline Model:
    ## 
    ##   Comparative Fit Index (CFI)                    0.931       0.950
    ##   Tucker-Lewis Index (TLI)                       0.861       0.900
    ##                                                                   
    ##   Robust Comparative Fit Index (CFI)                         0.945
    ##   Robust Tucker-Lewis Index (TLI)                            0.890
    ## 
    ## Loglikelihood and Information Criteria:
    ## 
    ##   Loglikelihood user model (H0)              -2597.411   -2597.411
    ##   Scaling correction factor                                  1.673
    ##       for the MLR correction                                      
    ##   Loglikelihood unrestricted model (H1)             NA          NA
    ##   Scaling correction factor                                  1.825
    ##       for the MLR correction                                      
    ##                                                                   
    ##   Akaike (AIC)                                5214.823    5214.823
    ##   Bayesian (BIC)                              5251.760    5251.760
    ##   Sample-size adjusted Bayesian (BIC)         5220.047    5220.047
    ## 
    ## Root Mean Square Error of Approximation:
    ## 
    ##   RMSEA                                          0.130       0.079
    ##   90 Percent confidence interval - lower         0.088       0.046
    ##   90 Percent confidence interval - upper         0.177       0.112
    ##   P-value RMSEA <= 0.05                          0.001       0.070
    ##                                                                   
    ##   Robust RMSEA                                               0.115
    ##   90 Percent confidence interval - lower                     0.046
    ##   90 Percent confidence interval - upper                     0.187
    ## 
    ## Standardized Root Mean Square Residual:
    ## 
    ##   SRMR                                           0.095       0.095
    ## 
    ## Parameter Estimates:
    ## 
    ##   Standard errors                             Sandwich
    ##   Information bread                           Observed
    ##   Observed information based on                Hessian
    ## 
    ## Latent Variables:
    ##                    Estimate  Std.Err  z-value  P(>|z|)   Std.lv  Std.all
    ##   f_ec1 =~                                                              
    ##     q0007_0014        1.000                               0.361    0.622
    ##     q0007_0021        1.533    0.228    6.719    0.000    0.554    0.694
    ##     q0007_0031        1.112    0.214    5.206    0.000    0.402    0.619
    ## 
    ## Covariances:
    ##                    Estimate  Std.Err  z-value  P(>|z|)   Std.lv  Std.all
    ##   f_ec1 ~~                                                              
    ##     suma_eq_ec        0.650    0.175    3.712    0.000    1.800    0.318
    ##     suma_iri_ec       1.315    0.265    4.971    0.000    3.643    0.715
    ## 
    ## Variances:
    ##                    Estimate  Std.Err  z-value  P(>|z|)   Std.lv  Std.all
    ##    .q0007_0014        0.206    0.027    7.647    0.000    0.206    0.613
    ##    .q0007_0021        0.330    0.042    7.921    0.000    0.330    0.518
    ##    .q0007_0031        0.260    0.042    6.188    0.000    0.260    0.617
    ##     suma_eq_ec       32.000    2.863   11.178    0.000   32.000    1.000
    ##     suma_iri_ec      25.977    2.657    9.777    0.000   25.977    1.000
    ##     f_ec1             0.130    0.029    4.550    0.000    1.000    1.000

## Reflexibility:

``` r
cod_ec_cov1 = "f_ec1 =~ q0007_0006+q0007_0014+q0007_0021+q0007_0031
f_ec1~~suma_eq_ec
f_ec1~~suma_iri_ec"

mod_ec_cov1 = sem(model=cod_ec_cov1, data = datos_fa,
                  estimator = "MLR")

summary(mod_ec_cov1, fit.measures = T, standardized = TRUE)
```

    ## lavaan 0.6-10 ended normally after 36 iterations
    ## 
    ##   Estimator                                         ML
    ##   Optimization method                           NLMINB
    ##   Number of model parameters                        12
    ##                                                       
    ##                                                   Used       Total
    ##   Number of observations                           297         422
    ##                                                                   
    ## Model Test User Model:
    ##                                                Standard      Robust
    ##   Test Statistic                                 31.615      18.645
    ##   Degrees of freedom                                  9           9
    ##   P-value (Chi-square)                            0.000       0.028
    ##   Scaling correction factor                                   1.696
    ##        Yuan-Bentler correction (Mplus variant)                     
    ## 
    ## Model Test Baseline Model:
    ## 
    ##   Test statistic                               393.504     232.529
    ##   Degrees of freedom                                15          15
    ##   P-value                                        0.000       0.000
    ##   Scaling correction factor                                  1.692
    ## 
    ## User Model versus Baseline Model:
    ## 
    ##   Comparative Fit Index (CFI)                    0.940       0.956
    ##   Tucker-Lewis Index (TLI)                       0.900       0.926
    ##                                                                   
    ##   Robust Comparative Fit Index (CFI)                         0.956
    ##   Robust Tucker-Lewis Index (TLI)                            0.926
    ## 
    ## Loglikelihood and Information Criteria:
    ## 
    ##   Loglikelihood user model (H0)              -2901.995   -2901.995
    ##   Scaling correction factor                                  1.792
    ##       for the MLR correction                                      
    ##   Loglikelihood unrestricted model (H1)             NA          NA
    ##   Scaling correction factor                                  1.751
    ##       for the MLR correction                                      
    ##                                                                   
    ##   Akaike (AIC)                                5827.990    5827.990
    ##   Bayesian (BIC)                              5872.315    5872.315
    ##   Sample-size adjusted Bayesian (BIC)         5834.259    5834.259
    ## 
    ## Root Mean Square Error of Approximation:
    ## 
    ##   RMSEA                                          0.092       0.060
    ##   90 Percent confidence interval - lower         0.058       0.030
    ##   90 Percent confidence interval - upper         0.128       0.090
    ##   P-value RMSEA <= 0.05                          0.022       0.260
    ##                                                                   
    ##   Robust RMSEA                                               0.078
    ##   90 Percent confidence interval - lower                     0.024
    ##   90 Percent confidence interval - upper                     0.129
    ## 
    ## Standardized Root Mean Square Residual:
    ## 
    ##   SRMR                                           0.082       0.082
    ## 
    ## Parameter Estimates:
    ## 
    ##   Standard errors                             Sandwich
    ##   Information bread                           Observed
    ##   Observed information based on                Hessian
    ## 
    ## Latent Variables:
    ##                    Estimate  Std.Err  z-value  P(>|z|)   Std.lv  Std.all
    ##   f_ec1 =~                                                              
    ##     q0007_0006        1.000                               0.183    0.264
    ##     q0007_0014        1.962    0.608    3.229    0.001    0.360    0.620
    ##     q0007_0021        3.011    0.803    3.751    0.000    0.552    0.692
    ##     q0007_0031        2.203    0.582    3.786    0.000    0.404    0.622
    ## 
    ## Covariances:
    ##                    Estimate  Std.Err  z-value  P(>|z|)   Std.lv  Std.all
    ##   f_ec1 ~~                                                              
    ##     suma_eq_ec        0.332    0.113    2.926    0.003    1.809    0.320
    ##     suma_iri_ec       0.668    0.201    3.314    0.001    3.642    0.715
    ## 
    ## Variances:
    ##                    Estimate  Std.Err  z-value  P(>|z|)   Std.lv  Std.all
    ##    .q0007_0006        0.448    0.070    6.406    0.000    0.448    0.930
    ##    .q0007_0014        0.208    0.027    7.583    0.000    0.208    0.616
    ##    .q0007_0021        0.331    0.041    8.058    0.000    0.331    0.521
    ##    .q0007_0031        0.258    0.042    6.214    0.000    0.258    0.613
    ##     suma_eq_ec       32.000    2.863   11.178    0.000   32.000    1.000
    ##     suma_iri_ec      25.977    2.657    9.777    0.000   25.977    1.000
    ##     f_ec1             0.034    0.018    1.919    0.055    1.000    1.000

``` r
cod_ec_cov1 = "f_ec1 =~ q0007_0006+q0007_0014+q0007_0021+q0007_0031
f_ec1~~suma_eq_ec
f_ec1~~suma_iri_ec"

mod_ec_cov1 = sem(model=cod_ec_cov1, data = datos_fa,
                  estimator = "MLR")

summary(mod_ec_cov1, fit.measures = T, standardized = TRUE)
```

    ## lavaan 0.6-10 ended normally after 36 iterations
    ## 
    ##   Estimator                                         ML
    ##   Optimization method                           NLMINB
    ##   Number of model parameters                        12
    ##                                                       
    ##                                                   Used       Total
    ##   Number of observations                           297         422
    ##                                                                   
    ## Model Test User Model:
    ##                                                Standard      Robust
    ##   Test Statistic                                 31.615      18.645
    ##   Degrees of freedom                                  9           9
    ##   P-value (Chi-square)                            0.000       0.028
    ##   Scaling correction factor                                   1.696
    ##        Yuan-Bentler correction (Mplus variant)                     
    ## 
    ## Model Test Baseline Model:
    ## 
    ##   Test statistic                               393.504     232.529
    ##   Degrees of freedom                                15          15
    ##   P-value                                        0.000       0.000
    ##   Scaling correction factor                                  1.692
    ## 
    ## User Model versus Baseline Model:
    ## 
    ##   Comparative Fit Index (CFI)                    0.940       0.956
    ##   Tucker-Lewis Index (TLI)                       0.900       0.926
    ##                                                                   
    ##   Robust Comparative Fit Index (CFI)                         0.956
    ##   Robust Tucker-Lewis Index (TLI)                            0.926
    ## 
    ## Loglikelihood and Information Criteria:
    ## 
    ##   Loglikelihood user model (H0)              -2901.995   -2901.995
    ##   Scaling correction factor                                  1.792
    ##       for the MLR correction                                      
    ##   Loglikelihood unrestricted model (H1)             NA          NA
    ##   Scaling correction factor                                  1.751
    ##       for the MLR correction                                      
    ##                                                                   
    ##   Akaike (AIC)                                5827.990    5827.990
    ##   Bayesian (BIC)                              5872.315    5872.315
    ##   Sample-size adjusted Bayesian (BIC)         5834.259    5834.259
    ## 
    ## Root Mean Square Error of Approximation:
    ## 
    ##   RMSEA                                          0.092       0.060
    ##   90 Percent confidence interval - lower         0.058       0.030
    ##   90 Percent confidence interval - upper         0.128       0.090
    ##   P-value RMSEA <= 0.05                          0.022       0.260
    ##                                                                   
    ##   Robust RMSEA                                               0.078
    ##   90 Percent confidence interval - lower                     0.024
    ##   90 Percent confidence interval - upper                     0.129
    ## 
    ## Standardized Root Mean Square Residual:
    ## 
    ##   SRMR                                           0.082       0.082
    ## 
    ## Parameter Estimates:
    ## 
    ##   Standard errors                             Sandwich
    ##   Information bread                           Observed
    ##   Observed information based on                Hessian
    ## 
    ## Latent Variables:
    ##                    Estimate  Std.Err  z-value  P(>|z|)   Std.lv  Std.all
    ##   f_ec1 =~                                                              
    ##     q0007_0006        1.000                               0.183    0.264
    ##     q0007_0014        1.962    0.608    3.229    0.001    0.360    0.620
    ##     q0007_0021        3.011    0.803    3.751    0.000    0.552    0.692
    ##     q0007_0031        2.203    0.582    3.786    0.000    0.404    0.622
    ## 
    ## Covariances:
    ##                    Estimate  Std.Err  z-value  P(>|z|)   Std.lv  Std.all
    ##   f_ec1 ~~                                                              
    ##     suma_eq_ec        0.332    0.113    2.926    0.003    1.809    0.320
    ##     suma_iri_ec       0.668    0.201    3.314    0.001    3.642    0.715
    ## 
    ## Variances:
    ##                    Estimate  Std.Err  z-value  P(>|z|)   Std.lv  Std.all
    ##    .q0007_0006        0.448    0.070    6.406    0.000    0.448    0.930
    ##    .q0007_0014        0.208    0.027    7.583    0.000    0.208    0.616
    ##    .q0007_0021        0.331    0.041    8.058    0.000    0.331    0.521
    ##    .q0007_0031        0.258    0.042    6.214    0.000    0.258    0.613
    ##     suma_eq_ec       32.000    2.863   11.178    0.000   32.000    1.000
    ##     suma_iri_ec      25.977    2.657    9.777    0.000   25.977    1.000
    ##     f_ec1             0.034    0.018    1.919    0.055    1.000    1.000

### F2 (ego-strength)

Escala original: 11,22,24,25,26,30

``` r
cod_es_cov1 = "f_es1 =~ q0007_0011+q0007_0022+q0007_0024+q0007_0025+q0007_0026+q0007_0030
f_es1~~suma_eq_ec
f_es1~~suma_iri_ec
f_es1~~suma_iri_pd"

mod_es_cov1 = sem(model=cod_es_cov1, data = datos_fa,
                  estimator = "MLR")

summary(mod_es_cov1, fit.measures = T, standardized = TRUE)
```

    ## lavaan 0.6-10 ended normally after 39 iterations
    ## 
    ##   Estimator                                         ML
    ##   Optimization method                           NLMINB
    ##   Number of model parameters                        18
    ##                                                       
    ##                                                   Used       Total
    ##   Number of observations                           297         422
    ##                                                                   
    ## Model Test User Model:
    ##                                                Standard      Robust
    ##   Test Statistic                                 94.922      81.007
    ##   Degrees of freedom                                 27          27
    ##   P-value (Chi-square)                            0.000       0.000
    ##   Scaling correction factor                                   1.172
    ##        Yuan-Bentler correction (Mplus variant)                     
    ## 
    ## Model Test Baseline Model:
    ## 
    ##   Test statistic                               922.546     745.742
    ##   Degrees of freedom                                36          36
    ##   P-value                                        0.000       0.000
    ##   Scaling correction factor                                  1.237
    ## 
    ## User Model versus Baseline Model:
    ## 
    ##   Comparative Fit Index (CFI)                    0.923       0.924
    ##   Tucker-Lewis Index (TLI)                       0.898       0.899
    ##                                                                   
    ##   Robust Comparative Fit Index (CFI)                         0.928
    ##   Robust Tucker-Lewis Index (TLI)                            0.904
    ## 
    ## Loglikelihood and Information Criteria:
    ## 
    ##   Loglikelihood user model (H0)              -4564.830   -4564.830
    ##   Scaling correction factor                                  1.367
    ##       for the MLR correction                                      
    ##   Loglikelihood unrestricted model (H1)             NA          NA
    ##   Scaling correction factor                                  1.250
    ##       for the MLR correction                                      
    ##                                                                   
    ##   Akaike (AIC)                                9165.660    9165.660
    ##   Bayesian (BIC)                              9232.147    9232.147
    ##   Sample-size adjusted Bayesian (BIC)         9175.063    9175.063
    ## 
    ## Root Mean Square Error of Approximation:
    ## 
    ##   RMSEA                                          0.092       0.082
    ##   90 Percent confidence interval - lower         0.072       0.063
    ##   90 Percent confidence interval - upper         0.112       0.101
    ##   P-value RMSEA <= 0.05                          0.000       0.003
    ##                                                                   
    ##   Robust RMSEA                                               0.089
    ##   90 Percent confidence interval - lower                     0.067
    ##   90 Percent confidence interval - upper                     0.111
    ## 
    ## Standardized Root Mean Square Residual:
    ## 
    ##   SRMR                                           0.072       0.072
    ## 
    ## Parameter Estimates:
    ## 
    ##   Standard errors                             Sandwich
    ##   Information bread                           Observed
    ##   Observed information based on                Hessian
    ## 
    ## Latent Variables:
    ##                    Estimate  Std.Err  z-value  P(>|z|)   Std.lv  Std.all
    ##   f_es1 =~                                                              
    ##     q0007_0011        1.000                               0.512    0.581
    ##     q0007_0022        0.943    0.108    8.748    0.000    0.483    0.547
    ##     q0007_0024        1.120    0.140    7.995    0.000    0.574    0.788
    ##     q0007_0025        1.467    0.161    9.135    0.000    0.752    0.813
    ##     q0007_0026        1.211    0.169    7.178    0.000    0.621    0.684
    ##     q0007_0030        1.021    0.121    8.461    0.000    0.523    0.786
    ## 
    ## Covariances:
    ##                    Estimate  Std.Err  z-value  P(>|z|)   Std.lv  Std.all
    ##   f_es1 ~~                                                              
    ##     suma_eq_ec        0.465    0.167    2.790    0.005    0.907    0.160
    ##     suma_iri_ec       0.079    0.192    0.414    0.679    0.155    0.030
    ##     suma_iri_pd      -1.628    0.286   -5.689    0.000   -3.176   -0.564
    ## 
    ## Variances:
    ##                    Estimate  Std.Err  z-value  P(>|z|)   Std.lv  Std.all
    ##    .q0007_0011        0.516    0.069    7.496    0.000    0.516    0.663
    ##    .q0007_0022        0.548    0.054   10.106    0.000    0.548    0.701
    ##    .q0007_0024        0.202    0.029    6.903    0.000    0.202    0.380
    ##    .q0007_0025        0.290    0.039    7.523    0.000    0.290    0.339
    ##    .q0007_0026        0.437    0.050    8.768    0.000    0.437    0.532
    ##    .q0007_0030        0.170    0.020    8.677    0.000    0.170    0.383
    ##     suma_eq_ec       32.000    2.863   11.178    0.000   32.000    1.000
    ##     suma_iri_ec      25.977    2.657    9.777    0.000   25.977    1.000
    ##     suma_iri_pd      31.716    2.692   11.783    0.000   31.716    1.000
    ##     f_es1             0.263    0.063    4.155    0.000    1.000    1.000

### F5 (distrust)

Escala original: 13,19,20,29

``` r
cod_dt_cov1 = "f_dt1 =~ q0007_0013+q0007_0019+q0007_0020+q0007_0029
f_dt1~~suma_iri_ec
f_dt1~~suma_aq_h"

# f_dt1~~suma_eq_ec

mod_dt_cov1 = sem(model=cod_dt_cov1, data = datos_fa,
                  estimator = "MLR")

summary(mod_dt_cov1, fit.measures = T, standardized = TRUE)
```

    ## lavaan 0.6-10 ended normally after 31 iterations
    ## 
    ##   Estimator                                         ML
    ##   Optimization method                           NLMINB
    ##   Number of model parameters                        12
    ##                                                       
    ##                                                   Used       Total
    ##   Number of observations                           290         422
    ##                                                                   
    ## Model Test User Model:
    ##                                                Standard      Robust
    ##   Test Statistic                                 36.554      33.382
    ##   Degrees of freedom                                  9           9
    ##   P-value (Chi-square)                            0.000       0.000
    ##   Scaling correction factor                                   1.095
    ##        Yuan-Bentler correction (Mplus variant)                     
    ## 
    ## Model Test Baseline Model:
    ## 
    ##   Test statistic                               356.383     320.082
    ##   Degrees of freedom                                15          15
    ##   P-value                                        0.000       0.000
    ##   Scaling correction factor                                  1.113
    ## 
    ## User Model versus Baseline Model:
    ## 
    ##   Comparative Fit Index (CFI)                    0.919       0.920
    ##   Tucker-Lewis Index (TLI)                       0.865       0.867
    ##                                                                   
    ##   Robust Comparative Fit Index (CFI)                         0.921
    ##   Robust Tucker-Lewis Index (TLI)                            0.869
    ## 
    ## Loglikelihood and Information Criteria:
    ## 
    ##   Loglikelihood user model (H0)              -3188.667   -3188.667
    ##   Scaling correction factor                                  1.111
    ##       for the MLR correction                                      
    ##   Loglikelihood unrestricted model (H1)             NA          NA
    ##   Scaling correction factor                                  1.104
    ##       for the MLR correction                                      
    ##                                                                   
    ##   Akaike (AIC)                                6401.333    6401.333
    ##   Bayesian (BIC)                              6445.372    6445.372
    ##   Sample-size adjusted Bayesian (BIC)         6407.318    6407.318
    ## 
    ## Root Mean Square Error of Approximation:
    ## 
    ##   RMSEA                                          0.103       0.097
    ##   90 Percent confidence interval - lower         0.069       0.064
    ##   90 Percent confidence interval - upper         0.139       0.131
    ##   P-value RMSEA <= 0.05                          0.006       0.011
    ##                                                                   
    ##   Robust RMSEA                                               0.101
    ##   90 Percent confidence interval - lower                     0.066
    ##   90 Percent confidence interval - upper                     0.139
    ## 
    ## Standardized Root Mean Square Residual:
    ## 
    ##   SRMR                                           0.065       0.065
    ## 
    ## Parameter Estimates:
    ## 
    ##   Standard errors                             Sandwich
    ##   Information bread                           Observed
    ##   Observed information based on                Hessian
    ## 
    ## Latent Variables:
    ##                    Estimate  Std.Err  z-value  P(>|z|)   Std.lv  Std.all
    ##   f_dt1 =~                                                              
    ##     q0007_0013        1.000                               0.816    0.796
    ##     q0007_0019        0.319    0.105    3.037    0.002    0.261    0.246
    ##     q0007_0020        0.814    0.092    8.847    0.000    0.664    0.648
    ##     q0007_0029        0.853    0.062   13.802    0.000    0.696    0.699
    ## 
    ## Covariances:
    ##                    Estimate  Std.Err  z-value  P(>|z|)   Std.lv  Std.all
    ##   f_dt1 ~~                                                              
    ##     suma_iri_ec      -0.623    0.283   -2.202    0.028   -0.764   -0.150
    ##     suma_aq_h         1.591    0.213    7.454    0.000    1.950    0.541
    ## 
    ## Variances:
    ##                    Estimate  Std.Err  z-value  P(>|z|)   Std.lv  Std.all
    ##    .q0007_0013        0.384    0.058    6.655    0.000    0.384    0.366
    ##    .q0007_0019        1.056    0.094   11.218    0.000    1.056    0.940
    ##    .q0007_0020        0.609    0.071    8.559    0.000    0.609    0.580
    ##    .q0007_0029        0.508    0.065    7.824    0.000    0.508    0.511
    ##     suma_iri_ec      25.911    2.728    9.497    0.000   25.911    1.000
    ##     suma_aq_h        13.007    1.165   11.165    0.000   13.007    1.000
    ##     f_dt1             0.666    0.083    7.990    0.000    1.000    1.000

Escala propuesta: 13,15,20,29

``` r
cod_dt_cov2 = "f_dt1 =~ q0007_0013+q0007_0015+q0007_0020+q0007_0029
f_dt1~~suma_iri_ec
f_dt1~~suma_aq_h"

mod_dt_cov2 = sem(model=cod_dt_cov2, data = datos_fa,
                  estimator = "MLR")

summary(mod_dt_cov2, fit.measures = T, standardized = TRUE)
```

    ## lavaan 0.6-10 ended normally after 31 iterations
    ## 
    ##   Estimator                                         ML
    ##   Optimization method                           NLMINB
    ##   Number of model parameters                        12
    ##                                                       
    ##                                                   Used       Total
    ##   Number of observations                           290         422
    ##                                                                   
    ## Model Test User Model:
    ##                                                Standard      Robust
    ##   Test Statistic                                 37.750      34.891
    ##   Degrees of freedom                                  9           9
    ##   P-value (Chi-square)                            0.000       0.000
    ##   Scaling correction factor                                   1.082
    ##        Yuan-Bentler correction (Mplus variant)                     
    ## 
    ## Model Test Baseline Model:
    ## 
    ##   Test statistic                               398.817     361.985
    ##   Degrees of freedom                                15          15
    ##   P-value                                        0.000       0.000
    ##   Scaling correction factor                                  1.102
    ## 
    ## User Model versus Baseline Model:
    ## 
    ##   Comparative Fit Index (CFI)                    0.925       0.925
    ##   Tucker-Lewis Index (TLI)                       0.875       0.876
    ##                                                                   
    ##   Robust Comparative Fit Index (CFI)                         0.927
    ##   Robust Tucker-Lewis Index (TLI)                            0.878
    ## 
    ## Loglikelihood and Information Criteria:
    ## 
    ##   Loglikelihood user model (H0)              -3199.018   -3199.018
    ##   Scaling correction factor                                  1.041
    ##       for the MLR correction                                      
    ##   Loglikelihood unrestricted model (H1)             NA          NA
    ##   Scaling correction factor                                  1.059
    ##       for the MLR correction                                      
    ##                                                                   
    ##   Akaike (AIC)                                6422.036    6422.036
    ##   Bayesian (BIC)                              6466.075    6466.075
    ##   Sample-size adjusted Bayesian (BIC)         6428.021    6428.021
    ## 
    ## Root Mean Square Error of Approximation:
    ## 
    ##   RMSEA                                          0.105       0.100
    ##   90 Percent confidence interval - lower         0.072       0.067
    ##   90 Percent confidence interval - upper         0.141       0.134
    ##   P-value RMSEA <= 0.05                          0.004       0.007
    ##                                                                   
    ##   Robust RMSEA                                               0.104
    ##   90 Percent confidence interval - lower                     0.069
    ##   90 Percent confidence interval - upper                     0.141
    ## 
    ## Standardized Root Mean Square Residual:
    ## 
    ##   SRMR                                           0.063       0.063
    ## 
    ## Parameter Estimates:
    ## 
    ##   Standard errors                             Sandwich
    ##   Information bread                           Observed
    ##   Observed information based on                Hessian
    ## 
    ## Latent Variables:
    ##                    Estimate  Std.Err  z-value  P(>|z|)   Std.lv  Std.all
    ##   f_dt1 =~                                                              
    ##     q0007_0013        1.000                               0.802    0.782
    ##     q0007_0015        0.689    0.097    7.128    0.000    0.552    0.469
    ##     q0007_0020        0.839    0.085    9.881    0.000    0.673    0.656
    ##     q0007_0029        0.873    0.061   14.299    0.000    0.700    0.702
    ## 
    ## Covariances:
    ##                    Estimate  Std.Err  z-value  P(>|z|)   Std.lv  Std.all
    ##   f_dt1 ~~                                                              
    ##     suma_iri_ec      -0.540    0.274   -1.967    0.049   -0.673   -0.132
    ##     suma_aq_h         1.605    0.214    7.498    0.000    2.001    0.555
    ## 
    ## Variances:
    ##                    Estimate  Std.Err  z-value  P(>|z|)   Std.lv  Std.all
    ##    .q0007_0013        0.410    0.056    7.291    0.000    0.410    0.389
    ##    .q0007_0015        1.082    0.094   11.462    0.000    1.082    0.780
    ##    .q0007_0020        0.598    0.068    8.797    0.000    0.598    0.569
    ##    .q0007_0029        0.504    0.062    8.111    0.000    0.504    0.507
    ##     suma_iri_ec      25.911    2.728    9.497    0.000   25.911    1.000
    ##     suma_aq_h        13.007    1.165   11.165    0.000   13.007    1.000
    ##     f_dt1             0.643    0.078    8.218    0.000    1.000    1.000

Conclusión: nos quedamos con el modelo alternativo.

### Emotional discontrol

Escala original: 2,3,7,23

``` r
cod_ed_cov1 = "f_ed1 =~ q0007_0002+q0007_0003+q0007_0007+q0007_0023
f_ed1~~suma_aq_r
f_ed1~~suma_iri_pd
"

# f_ed1~~suma_iri_pd; f_ed1~~suma_aq_h

mod_ed_COV1 = sem(model=cod_ed_cov1, data = datos_fa,
                  estimator = "MLR")

summary(mod_ed_COV1, fit.measures = T, standardized = TRUE)
```

    ## lavaan 0.6-10 ended normally after 35 iterations
    ## 
    ##   Estimator                                         ML
    ##   Optimization method                           NLMINB
    ##   Number of model parameters                        12
    ##                                                       
    ##                                                   Used       Total
    ##   Number of observations                           290         422
    ##                                                                   
    ## Model Test User Model:
    ##                                                Standard      Robust
    ##   Test Statistic                                 32.398      30.294
    ##   Degrees of freedom                                  9           9
    ##   P-value (Chi-square)                            0.000       0.000
    ##   Scaling correction factor                                   1.069
    ##        Yuan-Bentler correction (Mplus variant)                     
    ## 
    ## Model Test Baseline Model:
    ## 
    ##   Test statistic                               417.167     376.385
    ##   Degrees of freedom                                15          15
    ##   P-value                                        0.000       0.000
    ##   Scaling correction factor                                  1.108
    ## 
    ## User Model versus Baseline Model:
    ## 
    ##   Comparative Fit Index (CFI)                    0.942       0.941
    ##   Tucker-Lewis Index (TLI)                       0.903       0.902
    ##                                                                   
    ##   Robust Comparative Fit Index (CFI)                         0.943
    ##   Robust Tucker-Lewis Index (TLI)                            0.905
    ## 
    ## Loglikelihood and Information Criteria:
    ## 
    ##   Loglikelihood user model (H0)              -3186.074   -3186.074
    ##   Scaling correction factor                                  1.004
    ##       for the MLR correction                                      
    ##   Loglikelihood unrestricted model (H1)             NA          NA
    ##   Scaling correction factor                                  1.032
    ##       for the MLR correction                                      
    ##                                                                   
    ##   Akaike (AIC)                                6396.148    6396.148
    ##   Bayesian (BIC)                              6440.187    6440.187
    ##   Sample-size adjusted Bayesian (BIC)         6402.133    6402.133
    ## 
    ## Root Mean Square Error of Approximation:
    ## 
    ##   RMSEA                                          0.095       0.090
    ##   90 Percent confidence interval - lower         0.061       0.057
    ##   90 Percent confidence interval - upper         0.131       0.126
    ##   P-value RMSEA <= 0.05                          0.017       0.025
    ##                                                                   
    ##   Robust RMSEA                                               0.093
    ##   90 Percent confidence interval - lower                     0.058
    ##   90 Percent confidence interval - upper                     0.131
    ## 
    ## Standardized Root Mean Square Residual:
    ## 
    ##   SRMR                                           0.070       0.070
    ## 
    ## Parameter Estimates:
    ## 
    ##   Standard errors                             Sandwich
    ##   Information bread                           Observed
    ##   Observed information based on                Hessian
    ## 
    ## Latent Variables:
    ##                    Estimate  Std.Err  z-value  P(>|z|)   Std.lv  Std.all
    ##   f_ed1 =~                                                              
    ##     q0007_0002        1.000                               0.478    0.456
    ##     q0007_0003        1.978    0.297    6.661    0.000    0.945    0.824
    ##     q0007_0007        1.851    0.262    7.059    0.000    0.884    0.743
    ##     q0007_0023        1.025    0.198    5.179    0.000    0.489    0.499
    ## 
    ## Covariances:
    ##                    Estimate  Std.Err  z-value  P(>|z|)   Std.lv  Std.all
    ##   f_ed1 ~~                                                              
    ##     suma_aq_r         0.717    0.146    4.902    0.000    1.502    0.546
    ##     suma_iri_pd       0.776    0.210    3.695    0.000    1.624    0.287
    ## 
    ## Variances:
    ##                    Estimate  Std.Err  z-value  P(>|z|)   Std.lv  Std.all
    ##    .q0007_0002        0.867    0.074   11.779    0.000    0.867    0.792
    ##    .q0007_0003        0.422    0.072    5.843    0.000    0.422    0.321
    ##    .q0007_0007        0.636    0.078    8.186    0.000    0.636    0.449
    ##    .q0007_0023        0.721    0.071   10.210    0.000    0.721    0.751
    ##     suma_aq_r         7.575    0.669   11.315    0.000    7.575    1.000
    ##     suma_iri_pd      32.107    2.757   11.647    0.000   32.107    1.000
    ##     f_ed1             0.228    0.062    3.659    0.000    1.000    1.000

Escala propuesta: 2,3,7,19,23

``` r
cod_ed_cov2 = "f_ed1 =~ q0007_0002+q0007_0003+q0007_0007+q0007_0023+q0007_0019
f_ed1~~suma_aq_r
f_ed1~~suma_iri_pd
"

mod_ed_COV2 = sem(model=cod_ed_cov2, data = datos_fa,
                  estimator = "MLR")

summary(mod_ed_COV2, fit.measures = T, standardized = TRUE)
```

    ## lavaan 0.6-10 ended normally after 33 iterations
    ## 
    ##   Estimator                                         ML
    ##   Optimization method                           NLMINB
    ##   Number of model parameters                        14
    ##                                                       
    ##                                                   Used       Total
    ##   Number of observations                           290         422
    ##                                                                   
    ## Model Test User Model:
    ##                                                Standard      Robust
    ##   Test Statistic                                 49.098      45.516
    ##   Degrees of freedom                                 14          14
    ##   P-value (Chi-square)                            0.000       0.000
    ##   Scaling correction factor                                   1.079
    ##        Yuan-Bentler correction (Mplus variant)                     
    ## 
    ## Model Test Baseline Model:
    ## 
    ##   Test statistic                               457.966     413.951
    ##   Degrees of freedom                                21          21
    ##   P-value                                        0.000       0.000
    ##   Scaling correction factor                                  1.106
    ## 
    ## User Model versus Baseline Model:
    ## 
    ##   Comparative Fit Index (CFI)                    0.920       0.920
    ##   Tucker-Lewis Index (TLI)                       0.880       0.880
    ##                                                                   
    ##   Robust Comparative Fit Index (CFI)                         0.922
    ##   Robust Tucker-Lewis Index (TLI)                            0.883
    ## 
    ## Loglikelihood and Information Criteria:
    ## 
    ##   Loglikelihood user model (H0)              -3602.699   -3602.699
    ##   Scaling correction factor                                  1.033
    ##       for the MLR correction                                      
    ##   Loglikelihood unrestricted model (H1)             NA          NA
    ##   Scaling correction factor                                  1.056
    ##       for the MLR correction                                      
    ##                                                                   
    ##   Akaike (AIC)                                7233.398    7233.398
    ##   Bayesian (BIC)                              7284.776    7284.776
    ##   Sample-size adjusted Bayesian (BIC)         7240.380    7240.380
    ## 
    ## Root Mean Square Error of Approximation:
    ## 
    ##   RMSEA                                          0.093       0.088
    ##   90 Percent confidence interval - lower         0.066       0.061
    ##   90 Percent confidence interval - upper         0.122       0.116
    ##   P-value RMSEA <= 0.05                          0.006       0.011
    ##                                                                   
    ##   Robust RMSEA                                               0.092
    ##   90 Percent confidence interval - lower                     0.063
    ##   90 Percent confidence interval - upper                     0.122
    ## 
    ## Standardized Root Mean Square Residual:
    ## 
    ##   SRMR                                           0.070       0.070
    ## 
    ## Parameter Estimates:
    ## 
    ##   Standard errors                             Sandwich
    ##   Information bread                           Observed
    ##   Observed information based on                Hessian
    ## 
    ## Latent Variables:
    ##                    Estimate  Std.Err  z-value  P(>|z|)   Std.lv  Std.all
    ##   f_ed1 =~                                                              
    ##     q0007_0002        1.000                               0.491    0.469
    ##     q0007_0003        1.874    0.278    6.744    0.000    0.920    0.802
    ##     q0007_0007        1.810    0.257    7.034    0.000    0.888    0.746
    ##     q0007_0023        1.014    0.192    5.287    0.000    0.498    0.508
    ##     q0007_0019        0.660    0.159    4.150    0.000    0.324    0.306
    ## 
    ## Covariances:
    ##                    Estimate  Std.Err  z-value  P(>|z|)   Std.lv  Std.all
    ##   f_ed1 ~~                                                              
    ##     suma_aq_r         0.756    0.153    4.946    0.000    1.540    0.559
    ##     suma_iri_pd       0.796    0.210    3.792    0.000    1.623    0.286
    ## 
    ## Variances:
    ##                    Estimate  Std.Err  z-value  P(>|z|)   Std.lv  Std.all
    ##    .q0007_0002        0.854    0.074   11.545    0.000    0.854    0.780
    ##    .q0007_0003        0.470    0.071    6.607    0.000    0.470    0.357
    ##    .q0007_0007        0.627    0.079    7.920    0.000    0.627    0.443
    ##    .q0007_0023        0.712    0.069   10.257    0.000    0.712    0.742
    ##    .q0007_0019        1.014    0.089   11.340    0.000    1.014    0.906
    ##     suma_aq_r         7.575    0.669   11.315    0.000    7.575    1.000
    ##     suma_iri_pd      32.107    2.757   11.647    0.000   32.107    1.000
    ##     f_ed1             0.241    0.064    3.752    0.000    1.000    1.000

Conclusión: nos quedamos con el modelo original
