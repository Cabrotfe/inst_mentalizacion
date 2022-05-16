Análisis instrumentos
================

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
    ## 1 estudio_actual     70    25.1 
    ## 2 genero              9     3.23
    ## 3 educacion           9     3.23
    ## 4 estudias            9     3.23
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

|                                                               | Overall (N=279) |
|:--------------------------------------------------------------|:---------------:|
| **edad**                                                      |                 |
|    N-Miss                                                     |        9        |
|    Mean (SD)                                                  | 27.419 (12.487) |
|    Range                                                      | 17.000 - 85.000 |
| **¿Con qué género te identificas?**                           |                 |
|    N-Miss                                                     |        9        |
|    Masculino                                                  |   81 (30.0%)    |
|    Femenino                                                   |   180 (66.7%)   |
|    Otro                                                       |    6 (2.2%)     |
|    Prefiero no decirlo                                        |    3 (1.1%)     |
| **¿Cuál es el nivel educacional más alto que has alcanzado?** |                 |
|    N-Miss                                                     |        9        |
|    Educación básica completa                                  |    0 (0.0%)     |
|    Educación media completa                                   |   179 (66.3%)   |
|    Educación técnico profesional completa                     |    11 (4.1%)    |
|    Educación universitaria completa                           |   36 (13.3%)    |
|    Educación de postgrado completa                            |   44 (16.3%)    |
| **¿Actualmente te encuentras estudiando?**                    |                 |
|    N-Miss                                                     |        9        |
|    Sí                                                         |   209 (77.4%)   |
|    No                                                         |   61 (22.6%)    |
| **estudio_actual**                                            |                 |
|    N-Miss                                                     |        9        |
|    No                                                         |   61 (22.6%)    |
|    Universitaria                                              |   190 (70.4%)   |
|    Postgrado                                                  |    19 (7.0%)    |

Diferencias entre personas según están estudiando actualmente:

``` r
summary(tableby(estudio_actual~edad+genero+educacion+estudias, data = datos))
```

|                                                               |    No (N=61)    | Universitaria (N=190) | Postgrado (N=19) |  Total (N=270)  |  p value |
|:--------------------------------------------------------------|:---------------:|:---------------------:|:----------------:|:---------------:|---------:|
| **edad**                                                      |                 |                       |                  |                 | \< 0.001 |
|    Mean (SD)                                                  | 43.820 (14.245) |    21.484 (3.871)     | 34.105 (12.884)  | 27.419 (12.487) |          |
|    Range                                                      | 23.000 - 85.000 |    17.000 - 49.000    | 23.000 - 70.000  | 17.000 - 85.000 |          |
| **¿Con qué género te identificas?**                           |                 |                       |                  |                 |    0.006 |
|    Masculino                                                  |   26 (42.6%)    |      44 (23.2%)       |    11 (57.9%)    |   81 (30.0%)    |          |
|    Femenino                                                   |   35 (57.4%)    |      137 (72.1%)      |    8 (42.1%)     |   180 (66.7%)   |          |
|    Otro                                                       |    0 (0.0%)     |       6 (3.2%)        |     0 (0.0%)     |    6 (2.2%)     |          |
|    Prefiero no decirlo                                        |    0 (0.0%)     |       3 (1.6%)        |     0 (0.0%)     |    3 (1.1%)     |          |
| **¿Cuál es el nivel educacional más alto que has alcanzado?** |                 |                       |                  |                 |          |
|    Educación básica completa                                  |    0 (0.0%)     |       0 (0.0%)        |     0 (0.0%)     |    0 (0.0%)     |          |
|    Educación media completa                                   |    1 (1.6%)     |      177 (93.2%)      |     1 (5.3%)     |   179 (66.3%)   |          |
|    Educación técnico profesional completa                     |    6 (9.8%)     |       5 (2.6%)        |     0 (0.0%)     |    11 (4.1%)    |          |
|    Educación universitaria completa                           |   18 (29.5%)    |       7 (3.7%)        |    11 (57.9%)    |   36 (13.3%)    |          |
|    Educación de postgrado completa                            |   36 (59.0%)    |       1 (0.5%)        |    7 (36.8%)     |   44 (16.3%)    |          |
| **¿Actualmente te encuentras estudiando?**                    |                 |                       |                  |                 | \< 0.001 |
|    Sí                                                         |    0 (0.0%)     |     190 (100.0%)      |   19 (100.0%)    |   209 (77.4%)   |          |
|    No                                                         |   61 (100.0%)   |       0 (0.0%)        |     0 (0.0%)     |   61 (22.6%)    |          |

# Instrumentos de mentalización:

## Datos perdidos:

### Omisiones en MMQ

``` r
naniar::miss_var_summary(datos[,17:49])
```

    ## # A tibble: 33 x 3
    ##    variable   n_miss pct_miss
    ##    <chr>       <int>    <dbl>
    ##  1 q0007_0001     35     12.5
    ##  2 q0007_0002     35     12.5
    ##  3 q0007_0003     35     12.5
    ##  4 q0007_0004     35     12.5
    ##  5 q0007_0005     35     12.5
    ##  6 q0007_0006     35     12.5
    ##  7 q0007_0007     35     12.5
    ##  8 q0007_0008     35     12.5
    ##  9 q0007_0009     35     12.5
    ## 10 q0007_0010     35     12.5
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
    ##  1 q0008_0001     44     15.8
    ##  2 q0008_0002     44     15.8
    ##  3 q0008_0003     44     15.8
    ##  4 q0008_0004     44     15.8
    ##  5 q0008_0005     44     15.8
    ##  6 q0008_0006     44     15.8
    ##  7 q0008_0007     44     15.8
    ##  8 q0008_0008     44     15.8
    ##  9 q0008_0009     44     15.8
    ## 10 q0008_0010     44     15.8
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

|                                                               |   no (N=235)    |    sí (N=44)    |  Total (N=279)  | p value |
|:--------------------------------------------------------------|:---------------:|:---------------:|:---------------:|--------:|
| **edad**                                                      |                 |                 |                 |   0.243 |
|    N-Miss                                                     |        0        |        9        |        9        |         |
|    Mean (SD)                                                  | 27.762 (12.815) | 25.114 (9.851)  | 27.419 (12.487) |         |
|    Range                                                      | 17.000 - 85.000 | 18.000 - 67.000 | 17.000 - 85.000 |         |
| **¿Con qué género te identificas?**                           |                 |                 |                 |   0.689 |
|    N-Miss                                                     |        0        |        9        |        9        |         |
|    Masculino                                                  |   68 (28.9%)    |   13 (37.1%)    |   81 (30.0%)    |         |
|    Femenino                                                   |   159 (67.7%)   |   21 (60.0%)    |   180 (66.7%)   |         |
|    Otro                                                       |    5 (2.1%)     |    1 (2.9%)     |    6 (2.2%)     |         |
|    Prefiero no decirlo                                        |    3 (1.3%)     |    0 (0.0%)     |    3 (1.1%)     |         |
| **¿Cuál es el nivel educacional más alto que has alcanzado?** |                 |                 |                 |         |
|    N-Miss                                                     |        0        |        9        |        9        |         |
|    Educación básica completa                                  |    0 (0.0%)     |    0 (0.0%)     |    0 (0.0%)     |         |
|    Educación media completa                                   |   154 (65.5%)   |   25 (71.4%)    |   179 (66.3%)   |         |
|    Educación técnico profesional completa                     |    9 (3.8%)     |    2 (5.7%)     |    11 (4.1%)    |         |
|    Educación universitaria completa                           |   30 (12.8%)    |    6 (17.1%)    |   36 (13.3%)    |         |
|    Educación de postgrado completa                            |   42 (17.9%)    |    2 (5.7%)     |   44 (16.3%)    |         |
| **¿Actualmente te encuentras estudiando?**                    |                 |                 |                 |   0.208 |
|    N-Miss                                                     |        0        |        9        |        9        |         |
|    Sí                                                         |   179 (76.2%)   |   30 (85.7%)    |   209 (77.4%)   |         |
|    No                                                         |   56 (23.8%)    |    5 (14.3%)    |   61 (22.6%)    |         |
| **estudio_actual**                                            |                 |                 |                 |   0.444 |
|    N-Miss                                                     |        0        |        9        |        9        |         |
|    No                                                         |   56 (23.8%)    |    5 (14.3%)    |   61 (22.6%)    |         |
|    Universitaria                                              |   163 (69.4%)   |   27 (77.1%)    |   190 (70.4%)   |         |
|    Postgrado                                                  |    16 (6.8%)    |    3 (8.6%)     |    19 (7.0%)    |         |

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
print(fa(datos[,17:49], nfactors = 6,rotate = "oblimin"),cut = .2)
```

    ## Loading required namespace: GPArotation

    ## Factor Analysis using method =  minres
    ## Call: fa(r = datos[, 17:49], nfactors = 6, rotate = "oblimin")
    ## Standardized loadings (pattern matrix) based upon correlation matrix
    ##              MR1   MR6   MR2   MR4   MR3   MR5   h2   u2 com
    ## q0007_0001              0.28       -0.33       0.17 0.83 2.8
    ## q0007_0002        0.54                         0.27 0.73 1.3
    ## q0007_0003        0.71                         0.61 0.39 1.1
    ## q0007_0004                    0.72             0.54 0.46 1.0
    ## q0007_0005                    0.68             0.48 0.52 1.0
    ## q0007_0006                    0.24       -0.39 0.26 0.74 2.6
    ## q0007_0007        0.68                         0.65 0.35 1.2
    ## q0007_0008  0.32  0.20  0.40 -0.21             0.33 0.67 3.4
    ## q0007_0009 -0.24                          0.33 0.31 0.69 2.6
    ## q0007_0010              0.64                   0.44 0.56 1.1
    ## q0007_0011  0.52 -0.23                         0.49 0.51 1.5
    ## q0007_0012        0.22                    0.44 0.45 0.55 2.3
    ## q0007_0013                          0.68       0.53 0.47 1.1
    ## q0007_0014                    0.29       -0.38 0.39 0.61 3.0
    ## q0007_0015        0.25              0.43       0.32 0.68 1.8
    ## q0007_0016              0.70                   0.54 0.46 1.2
    ## q0007_0017              0.53                   0.37 0.63 1.3
    ## q0007_0018              0.59                   0.35 0.65 1.3
    ## q0007_0019        0.35                    0.24 0.23 0.77 1.9
    ## q0007_0020                          0.57       0.47 0.53 1.3
    ## q0007_0021        0.28        0.23       -0.39 0.39 0.61 3.2
    ## q0007_0022  0.54                               0.36 0.64 1.2
    ## q0007_0023        0.53                         0.35 0.65 1.3
    ## q0007_0024  0.76                               0.67 0.33 1.1
    ## q0007_0025  0.73 -0.20                         0.69 0.31 1.2
    ## q0007_0026  0.50 -0.31                         0.49 0.51 1.7
    ## q0007_0027        0.23                    0.53 0.56 0.44 2.0
    ## q0007_0028                    0.65             0.49 0.51 1.2
    ## q0007_0029                          0.77       0.56 0.44 1.1
    ## q0007_0030  0.78                               0.61 0.39 1.1
    ## q0007_0031              0.22  0.22       -0.34 0.33 0.67 3.2
    ## q0007_0032 -0.21        0.40  0.22             0.24 0.76 2.3
    ## q0007_0033        0.24                    0.39 0.38 0.62 2.5
    ## 
    ##                        MR1  MR6  MR2  MR4  MR3  MR5
    ## SS loadings           3.40 2.69 2.24 2.05 2.11 1.81
    ## Proportion Var        0.10 0.08 0.07 0.06 0.06 0.05
    ## Cumulative Var        0.10 0.18 0.25 0.31 0.38 0.43
    ## Proportion Explained  0.24 0.19 0.16 0.14 0.15 0.13
    ## Cumulative Proportion 0.24 0.43 0.58 0.73 0.87 1.00
    ## 
    ##  With factor correlations of 
    ##       MR1   MR6   MR2   MR4   MR3   MR5
    ## MR1  1.00 -0.43  0.16  0.10 -0.30 -0.21
    ## MR6 -0.43  1.00  0.21  0.23  0.28  0.12
    ## MR2  0.16  0.21  1.00  0.25  0.16 -0.10
    ## MR4  0.10  0.23  0.25  1.00  0.01 -0.14
    ## MR3 -0.30  0.28  0.16  0.01  1.00  0.29
    ## MR5 -0.21  0.12 -0.10 -0.14  0.29  1.00
    ## 
    ## Mean item complexity =  1.8
    ## Test of the hypothesis that 6 factors are sufficient.
    ## 
    ## The degrees of freedom for the null model are  528  and the objective function was  12.57 with Chi Square of  3346.64
    ## The degrees of freedom for the model are 345  and the objective function was  2.15 
    ## 
    ## The root mean square of the residuals (RMSR) is  0.04 
    ## The df corrected root mean square of the residuals is  0.04 
    ## 
    ## The harmonic number of observations is  244 with the empirical chi square  324.92  with prob <  0.77 
    ## The total number of observations was  279  with Likelihood Chi Square =  563.6  with prob <  9.4e-13 
    ## 
    ## Tucker Lewis Index of factoring reliability =  0.879
    ## RMSEA index =  0.048  and the 90 % confidence intervals are  0.041 0.055
    ## BIC =  -1379.17
    ## Fit based upon off diagonal values = 0.97
    ## Measures of factor score adequacy             
    ##                                                    MR1  MR6  MR2  MR4  MR3  MR5
    ## Correlation of (regression) scores with factors   0.94 0.91 0.89 0.89 0.89 0.86
    ## Multiple R square of scores with factors          0.89 0.84 0.80 0.79 0.80 0.74
    ## Minimum correlation of possible factor scores     0.77 0.67 0.59 0.58 0.59 0.48

### Estructura interna:

**Respecto a reflexibility:** la estructura general se mantiene de forma
parcial, el ítem 6 no está asociado al factor.

**Respecto a ego-strength:** la estructura general se mantiene.

**Respecto a relational attunement:** la estructura general se mantiene.

**Respecto a relational discomfort:** la estructura se mantiene de forma
parcial, el ítem 15 no se asocia al factor.

**Respecto a distrust:** la estructura se mantiene de forma parcial, el
ítem 19 no se asocia al mismo factor, y el ítem 15 (relational
discomfort) sí se asocia.

**Respecto a emotional discontrol:** la estructura se mantiene en su
totalidad.

**Notas generales:** El ítem 6 (Para entender las acciones de los demás,
es fundamental comprender lo que sienten) tiene baja carga con todos los
factores, y su asociación más alta es con el factor “relational
attunement”, no con reflexión. Esto, en base a su contenido, tiene
sentido.

El ítem 19 (Para mí las cosas son blancas o son negras) se asocia a
emotional discontrol (aunque poco) y un poco a relational discomfort y
no a distrust.

El ítem 15 (Me asusta abrirme con los demás) se asocia a distrust y no a
relational discomfort.
