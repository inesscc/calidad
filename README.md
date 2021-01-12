Instrucciones de uso paquete para evaluar calidad de estimaciones
================

## Edición de los datos

Antes de realizar las estimaciones, se debe hacer un trabajo de edición.
En este tutorial se utilizan 2 conjuntos de datos:

  - Encuesta Nacional de Empleo (efm 2020)  
  - VIII Encuesta de Presupuestos Familiares

Ambos datasets están dentro del paquete y pueden ser utilizados cuando
calidad está cargado \[1\].

``` r
library(survey)
library(calidad)
library(tidyverse)

# Generar algunas variables para la ENE
ene <- ene %>% 
  mutate(fdt = if_else(cae_especifico >= 1 & cae_especifico <= 9, 1, 0),
         ocupado = if_else(cae_especifico >= 1 & cae_especifico <= 7, 1, 0),
         desocupado = if_else(cae_especifico >= 8 & cae_especifico <= 9, 1, 0))

# Llevar epf a nivel de hogar
epf <- epf_personas %>% 
  group_by(folio) %>% 
  slice(1)
```

## Declarar el diseño muestral

Antes de comenzar a usar el paquete `calidad` es necesario declarar el
diseño muestral de la encuesta que se está evaluando, para lo cual
utilizamos el paquete `survey`. Se debe declarar cuál es la UPM, el
estrato y el factor de expansión. Declaramos el diseño para las dos
encuestas (EPF y ENE)

``` r
options(survey.lonely.psu = "certainty")

# Declarar el diseño muestral para la ENE
dc_ene <- svydesign(ids = ~conglomerado , strata = ~estrato_unico, data = ene, weights = ~fact_cal)

# Declarar el diseño muestral para la EPF
dc_epf <- svydesign(ids = ~varunit, strata = ~varstrat, data = epf, weights = ~fe)
```

## Generar insumos para la evaluación

### Encuesta de Empleo (parte 1)

Para evaluar la calidad de una estimación, la [metodología del
INE](https://www.ine.cl/docs/default-source/documentos-de-trabajo/20200318-lineamientos-medidas-de-precisi%C3%B3n.pdf?sfvrsn=f1ab2dbe_4)
establece criterios diferenciados para estimaciones de nivel y
estimaciones de proporción. En el caso estimaciones de nivel se requiere
contar con el tamaño muestral, los grados de libertad y el coeficiente
de variación. Por su parte, la estimaciones de proporción requieren el
tamaño muestral, los grados de libertad y el error estándar.

El paquete incluye funciones diferenciadas para crear los insumos de
distintos tipos de estimaciones. El uso de cada uno de ellos es el
siguiente:

``` r
insumos_prop <- crear_insumos_prop(var = desocupado, dominios = sexo, subpop = fdt, disenio =  dc_ene)
insumos_total <-  crear_insumos_tot(var = desocupado, dominios = sexo, subpop = fdt, disenio =  dc_ene)
```

  - `var`: variable que se quiere estimar. Debe ser una variable dummy
  - `dominios`: desagregación que se requiere.
  - `subpop`: selección de una subpoblación de referencia. Es opcional y
    funciona a modo de filtro (debe ser una variable dummy)
  - `disenio`: diseño muestral

Para obtener más desagregaciones, podemos usar el símbolo “+” de la
siguiente manera:

``` r
desagregar <- crear_insumos_prop(var = desocupado, dominios = sexo+region, subpop = fdt, disenio =  dc_ene)
```

### Encuesta de Presupuestos Familiares (parte 2)

En ciertas ocasiones puede ser de interés evaluar la calidad de una
suma. Por ejemplo, la suma de todos los ingresos de la EPF a nivel de
zona geográfica (Gran Santiago y resto de capitales regionales). Para
ello, existe la función `crear_insumos_suma`

``` r
insumos_suma <-  crear_insumos_suma(var = gastot_hd, dominios = zona, disenio =  dc_epf)
```

Si queremos evaluar la estimación de una media, contamos con la función
`crear_insumos`. En este caso, calcularemos la media de gasto de los
hogares, según área geográfica.

``` r
insumos_media <-  crear_insumos(var = gastot_hd, dominios = zona, disenio =  dc_epf)
```

Cabe mencionar que el uso por defecto es no desagregar, en cuyo caso las
funciones deben ser utilizadas del siguiente modo:

``` r
insumos_prop_nacional <- crear_insumos_prop(desocupado, subpop = fdt, disenio = dc_ene)
insumos_total_nacional <-  crear_insumos_tot(desocupado, subpop = fdt, disenio = dc_ene)

insumos_suma_nacional <- crear_insumos_suma(gastot_hd, disenio = dc_epf)
insumos_media_nacional <-  crear_insumos(gastot_hd, disenio = dc_epf)
```

Nótese que en el caso de las funciones `crear_insumos_prop` y
`crear_insumos_tot`, seguimos usando el argumento subpop. Esto no es
necesario para que la función corra, pero dado que se quiere estimar el
desempleo, se debe considerar la subpoblación de referencia.

## Generar evaluación

Una vez que se han generado los insumos, podemos hacer la evaluación.
Nuevamente, usamos funciones diferentes para cada uno de los tipos de
estimación.

``` r
evaluacion_prop <- evaluacion_calidad_prop(insumos_prop)
evaluacion_tot <- evaluacion_calidad_tot(insumos_total)

evaluacion_suma <- evaluacion_calidad_suma(insumos_suma)
evaluacion_media <- evaluacion_calidad(insumos_media)
```

La salida de estas últimas funciones es un `dataframe` que, además de
contener la información ya generada, incluye una columna que indica si
la estimación es no fiable, poco fiable o fiable.

Las mismas funciones para evaluar tienen un parámetro que nos permite
saber si el tabulado debe o no ser publicado. Siguiendo el criterio del
estándar, si menos del 50% de las estimaciones de un tabulado no son
fiables, este no debería ser publicado.

``` r
# Desempleo desagregado por region
desagregar <- crear_insumos_tot(var = desocupado, dominios = region, subpop = fdt, disenio =  dc_ene)

# Evaluar tabulado
evaluacion_tot_desagreg <- evaluacion_calidad_tot(desagregar, publicar = T)
```

En el caso de un tabulado de total de desempleados por región, el
resultado de la función nos muestra que el 81,25 de las celdas cumple
con el criterio de fiabilidad, por ende, puede ser publicado.

## Visualización en html

Actualmente, están en desarrollo algunas funcionalidades para visualizar
los resultados. La función `tabla_html` genera una tabla en html que
señaliza con los colores amarillo, verde y rojo la calidad de las
estimaciones. Además, indica cuál de los insumos es el que está
afectando a la clasificación de calidad.

``` r
tabla_html(evaluacion_tot_desagreg)
```

1.  Los datos contenidos dentro del paquete tienen algunas ediciones. Es
    importante tener en consideración que los objetos haven::labelled
    pueden tener alguna colisión con las funciones de calidad. Si se
    quiere importar un archivo dta, se deben convertir a numeric o
    character todas las variables que sean de tipo haven::labelled.
