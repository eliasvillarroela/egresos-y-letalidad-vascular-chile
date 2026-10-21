# Egresos hospitalarios por patología vascular cerebral neuroquirúrgica/endovascular, Chile 2015-2024

Código en R para calcular tasas de egreso (cruda y estandarizada), letalidad
intrahospitalaria, tendencias (APC) y un modelo logístico de mortalidad hospitalaria.

## Datos (no incluidos en este repositorio)

1. **Egresos hospitalarios (DEIS-MINSAL):** descargar los CSV anuales desde https://deis.minsal.cl
   y guardarlos en la carpeta `datos_deis/` (un archivo por año, p. ej. `EGR_DATOS_ABIERTO_2015.csv`).
2. **Proyecciones de población (INE):** descargar desde https://www.ine.gob.cl y crear
   `poblacion_ine.csv` con las columnas `ano, sexo, grupo, poblacion`
   (grupos decenales 0-9, ..., 80+). Si el archivo no existe, el script genera
   `poblacion_ine_PLANTILLA.csv` para completarlo.

## Cómo ejecutar

```r
source("codigo_vascular_corregido.r")
```

Ejecutar desde la carpeta del proyecto. Los resultados (tablas, figuras y archivos de
versiones) se guardan en `resultados_vascular/`.

## Versiones de software

El análisis se ejecutó con las siguientes versiones. Al terminar, el script genera
`resultados_vascular/versiones_para_README.md` y `resultados_vascular/session_info.txt`;
pegue aquí el contenido de la tabla:

| Componente | Version |
|---|---|
| R | [COMPLETAR] |
| tidyverse | [COMPLETAR] |
| data.table | [COMPLETAR] |
| gtsummary | [COMPLETAR] |
| flextable | [COMPLETAR] |
| broom | [COMPLETAR] |

Sistema operativo: [COMPLETAR, ver `session_info.txt`]

## Licencia

MIT (ver `LICENSE`).
