Los archivos de este repositorio realizan la validación de modelos predictivos para seis variables dominicanas: Cotizantes AFP, Crédito en Moneda Nacional, IPC, Remesas, Llegada de Turistas e IMAE. Los mismos corresponden al paper "Prediciendo series de tiempo de variables económicas dominicanas" (2023). 
Los archivos que se deben correr son los .R que tienen la palabra "depurado" que se encuentran en la carpeta "codigos", por ejemplo, "Cotizantes AFP depurado.R". La data a utilizar está en formato csv o xlsx en la carpeta "series". En la carpeta "auxiliares" se encuentran códigos auxiliares y el archivo paquetes.R que se debe de correr para instalar la paquetería necesaria. El archivo en .pdf es el paper.

Las funciones principales que se utilizan para el código correspondiente a cada variable se encuentran en el archivo "funciones.R" y en la paquetería, ambas cargadas al inicio del código correspondiente a cada variable. Las funciones auxiliares que se utilizan en los códigos son "regression_forecast_combination_function.R", "sd_hit_single.R" y "regression_forecast_combination_synthetic".

Para realizar el proceso para una de las variables se debe hacer los siguiente:

1) Descargar las carpetas "codigos", "series" y "auxiliares"
2) Correr archivo paquetes.R para instalar la paquetería necesaria
3) Elegir el código de la variable de su elección que tiene la palabra "depurado".
4) Correr dicho archivo

Para ver un ejemplo véase el blog post en [Medium]([URL](https://medium.com/@SB-ESTUDIOS/3aaa3a90437e)https://medium.com/@SB-ESTUDIOS/3aaa3a90437e)
