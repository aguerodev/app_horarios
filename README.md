# Generador de Horarios
Este proyecto es un experimento que transforma los PDFs de cursos y horarios en una herramienta para generar horarios optimizados. La aplicación permite seleccionar cursos, indicar si tiene cupo y utiliza algoritmos genéticos para encontrar la combinación óptima que minimice los días de asistencia a la universidad.

Desarrollé esto principalmente para facilitarme el proceso de matrícula ya que cuando llega el momento siempre me pongo nervioso y me aterra equivocarme, pero también fue una excelente excusa para experimentar con paquetes como [box](https://klmr.me/box/) (que uso para encapsular las dependencias de funciones) y [mirai](https://shikokuchuo.net/mirai/), una alternativa más ligera y moderna a future para evaluación asíncrona.

## Características
- Extracción de datos desde el PDF de horarios con [pdftools](https://github.com/ropensci/pdftools) + [dplyr](https://dplyr.tidyverse.org/) + [stringr (regex)](https://stringr.tidyverse.org/)
- Conversión a base SQL de datos en este caso [DuckDB](https://duckdb.org/) ❤️
- Interfaz desarrollada con [R/Shiny](https://shiny.posit.co/)
- Optimización mediante algoritmos genéticos con el paquete [GA](https://luca-scr.github.io/GA/)
- Descarga 'el mejor horario' en formato de png con [shinyscreenshot](https://github.com/daattali/shinyscreenshot)

## Criterios de optimización
El algoritmo busca crear horarios que:
- Minimicen la cantidad de días con clases
- Agrupen las clases en días consecutivos (ej. preferir lunes-martes en lugar de lunes-jueves)
- Respeten las restricciones de cupos disponibles

## Limitaciones conocidas
1. **Extracción de datos**: Aunque el parser de PDF funciona en la mayoría de los casos, son demasiados cursos y no puedo ni quiero revisarlos a mano para confirmar.
2. **Optimización limitada**: Para limitar el uso de recursos, el algoritmo tiene un tiempo máximo de ejecución, cuando hay demasiados cursos puede dar soluciones subóptimas.
3. **Accesibilidad**: La interfaz necesita mejoras en términos de accesibilidad.

## Demo
![img](https://i.imgur.com/Zad6ntP.png/img)

La aplicación se puede probar en: https://aguero.shinyapps.io/app_horarios/

**Credenciales de acceso:**
- Usuario: demo@demo.com
- Contraseña: Demo-123

> [!WARNING] 
> Por favor, utilice la demo con moderación ya que consume recursos limitados de shinyapps.io.
