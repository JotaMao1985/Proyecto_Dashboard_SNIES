# Dashboard SNIES - Educación Superior en Colombia  
  
[![Streamlit App](https://static.streamlit.io/badges/streamlit_badge_black_white.svg)](https://streamlit.io/)  
  
Este proyecto es un dashboard interactivo construido en **Python (Streamlit)** que visualiza datos del Sistema Nacional de Información de la Educación Superior (SNIES) de Colombia.  
  
Originalmente desarrollado como una aplicación en **R (Shiny)**, este repositorio representa la migración completa a Python, aprovechando librerías como Plotly, Pandas y Folium para crear una herramienta de análisis de datos moderna y reactiva.  
  
## 📸 Vista Previa  
  
*(Te recomiendo tomar una captura de pantalla de tu dashboard finalizado y colocarla aquí. Simplemente arrastra la imagen al editor de README de GitHub)*  
  
`[Imagen de la app de Streamlit mostrando los KPIs y el mapa]`  
  
## 🚀 Características Principales  
  
* **KPIs Dinámicos:** Métricas clave (Total Matriculados, Graduados, IES Únicas) que se actualizan según los filtros.  
* **Filtros Interactivos:** Filtra los datos por Rango de Años, Departamento, Nivel Académico, Sector y Caracter de la IES.  
* **Visualizaciones Detalladas:**  
    * Gráfico de tendencias de matriculados vs. graduados.  
    * Gráfico de barras de distribución por sexo y nivel académico.  
    * Treemap de matriculados por sector y caracter.  
    * Gráfico de dispersión de IES (Matriculados vs. Graduados).  
* **Análisis Geográfico:** Un mapa coroplético interactivo (usando Folium) que muestra la distribución de IES o matriculados por departamento.  
* **Exploración de Datos:** Una tabla de datos filtrada y la capacidad de descargar los datos filtrados como un archivo `.csv`.  
  
## 🛠️ Tecnologías Utilizadas  
  
* **Framework:** Streamlit  
* **Manipulación de Datos:** Pandas  
* **Visualización de Datos:** Plotly Express  
* **Mapas:** Geopandas, Folium, y `streamlit-folium`  
* **Limpieza de Datos:** `re` y `unicodedata` (para la normalización de nombres de departamentos)  
  
---  
  
## ⚙️ Configuración y Ejecución Local  
  
Sigue estos pasos para ejecutar el dashboard en tu máquina local.  
  
### 1. Prerrequisitos  
  
* Python 3.8 o superior  
* Git  
  
### 2. Clonar el Repositorio  
  
```bash  
git clone [https://github.com/TU_USUARIO/TU_REPOSITORIO.git](https://github.com/TU_USUARIO/TU_REPOSITORIO.git)  
cd TU_REPOSITORIO  
