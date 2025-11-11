import streamlit as st
import pandas as pd
import plotly.express as px
import geopandas as gpd
import folium
from streamlit_folium import st_folium
import unicodedata
import re

# --- 1. Configuración de la Página y Definiciones Globales ---

# Columnas clave (traducidas de R)
COL_YEAR = "Año"
COL_SECTOR = "Sector IES"
COL_CARACTER = "Caracter IES"
COL_DEPTO = "Departamento de domicilio de la IES"
COL_NIVEL = "Nivel Académico"
COL_MATRIC = "Matriculados"
COL_GRAD = "Graduados"
COL_IES = "Institución de Educación Superior (IES)"
COL_SEXO = "Sexo"
COL_METODOLOGIA = "Metodología"
COL_PROGRAMA = "Programa Académico"


USTA_PALETTE = {
    "primary": "#0a2f6b",
    "secondary": "#1d4f91",
    "accent": "#f9a602",
    "teal": "#1c9c9c",
    "neutral_light": "#f5f7fa",
    "neutral_mid": "#60738a",
    "neutral_dark": "#17213c",
}

PLOTLY_COLOR_SEQUENCE = [
    USTA_PALETTE["primary"],
    USTA_PALETTE["secondary"],
    USTA_PALETTE["teal"],
    USTA_PALETTE["accent"],
]


def inject_brand_theme() -> None:
    """Aplica estilos globales inspirados en la identidad visual de la USTA."""
    st.markdown(
        f"""
        <style>
            :root {{
                --usta-primary: {USTA_PALETTE['primary']};
                --usta-secondary: {USTA_PALETTE['secondary']};
                --usta-accent: {USTA_PALETTE['accent']};
                --usta-teal: {USTA_PALETTE['teal']};
                --usta-neutral-light: {USTA_PALETTE['neutral_light']};
                --usta-neutral-mid: {USTA_PALETTE['neutral_mid']};
                --usta-neutral-dark: {USTA_PALETTE['neutral_dark']};
            }}

            .stApp {{
                background: linear-gradient(180deg, rgba(10,47,107,0.08) 0%, rgba(28,156,156,0.03) 100%);
                color: var(--usta-neutral-dark);
            }}

            section[data-testid="stSidebar"] {{
                background: linear-gradient(180deg, rgba(10,47,107,0.95) 0%, rgba(29,79,145,0.92) 40%, rgba(28,156,156,0.9) 100%);
                color: #ffffff;
                border-right: 0;
            }}

            section[data-testid="stSidebar"] * {{
                color: #ffffff !important;
            }}

            section[data-testid="stSidebar"] label {{
                font-weight: 600;
                text-transform: uppercase;
                letter-spacing: 0.04em;
            }}

            section[data-testid="stSidebar"] .stSlider, 
            section[data-testid="stSidebar"] .stSelectbox,
            section[data-testid="stSidebar"] .stRadio,
            section[data-testid="stSidebar"] .stNumberInput {{
                color: var(--usta-neutral-dark) !important;
            }}

            section[data-testid="stSidebar"] .stSelectbox div[data-baseweb="select"] > div,
            section[data-testid="stSidebar"] .stNumberInput input,
            section[data-testid="stSidebar"] .stTextInput input {{
                background: rgba(255,255,255,0.88);
                border: 1px solid rgba(255,255,255,0.45);
                border-radius: 10px;
                color: var(--usta-neutral-dark) !important;
            }}

            section[data-testid="stSidebar"] .stSelectbox div[data-baseweb="select"] svg,
            section[data-testid="stSidebar"] .stSelectbox div[data-baseweb="select"] span {{
                color: var(--usta-neutral-dark) !important;
            }}

            section[data-testid="stSidebar"] .stSlider > div > div {{
                background-color: rgba(10,47,107,0.2);
            }}

            section[data-testid="stSidebar"] .stSlider > div > div > div {{
                background-color: var(--usta-primary);
            }}

            .stMetric {{
                background: rgba(255,255,255,0.86);
                padding: 1rem;
                border-radius: 14px;
                border: 1px solid rgba(10,47,107,0.12);
                box-shadow: 0 8px 24px rgba(23,33,60,0.07);
            }}

            div[data-testid="stMetricValue"] {{
                color: var(--usta-primary) !important;
                font-weight: 700;
            }}

            div[data-testid="stMetricLabel"] {{
                color: var(--usta-neutral-mid) !important;
            }}

            .stDataFrame {{
                border-radius: 16px;
                box-shadow: 0 10px 26px rgba(15,33,58,0.08);
            }}

            .stDownloadButton button {{
                background: linear-gradient(135deg, var(--usta-accent), #ffd166);
                color: var(--usta-neutral-dark) !important;
                font-weight: 700;
                border-radius: 999px;
                border: none;
                padding: 0.65rem 1.8rem;
            }}

            .stButton button {{
                background-color: var(--usta-accent);
                color: var(--usta-neutral-dark) !important;
                font-weight: 600;
                border-radius: 10px;
                border: none;
            }}

            h1, h2, h3, h4 {{
                color: var(--usta-primary) !important;
                font-family: "Montserrat", "Segoe UI", sans-serif;
            }}

            .block-container {{
                padding-top: 2rem;
            }}
        </style>
        """,
        unsafe_allow_html=True,
    )


def apply_plotly_theme(fig):
    """Aplica colores y estilos de la USTA a un gráfico Plotly."""
    fig.update_layout(
        font=dict(family="Montserrat, 'Segoe UI', sans-serif", color=USTA_PALETTE["neutral_dark"]),
        paper_bgcolor=USTA_PALETTE["neutral_light"],
        plot_bgcolor="#ffffff",
        title=dict(font=dict(color=USTA_PALETTE["primary"], size=18)),
        legend=dict(
            bgcolor="rgba(255,255,255,0.8)",
            bordercolor="rgba(10,47,107,0.15)",
            borderwidth=0.5,
        ),
        margin=dict(l=40, r=30, t=60, b=40),
    )
    fig.update_xaxes(showgrid=True, gridcolor="rgba(10,47,107,0.08)")
    fig.update_yaxes(showgrid=True, gridcolor="rgba(10,47,107,0.08)")
    return fig


st.set_page_config(
    page_title="Dashboard SNIES - Educación Superior",
    page_icon="🎓",
    layout="wide"
)

inject_brand_theme()



def normalize_depto_names(depto_name):
    try:
        name = str(depto_name).upper()
        
        # 1. Quitar acentos (ej. BOGOTÁ -> BOGOTA)
        name = unicodedata.normalize('NFD', name)
        name = name.encode('ascii', 'ignore').decode('utf-8')
        
        # 2. Quitar puntuación (ej. BOGOTA D.C. -> BOGOTA D C)
        name = re.sub(r"[^A-Z ]", "", name)
        
        # 3. Corrección Específica de Bogotá (CRUCIAL)
        # Convertir "SANTAFE DE BOGOTA D C" en "BOGOTA D C"
        name = re.sub(r"SANTAFE DE ", "", name)
        
        # 4. Quitar prefijos estándar
        name = re.sub(r"^LA ", "", name)
        name = re.sub(r"^EL ", "", name)
        
        # 5. Quitar espacios dobles/extra
        name = re.sub(r"\s+", " ", name).strip()
        
        # 6. Quitar sufijos (Manejar AMBOS " D C" y " DC")
        name = re.sub(r" D C$", "", name) # Con espacios
        name = re.sub(r" DC$", "", name)  # Sin espacios (para el CSV)
        
        # 7. Recodificaciones manuales
        recode_map = {
            "VALLE": "VALLE DEL CAUCA",
            "ARCHIPELAGO DE SAN ANDRES PROVIDENCIA Y SANTA CATALINA": "SAN ANDRES Y PROVIDENCIA",
        }
        return recode_map.get(name, name)
    except Exception:
        return str(depto_name)
    
# st.cache_data es el equivalente a memoise.
@st.cache_data
def load_data():
    try:
        # fileEncoding = "UTF-8-BOM" en R es encoding = "utf-8-sig" en Python
        df = pd.read_csv("MEN_SNIES_test.csv", encoding="utf-8-sig")
        
        # Limpieza de nombres de columnas
        df = df.rename(columns={
            "Institución.de.Educación.Superior..IES.": COL_IES,
            "Departamento.de.domicilio.de.la.IES": COL_DEPTO,
            "Nivel.Académico": COL_NIVEL,
            "Sector.IES": COL_SECTOR,
            "Caracter.IES": COL_CARACTER,
            "Programa.Académico": COL_PROGRAMA
        })
        
        # Conversión de tipos
        df[COL_MATRIC] = pd.to_numeric(df[COL_MATRIC], errors='coerce').fillna(0)
        df[COL_GRAD] = pd.to_numeric(df[COL_GRAD], errors='coerce').fillna(0)
        df[COL_YEAR] = pd.to_numeric(df[COL_YEAR], errors='coerce')

        df = df.dropna(subset=[COL_YEAR])
        if df.empty:
            st.error("El archivo de datos no contiene registros válidos con año.")
            return pd.DataFrame()
        df[COL_YEAR] = df[COL_YEAR].astype(int)

        # Asegurar que las columnas de filtro sean strings limpias
        cols_to_str = [COL_SECTOR, COL_CARACTER, COL_DEPTO, COL_NIVEL, COL_METODOLOGIA, COL_SEXO]
        for col in cols_to_str:
            if col in df.columns:
                df[col] = (
                    df[col]
                    .fillna("Sin registro")
                    .astype(str)
                    .str.strip()
                    .replace({"": "Sin registro", "nan": "Sin registro", "None": "Sin registro"})
                )

        return df
    except FileNotFoundError:
        st.error("Error fatal: No se encontró el archivo 'MEN_SNIES_test.csv'.")
        return pd.DataFrame() # Retorna un DF vacío para evitar que la app crashee

@st.cache_data
def load_geojson_data():
    try:
        gdf = gpd.read_file("colombia.geo.json")
        # Asegúrate de que la columna se llama 'NOMBRE_DPT'
        if 'NOMBRE_DPT' not in gdf.columns:
            st.error("GeoJSON no tiene la columna 'NOMBRE_DPT'. Por favor, verifique el archivo.")
            return gpd.GeoDataFrame()
            
        gdf['Depto_Join_Key'] = gdf['NOMBRE_DPT'].apply(normalize_depto_names)
        return gdf
    except Exception as e:
        st.error(f"Error al cargar 'colombia.geo.json': {e}")
        return gpd.GeoDataFrame()


@st.cache_data
def convert_df_to_csv(df: pd.DataFrame) -> bytes:
    """Convierte un DataFrame en CSV UTF-8 con BOM (ideal para Excel)."""
    return df.to_csv(index=False, encoding='utf-8-sig').encode('utf-8-sig')

# Cargar datos globalmente (será cacheado después de la primera ejecución)
data_snies = load_data()
geojson_data = load_geojson_data()


# Si los datos no se cargaron, detener la app
if data_snies.empty or geojson_data.empty:
    st.stop()

# --- 3. Barra Lateral de Filtros (UI) ---

with st.sidebar:
    st.title("Filtros Principales")

    # Obtener valores para los filtros
    min_year = int(data_snies[COL_YEAR].min())
    max_year = int(data_snies[COL_YEAR].max())
    choice_depto = ["Todos"] + sorted(data_snies[COL_DEPTO].unique())
    choice_nivel = ["Todos"] + sorted(data_snies[COL_NIVEL].unique())
    choice_sektor = ["Todos"] + sorted(data_snies[COL_SECTOR].unique())
    choice_metodologia = ["Todos"] + sorted(data_snies[COL_METODOLOGIA].unique())

    # --- Definición de Widgets ---
    
    range_year = st.slider(
        "Rango de Años:",
        min_value=min_year,
        max_value=max_year,
        value=(min_year, max_year)
    )

    filter_depto = st.selectbox(
        "Departamento:",
        options=choice_depto,
        index=choice_depto.index("BOGOTÁ D.C.") if "BOGOTÁ D.C." in choice_depto else 0 # Predeterminado a Bogotá
    )

    filter_nivel = st.selectbox(
        "Nivel Académico:",
        options=choice_nivel,
        index=0 # Predeterminado a "Todos"
    )

    filter_sector = st.selectbox(
        "Sector IES:",
        options=choice_sektor,
        index=0 # Predeterminado a "Todos"
    )

    # --- Filtro en Cascada (Lógica de Streamlit) ---
    # Streamlit: Filtramos las opciones ANTES de renderizar el widget.
    if filter_sector == "Todos":
        choice_caracter = ["Todos"] + sorted(data_snies[COL_CARACTER].unique())
    else:
        filtered_caracter = data_snies[data_snies[COL_SECTOR] == filter_sector][COL_CARACTER].unique()
        choice_caracter = ["Todos"] + sorted(filtered_caracter)
    
    filter_caracter = st.selectbox(
        "Caracter IES:",
        options=choice_caracter,
        index=0 # Siempre resetea a "Todos"
    )

    filter_metodologia = st.selectbox(
        "Metodología:",
        options=choice_metodologia,
        index=0,
        help="Filtrar por modalidad del programa"
    )

# --- 4. Lógica de Negocio y Filtrado de Datos ---

# Streamlit: Simplemente filtramos el DataFrame.
df_filtered = data_snies.copy()
df_filtered = df_filtered[
    (df_filtered[COL_YEAR] >= range_year[0]) &
    (df_filtered[COL_YEAR] <= range_year[1])
]

if filter_depto != "Todos":
    df_filtered = df_filtered[df_filtered[COL_DEPTO] == filter_depto]
if filter_nivel != "Todos":
    df_filtered = df_filtered[df_filtered[COL_NIVEL] == filter_nivel]
if filter_sector != "Todos":
    df_filtered = df_filtered[df_filtered[COL_SECTOR] == filter_sector]
if filter_caracter != "Todos":
    df_filtered = df_filtered[df_filtered[COL_CARACTER] == filter_caracter]
if filter_metodologia != "Todos":
    df_filtered = df_filtered[df_filtered[COL_METODOLOGIA] == filter_metodologia]

# --- 5. Cuerpo Principal: KPIs y Pestañas ---

st.title("Dashboard SNIES - Educación Superior")

# --- KPIs (R: value_box) ---
def format_kpi(num):
    if num >= 1e6:
        return f"{num / 1e6:.1f} M"
    if num >= 1e3:
        return f"{num / 1e3:.0f} K"
    return str(num)

kpi_matriculados = df_filtered[COL_MATRIC].sum()
kpi_graduados = df_filtered[COL_GRAD].sum()
kpi_ies = df_filtered[COL_IES].nunique()

kpi_cols = st.columns(3)
kpi_cols[0].metric(
    label="Total Matriculados",
    value=format_kpi(kpi_matriculados)
)
kpi_cols[1].metric(
    label="Total Graduados",
    value=format_kpi(kpi_graduados)
)
kpi_cols[2].metric(
    label="Instituciones (IES) Únicas",
    value=kpi_ies
)

st.divider()

# --- Pestañas (R: navset_tab) ---
tab1, tab2, tab3, tab4 = st.tabs([
    "Resumen y Tendencias",
    "Análisis Geográfico",
    "Análisis Detallado",
    "Datos y Reportes"
])

# --- Pestaña 1: Resumen y Tendencias ---
with tab1:
    with st.spinner("Cargando gráficos de tendencias..."):
        
        if df_filtered.empty:
            st.warning("No hay datos para los filtros seleccionados.")
        else:
            # Gráfico 1: Tendencia (R: output$plot_tendencia)
            df_plot_tendencia = df_filtered.groupby(COL_YEAR).agg(
                Matriculados=(COL_MATRIC, 'sum'),
                Graduados=(COL_GRAD, 'sum')
            ).reset_index()
            
            df_plot_tendencia = df_plot_tendencia.melt(
                id_vars=COL_YEAR,
                value_vars=['Matriculados', 'Graduados'],
                var_name='Metrica',
                value_name='Total'
            )
            
            # Streamlit: px.line(...)
            fig_tendencia = px.line(
                df_plot_tendencia,
                x=COL_YEAR,
                y='Total',
                color='Metrica',
                markers=True,
                title="Evolución de Matriculados y Graduados (por Año)",
                labels={'Total': 'Total Personas', COL_YEAR: 'Año'},
                color_discrete_map={
                    'Matriculados': USTA_PALETTE['primary'],
                    'Graduados': USTA_PALETTE['accent'],
                }
            )
            fig_tendencia.update_layout(legend=dict(orientation="h", yanchor="bottom", y=1.02, xanchor="right", x=1))
            fig_tendencia = apply_plotly_theme(fig_tendencia)
            st.plotly_chart(fig_tendencia, use_container_width=True)

            # Gráfico 2: Distribución Sexo (R: output$plot_distribucion_sexo)
            df_plot_sexo = df_filtered.groupby([COL_NIVEL, COL_SEXO]).agg(
                Matriculados=(COL_MATRIC, 'sum')
            ).reset_index()

            # Streamlit: px.bar(..., barmode="group")
            fig_sexo = px.bar(
                df_plot_sexo,
                x=COL_NIVEL,
                y='Matriculados',
                color=COL_SEXO,
                barmode='group',
                title="Distribución por Nivel Académico y Sexo",
                labels={COL_NIVEL: 'Nivel Académico', 'Matriculados': 'Total Matriculados'},
                color_discrete_sequence=PLOTLY_COLOR_SEQUENCE
            )
            fig_sexo = apply_plotly_theme(fig_sexo)
            st.plotly_chart(fig_sexo, use_container_width=True)

# --- Pestaña 2: Análisis Geográfico ---
with tab2:
    st.header("Distribución Geográfica de IES")
    st.caption("El mapa muestra el número de Instituciones (IES) únicas activas según los filtros (excepto Departamento).")

    with st.spinner("Cargando mapa..."):
    # Creamos el DF para el mapa aplicando los filtros relevantes
        df_map = data_snies.copy()
        df_map = df_map[
            (df_map[COL_YEAR] >= range_year[0]) &
            (df_map[COL_YEAR] <= range_year[1])
        ]
        if filter_nivel != "Todos":
            df_map = df_map[df_map[COL_NIVEL] == filter_nivel]
        if filter_sector != "Todos":
            df_map = df_map[df_map[COL_SECTOR] == filter_sector]
        if filter_caracter != "Todos":
            df_map = df_map[df_map[COL_CARACTER] == filter_caracter]
        if filter_metodologia != "Todos":
            df_map = df_map[df_map[COL_METODOLOGIA] == filter_metodologia]
        if filter_depto != "Todos":
            df_map = df_map[df_map[COL_DEPTO] == filter_depto]

        
        data_summary = df_map.groupby(COL_DEPTO).agg(
            IES_Unicas=(COL_IES, 'nunique'),
            Matriculados=(COL_MATRIC, 'sum'),
            Graduados=(COL_GRAD, 'sum')
        ).reset_index()
        
        data_summary['Depto_Join_Key'] = data_summary[COL_DEPTO].apply(normalize_depto_names)
        
        map_data = geojson_data.merge(data_summary, on='Depto_Join_Key', how='left')
        map_data['IES_Unicas'] = map_data['IES_Unicas'].fillna(0)
        map_data['Matriculados'] = map_data['Matriculados'].fillna(0)

        if filter_depto != "Todos":
            selected_key = normalize_depto_names(filter_depto)
            map_data = map_data[map_data['Depto_Join_Key'] == selected_key]
        
        # Streamlit: Calculamos el zoom ANTES de renderizar el mapa
        if filter_depto == "Todos":
            map_location = [4.570868, -74.297333]
            map_zoom = 5
        else:
            try:
                selected_key = normalize_depto_names(filter_depto)
                selected_polygon = geojson_data[geojson_data['Depto_Join_Key'] == selected_key]
                if not selected_polygon.empty:
                    bounds = selected_polygon.total_bounds
                    map_location = [(bounds[1] + bounds[3]) / 2, (bounds[0] + bounds[2]) / 2]
                    # folium no tiene fitBounds, así que estimamos un zoom
                    map_zoom = 7 
                else:
                    map_location = [4.570868, -74.297333]
                    map_zoom = 5
            except Exception:
                map_location = [4.570868, -74.297333]
                map_zoom = 5

        m = folium.Map(location=map_location, zoom_start=map_zoom, tiles="CartoDB positron")

        if not map_data.empty and map_data['IES_Unicas'].sum() > 0:
            folium.Choropleth(
                geo_data=map_data.to_json(),
                data=map_data,
                columns=['Depto_Join_Key', 'IES_Unicas'],
                key_on='feature.properties.Depto_Join_Key',
                fill_color='YlOrBr',
                fill_opacity=0.7,
                line_opacity=0.2,
                legend_name='IES Únicas',
            ).add_to(m)

            tooltip = folium.features.GeoJsonTooltip(
                fields=['NOMBRE_DPT', 'IES_Unicas', 'Matriculados'],
                aliases=['Departamento:', 'IES Únicas:', 'Matriculados:'],
                style=("background-color: white; color: #333333; font-family: sans-serif; font-size: 12px; padding: 10px;")
            )
            
            # Añadir tooltips
            folium.GeoJson(
                map_data,
                style_function=lambda x: {'fillColor': 'transparent', 'color': 'transparent'},
                tooltip=tooltip
            ).add_to(m)

        st_folium(m, use_container_width=True, height=600)


# --- Pestaña 3: Análisis Detallado ---
with tab3:
    col1, col2 = st.columns(2)
    
    with col1:
        with st.spinner("Cargando treemap..."):
            if df_filtered.empty:
                st.warning("No hay datos para el treemap.")
            else:
                # Plotly Express hace esto MUCHO más fácil que plot_ly
                df_treemap = df_filtered.groupby([COL_SECTOR, COL_CARACTER]).agg(
                    Matriculados=(COL_MATRIC, 'sum')
                ).reset_index()
                df_treemap = df_treemap[df_treemap['Matriculados'] > 0]
                
                # Añadir una raíz "Total"
                df_treemap["Total"] = "Total"
                
                fig_treemap = px.treemap(
                    df_treemap,
                    path=['Total', COL_SECTOR, COL_CARACTER],
                    values='Matriculados',
                    title="Matriculados por Sector y Caracter IES",
                    color='Matriculados',
                    color_continuous_scale=[
                        USTA_PALETTE['neutral_light'],
                        USTA_PALETTE['accent'],
                        USTA_PALETTE['primary'],
                    ],
                )
                fig_treemap.update_layout(margin = dict(t=50, l=0, r=0, b=0))
                fig_treemap = apply_plotly_theme(fig_treemap)
                st.plotly_chart(fig_treemap, use_container_width=True)

    with col2:
        with st.spinner("Cargando gráfico de dispersión..."):
            if df_filtered.empty:
                st.warning("No hay datos para el gráfico de dispersión.")
            else:
                df_scatter = df_filtered.groupby(COL_IES).agg(
                    Matriculados=(COL_MATRIC, 'sum'),
                    Graduados=(COL_GRAD, 'sum'),
                    Programas=(COL_PROGRAMA, 'nunique')
                ).reset_index()
                df_scatter = df_scatter[(df_scatter['Matriculados'] > 0) | (df_scatter['Graduados'] > 0)]

                fig_scatter = px.scatter(
                    df_scatter,
                    x='Matriculados',
                    y='Graduados',
                    size='Programas',
                    hover_name=COL_IES,
                    title="Matriculados vs. Graduados (por IES)",
                    labels={'Matriculados': 'Total Matriculados', 'Graduados': 'Total Graduados'},
                    color_discrete_sequence=[USTA_PALETTE['teal']]
                )
                fig_scatter.update_traces(
                    opacity=0.7,
                    marker=dict(
                        line=dict(color=USTA_PALETTE['primary'], width=1.2)
                    )
                )
                fig_scatter = apply_plotly_theme(fig_scatter)
                st.plotly_chart(fig_scatter, use_container_width=True)
                st.caption("Cada punto representa una institución. El tamaño indica el total de programas.")


# --- Pestaña 4: Datos y Reportes ---
with tab4:
    st.header("Datos Filtrados")

    with st.spinner("Preparando datos..."):
        csv_data = convert_df_to_csv(df_filtered)
        
        st.download_button(
            label="Descargar CSV Filtrado",
            data=csv_data,
            file_name=f"reporte_snies_filtrado_{pd.Timestamp.now().strftime('%Y%m%d')}.csv",
            mime='text/csv',
        )
        
        st.divider()
        
        # Streamlit: st.dataframe(...)
        st.caption("Mostrando los datos filtrados. Puede ordenar y expandir la tabla.")
        
        cols_display = [COL_IES, COL_PROGRAMA, COL_NIVEL, COL_METODOLOGIA, 
                        COL_YEAR, 'Semestre', COL_MATRIC, COL_GRAD]
        # Asegurarnos de que solo mostramos columnas que existen
        cols_final_display = [col for col in cols_display if col in df_filtered.columns]
        
        st.dataframe(
            df_filtered[cols_final_display],
            use_container_width=True,
            height=600
        )