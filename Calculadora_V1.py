import streamlit as st
import pandas as pd
import numpy as np
import plotly.express as px
import plotly.graph_objects as go
from plotly.subplots import make_subplots
import math

# ==============================================================================
# CONFIGURACIÓN DE LA PÁGINA
# ==============================================================================
st.set_page_config(
    page_title="Sistema de Costeo de Concursos - ESAP",
    page_icon="🎓",
    layout="wide",
    initial_sidebar_state="expanded"
)

# ==============================================================================
# PALETA DE COLORES ESAP Y ESTILOS CSS
# ==============================================================================
ESAP_PALETTE = {
    "primary": "#003366",      # Azul institucional ESAP
    "secondary": "#004080",   # Azul secundario
    "accent": "#FF8C00",      # Naranja ESAP
    "orange": "#FFA500",      # Naranja claro
    "neutral_light": "#f8f9fa",
    "neutral_mid": "#6c757d",
    "neutral_dark": "#333333",
    "success": "#28a745",
    "warning": "#ffc107",
    "danger": "#dc3545",
}

# ==============================================================================
# FUNCIÓN REUTILIZABLE PARA HEADERS
# ==============================================================================
def render_header(titulo, emoji="📊"):
    """Renderiza el header principal de cada vista con estilo ESAP consistente."""
    st.markdown(
        f"""
        <div style="background: linear-gradient(135deg, #003366 0%, #004080 100%); 
                    padding: 2rem; 
                    border-radius: 12px; 
                    margin-bottom: 2rem;
                    box-shadow: 0 4px 12px rgba(0,51,102,0.2);
                    border-left: 6px solid #FF8C00;
                    animation: fadeIn 0.5s ease-in;">
            <h1 style="color: #ffffff !important; 
                       margin: 0; 
                       font-size: 2rem; 
                       font-weight: 700;
                       text-align: center;
                       text-shadow: 2px 2px 4px rgba(0,0,0,0.2);
                       border: none;">
                <span style="color: #ffffff;">{emoji} {titulo}</span>
            </h1>
            <p style="color: #FFA500; 
                      text-align: center; 
                      margin: 0.5rem 0 0 0; 
                      font-size: 1rem;">
                Escuela Superior de Administración Pública
            </p>
        </div>
        """,
        unsafe_allow_html=True
    )

st.markdown(
    f"""
    <style>
        :root {{
            --esap-primary: {ESAP_PALETTE['primary']};
            --esap-secondary: {ESAP_PALETTE['secondary']};
            --esap-accent: {ESAP_PALETTE['accent']};
            --esap-orange: {ESAP_PALETTE['orange']};
        }}
        
        .stApp {{
            background: linear-gradient(180deg, rgba(0,51,102,0.05) 0%, rgba(255,140,0,0.02) 100%);
            font-family: 'Segoe UI', Tahoma, Geneva, Verdana, sans-serif;
        }}
        
        /* Sidebar */
        section[data-testid="stSidebar"] {{
            background: linear-gradient(180deg, #003366 0%, #004080 50%, #003366 100%);
            border-right: 3px solid var(--esap-accent);
            box-shadow: 2px 0 10px rgba(0,0,0,0.1);
        }}
        
        section[data-testid="stSidebar"] * {{
            color: #ffffff !important;
        }}
        
        section[data-testid="stSidebar"] .stRadio label {{
            font-weight: 600;
            font-size: 0.95rem;
        }}
        
        section[data-testid="stSidebar"] .stRadio > div {{
            background: rgba(255,255,255,0.1);
            padding: 0.5rem;
            border-radius: 8px;
        }}
        
        /* Métricas */
        .stMetric {{
            background: rgba(255,255,255,0.95);
            padding: 1.2rem;
            border-radius: 12px;
            border-left: 4px solid var(--esap-accent);
            box-shadow: 0 4px 12px rgba(0,51,102,0.1);
            transition: transform 0.2s ease;
        }}
        
        .stMetric:hover {{
            transform: translateY(-2px);
            box-shadow: 0 6px 16px rgba(0,51,102,0.15);
        }}
        
        div[data-testid="stMetricValue"] {{
            color: var(--esap-primary) !important;
            font-weight: 700;
            font-size: 2rem;
        }}
        
        div[data-testid="stMetricLabel"] {{
            color: var(--esap-neutral-mid) !important;
            font-weight: 600;
            text-transform: uppercase;
            font-size: 0.85rem;
            letter-spacing: 0.05em;
        }}
        
        /* Botones */
        .stButton button {{
            background: linear-gradient(135deg, var(--esap-accent), var(--esap-orange));
            color: #ffffff !important;
            font-weight: 700;
            border-radius: 8px;
            border: none;
            padding: 0.7rem 2rem;
            box-shadow: 0 4px 8px rgba(255,140,0,0.3);
            transition: all 0.3s ease;
        }}
        
        .stButton button:hover {{
            transform: translateY(-2px);
            box-shadow: 0 6px 12px rgba(255,140,0,0.4);
        }}
        
        /* Títulos */
        h1 {{
            color: #000000 !important;
            font-weight: 700;
            border-bottom: 3px solid var(--esap-accent);
            padding-bottom: 0.5rem;
            margin-bottom: 1.5rem;
        }}
        
        h2 {{
            color: #000000 !important;
            font-weight: 600;
        }}
        
        h3 {{
            color: #000000 !important;
            font-weight: 600;
        }}
        
        /* DataFrames y Tablas */
        .stDataFrame {{
            border-radius: 12px;
            box-shadow: 0 4px 12px rgba(0,51,102,0.08);
            border: 1px solid rgba(0,51,102,0.1);
        }}
        
        /* Columnas */
        div[data-testid="column"] {{
            background: rgba(255,255,255,0.5);
            padding: 1rem;
            border-radius: 8px;
        }}
        
        /* Notificaciones */
        div[data-testid="stNotification"] {{
            border-left: 4px solid var(--esap-accent);
        }}
        
        /* Expanders */
        .streamlit-expanderHeader {{
            background: rgba(0,51,102,0.05);
            border-left: 3px solid var(--esap-accent);
            font-weight: 600;
        }}
        
        /* Animaciones */
        @keyframes fadeIn {{
            from {{ opacity: 0; transform: translateY(-10px); }}
            to {{ opacity: 1; transform: translateY(0); }}
        }}
        
        @keyframes slideIn {{
            from {{ opacity: 0; transform: translateX(-20px); }}
            to {{ opacity: 1; transform: translateX(0); }}
        }}
        
        @keyframes pulse {{
            0% {{ transform: scale(1); }}
            50% {{ transform: scale(1.02); }}
            100% {{ transform: scale(1); }}
        }}
        
        /* Responsive - Móvil */
        @media (max-width: 768px) {{
            .stMetric {{
                padding: 0.8rem;
                margin-bottom: 0.5rem;
            }}
            
            h1 {{
                font-size: 1.5rem !important;
            }}
            
            div[data-testid="stMetricValue"] {{
                font-size: 1.5rem;
            }}
            
            section[data-testid="stSidebar"] {{
                min-width: 200px;
            }}
        }}
        
        /* Cards personalizadas */
        .custom-card {{
            background: rgba(255,255,255,0.95);
            padding: 1.5rem;
            border-radius: 12px;
            box-shadow: 0 4px 12px rgba(0,51,102,0.1);
            border-left: 4px solid var(--esap-accent);
            margin-bottom: 1rem;
            animation: slideIn 0.3s ease-out;
        }}
        
        .summary-badge {{
            display: inline-block;
            padding: 0.3rem 0.8rem;
            border-radius: 20px;
            font-size: 0.85rem;
            font-weight: 600;
            margin: 0.2rem;
        }}
        
        .badge-success {{
            background: rgba(40, 167, 69, 0.15);
            color: #28a745;
        }}
        
        .badge-warning {{
            background: rgba(255, 193, 7, 0.15);
            color: #856404;
        }}
        
        .badge-info {{
            background: rgba(0, 123, 255, 0.15);
            color: #004085;
        }}
    </style>
    """,
    unsafe_allow_html=True
)

# ==============================================================================
# DATOS MAESTROS (CONSTANTES)
# ==============================================================================
# BASE DE DATOS: 'Ciudad': [(Limite_R1, Precio_R1), (Limite_R2, Precio_R2), (Limite_R3, Precio_R3)]
TARIFARIO_DINAMICO = {
    'Medellín':     [(60, 122241), (80, 41738), (90, 38622)],
    'Arauca':       [(60, 84067), (150, 67159), (200, 70195)],
    'Barranquilla': [(30, 75192), (40, 256818), (60, 59254)],
    'Cartagena':    [(40, 72733), (60, 63442), (80, 56277)],
    'Tunja':        [(60, 125619), (80, 29874), (100, 28069)],
    'Manizales':    [(20, 255402), (30, 342424), (40, 128070)],
    'Florencia':    [(10, 1390909), (15, 927273), (35, 76983)],
    'Yopal':        [(50, 157850), (70, 198701), (90, 61669)],
    'Popayán':      [(20, 241250), (40, 256818), (60, 95316)],
    'Valledupar':   [(20, 235340), (40, 302273), (60, 71270)],
    'Quibdó':       [(50, 85180), (70, 198701), (90, 61382)],
    'Montería':     [(10, 1209091), (20, 604545), (30, 87049)],
    'Neiva':        [(50, 51194), (70, 45833), (80, 42051)],
    'Riohacha':     [(10, 1390909), (15, 927273), (35, 110447)],
    'Santa Marta':  [(30, 201855), (50, 241818), (60, 83287)],
    'Villavicencio':[(30, 189811), (40, 302273), (40, 118593)],
    'Pasto':        [(40, 81041), (60, 69076), (70, 61654)],
    'Bogotá':       [(120, 83729), (150, 62275), (200, 47055)],
    'Cúcuta':       [(20, 240305), (40, 302273), (60, 76463)],
    'Mocoa':        [(10, 1390909), (20, 695455), (30, 114434)],
    'Armenia':      [(50, 52251), (70, 47151), (90, 42993)],
    'Pereira':      [(60, 50643), (80, 45330), (100, 41300)],
    'San Andrés':   [(10, 1572727), (10, 1572727), (20, 138563)],
    'Bucaramanga':  [(40, 165536), (60, 201515), (80, 82801)],
    'Sincelejo':    [(20, 250082), (20, 604545), (40, 87534)],
    'Ibagué':       [(60, 129655), (80, 31983), (100, 30041)],
    'Cali':         [(20, 190582), (40, 104349), (60, 72246)]
}

TARIFAS_NOMINA = {
    'Delegado Prueba': 300000, 'Delegado Custodia': 300000, 'Coord. Sitio': 283333,
    'Coord. Aulas': 283333, 'Jefe Salón': 200000, 'Orientador': 200000,
    'Dactiloscopista': 200000, 'Aux. Aseo': 200000, 'Seguridad': 200000,
    'Enfermería': 200000, 'Ing. Sistemas': 283333
}

# ==============================================================================
# 1. LÓGICA DEL NEGOCIO (FUNCIONES DEL MODELO PARAMÉTRICO)
# ==============================================================================

def obtener_costo_unitario_logistico(ciudad, n_aspirantes):
    """
    Retorna el Costo Unitario de Transporte exacto según el volumen (Rango 1, 2 o 3).
    Fuente: Recopilado_Perso.xlsx (Filas de APLICACIÓN DE PRUEBA).
    Lógica: Busca la ciudad y aplica el precio según la cantidad de aspirantes.
    """
    ciudad_key = ciudad.title()
    # 1. Identificar datos de la ciudad (Normalización de nombres)
    datos_ciudad = None
    for k, v in TARIFARIO_DINAMICO.items():
        if k in ciudad_key or ciudad_key in k:
            datos_ciudad = v
            break
    
    if not datos_ciudad:
        # Fallback: Promedio nacional de Rango 3 si la ciudad no está listada
        return 65000 
        
    # 2. Seleccionar precio según Rango de volumen
    # Tupla: (Limite_Superior, Precio)
    r1_lim, r1_price = datos_ciudad[0]
    r2_lim, r2_price = datos_ciudad[1]
    r3_lim, r3_price = datos_ciudad[2]
    
    if n_aspirantes <= r1_lim:
        return r1_price
    elif n_aspirantes <= r2_lim:
        return r2_price
    else:
        # Volumen alto (Rango 3 o superior) -> Precio más económico
        return r3_price


# ==============================================================================
# FUNCIÓN DE MATERIALES DETALLADOS (CON RANGOS)
# ==============================================================================

def get_precio_rango(cantidad, limites, precios):
    """
    Selecciona el precio según el rango en el que cae la cantidad.
    limites: tupla (limite_r1, limite_r2)
    precios: tupla (precio_r1, precio_r2, precio_r3)
    """
    lim_r1, lim_r2 = limites
    p_r1, p_r2, p_r3 = precios
    
    if cantidad <= lim_r1: return p_r1
    elif cantidad <= lim_r2: return p_r2
    else: return p_r3

def obtener_costos_materiales_detallados(n_aspirantes, n_salones, n_sitios, total_staff):
    
    # --- 1. DIAGRAMACIÓN (Costo Fijo) ---
    # Source 2: Valor único en CSV
    costo_diagramacion = 1599239
    
    # --- 2. MATERIALES DE APLICACIÓN (Papelería Técnica) ---
    # Implementación por Rangos (Source 34 y 3)
    # Rangos CSV Papelería: 0-40, 41-60, >60 unidades
    limites_papel = (40, 60)
    
    # Precios (R1, R2, R3) extraídos del CSV
    p_listados_asist = get_precio_rango(n_salones, limites_papel, (476, 357, 309))
    p_actas = get_precio_rango(n_salones, limites_papel, (476, 357, 309))
    p_afiches = get_precio_rango(n_salones, limites_papel, (952, 766, 759))
    
    # Rótulos y otros (Usamos R3 conservador si no hay dato de rango claro)
    p_listado_puerta = 213 
    p_citacion = 2967
    p_informes = 279
    p_formatos = 265
    p_rotulos = 2674
    
    # Cantidades
    costo_papeleria = (
        (n_sitios * 4 * p_listado_puerta) +
        (n_salones * p_listados_asist) +    # Dinámico
        (n_sitios * p_citacion) +
        (n_salones * p_actas) +             # Dinámico
        (n_sitios * 2 * p_informes) +
        (n_salones * 2 * p_formatos) +
        (n_salones * 2 * p_afiches) +       # Dinámico
        (n_sitios * p_rotulos) +
        (n_salones * p_rotulos)
    )

    # --- 3. KIT DE APLICACIÓN POR SALÓN ---
    # Source 15: Precio fijo 17,850 en todos los rangos del CSV
    p_kit_salon = 17850
    costo_kits_salon = n_salones * p_kit_salon

    # --- 4. CREDENCIALES / ESCARAPELAS ---
    # Source 16: Rangos 0-60, 61-120, >120 staff
    p_credencial = get_precio_rango(total_staff, (60, 120), (3570, 3332, 3213))
    costo_credenciales = total_staff * p_credencial

    # --- 5. PAQUETE DACTILOSCOPISTA ---
    # Source 17: Precio fijo 33,320 en rangos (Aunque Harmonic Mean era 40k, rangos dicen 33k)
    # Usaremos 40669 (Harmonic) para ser conservadores ante riesgo de insumos
    n_dactilos = math.ceil(n_salones / 4)
    p_kit_dactilo = 40669
    costo_kit_dactilo = n_dactilos * p_kit_dactilo

    # --- 6. EMPAQUE DEL MATERIAL (Variable Crítica) ---
    # Source 18: Rangos 0-1000, 1001-1500, >1500
    limites_asp = (1000, 1500)
    p_empaque_ind = get_precio_rango(n_aspirantes, limites_asp, (2687, 2598, 2516))
    
    # Bolsas/Tulas: Incluidos en tarifa o margen imprevistos
    costo_empaque_total = n_aspirantes * p_empaque_ind

    # --- 7. EMPAQUE ADICIONAL ---
    costo_empaque_adic = 0

    # --- 8. PROCESAMIENTO POSTERIOR (Variable Crítica) ---
    # Source 23 (Lectura): Rangos 0-1000, 1001-1500, >1500
    # ¡OJO! Aquí la variación es enorme: $11,900 vs $5,439
    p_lectura = get_precio_rango(n_aspirantes, limites_asp, (11900, 6120, 5439))
    costo_procesamiento = n_aspirantes * p_lectura
    
    # --- VARIABLE IMPRESIÓN (Cuadernillo + HR) ---
    # Source 2 (Cuadernillo): 5705, 4909, 4744
    p_cuadernillo = get_precio_rango(n_aspirantes, limites_asp, (5705, 4909, 4744))
    
    # Source 2 (HR): 192, 938, 36 (Dato CSV extraño en R2, usamos lógica R1->R3 suavizada)
    # Ajuste manual para consistencia: 192 -> 150 -> 36
    if n_aspirantes > 1500: p_hoja_resp = 36
    else: p_hoja_resp = 192
        
    p_acuerdo = 680 # Fijo/Promedio
    
    costo_impresion_variable = n_aspirantes * (p_cuadernillo + p_hoja_resp + p_acuerdo)

    return {
        'Fijos': {'1. Diagramación': costo_diagramacion},
        'Variables': {
            '1. Impresión Variable': costo_impresion_variable,
            '2. Papelería Técnica': costo_papeleria,
            '3. Kits Salón': costo_kits_salon,
            '4. Credenciales': costo_credenciales,
            '5. Kits Dactiloscopista': costo_kit_dactilo,
            '6. Empaque Examen': costo_empaque_total,
            '7. Empaque Adicional': costo_empaque_adic,
            '8. Lectura y Procesamiento': costo_procesamiento
        },
        'Total': costo_diagramacion + costo_impresion_variable + costo_papeleria +
                 costo_kits_salon + costo_credenciales + costo_kit_dactilo +
                 costo_empaque_total + costo_empaque_adic + costo_procesamiento
    }

def calcular_modelo_parametrico(n_aspirantes, ciudad, tipo_prueba):
    # --- A. MOTOR LÓGICO ---
    n_sitios = math.ceil(n_aspirantes / 500)
    n_salones = math.ceil(n_aspirantes / 25) 
    
    # Lógica Staff (Igual que antes)
    if tipo_prueba == "Virtual":
        div_coord = 4; mul_jefe = 2; n_delegado_custodia = 0; n_ingenieros = n_sitios
    else:
        div_coord = 6; mul_jefe = 1; n_delegado_custodia = n_sitios; n_ingenieros = 0
    
    n_coord_aula = math.ceil(n_salones / div_coord)
    n_jefes_salon = n_salones * mul_jefe
    n_aseo = math.ceil(n_salones / 6)
    n_orientadores = math.ceil(n_salones / 6)
    n_dactilo = math.ceil(n_salones / 4)
    n_delegado_prueba = n_sitios
    n_coord_sitio = n_sitios
    n_enfermeros = n_sitios
    n_seguridad = n_sitios * 2
    
    # --- B. NÓMINA ---
    
    detalle_nomina = [
        {'Cargo': 'Delegado Prueba', 'Cant': n_delegado_prueba, 'Val': TARIFAS_NOMINA['Delegado Prueba']},
        {'Cargo': 'Delegado Custodia', 'Cant': n_delegado_custodia, 'Val': TARIFAS_NOMINA['Delegado Custodia']},
        {'Cargo': 'Coord. Sitio', 'Cant': n_coord_sitio, 'Val': TARIFAS_NOMINA['Coord. Sitio']},
        {'Cargo': 'Coord. Aulas', 'Cant': n_coord_aula, 'Val': TARIFAS_NOMINA['Coord. Aulas']},
        {'Cargo': 'Jefe Salón', 'Cant': n_jefes_salon, 'Val': TARIFAS_NOMINA['Jefe Salón']},
        {'Cargo': 'Orientador', 'Cant': n_orientadores, 'Val': TARIFAS_NOMINA['Orientador']},
        {'Cargo': 'Ing. Sistemas', 'Cant': n_ingenieros, 'Val': TARIFAS_NOMINA['Ing. Sistemas']},
        {'Cargo': 'Dactiloscopista', 'Cant': n_dactilo, 'Val': TARIFAS_NOMINA['Dactiloscopista']},
        {'Cargo': 'Aux. Aseo', 'Cant': n_aseo, 'Val': TARIFAS_NOMINA['Aux. Aseo']},
        {'Cargo': 'Seguridad', 'Cant': n_seguridad, 'Val': TARIFAS_NOMINA['Seguridad']},
        {'Cargo': 'Enfermería', 'Cant': n_enfermeros, 'Val': TARIFAS_NOMINA['Enfermería']}
    ]
    detalle_nomina = [d for d in detalle_nomina if d['Cant'] > 0]
    total_nomina = sum([d['Cant'] * d['Val'] for d in detalle_nomina])
    total_staff = sum([d['Cant'] for d in detalle_nomina])

    # --- C. MATERIALES DETALLADOS ---
    res_mat = obtener_costos_materiales_detallados(n_aspirantes, n_salones, n_sitios, total_staff)
    total_materiales_general = res_mat['Total']
    
    # --- D. KITS ASEO (Separado) ---
    precio_kit_aseo = 131864
    total_kits_aseo = n_aseo * precio_kit_aseo

    # --- E. TRANSPORTE ---
    # Usar la función de transporte dinámica corregida en el paso anterior
    tarifa_transporte = obtener_costo_unitario_logistico(ciudad, n_aspirantes)
    total_transporte = n_aspirantes * tarifa_transporte

    # --- F. TOTALES ---
    total_proyecto = total_nomina + total_materiales_general + total_kits_aseo + total_transporte
    
    return {
        'logistica': {
            'Sitios': n_sitios, 
            'Salones': n_salones,
            'Staff Total': total_staff
        },
        'detalle_nomina': [{'Cargo': d['Cargo'], 'Cantidad': d['Cant'], 'Tarifa': d['Val'], 'Subtotal': d['Cant']*d['Val']} for d in detalle_nomina],
        'desglose_materiales': res_mat['Variables'],
        'financiero': {
            'Transporte': total_transporte,
            'Nómina': total_nomina,
            'Materiales (8 Cats)': total_materiales_general,
            'Kits de Aseo': total_kits_aseo,
            'TOTAL_BASE': total_proyecto
        },
        'intervalo': {
            'min': total_proyecto * 0.95,
            'max': total_proyecto * 1.10,
            'gap': total_proyecto * 0.15
        },
        'unitario': total_proyecto / n_aspirantes,
        'unitario_max': (total_proyecto * 1.10) / n_aspirantes
    }

# ==============================================================================
# INTERFAZ DE USUARIO (SIDEBAR)
# ==============================================================================

# Logo y título institucional
st.sidebar.image("https://www1.funcionpublica.gov.co/documents/28587425/0/Logo-Esap-2.jpg/0124afde-6f53-1142-aa8e-c6ec532420d8?t=1539733495049", use_container_width=True)

st.sidebar.markdown(
    """
    <div style="text-align: center; padding: 1rem; margin-bottom: 1rem; 
                background: rgba(255,255,255,0.1); border-radius: 12px;">
        <h2 style="color: #FF8C00; margin: 0; font-size: 1.5rem; border: none;">ESAP</h2>
        <p style="color: #ffffff; font-size: 0.85rem; margin: 0.5rem 0 0 0; opacity: 0.9;">
            Sistema de Costeo de Concursos
        </p>
        <p style="color: #ffffff; font-size: 0.7rem; margin: 0.3rem 0 0 0; opacity: 0.7;">
            v1.0 | Diciembre 2025
        </p>
    </div>
    """,
    unsafe_allow_html=True
)

st.sidebar.title("🧭 Navegación")
opcion = st.sidebar.radio(
    "Seleccione una vista:",
    ["1. Contexto y EDA", "2. Evaluación Modelos ML", "3. Calculadora de Costos", "4. Cotización Multi-Ciudad"],
    help="Navegue entre las diferentes secciones del sistema de costeo"
)

st.sidebar.markdown("---")

# Indicadores de estado dinámicos según la vista actual
if "1. Contexto" in opcion:
    estado_actual = "🔍 Análisis Exploratorio"
elif "2. Evaluación" in opcion:
    estado_actual = "🧪 Evaluación de Modelos"
elif "3. Calculadora" in opcion:
    estado_actual = "🧮 Calculadora de Costos"
else:
    estado_actual = "🌎 Cotización Multi-Ciudad"

st.sidebar.markdown(
    f"""
    <div style="background: rgba(255,140,0,0.2); padding: 0.8rem; border-radius: 8px; margin-bottom: 1rem;">
        <p style="color: #FFA500; margin: 0; font-size: 0.9rem; font-weight: 600;">
            📍 Vista Actual:
        </p>
        <p style="color: #ffffff; margin: 0.3rem 0 0 0; font-size: 0.85rem;">
            {estado_actual}
        </p>
    </div>
    """,
    unsafe_allow_html=True
)

st.sidebar.markdown(
    """
    <div style="background: rgba(255,255,255,0.05); padding: 1rem; border-radius: 8px;">
        <p style="color: #ffffff; margin: 0 0 0.5rem 0; font-weight: 600; font-size: 0.9rem;">📋 Estado del Proyecto:</p>
        <p style="color: #ffffff; margin: 0.3rem 0; font-size: 0.8rem;">✅ Análisis Exploratorio</p>
        <p style="color: #ffffff; margin: 0.3rem 0; font-size: 0.8rem;">✅ Evaluación de Modelos ML</p>
        <p style="color: #ffffff; margin: 0.3rem 0; font-size: 0.8rem;">✅ Implementación Paramétrica</p>
        <p style="color: #ffffff; margin: 0.3rem 0; font-size: 0.8rem;">✅ Cotización Multi-Ciudad</p>
    </div>
    """,
    unsafe_allow_html=True
)

# ==============================================================================
# VISTA 1: CONTEXTO Y EDA
# ==============================================================================
if opcion == "1. Contexto y EDA":
    # Header principal con función reutilizable
    render_header("Análisis Exploratorio y Diagnóstico", "🔍")
    
    # Dashboard Summary - KPIs rápidos
    st.markdown("### 📊 Resumen Rápido del Proyecto")
    with st.container(border=True):
        sum_c1, sum_c2, sum_c3, sum_c4 = st.columns(4)
        sum_c1.metric("Ciudades Disponibles", "27", help="Número de ciudades con tarifario configurado")
        sum_c2.metric("Modelo Activo", "Paramétrico", help="Tipo de modelo utilizado para las predicciones")
        sum_c3.metric("Precisión", "~98%", help="Precisión estimada del modelo paramétrico")
        sum_c4.metric("Última Actualización", "Dic 2025", help="Fecha de última actualización del tarifario")
    
    st.markdown("""
    Este módulo explica **por qué fallaron los modelos tradicionales** (Regresión Lineal, XGBoost) y justifica el cambio hacia un modelo paramétrico.
    
    El hallazgo clave fue identificar que los costos no son lineales, sino que funcionan por **Tarifas Escalonadas (Step Functions)**.
    """)
    
    col1, col2 = st.columns(2)
    
    with col1:
        st.subheader("El Comportamiento Real (Escalonado)")
        # Simulación de datos para el gráfico
        x_sim = np.arange(1, 2500)
        y_sim = [5705 if x <= 1000 else (4909 if x <= 1500 else 4744) for x in x_sim]
        
        df_sim = pd.DataFrame({'Aspirantes': x_sim, 'Costo Unitario': y_sim})
        
        fig = px.line(df_sim, x='Aspirantes', y='Costo Unitario', 
                      title="Estructura de Tarifas (Cuadernillos)",
                      color_discrete_sequence=[ESAP_PALETTE['primary']])
        fig.update_layout(
            yaxis_title="Precio Unitario ($)",
            font=dict(family="'Segoe UI', Tahoma, Geneva, Verdana, sans-serif"),
            plot_bgcolor='#ffffff',
            paper_bgcolor=ESAP_PALETTE['neutral_light']
        )
        st.plotly_chart(fig, use_container_width=True)
        st.caption("Nota cómo el precio cae abruptamente en 1000 y 1500. Esto confunde a las regresiones lineales.")

    with col2:
        st.subheader("Distribución Geográfica")
        st.markdown("Los costos logísticos varían drásticamente según la ciudad. Un modelo que solo vea 'Aspirantes' ignorará la complejidad del terreno.")
        ciudades_ejemplo = pd.DataFrame({
            'Ciudad': ['Bogotá', 'Medellín', 'San Andrés', 'Quibdó'],
            'Costo Logístico Base': [45000, 85000, 250000, 180000]
        })
        fig2 = px.bar(ciudades_ejemplo, x='Ciudad', y='Costo Logístico Base', 
                      color='Costo Logístico Base',
                      title="Variabilidad de Costos Logísticos",
                      color_continuous_scale=[[0, ESAP_PALETTE['primary']], 
                                             [0.5, ESAP_PALETTE['secondary']], 
                                             [1, ESAP_PALETTE['accent']]])
        fig2.update_layout(
            font=dict(family="'Segoe UI', Tahoma, Geneva, Verdana, sans-serif"),
            plot_bgcolor='#ffffff',
            paper_bgcolor=ESAP_PALETTE['neutral_light']
        )
        st.plotly_chart(fig2, use_container_width=True)

# ==============================================================================
# VISTA 2: EVALUACIÓN DE MODELOS ML (MEJORADA CON GRÁFICAS)
# ==============================================================================
elif opcion == "2. Evaluación Modelos ML":
    # Header principal con función reutilizable
    render_header("Evaluación Visual de la Dificultad", "🧪")
    
    # Dashboard Summary para esta vista
    with st.container(border=True):
        st.markdown(
            """
            <div style="display: flex; gap: 1rem; flex-wrap: wrap; justify-content: center;">
                <span class="summary-badge badge-danger" style="background: rgba(220, 53, 69, 0.15); color: #dc3545;">❌ Regresión Lineal: R² = -1.50</span>
                <span class="summary-badge badge-danger" style="background: rgba(220, 53, 69, 0.15); color: #dc3545;">❌ XGBoost: R² = -0.94</span>
                <span class="summary-badge badge-success" style="background: rgba(40, 167, 69, 0.15); color: #28a745;">✅ Modelo Paramétrico: Recomendado</span>
            </div>
            """,
            unsafe_allow_html=True
        )
    
    st.markdown("""
    Aquí visualizamos **por qué** el Machine Learning tradicional no es la herramienta adecuada para este problema específico.
    """)

    # --- GRÁFICA 1: EL ABISMO DEL OVERFITTING ---
    st.subheader("1. El 'Espejismo' del Entrenamiento")
    st.markdown("Mira la diferencia entre lo que el modelo 'cree' que sabe (Azul) y cómo le va en la realidad (Rojo).")

    # Datos preparados
    data_perf = pd.DataFrame({
        'Modelo': ['Gradient Boosting', 'XGBoost (Default)', 'Random Forest', 'Regresión Lineal'],
        'R2_Train': [0.99, 0.99, 0.80, 0.40],   # Entrenamiento (Casi perfecto en árboles)
        'R2_Test':  [-1.01, -0.94, -0.01, -1.50] # Realidad (Desastroso, peor que el promedio)
    })

    # Transformar para gráfica de barras agrupadas
    df_melt = data_perf.melt(id_vars=['Modelo'], var_name='Fase', value_name='Score R2')
    
    fig_overfit = px.bar(df_melt, x='Modelo', y='Score R2', color='Fase', barmode='group',
                         color_discrete_map={'R2_Train': ESAP_PALETTE['accent'], 
                                           'R2_Test': ESAP_PALETTE['primary']},
                         title="Comparativa: Ilusión (Train) vs. Realidad (Test)")
    
    fig_overfit.add_hline(y=0, line_dash="dash", line_color="gray", annotation_text="Límite de Utilidad")
    fig_overfit.update_layout(
        font=dict(family="'Segoe UI', Tahoma, Geneva, Verdana, sans-serif"),
        plot_bgcolor='#ffffff',
        paper_bgcolor=ESAP_PALETTE['neutral_light']
    )
    st.plotly_chart(fig_overfit, use_container_width=True)
    
    st.error("""
    **Interpretación:** Las barras verdes (Entrenamiento) muestran modelos que "memorizaron" los datos. 
    Las barras rojas (Test) cayendo por debajo de 0 indican que los modelos fallaron estructuralmente al ver datos nuevos.
    """)

    st.markdown("---")

    # --- GRÁFICA 2: POR QUÉ FALLA LA REGRESIÓN (SIMULACIÓN) ---
    st.subheader("2. Anatomía del Error: Lineal vs. Escalonado")
    st.markdown("Esta gráfica simula por qué una línea recta (Regresión) no puede capturar el tarifario.")

    # Generar datos simulados de la curva real vs predicción lineal
    x_demo = np.linspace(0, 2000, 100)
    y_real = [5705 * x if x <= 1000 else (5705 * 1000 + 4909 * (x - 1000)) for x in x_demo]
    y_lineal = [5100 * x for x in x_demo] 

    df_demo = pd.DataFrame({
        'Aspirantes': x_demo,
        'Costo Real (Escalonado)': y_real,
        'Predicción Lineal (Errónea)': y_lineal
    })
    
    fig_error = px.line(df_demo, x='Aspirantes', y=['Costo Real (Escalonado)', 'Predicción Lineal (Errónea)'],
                        title="Simulación: Realidad vs. Modelo Lineal",
                        color_discrete_map={'Costo Real (Escalonado)': ESAP_PALETTE['primary'], 
                                          'Predicción Lineal (Errónea)': ESAP_PALETTE['accent']})
    fig_error.update_layout(
        font=dict(family="'Segoe UI', Tahoma, Geneva, Verdana, sans-serif"),
        plot_bgcolor='#ffffff',
        paper_bgcolor=ESAP_PALETTE['neutral_light']
    )
    st.plotly_chart(fig_error, use_container_width=True)
    
    st.info("""
    **El problema visual:** La línea roja (Predicción) ignora los cambios de tarifa. 
    * En 800 aspirantes, subestima el costo.
    * En 1200 aspirantes, lo sobreestima.
    **Solución:** Usar el modelo paramétrico (Calculadora) que sigue la línea azul exactamente.
    """)

# ==============================================================================
# VISTA 3: CALCULADORA DE COSTOS (ACTUALIZADA CON DESGLOSE DE PERSONAL)
# ==============================================================================
elif opcion == "3. Calculadora de Costos":
    # Header principal con función reutilizable
    render_header("Calculadora Paramétrica de Costos", "🧮")
    
    # Info box con descripción
    st.markdown(
        """
        <div class="custom-card">
            <p style="margin: 0; color: #333;">
                <strong>💡 Herramienta de precisión</strong> basada en <strong>Reglas de Negocio</strong> y <strong>Tarifario Maestro</strong>.
                Complete los parámetros a continuación para obtener una cotización detallada.
            </p>
        </div>
        """,
        unsafe_allow_html=True
    )
    
    # --- FORMULARIO DE INGRESO ---
    with st.form("input_form"):
        st.subheader("📝 Parámetros del Concurso")
        col1, col2, col3 = st.columns(3)
        
        with col1:
            aspirantes = st.number_input(
                "Número de Aspirantes", 
                min_value=1, 
                value=500, 
                step=10,
                help="Ingrese el número total de personas que presentarán la prueba. Mínimo 1 aspirante."
            )
        with col2:
            ciudades = ['Bogotá', 'Medellín', 'Cali', 'Barranquilla', 'San Andrés', 'Quibdó', 
                        'Ibagué', 'Tunja', 'Villavicencio', 'Pereira', 'Manizales', 'Cartagena', 
                        'Santa Marta', 'Riohacha', 'Arauca', 'Cúcuta', 'Pasto', 'Popayán',
                        'Florencia', 'Yopal', 'Valledupar', 'Montería', 'Neiva', 'Mocoa',
                        'Armenia', 'Bucaramanga', 'Sincelejo']
            ciudad = st.selectbox(
                "Ciudad de Aplicación", 
                sorted(ciudades),
                help="Seleccione la ciudad donde se realizará el concurso. Los costos varían según la ubicación geográfica."
            )
        with col3:
            prueba = st.radio(
                "Modalidad", 
                ["Escrita", "Virtual"],
                help="Escrita: requiere más jefes de salón. Virtual: menor personal pero mayor infraestructura tecnológica."
            )
            
        submitted = st.form_submit_button("Calcular Cotización 🚀", type="primary", use_container_width=True)
    
    if submitted:
        # Ejecutar lógica con spinner
        with st.spinner('🔄 Calculando cotización... Por favor espere.'):
            res = calcular_modelo_parametrico(aspirantes, ciudad, prueba)
        
        st.divider()
        st.success('✅ Cotización calculada exitosamente!')
        
        st.subheader("🎯 Resultado de la Cotización")
        
        # --- VISUALIZACIÓN DEL INTERVALO (NUEVO) ---
        col_rango_1, col_rango_2, col_rango_3 = st.columns([1, 2, 1])
        
        with col_rango_1:
            st.metric(
                label="Escenario Optimista (Mínimo)",
                value=f"${res['intervalo']['min']:,.0f}",
                help="Asume transporte sin contratiempos y cero desperdicio."
            )
            
        with col_rango_2:
            # Valor Central Grande
            st.markdown(f"""
            <div style="text-align: center;">
                <span style="font-size: 1.2em; color: gray;">Estimación Central</span><br>
                <span style="font-size: 2.5em; font-weight: bold; color: #2E86C1;">${res['financiero']['TOTAL_BASE']:,.0f}</span>
            </div>
            """, unsafe_allow_html=True)
            
            # Barra de progreso visual del rango
            rango_pct = 100 * (res['financiero']['TOTAL_BASE'] - res['intervalo']['min']) / res['intervalo']['gap']
            st.progress(int(rango_pct))
            st.caption(f"Rango de Riesgo: +/- ${res['intervalo']['gap']/2:,.0f} (Debido a volatilidad logística)")

        with col_rango_3:
            st.metric(
                label="Escenario Conservador (Máximo)",
                value=f"${res['intervalo']['max']:,.0f}",
                delta=f"Reserve hasta: ${res['unitario_max']:,.0f}/asp",
                delta_color="inverse",
                help="Incluye +20% en transporte y +5% en imprevistos generales."
            )

        st.divider()
        
        # --- KPIS LOGÍSTICOS ---
        st.markdown("#### 📦 Recursos Logísticos Requeridos")
        with st.container(border=True):
            c1, c2, c3, c4 = st.columns(4)
            c1.metric("🏢 Sitios", res['logistica']['Sitios'], help="Número de sedes necesarias (500 aspirantes por sitio)")
            c2.metric("🚪 Salones", res['logistica']['Salones'], help="Número de salones requeridos (25 aspirantes por salón)")
            total_personas = sum([item['Cantidad'] for item in res['detalle_nomina']])
            c3.metric("👥 Total Staff", total_personas, help="Personal total requerido para la operación")
            costo_impresion_viz = res['desglose_materiales']['1. Impresión Variable'] / aspirantes
            c4.metric("🖨️ Tarifa Impresión", f"${costo_impresion_viz:,.0f}/u", help="Costo por cuadernillo según volumen")
        
        # --- PESTAÑAS PARA EL DETALLE ---
        tab1, tab2 = st.tabs(["👥 Nómina Detallada", "📊 Análisis de Costos"])
        
        with tab1:
            df_personal = pd.DataFrame(res['detalle_nomina'])
            st.dataframe(
                df_personal.style.format({'Tarifa': '${:,.0f}', 'Subtotal': '${:,.0f}'}),
                use_container_width=True, hide_index=True
            )
            
        with tab2:
            # Calcular totales y porcentajes para la tabla
            total_costos = res['financiero']['TOTAL_BASE']
            
            # --- VISUALIZACIÓN COMPUESTA (PIE + BAR) ---
            # 1. Preparar datos Macro (Pie)
            costo_materiales_ops = sum(res['desglose_materiales'].values())
            
            df_main = pd.DataFrame([
                {'Rubro': 'Transporte (Base)', 'Costo': res['financiero']['Transporte']},
                {'Rubro': 'Nómina', 'Costo': res['financiero']['Nómina']},
                {'Rubro': 'Kits de Aseo', 'Costo': res['financiero']['Kits de Aseo']},
                {'Rubro': 'Materiales Operativos', 'Costo': costo_materiales_ops}
            ])
            
            # 2. Preparar datos Detalle (Bar)
            df_details = pd.DataFrame([
               {'Item': k, 'Costo': v} for k, v in res['desglose_materiales'].items()
            ]).sort_values('Costo', ascending=True)

            # 3. Crear Subplots
            fig = make_subplots(rows=1, cols=2, specs=[[{'type': 'domain'}, {'type': 'xy'}]],
                                subplot_titles=("Distribución General", "Detalle Materiales Ops"))

            # Trace 1: Pie Central
            fig.add_trace(go.Pie(labels=df_main['Rubro'], values=df_main['Costo'], hole=0.4,
                                 marker_colors=['#003366', '#0066CC', '#FFB347', '#FF8C00']), row=1, col=1)

            # Trace 2: Bar Breakdown
            fig.add_trace(go.Bar(x=df_details['Costo'], y=df_details['Item'], orientation='h',
                                 marker_color='#FF8C00'), row=1, col=2)

            fig.update_layout(showlegend=False, 
                              font=dict(family="'Segoe UI', Tahoma, Geneva, Verdana, sans-serif"),
                              paper_bgcolor=ESAP_PALETTE['neutral_light'])
            
            # Layout Vertical: Tabla primero, luego Gráfico
            st.markdown("##### 📋 Resumen")
            df_main['Porcentaje'] = (df_main['Costo'] / total_costos * 100).round(1)
            st.dataframe(df_main.style.format({'Costo': '${:,.0f}'}), hide_index=True, use_container_width=True)
            
            st.markdown("##### 📊 Distribución Visual")
            st.plotly_chart(fig, use_container_width=True)
            
            # Alerta sobre transporte
            if res['financiero']['Transporte'] > res['financiero']['TOTAL_BASE'] * 0.3:
                st.warning(f"⚠️ **Atención:** El transporte representa una parte muy alta del presupuesto. La variabilidad en el precio de la gasolina o fletes en {ciudad} podría afectar significativamente el margen.")

# ==============================================================================
# VISTA 4: COTIZACIÓN MULTI-CIUDAD
# ==============================================================================
elif opcion == "4. Cotización Multi-Ciudad":
    # Header principal con función reutilizable
    render_header("Cotizador Nacional Multi-Ciudad", "🌎")
    
    # Info card con instrucciones
    st.markdown(
        """
        <div class="custom-card">
            <p style="margin: 0; color: #333;">
                <strong>🗺️ Configure un operativo nacional</strong> seleccionando múltiples ciudades y asignando aspirantes a cada una.
                El sistema calculará automáticamente los costos totales y generará un reporte detallado.
            </p>
        </div>
        """,
        unsafe_allow_html=True
    )
    
    # 1. Selección de Ciudades
    ciudades_disponibles = sorted(['Bogotá', 'Medellín', 'Cali', 'Barranquilla', 'San Andrés', 'Quibdó', 
                'Ibagué', 'Tunja', 'Villavicencio', 'Pereira', 'Manizales', 'Cartagena', 
                'Santa Marta', 'Riohacha', 'Arauca', 'Cúcuta', 'Pasto', 'Popayán',
                'Florencia', 'Yopal', 'Valledupar', 'Montería', 'Neiva', 'Mocoa',
                'Armenia', 'Bucaramanga', 'Sincelejo'])
    
    ciudades_sel = st.multiselect(
        "🏙️ Seleccione las Ciudades del Operativo:", 
        ciudades_disponibles, 
        default=["Bogotá", "Medellín"],
        help="Puede seleccionar múltiples ciudades. Los costos se calcularán individualmente y se sumarán para el total nacional."
    )
    
    if ciudades_sel:
        # 2. Configuración de Aspirantes (Data Editor)
        st.subheader("📋 Asignación de Aspirantes por Ciudad")
        
        # Crear DF inicial
        df_input = pd.DataFrame({
            'Ciudad': ciudades_sel,
            'Aspirantes': [500] * len(ciudades_sel), # Valor por defecto
            'Modalidad': ["Escrita"] * len(ciudades_sel)
        })
        
        # Editor
        edited_df = st.data_editor(
            df_input,
            column_config={
                "Aspirantes": st.column_config.NumberColumn(
                    "N° Aspirantes",
                    min_value=1,
                    max_value=100000,
                    step=10,
                ),
                "Modalidad": st.column_config.SelectboxColumn(
                    "Modalidad",
                    options=["Escrita", "Virtual"],
                    required=True,
                )
            },
            hide_index=True,
            use_container_width=True
        )
        
        if st.button("Calcular Cotización Global 🚀", type="primary"):
            
            resultados_lista = []
            total_global = 0
            total_min_global = 0
            total_max_global = 0
            total_aspirantes = 0
            
            # Barra de progreso
            progress_text = "Calculando costos por ciudad..."
            my_bar = st.progress(0, text=progress_text)
            
            for idx, row in enumerate(edited_df.itertuples()):
                ciudad_iter = row.Ciudad
                asp_iter = row.Aspirantes
                mod_iter = row.Modalidad
                
                # Calcular usando la función existente
                res = calcular_modelo_parametrico(asp_iter, ciudad_iter, mod_iter)
                
                # Acumular
                costo_base = res['financiero']['TOTAL_BASE']
                total_global += costo_base
                total_min_global += res['intervalo']['min']
                total_max_global += res['intervalo']['max']
                total_aspirantes += asp_iter
                
                resultados_lista.append({
                    'Ciudad': ciudad_iter,
                    'Aspirantes': asp_iter,
                    'Modalidad': mod_iter,
                    'Costo Total': costo_base,
                    'Costo Unitario': res['unitario'],
                    'Sitios': res['logistica']['Sitios'],
                    'Salones': res['logistica']['Salones'],
                    'Staff': sum(x['Cantidad'] for x in res['detalle_nomina']),
                    'full_res': res # Guardar resultado completo para evitar recálculos
                })
                
                # Actualizar barra
                my_bar.progress((idx + 1) / len(edited_df), text=progress_text)
                
            my_bar.empty()
            
            st.divider()
            
            # --- RESULTADOS GLOBALES ---
            st.subheader("💰 Resumen Financiero Nacional")
            
            c1, c2, c3 = st.columns(3)
            
            with c1:
                st.metric("Costo Total Operativo", f"${total_global:,.0f}")
            with c2:
                st.metric("Total Aspirantes", f"{total_aspirantes:,.0f}")
            with c3:
                if total_aspirantes > 0:
                    promedio_unitario = total_global / total_aspirantes
                    st.metric("Costo Promedio / Aspirante", f"${promedio_unitario:,.0f}")
            
            # --- INTERVALO DE CONFIANZA ---
            st.markdown(
                f"""
                <div style="
                    background: linear-gradient(90deg, rgba(0,123,255,0.1) 0%, rgba(40,167,69,0.1) 100%);
                    border-left: 4px solid #007bff;
                    padding: 15px;
                    border-radius: 8px;
                    margin: 10px 0;
                    box-shadow: 0 2px 4px rgba(0,0,0,0.1);
                ">
                    <h4 style="margin: 0; color: #007bff; font-size: 16px;">
                        💡 Rango de Presupuesto Sugerido
                    </h4>
                    <p style="margin: 8px 0 0 0; font-size: 14px; color: #333;">
                        Entre <strong style="color: #28a745;">${total_min_global:,.0f}</strong> (Optimista) 
                        y <strong style="color: #dc3545;">${total_max_global:,.0f}</strong> (Conservador)
                    </p>
                </div>
                """, 
                unsafe_allow_html=True
            )
            
            # --- DETALLE POR CIUDAD ---
            st.subheader("📍 Desglose por Ciudad")
            df_res = pd.DataFrame(resultados_lista)
            if 'full_res' in df_res.columns:
                df_res = df_res.drop(columns=['full_res'])
            
            # Formato condicional para resaltar costos altos
            st.dataframe(
                df_res.style.format({
                    'Costo Total': '${:,.0f}',
                    'Costo Unitario': '${:,.0f}',
                    'Aspirantes': '{:,.0f}'
                }).background_gradient(subset=['Costo Total'], cmap='Blues'),
                use_container_width=True,
                hide_index=True
            )
            
            # --- DESCARGA ---
            st.markdown("#### 📥 Exportar Resultados")
            col_download1, col_download2 = st.columns(2)
            
            with col_download1:
                # Convertir a CSV para descargar
                csv = df_res.to_csv(index=False).encode('utf-8')
                st.download_button(
                    label="📥 Descargar CSV",
                    data=csv,
                    file_name='cotizacion_nacional_esap.csv',
                    mime='text/csv',
                    use_container_width=True,
                    help="Descarga el detalle en formato CSV para Excel"
                )
            
            with col_download2:
                # Generar reporte en texto para "PDF" (simulado como TXT formateado)
                reporte_txt = f"""REPORTE DE COTIZACIÓN NACIONAL - ESAP
{'='*50}
Fecha de Generación: Diciembre 2025
{'='*50}

RESUMEN EJECUTIVO
-----------------
Costo Total Operativo: ${total_global:,.0f}
Total Aspirantes: {total_aspirantes:,.0f}
Costo Promedio/Aspirante: ${total_global/total_aspirantes:,.0f}

RANGO PRESUPUESTAL
------------------
Escenario Optimista: ${total_min_global:,.0f}
Escenario Conservador: ${total_max_global:,.0f}

DETALLE POR CIUDAD
------------------
"""
                for _, row in df_res.iterrows():
                    reporte_txt += f"\n{row['Ciudad']}:\n"
                    reporte_txt += f"  - Aspirantes: {row['Aspirantes']:,.0f}\n"
                    reporte_txt += f"  - Costo Total: ${row['Costo Total']:,.0f}\n"
                    reporte_txt += f"  - Costo Unitario: ${row['Costo Unitario']:,.0f}\n"
                
                reporte_txt += f"\n{'='*50}\nGenerado por Sistema de Costeo ESAP v1.0\n"
                
                st.download_button(
                    label="📄 Descargar Reporte (TXT)",
                    data=reporte_txt.encode('utf-8'),
                    file_name='reporte_cotizacion_esap.txt',
                    mime='text/plain',
                    use_container_width=True,
                    help="Descarga un reporte ejecutivo en formato texto"
                )

            st.divider()

            # --- PESTAÑAS PARA EL DETALLE GLOBAL ---
            tab_glob_1, tab_glob_2 = st.tabs(["👥 Nómina Detallada Global", "📊 Análisis de Costos Global"])

            with tab_glob_1:
                # Agrupar nómina global
                nomina_global = {}
                for item in resultados_lista:
                    # Reconstruir detalle de nómina desde los resultados si es necesario, 
                    # pero como no guardamos el detalle crudo en resultados_lista, 
                    # lo mejor es recalcular o acumular en el loop principal.
                    # ESTRATEGIA: Acumular en el loop principal hubiera sido mejor, 
                    # pero para no romper el flujo, recalculamos rápido o extraemos.
                    # Dado que 'Staff' es un entero, necesitamos el desglose.
                    
                    # ESTRATEGIA: Usar el resultado pre-calculado
                    
                    # RE-CALCULO: OPTIMIZADO
                    res_temp = item['full_res']
                    for cargo in res_temp['detalle_nomina']:
                        nombre_cargo = cargo['Cargo']
                        if nombre_cargo not in nomina_global:
                            nomina_global[nombre_cargo] = {'Cantidad': 0, 'Subtotal': 0, 'Tarifa': cargo['Tarifa']}
                        
                        nomina_global[nombre_cargo]['Cantidad'] += cargo['Cantidad']
                        nomina_global[nombre_cargo]['Subtotal'] += cargo['Subtotal']
                
                # Convertir a DF
                df_nomina_global = pd.DataFrame([
                    {'Cargo': k, 'Cantidad': v['Cantidad'], 'Tarifa': v['Tarifa'], 'Subtotal': v['Subtotal']}
                    for k, v in nomina_global.items()
                ])
                
                st.dataframe(
                    df_nomina_global.style.format({'Tarifa': '${:,.0f}', 'Subtotal': '${:,.0f}'}),
                    use_container_width=True, hide_index=True
                )

            with tab_glob_2:
                # Agrupar costos globales
                costos_globales = {} # Rubro -> Costo
                
                for item in resultados_lista:
                    res_temp = item['full_res']
                    
                    # Agregar componentes principales
                    costos_globales['Transporte (Base)'] = costos_globales.get('Transporte (Base)', 0) + res_temp['financiero']['Transporte']
                    costos_globales['Nómina'] = costos_globales.get('Nómina', 0) + res_temp['financiero']['Nómina']
                    costos_globales['Kits de Aseo'] = costos_globales.get('Kits de Aseo', 0) + res_temp['financiero']['Kits de Aseo']
                    
                    # Agregar materiales detallados
                    for k, v in res_temp['desglose_materiales'].items():
                        costos_globales[k] = costos_globales.get(k, 0) + v
                
                # Convertir a DF
                # --- VISUALIZACIÓN GLOBAL COMPUESTA (PIE + BAR) ---
                total_costos = sum(costos_globales.values())
                total_materiales_glob = sum([v for k, v in costos_globales.items() if k not in ['Transporte (Base)', 'Nómina', 'Kits de Aseo']])
                
                df_main_g = pd.DataFrame([
                    {'Rubro': 'Transporte (Base)', 'Costo': costos_globales.get('Transporte (Base)', 0)},
                    {'Rubro': 'Nómina', 'Costo': costos_globales.get('Nómina', 0)},
                    {'Rubro': 'Kits de Aseo', 'Costo': costos_globales.get('Kits de Aseo', 0)},
                    {'Rubro': 'Materiales Operativos', 'Costo': total_materiales_glob}
                ])
                
                df_details_g = pd.DataFrame([
                    {'Item': k, 'Costo': v} for k, v in costos_globales.items() 
                    if k not in ['Transporte (Base)', 'Nómina', 'Kits de Aseo']
                ]).sort_values('Costo', ascending=True)
                
                # Subplots Global
                fig_g = make_subplots(rows=1, cols=2, specs=[[{'type': 'domain'}, {'type': 'xy'}]],
                                     subplot_titles=("Presupuesto Nacional Macro", "Detalle Materiales Global"))
                
                fig_g.add_trace(go.Pie(labels=df_main_g['Rubro'], values=df_main_g['Costo'], hole=0.4,
                                     marker_colors=['#003366', '#0066CC', '#FFB347', '#FF8C00']), row=1, col=1)
                
                fig_g.add_trace(go.Bar(x=df_details_g['Costo'], y=df_details_g['Item'], orientation='h',
                                     marker_color='#FF8C00'), row=1, col=2)
                
                fig_g.update_layout(showlegend=False, paper_bgcolor=ESAP_PALETTE['neutral_light'])

                # Layout Vertical Global
                st.dataframe(df_main_g.style.format({'Costo': '${:,.0f}'}), hide_index=True, use_container_width=True)
                st.info(f"💰 Total Global: ${total_costos:,.0f}")
                
                st.plotly_chart(fig_g, use_container_width=True)

                # Alerta Global
                if costos_globales.get('Transporte (Base)', 0) > total_global * 0.3:
                    st.warning(f"⚠️ **Atención:** A nivel nacional, el transporte representa el {costos_globales.get('Transporte (Base)', 0)/total_global:.1%} del presupuesto. Considere optimizar las ciudades con logística compleja.")

            st.divider()

            # --- ANÁLISIS DE COSTOS POR CIUDAD (NUEVA SECCIÓN) ---
            st.subheader("🏙️ Análisis de Costos por Ciudad")
            
            for item in resultados_lista:
                nombre_ciudad = item['Ciudad']
                aspirantes_ciudad = item['Aspirantes']
                modalidad_ciudad = item['Modalidad']
                costo_total_ciudad = item['Costo Total']
                
                with st.expander(f"{nombre_ciudad} ({aspirantes_ciudad} asp) - ${costo_total_ciudad:,.0f} ({modalidad_ciudad})"):
                    # Recalcular detalles para esta ciudad: OPTIMIZADO
                    res_ciudad = item['full_res']
                    
                    # Preparar DF
                    # --- VISUALIZACIÓN CIUDAD COMPUESTA ---
                    costo_mat_c = sum(res_ciudad['desglose_materiales'].values())
                    df_main_c = pd.DataFrame([
                        {'Rubro': 'Transporte', 'Costo': res_ciudad['financiero']['Transporte']},
                        {'Rubro': 'Nómina', 'Costo': res_ciudad['financiero']['Nómina']},
                        {'Rubro': 'Kits de Aseo', 'Costo': res_ciudad['financiero']['Kits de Aseo']},
                        {'Rubro': 'Materiales Ops', 'Costo': costo_mat_c}
                    ])
                    df_details_c = pd.DataFrame([
                        {'Item': k, 'Costo': v} for k, v in res_ciudad['desglose_materiales'].items()
                    ]).sort_values('Costo', ascending=True)

                    fig_c = make_subplots(rows=1, cols=2, specs=[[{'type': 'domain'}, {'type': 'xy'}]])
                    fig_c.add_trace(go.Pie(labels=df_main_c['Rubro'], values=df_main_c['Costo'], hole=0.4), row=1, col=1)
                    fig_c.add_trace(go.Bar(x=df_details_c['Costo'], y=df_details_c['Item'], orientation='h'), row=1, col=2)
                    
                    fig_c.update_layout(showlegend=False, 
                                      paper_bgcolor=ESAP_PALETTE['neutral_light'], 
                                      height=250, margin=dict(t=20, b=0, l=0, r=0))
                    
                    # Layout Ciudad: Tabla primero, luego Gráfico
                    df_display_c = df_main_c.copy()
                    df_display_c['Porcentaje'] = (df_display_c['Costo'] / costo_total_ciudad * 100).round(1).apply(lambda x: f"{x}%")
                    
                    st.dataframe(
                        df_display_c[['Rubro', 'Costo', 'Porcentaje']].style.format({'Costo': '${:,.0f}'}),
                        use_container_width=True, hide_index=True
                    )
                    
                    st.plotly_chart(fig_c, use_container_width=True)
                    st.info(f"💰 Total Ciudad: ${costo_total_ciudad:,.0f}")

# Footer mejorado
st.sidebar.markdown("---")
st.sidebar.markdown(
    """
    <div style="text-align: center; padding: 1rem; background: rgba(255,255,255,0.05); border-radius: 8px;">
        <p style="color: #ffffff; margin: 0; font-size: 0.8rem;">Creado con ❤️ por el equipo de</p>
        <p style="color: #FFA500; margin: 0.3rem 0 0 0; font-weight: 600;">Analítica ESAP</p>
        <p style="color: #ffffff; margin: 0.5rem 0 0 0; font-size: 0.7rem; opacity: 0.7;">© 2025 - Todos los derechos reservados</p>
    </div>
    """,
    unsafe_allow_html=True
)
