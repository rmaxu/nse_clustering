#coding: utf-8
#rmuh

# Programa para crear el conjunto de datos de los indicadores de nivel socioeconómico para el clustering por agebs.
#Cada dataset fue obtenido de https://www.inegi.org.mx/programas/ccpv/2010/default.html#Datos_abiertos

import pandas as pd

dfs = []

#Creación del conjunto de datos de indicadores
for i in range(1,33) :
#	print(i)
# 	Se leen los conjuntos de datos de los resultados del censo del 2010 por ageb, para el estado en turno
	df = pd.read_csv('resultados_ageb_urbana_'+str(i)+'_cpv2010.csv', low_memory=False)
	df = df[df['nom_loc'].isin(['Total AGEB urbana'])]
#	Se omiten las ageb con población cero y se reemplazan los valores nulos por ceros
	df = df[df['pobtot'] > 0]
	df = df.replace('*', 0)
	df = df.fillna(0)
	df.to_csv('indicadores.csv', index=False, encoding='utf-8-sig')
	df = pd.read_csv('indicadores.csv')

#	Se crea un nuevo conjunto de datos con los indicadores necesarios para el nivel socioeconómico
	df_new = pd.DataFrame()
	df_new['ENT'] = df['entidad']
	df_new['MUN'] = df['mun']
	df_new['LOC'] = df['loc']
	df_new['ageb'] = df['ageb']
	df_new['IND01'] = df['vph_aguadv'] / df['vivpar_hab']
	df_new['IND02'] = df['vph_c_elec'] / df['vivpar_hab']
	df_new['IND03'] = df['vph_drenaj'] / df['vivpar_hab']
	df_new['IND04'] = df['vph_pisodt'] / df['vivpar_hab']
	df_new['IND05'] = df['vph_excsa'] / df['vivpar_hab']
	df_new['IND06'] = df['vph_refri'] / df['vivpar_hab']
	df_new['IND07'] = df['vph_tv'] / df['vivpar_hab']
	df_new['IND08'] = df['vph_telef'] / df['vivpar_hab']
	df_new['IND09'] = df['vph_cel'] / df['vivpar_hab']
	df_new['IND10'] = df['vph_inter'] / df['vivpar_hab']
#	df_new['IND11'] = df['vph_pc'] / df['vivpar_hab']
	df_new['IND12'] = df['vph_lavad'] / df['vivpar_hab']
	df_new['IND13'] = df['vph_autom'] / df['vivpar_hab']
	df_new['IND14'] = df['pder_ss'] / df['pobtot']
	df_new['IND15'] = (df['p_15ymas'] - df['p15ym_an']) / df['p_15ymas']
	df_new['IND16'] = (df['p_6a11'] - df['p6a11_noa']) / df['p_6a11']
	df_new['IND17'] = (df['p_12a14']- df['p12a14noa']) / df['p_12a14']
	df_new['IND18'] = df['p15a17a'] / df['p_15a17']
	df_new['IND19'] = df['p15sec_co'] / df['p_15ymas']
	df_new['IND20'] = df['p18ym_pb'] / df['p_18ymas']
	df_new['IND21'] = df['pocupada_f'] / df['p_12ymas_f']
	df_new['IND22'] = df['pea'] / df['p_12ymas']
	df_new = df_new.dropna()

	dfs.append(df_new)

# Se crea un conjunto de datos con los indicadores para todas las ageb de lpaís.
data = pd.concat(dfs)

data.to_csv('indicadores.csv', index=False, encoding='utf-8-sig')

