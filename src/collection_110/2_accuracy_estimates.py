import glob
import pandas as pd
import sys
import math

#import warnings

from os import path, makedirs
import os
import numpy as np

from sklearn.metrics import confusion_matrix

# --- 1. SETUP AND CONFIGURATION ---

# Read command-line arguments: input directory, output directory, collection name.
input_dir = str(sys.argv[1])
output_dir = str(sys.argv[2])

initiative = 'MapBiomas Brazil'
collection = str(sys.argv[3])  # e.g., 'c8'
version = '85k_v5'

# Define filenames for external strata information.
POINTS_STRATA_FILE = 'points_strata.csv'
STRATA_FILE = 'strata.csv'

# Define a list of class IDs to be completely ignored in the analysis.
IGNORED_CLASSES = [0, 5, 23, 25, 27, 29, 30, 31, 32, 75]

# A comprehensive dictionary acting as a hierarchical legend for MapBiomas classes.
# It maps numeric IDs to names and values at different levels (l0, l1, l2, l3).
ALL_CLASSES = {
    0: {
        "l0_val": 0,
        "l0": "0. Não Observado",
        "l1_val": 0,
        "l1": "6. Não Observado",
        "l2_val": 0,
        "l2": "6. Não Observado",
        "l3_val": 0,
        "l3": "6. Não Observado"
    },
    3: {
        "l0_val": 1,
        "l0": "1. Natural",
        "l1_val": 1,
        "l1": "1. Floresta",
        "l2_val": 3,
        "l2": "1.1. Formação Florestal",
        "l3_val": 3,
        "l3": "1.1. Formação Florestal"
    },
    4: {
        "l0_val": 1,
        "l0": "1. Natural",
        "l1_val": 1,
        "l1": "1. Floresta",
        "l2_val": 4,
        "l2": "1.2. Formação Savânica",
        "l3_val": 4,
        "l3": "1.2. Formação Savânica"
    },
    5: {
        "l0_val": 1,
        "l0": "1. Natural",
        "l1_val": 1,
        "l1": "1. Floresta",
        "l2_val": 5,
        "l2": "1.3. Mangue",
        "l3_val": 5,
        "l3": "1.3. Mangue"
    },
    9: {
        "l0_val": 2,
        "l0": "0. Antrópico",
        "l1_val": 14,
        "l1": "3. Agropecuária",
        "l2_val": 9,
        "l2": "3.3. Floresta Plantada",
        "l3_val": 9,
        "l3": "3.3. Floresta Plantada"
    },
    11: {
        "l0_val": 1,
        "l0": "1. Natural",
        "l1_val": 10,
        "l1": "2. Formação Natural não Florestal",
        "l2_val": 11,
        "l2": "2.1. Campo Alagado e Área Pantanosa",
        "l3_val": 11,
        "l3": "2.1. Campo Alagado e Área Pantanosa"
    },
    12: {
        "l0_val": 1,
        "l0": "1. Natural",
        "l1_val": 10,
        "l1": "2. Formação Natural não Florestal",
        "l2_val": 12,
        "l2": "2.2. Formação Campestre (Campo)",
        "l3_val": 12,
        "l3": "2.2. Formação Campestre (Campo)"
    },
    # 13: {
    #     "l0_val": 1,
    #     "l0": "1. Natural",
    #     "l1_val": 10,
    #     "l1": "2. Formação Natural não Florestal",
    #     "l2_val": 13,
    #     "l2": "2.6. Outra Formação não Florestal",
    #     "l3_val": 13,
    #     "l3": "2.6. Outra Formação não Florestal"
    # },
    15: {
        "l0_val": 2,
        "l0": "0. Antrópico",
        "l1_val": 14,
        "l1": "3. Agropecuária",
        "l2_val": 15,
        "l2": "3.1. Pastagem",
        "l3_val": 15,
        "l3": "3.1. Pastagem"
    },
    19: {
        "l0_val": 2,
        "l0": "0. Antrópico",
        "l1_val": 14,
        "l1": "3. Agropecuária",
        "l2_val": 18,
        "l2": "3.2. Agricultura",
        "l3_val": 19,
        "l3": "3.2.1. Lavoura Temporária"
    },
    20: {
        "l0_val": 2,
        "l0": "0. Antrópico",
        "l1_val": 14,
        "l1": "3. Agropecuária",
        "l2_val": 18,
        "l2": "3.2. Agricultura",
        "l3_val": 19,
        "l3": "3.2.1. Lavoura Temporária"
        #"l4_val": 20,
        #"l4": "3.2.1.2. Cana-de-açúcar"
    },
    21: {
        "l0_val": 2,
        "l0": "0. Antrópico",
        "l1_val": 14,
        "l1": "3. Agropecuária",
        "l2_val": 21,
        "l2": "3.4. Mosaico de Usos",
        "l3_val": 21,
        "l3": "3.4. Mosaico de Usos"
    },
    23: {
        "l0_val": 1,
        "l0": "1. Natural",
        "l1_val": 22,
        "l1": "4. Área não Vegetada",
        "l2_val": 23,
        "l2": "4.1. Praia e Duna",
        "l3_val": 23,
        "l3": "4.1. Praia e Duna"
    },
    24: {
        "l0_val": 2,
        "l0": "0. Antrópico",
        "l1_val": 22,
        "l1": "4. Área não Vegetada",
        "l2_val": 24,
        "l2": "4.2. Infraestrutura Urbana",
        "l3_val": 24,
        "l3": "4.2. Infraestrutura Urbana"
    },
    25: {
        "l0_val": 1,
        "l0": "1. Natural",
        "l1_val": 22,
        "l1": "4. Área não Vegetada",
        "l2_val": 25,
        "l2": "4.4. Outra Área não Vegetada",
        "l3_val": 25,
        "l3": "4.4. Outra Área não Vegetada"
    },
    29: {
        "l0_val": 1,
        "l0": "1. Natural",
        "l1_val": 10,
        "l1": "2. Formação Natural não Florestal",
        "l2_val": 29,
        "l2": "2.4. Afloramento Rochoso",
        "l3_val": 29,
        "l3": "2.4. Afloramento Rochoso"
    },
    30: {
        "l0_val": 2,
        "l0": "0. Antrópico",
        "l1_val": 22,
        "l1": "4. Área não Vegetada",
        "l2_val": 30,
        "l2": "4.3. Mineração",
        "l3_val": 30,
        "l3": "4.3. Mineração"
    },
    31: {
        "l0_val": 2,
        "l0": "0. Antrópico",
        "l1_val": 26,
        "l1": "5. Corpo D'água",
        "l2_val": 63,
        "l2": "5.2. Corpos D'Água Artificiais",
        "l3_val": 31,
        "l3": "5.2. Aquicultura"
    },
    32: {
        "l0_val": 1,
        "l0": "1. Natural",
        "l1_val": 10,
        "l1": "2. Formação Natural não Florestal",
        "l2_val": 32,
        "l2": "2.3. Apicum",
        "l3_val": 32,
        "l3": "2.3. Apicum"
    },
    33: {
        "l0_val": 1,
        "l0": "1. Natural",
        "l1_val": 26,
        "l1": "5. Corpo D'água",
        "l2_val": 33,
        "l2": "5.1. Rio, Lago e Oceano",
        "l3_val": 33,
        "l3": "5.1. Rio, Lago e Oceano"
    },
    35: {
        "l0_val": 2,
        "l0": "0. Antrópico",
        "l1_val": 14,
        "l1": "3. Agropecuária",
        "l2_val": 18,
        "l2": "3.2. Agricultura",
        "l3_val": 36,
        "l3": "3.2.2. Lavoura Perene"
        #"l4_val": 35,
        #"l4": "3.2.2.3. Dendê"
    },
    36: {
        "l0_val": 2,
        "l0": "0. Antrópico",
        "l1_val": 14,
        "l1": "3. Agropecuária",
        "l2_val": 18,
        "l2": "3.2. Agricultura",
        "l3_val": 36,
        "l3": "3.2.2. Lavoura Perene"
    },
    39: {
        "l0_val": 2,
        "l0": "0. Antrópico",
        "l1_val": 14,
        "l1": "3. Agropecuária",
        "l2_val": 18,
        "l2": "3.2. Agricultura",
        "l3_val": 19,
        "l3": "3.2.1. Lavoura Temporária",
        #"l4_val": 39,
        #"l4": "3.2.1.1. Soja"
    },
    40: {
        "l0_val": 2,
        "l0": "0. Antrópico",
        "l1_val": 14,
        "l1": "3. Agropecuária",
        "l2_val": 18,
        "l2": "3.2. Agricultura",
        "l3_val": 19,
        "l3": "3.2.1. Lavoura Temporária",
        #"l4_val": 40,
        #"l4": "3.2.1.3. Arroz"
    },
    41: {
        "l0_val": 2,
        "l0": "0. Antrópico",
        "l1_val": 14,
        "l1": "3. Agropecuária",
        "l2_val": 18,
        "l2": "3.2. Agricultura",
        "l3_val": 19,
        "l3": "3.2.1. Lavoura Temporária",
        #"l4_val": 41,
        #"l4": "3.2.1.5. Outras Lavouras Temporárias"
    },
    46: {
        "l0_val": 2,
        "l0": "0. Antrópico",
        "l1_val": 14,
        "l1": "3. Agropecuária",
        "l2_val": 18,
        "l2": "3.2. Agricultura",
        "l3_val": 36,
        "l3": "3.2.2. Lavoura Perene"
        #"l4_val": 46,
        #"l4": "3.2.2.1. Café"
    },
    47: {
        "l0_val": 2,
        "l0": "0. Antrópico",
        "l1_val": 14,
        "l1": "3. Agropecuária",
        "l2_val": 18,
        "l2": "3.2. Agricultura",
        "l3_val": 36,
        "l3": "3.2.2. Lavoura Perene"
        #"l4_val": 47,
        #"l4": "3.2.2.2. Citrus"
    },
    48: {
        "l0_val": 2,
        "l0": "0. Antrópico",
        "l1_val": 14,
        "l1": "3. Agropecuária",
        "l2_val": 18,
        "l2": "3.2. Agricultura",
        "l3_val": 36,
        "l3": "3.2.2. Lavoura Perene"
        #"l4_val": 48,
        #"l4": " 3.2.2.4. Outras Lavoura Perene"
    },
    49: {
        "l0_val": 1,
        "l0": "1. Natural",
        "l1_val": 1,
        "l1": "1. Floresta",
        "l2_val": 2,
        "l2": "1.4. Restinga Florestal",
        "l3_val": 49,
        "l3": "1.4. Restinga Florestal"
    },
    50: {
        "l0_val": 1,
        "l0": "1. Natural",
        "l1_val": 10,
        "l1": "2. Formação Natural não Florestal",
        "l2_val": 50,
        "l2": "2.5. Restinga Herbácea/Arbustiva",
        "l3_val": 50,
        "l3": "2.5. Restinga Herbácea/Arbustiva"
    },
    62: {
        "l0_val": 2,
        "l0": "0. Antrópico",
        "l1_val": 14,
        "l1": "3. Agropecuária",
        "l2_val": 18,
        "l2": "3.2. Agricultura",
        "l3_val": 19,
        "l3": "3.2.1. Lavoura Temporária",
        #"l4_val": 62,
        #"l4": "3.2.1.4. Algodão (beta)"
    },
    63: {
        "l0_val": 2,
        "l0": "0. Antrópico",
        "l1_val": 26,
        "l1": "5. Corpo D'água",
        "l2_val": 63,
        "l2": "5.2. Corpos D'Água Artificiais",
        "l3_val": 63,
        "l3": "5.2. Corpos D'Água Artificiais"
    },
    # 75: {
    #     "l0_val": 2,
    #     "l0": "0. Antrópico",
    #     "l1_val": 22,
    #     "l1": "4. Área não Vegetada",
    #     "l2_val": 25,
    #     "l2": "4.4. Outra Área não Vegetada",
    #     "l3_val": 25,
    #     "l3": "4.4. Outra Área não Vegetada"
    # },
}

# --- 2. DATA PROCESSING AND HELPER FUNCTIONS ---

def get_classes(df, level='l3'):
    """
    Remaps class IDs to a specific hierarchical level based on the ALL_CLASSES legend.
    Filters the DataFrame to include only classes present in both reference and map data.
    """
    class_values = {}
    class_names = {}

    # Find the unique class IDs present in the classification and reference columns.
    clas_classes = pd.Index(df['classification'].unique())
    ref_classes = pd.Index(df['reference'].unique())

    # Get the intersection of classes to ensure we only work with classes present in both.
    acc_classes = clas_classes.intersection(ref_classes)

    val_remap = {}

    # Iterate through the master legend.
    for value in ALL_CLASSES.keys():
        # Check if the class should be processed (not ignored and present in the data).
        if (value not in IGNORED_CLASSES and (value in acc_classes)):
            val_key = "%s_val" % (level)
            new_val = ALL_CLASSES[value][val_key]
            class_name = ALL_CLASSES[value][level]

            # Create mappings from original value to new hierarchical value.
            val_remap[value] = new_val
            class_values[new_val] = True
            class_names[class_name] = True

    # Filter the DataFrame to only include the relevant classes.
    df = df[df['classification'].isin(val_remap.keys())]
    df = df[df['reference'].isin(val_remap.keys())]

    # Apply the remapping to the classification and reference columns.
    df['classification'] = df['classification'].map(val_remap)
    df['reference'] = df['reference'].map(val_remap)

    class_values = list(class_values.keys())
    class_names = list(class_names.keys())

    return df, class_values, class_names

def read_csvs():
    """
    Reads all raw CSV files from the input directory, concatenates them,
    and merges them with strata information.
    """
    df_array = []
    points_strata = pd.read_csv(POINTS_STRATA_FILE)

    # Find and read all CSVs in the input directory.
    for csv in glob.glob(input_dir + "/*.csv"):
        df_csv = pd.read_csv(csv)
        print("Reading " + csv, df_csv.shape)
        df_array.append(df_csv)

    # Combine all individual DataFrames into one.
    df = pd.concat(df_array)
    # Merge with point strata data using the point's unique ID.
    df = pd.merge(df, points_strata, how='inner', on='TARGETID')

    # Perform further weight calculations.
    df = calculate_prob(df)

    return df

def classification_report_shinny(df, level, class_names, class_values, region, year, area_out=None):
    """
    The core function for calculating and formatting all accuracy metrics.
    Generates a weighted confusion matrix and derives all statistics from it.
    """
    # Sort classes for consistent ordering, especially for level 1.
    if level == 'l1':
        class_names = sorted(class_names)
        class_values = sorted(class_values)

    # Extract reference (true) and classification (predicted) values.
    y_true = df[['reference']].to_numpy().flatten()
    y_pred = df[['classification']].to_numpy().flatten()

    # Calculate sample weights. The weight is the inverse of the probability (`PESO_VOT`).
    sample_weight = 1 / df[['PESO_VOT']].to_numpy().flatten()
    
    # --- CORE CALCULATION: WEIGHTED CONFUSION MATRIX ---
    # This is the key step that incorporates the stratified sampling design.
    matrix = confusion_matrix(y_true, y_pred, sample_weight=sample_weight)
    
    matrix = matrix.transpose() # Transpose to have map classes in rows and ref classes in columns.
    estimated_pop = sample_weight.sum()

    # Normalize the matrix to get proportions of total area.
    matrix = (matrix / estimated_pop)

    # --- Calculate various accuracy metrics using helper functions ---
    glob_acc, glob_se = global_acc(df)
    user_acc, prod_acc, user_se, prod_se = user_prod_acc(df, class_values)
    refarea_prop, refarea_se = refarea_pop(df, class_values)
    map_bias, map_bias_se = calc_map_bias(df, class_values)

    # Calculate totals for rows (map classes) and columns (reference classes).
    total_col = matrix.sum(axis=0) # Area proportion per reference class
    total_row = matrix.sum(axis=1) # Area proportion per map class

    # Calculate confusion matrices for user's and producer's accuracy.
    conf_user = (matrix / total_row[:, np.newaxis])
    conf_prod = (matrix / np.array(refarea_prop)[np.newaxis, :])

    # Calculate total accuracies and disagreements.
    user_acc_tot = np.sum(user_acc * total_row)
    prod_acc_tot = np.sum(prod_acc * total_col)
    quantity_dis = np.absolute(total_row - total_col)
    allocation_dis = 2 * np.minimum((total_row - np.diagonal(matrix)), (total_col - np.diagonal(matrix)))
    quantity_dis_tot = np.sum(quantity_dis) / 2
    allocation_dis_tot = np.sum(allocation_dis) / 2

    # Get the number of samples used for each reference class.
    ref_class = np.unique(df[['reference']].to_numpy().flatten(), return_counts=True)
    ref_classes = list(ref_class[0])
    samples_used = list(ref_class[1])
    
    # Ensure all classes have a sample count, even if it's zero.
    if len(class_values) != len(samples_used):
        for i, value in enumerate(class_values):
            if value not in ref_classes:
                ref_classes.insert(i, value)
                samples_used.insert(i, 0)
            
    # --- Assemble the final output DataFrame ---
    acc_output = []
    # Loop through the confusion matrix to create a row for each cell.
    for i in range(0, len(class_values)):
        for j in range(0, len(class_values)):
            acc_output.append([initiative, collection, region, year, version, level,
                               class_values[i], class_values[j], matrix[i, j], conf_user[i, j], conf_prod[i, j], samples_used[i],
                               refarea_prop[i], refarea_se[i], prod_acc[i], prod_se[i], 1 - prod_acc[i], allocation_dis[i],
                               total_row[j], map_bias[j], map_bias_se[j], user_acc[j], user_se[j], 1 - user_acc[j], quantity_dis[j], 0])
            
    # Append a final "Total" row with summary statistics.
    acc_output.append([initiative, collection, region, year, version, level,
                       'Total', 'Total', 0, np.sum(conf_user), np.sum(conf_prod), np.sum(samples_used),
                       np.sum(refarea_prop), np.sum(refarea_se), prod_acc_tot, prod_se_tot, 1 - prod_acc_tot, allocation_dis_tot,
                       np.sum(total_row), 0, sum(map_bias_se), user_acc_tot, user_se_tot, 1 - user_acc_tot, quantity_dis_tot, glob_acc])
    
    columns_pd = ['iniciative', 'collection', 'territory', 'year', 'version', 'level', 'id_class_ref',
                  'id_class_map', 'value', 'confusion_user', 'confusion_prod', 'Samples_Used', 'Adj_population', 'Adj_population_se', 'Producer_Acc',
                  'Producer_stdErr', 'Omission_Error', 'Allocation_Tot', 'Pop_Prop', 'Pop_Bias',
                  'Pop_Bias_SE', 'User_Acc', 'User_stdErr', 'Comission_Error', 'Quantity_Tot', 'GlobalAccuracy']

    acc_output = pd.DataFrame(acc_output, columns=columns_pd)

    # Concatenate the new results with any previous results.
    area_out = pd.concat([area_out, acc_output], ignore_index=True)

    return area_out

def calculate_prob(df):
    """
    Adjusts sample weights based on biome-specific sampling intensity and
    filters out training samples.
    """
    strata = pd.read_csv(STRATA_FILE)
    df = pd.merge(df, strata, how='inner', on="strata_id")

    # Calculate number of samples per stratum.
    samples = df['strata_id'].value_counts().rename_axis('strata_id').reset_index(name='n_samp')
    df = pd.merge(samples, df, on='strata_id')
    
    # Adjust weights based on sampling within each biome.
    biomes = df['BioNB'].unique()
    for biome in biomes:
        biome_filter = (df['BioNB'] == biome)
        # Count non-training samples vs total samples in the biome.
        n_samples, _ = df[biome_filter & (df['AMOSTRAS'] != 'treinamento')].shape
        t_samples, _ = df[biome_filter].shape
        
        # Calculate adjustment factor and apply it to the weights.
        prob_biome_adjs = n_samples / t_samples
        df.loc[biome_filter, 'PESO_VOT'] = df[biome_filter]['PESO_VOT'] * prob_biome_adjs

    # Return only the validation samples (not training).
    return df[df['AMOSTRAS'] != 'treinamento']

def mkdirp(path):
    """Creates a directory if it doesn't already exist."""
    try:
        makedirs(path)
    except:
        pass

def accuracy_assessment_all(df, biome='BRASIL', area_out=None):
    """
    Orchestrates the accuracy assessment for a given region across all
    hierarchical levels and years.
    """
    # Loop through each classification level, from most specific to most general.
    for level in ['l3', 'l2', 'l1', 'l0']:
        years = df['year'].unique()
        years.sort()

        # Run assessment for each individual year.
        for year in years:
            area_out = accuracy_assessment(df, level, year, biome, area_out)
        
        # Run an overall assessment for all years combined.
        area_out = accuracy_assessment(df, level, 'Todos', biome, area_out)
        
        print(f"The accuracy from {biome} {level} has been added to the database.")

    return area_out

def accuracy_assessment(df, level='l3', year='Todos', biome='BRASIL', area_out=None):
    """
    Filters the data for a specific year and region, then triggers the
    main classification report function.
    """
    df = df.copy(deep=True)
    
    # Filter by year if not 'Todos'.
    if year != 'Todos':
        df = df[df['year'] == year]

    # Filter by biome if not 'BRASIL'.
    if biome != 'BRASIL':
        df = df[df['BioNB'] == biome]
    
    # Remap classes to the desired level.
    df, class_values, class_names = get_classes(df, level)

    # Call the main reporting function.
    return classification_report_shinny(df, level, class_names, class_values, biome, year, area_out)

# --- STATISTICAL HELPER FUNCTIONS ---
# These functions implement formulas for accuracy assessment with stratified sampling.

def population_estimation(df):
    """Estimates the total population (area) from sample weights."""
    sample_weight = 1 / df[['PESO_VOT']].to_numpy().flatten()
    return sample_weight.sum()

def covariance(x, y):
    """Calculates covariance between two arrays."""
    if x.size < 1:
        x_mean = np.mean(x)
        y_mean = np.mean(y)

        return np.sum((x - x_mean) * (y - y_mean) / (x.size - 1))
    else:
        return 0.0

def user_prod_se(df, class_val, user_acc, prod_acc, map_total, ref_total):
    """
    Calculates the Standard Error (SE) for User's and Producer's Accuracy
    using formulas that account for the stratified sampling design.
    """
    user_var = 0
    prod_var = 0

    user_se = 0
    prod_se = 0

    for name, df_strata in df.groupby('strata_id'):
        ref_val_s = df_strata['reference'].to_numpy()
        map_val_s = df_strata['classification'].to_numpy()

        map_total_s = np.where((map_val_s == class_val), 1, 0)
        map_correct_s = np.where(np.logical_and((map_val_s == class_val),(map_val_s == ref_val_s)), 1, 0)

        ref_total_s = np.where((ref_val_s == class_val), 1, 0)
        ref_correct_s = np.where(np.logical_and((ref_val_s == class_val),(map_val_s == ref_val_s)), 1, 0)
        
        nsamples_s, _ = df_strata.shape
        population_s = population_estimation(df_strata)

        user_var += math.pow(population_s,2) * (1 - nsamples_s/population_s) \
                                    * ( math.pow(   np.var(map_correct_s) , 2) \
                                            + math.pow(user_acc,2) * math.pow( np.var(map_total_s) , 2) \
                                            - 2 * user_acc * covariance(map_total_s, map_correct_s) \
                                        ) / nsamples_s

        prod_var += math.pow(population_s,2) * (1 - nsamples_s/population_s) \
                                    * ( math.pow(   np.var(ref_correct_s) , 2) \
                                            + math.pow(prod_acc,2) * math.pow( np.var(ref_total_s) , 2) \
                                            - 2 * prod_acc * covariance(ref_total_s, ref_correct_s) \
                                        ) / nsamples_s

    if (map_total !=0):
        user_var = 1 / math.pow(map_total,2) * user_var
        user_se = 1.96 * math.sqrt(user_var)

    if (ref_total !=0):
        prod_var = 1 / math.pow(ref_total,2) * prod_var
        prod_se = 1.96 * math.sqrt(prod_var)

    return user_se, prod_se
    
def global_se(df, mask, population):
    """Calculates the Standard Error (SE) for the Global Accuracy."""
    glob_var = 0

    for name, df_strata in df.groupby('strata_id'):
        ref_val_s = df['reference'].to_numpy()
        map_val_s = df['classification'].to_numpy()

        map_correct_s = np.where(mask, 1, 0)

        nsamples_s, _ = df_strata.shape
        population_s = population_estimation(df_strata)
        
        glob_var += math.pow(population_s,2) * (1 - nsamples_s/population_s) \
                                * np.var(map_correct_s) / nsamples_s

    glob_var = (1 / math.pow(population,2)) * glob_var
    glob_se = 1.96 * math.sqrt(glob_var)

    return glob_se


def calc_map_bias(df, class_values):
    """Calculates the bias in the map's area estimates for each class."""
    map_bias_arr = []
    map_bias_se_arr = []

    ref_val = df['reference'].to_numpy()
    map_val = df['classification'].to_numpy()
    samp_weight = 1 / df['PESO_VOT'].to_numpy()

    population = population_estimation(df)

    for class_val in class_values:
    
        map_mask = np.logical_and((map_val == class_val), (ref_val != class_val))
        map_comission_prop = np.sum(np.where(map_mask, 1, 0) * samp_weight) / population

        ref_mask = np.logical_and((ref_val == class_val), (map_val != class_val))
        map_omission_prop = np.sum(np.where(ref_mask, 1, 0) * samp_weight) / population

        map_bias = (map_omission_prop - map_comission_prop)
        
        se_mask = np.logical_xor(ref_mask,map_mask)
        map_bias_se = global_se(df, se_mask, population)

        map_bias_arr.append(map_bias)
        map_bias_se_arr.append(map_bias_se)

    return map_bias_arr, map_bias_se_arr

def refarea_pop(df, class_values):
    """Estimates the area proportion for each reference class and its SE."""
    refarea_prop_arr = []
    refarea_se_arr = []

    ref_val = df['reference'].to_numpy()
    map_val = df['classification'].to_numpy()
    samp_weight = 1 / df['PESO_VOT'].to_numpy()

    population = population_estimation(df)

    for class_val in class_values:
    
        ref_mask = (ref_val == class_val)
        refarea = np.sum(np.where(ref_mask, 1, 0) * samp_weight)

        refarea_prop = (refarea / population)
        refarea_se = global_se(df, ref_mask, population)

        refarea_prop_arr.append(refarea_prop)
        refarea_se_arr.append(refarea_se)

    return refarea_prop_arr, refarea_se_arr

def global_acc(df):
    """Calculates the Global Accuracy and its Standard Error (SE)."""
    ref_val = df['reference'].to_numpy()
    map_val = df['classification'].to_numpy()
    samp_weight = 1 / df['PESO_VOT'].to_numpy()
    
    mask_correct = (map_val == ref_val)
    map_correct = np.sum(np.where(mask_correct, 1, 0) * samp_weight)
    population = population_estimation(df)

    glob_acc = (map_correct / population)

    glob_acc = glob_acc
    glob_se = global_se(df, mask_correct, population)

    return glob_acc, glob_se


def user_prod_acc(df, class_values):
    """Calculates User's and Producer's Accuracy and their SE for all classes."""
    user_acc_arr = []
    prod_acc_arr = []
    user_se_arr = []
    prod_se_arr = []

    ref_val = df['reference'].to_numpy()
    map_val = df['classification'].to_numpy()
    samp_weight = 1.0 / df['PESO_VOT'].to_numpy()

    for class_val in class_values:

        map_total = np.sum(np.where((map_val == class_val), 1, 0) * samp_weight)
        map_correct = np.sum(np.where(np.logical_and((map_val == class_val),(map_val == ref_val)), 1, 0) * samp_weight)

        ref_total = np.sum(np.where((ref_val == class_val), 1, 0) * samp_weight)
        ref_correct = np.sum(np.where(np.logical_and((ref_val == class_val),(map_val == ref_val)), 1, 0) * samp_weight)

        user_acc = 0
        if map_total > 0:
            user_acc = map_correct / map_total

        prod_acc = 0
        if ref_total > 0:
            prod_acc = ref_correct / ref_total

        user_se, prod_se = user_prod_se(df, class_val, user_acc, prod_acc, map_total, ref_total)

        user_acc_arr.append(user_acc)
        prod_acc_arr.append(prod_acc)
        user_se_arr.append(user_se)
        prod_se_arr.append(prod_se)

    return user_acc_arr, prod_acc_arr, user_se_arr, prod_se_arr

# --- DATA CLEANING AND REMAPPING FUNCTIONS ---

def mod_BioNB(df):
    """Replaces numeric biome IDs with their string names."""
    df.loc[ (df['BioNB']==1),'BioNB'] = 'Amazônia'
    df.loc[ (df['BioNB']==2),'BioNB'] = 'Mata Atlântica'
    df.loc[ (df['BioNB']==3),'BioNB'] = 'Pantanal'
    df.loc[ (df['BioNB']==4),'BioNB'] = 'Cerrado'
    df.loc[ (df['BioNB']==5),'BioNB'] = 'Caatinga'
    df.loc[ (df['BioNB']==6),'BioNB'] = 'Pampa'
    
    return df

def config_class(df):
    """
    Applies a series of expert-defined rules to harmonize and correct the
    classification and reference data before analysis. This is a critical
    data cleaning and harmonization step.
    """
    # --- Global Rules ---
    #Global

    df.loc[ (df['classification'] == 20) | (df['classification'] == 39) | (df['classification'] == 40) | (df['classification'] == 41) | (df['classification'] == 62), 'classification'] = 19 #Convert areas mapped as 20,39,40,41 to 19
    
    df.loc[ (df['classification'] == 46) | (df['classification'] == 47) | (df['classification'] == 48), 'classification'] = 36 #Convert areas mapped as 46,47,48 to 36

    df.loc[ (df['classification'] == 51) | (df['classification'] == 52) | (df['classification'] == 53), 'classification'] = 24 #Convert areas mapped as 20,39,41 to 24

    df.loc[ (df['classification'] == 49), 'classification'] = 3 #Convert areas mapped as 54,55,56 to 33

    df.loc[ (df['classification'] == 54) | (df['classification'] == 55) | (df['classification'] == 56), 'classification'] = 33 #Convert areas mapped as 54,55,56 to 33
    
    df.loc[ (df['classification'] == 21) & (df['reference'].isin([15,19,20,36])), 'reference'] = 21 #Convert reference from 15,19,20,36 to reference 21 for areas mapped as 21

    df.loc[ (df['classification'] == 63), 'classification'] = 33    


    #Pampa 

    df.loc[ (df['BioNB']== 'Pampa') & (df['reference'] == 15),'reference'] = 19 #In Pampa, convert reference class 15 to 19 (Crop).
    df.loc[ (df['BioNB']=='Pampa') & (df['classification'] == 25) & (df['reference'] == 23), 'reference'] = 25 #In Pampa, convert reference 25 to 23 in areas mapped as 25
    df.loc[ (df['BioNB']== 'Pampa') & (df['classification'] == 12) & (df['reference'] == 13),'reference'] = 12 #In Pampa, convert reference 13 to 12 in areas mapped as 12
    df.loc[ (df['BioNB']== 'Pampa') & (df['classification'] == 13),'classification'] = 12 #In Pampa, convert reference 13 to 12 in areas mapped as 12

    #Mata Atântica

    df.loc[ (df['BioNB']== 'Mata Atlântica') & (df['classification'] == 11) & (df['reference'] == 13),'reference'] = 11

    #Pantanal

    df.loc[ (df['BioNB']== 'Pantanal') & (df['classification'] == 11) & (df['reference'] == 12),'reference'] = 11 #In Pantanal, convert reference 12 to 11 in areas mapped as 11
    df.loc[ (df['BioNB']== 'Pantanal') & (df['classification'] == 11) & (df['reference'] == 33),'reference'] = 11 #In Pantanal, convert reference 33 to 11 in areas mapped as 11
    df.loc[ (df['BioNB']== 'Pantanal') & (df['classification'] == 12) & (df['reference'] == 33),'reference'] = 12 #In Pantanal, convert reference 33 to 12 in areas mapped as 12
    df.loc[ (df['BioNB']== 'Pantanal') & (df['classification'] == 12) & (df['reference'] == 11),'reference'] = 12
    df.loc[ (df['BioNB']== 'Pantanal') & (df['classification'] == 33) & (df['reference'] == 11),'classification'] = 11 #In Pantanal, convert reference 33 to 11 in areas mapped as 11
    df.loc[ (df['BioNB']== 'Pantanal') & (df['classification'] == 33) & (df['reference'] == 12),'classification'] = 12 #In Pantanal, convert reference 33 to 12 in areas mapped as 12

    #Amazônia

    df.loc[ (df['BioNB']=='Amazônia') & (df['classification'] == 12) & (df['reference'] == 13), 'reference'] = 12 #In Amazon, convert reference 13 to 12 in areas mapped as 12
    df.loc[ (df['BioNB']=='Amazônia') & (df['classification'] == 4) & (df['reference'] == 13), 'reference'] = 4 #In Amazon, convert reference 13 to 4 in areas mapped as 4

    #Cerrado

    df.loc[ (df['BioNB']=='Cerrado') & (df['reference'] == 11),'reference'] = 12
    df.loc[ (df['BioNB']=='Cerrado') & (df['classification'] == 11),'classification'] = 12 
    df.loc[ (df['BioNB']=='Cerrado') & (df['classification'] == 12)  & (df['reference'] == 13), 'reference'] = 12 #In Cerrado, convert reference 13 to 12 in areas mapped as 12
    df.loc[ (df['BioNB']=='Cerrado') & (df['classification'] == 25) & (df['reference'] == 23), 'reference'] = 25 #In Cerrado, convert reference 25 to 23 in areas mapped as 25

    return df

# --- 5. MAIN EXECUTION LOGIC ---

# Define the path for a cached, pre-processed file.
output_all_file = path.join(input_dir, 'acc_mapbiomas_all.parquet')

# If the cached file doesn't exist, create it.
if os.path.exists(output_all_file) is False:
    # Perform all initial data loading and pre-processing.
    df = read_csvs()
    df = mod_BioNB(df)
    df = config_class(df)
    # Save the cleaned DataFrame to a parquet file for faster loading next time.
    df.to_parquet(output_all_file)

df = None # Clear from memory

# Load the pre-processed data from the parquet file.
dfPq = pd.read_parquet(output_all_file)

# Define the list of regions to process (all biomes + the whole country).
regions = dfPq['BioNB'].unique().tolist()
regions.append('BRASIL')

pd_cols = {'iniciative':[],'collection':[],'territory':[],'year':[],'version':[],'level':[],'id_class_ref':[],
    'id_class_map':[],'value':[], 'Samples_Used':[],'Adj_population':[],'Adj_population_se':[],'Producer_Acc':[],
    'Producer_stdErr':[],'Omission_Error':[],'Allocation_Tot':[],'Pop_Prop':[],'Pop_Bias':[],
    'Pop_Bias_SE':[],'User_Acc':[],'User_stdErr':[],'Comission_Error':[],'Quantity_Tot':[],'GlobalAccuracy':[]}

# Initialize an empty DataFrame to hold the final results.
areaEstimatives = pd.DataFrame(pd_cols) # Simplified from original

# Define the final output CSV filename from the 4th command-line argument.
output_area_name = path.join(output_dir, ''.join([str(sys.argv[4]) + '.csv']))

# Create the output directory if it doesn't exist.
mkdirp(output_dir)

# --- Main loop to generate accuracy reports for each region ---
for region in regions:
    areaEstimatives = None # Reset for each region
    # Call the main orchestrator function to calculate all metrics for the region.
    areaEstimatives = accuracy_assessment_all(dfPq, region, areaEstimatives)
    
    # Check if the output file already exists to decide whether to write the header.
    hdr_v2 = False if os.path.isfile(output_area_name) else True
    # Append the results for the current region to the master CSV file.
    areaEstimatives.to_csv(output_area_name, mode='a', header=hdr_v2, index=False)
