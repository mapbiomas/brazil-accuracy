// Bases de dados
var states = ee.Image('projects/mapbiomas-workspace/AUXILIAR/bioma25_uf24_30m').mod(100).int().round().rename('uf');
var biomes = ee.Image('projects/mapbiomas-workspace/AUXILIAR/bioma25_uf24_30m').divide(100).int().round().rename('bioma');

// A dictionary to map biome numeric codes to their names. Not used here, but good for context.
var bioDict = {1:'Amazônia', 2:'Caatinga', 3:'Cerrado', 4:'Mata Atlântica', 5:'Pampa', 6:'Pantanal'};

// An array of years to process, from 1985 to 2024.
var anos = ['1985', '1986', '1987','1988', '1989', '1990','1991', '1992', '1993','1994', '1995', '1996',
'1997', '1998', '1999','2000', '2001', '2002','2003', '2004', '2005','2006', '2007', '2008','2009', '2010',
'2011','2012', '2013', '2014','2015', '2016', '2017', '2018','2019','2020','2021','2022','2023','2024'];

// A list of reference class labels to be excluded from the analysis.
var excludedClasses = [
    "NÃO OBSERVADO",
    "ERRO",
    "DESMATAMENTO",
    'REGENERAÇÃO',
    'NÃO CONSOLIDADO',
    'Não consolidado',
];

// A dictionary to map string-based reference class names to their official MapBiomas numeric IDs.
// Note the duplicates for 'FORMAÇÃO FLORESTAL' to handle potential encoding errors.
var classes = ee.Dictionary({
  'AFLORAMENTO ROCHOSO':29,
  "APICUM": 32,
  "AQUICULTURA": 31,
  "CAMPO ALAGADO E ÁREA PANTANOSA": 11,
  "LAVOURA TEMPORÁRIA": 19,
  "LAVOURA PERENE": 36,
  "CANA": 20,
  "FLORESTA PLANTADA": 9,
  "FORMAÇÃO CAMPESTRE": 12,
  "FORMAÇÃO FLORESTAL": 3,
  'FORMAÇ��O FLORESTAL':3,
  'FORMAÇ??O FLORESTAL':3,
  "FORMAÇÃO SAVÂNICA": 4,
  "INFRAESTRUTURA URBANA": 24,
  "MANGUE": 5,
  "MINERAÇÃO": 30,
  "NÃO OBSERVADO": 27,
  "OUTRA FORMAÇÃO NÃO FLORESTAL": 13,
  "OUTRA ÁREA NÃO VEGETADA": 25,
  "PASTAGEM": 15,
  "PRAIA E DUNA": 23,
  'RESTINGA HERBÁCEA':50,
  "RIO, LAGO E OCEANO": 33,
  'VEGETAÇÃO URBANA': 24,
  'FLORESTA INUNDÁVEL':6,
});

// A dictionary mapping short collection names to their full GEE asset paths.
var cols = {
  'c31':'projects/mapbiomas-public/assets/brazil/lulc/collection3_1/mapbiomas_collection31_integration_v1',
  'c4':'projects/mapbiomas-public/assets/brazil/lulc/collection4/mapbiomas_collection40_integration_v1',
  'c41':'projects/mapbiomas-public/assets/brazil/lulc/collection4_1/mapbiomas_collection41_integration_v1',
  'c5':'projects/mapbiomas-public/assets/brazil/lulc/collection5/mapbiomas_collection50_integration_v1',
  'c6':'projects/mapbiomas-public/assets/brazil/lulc/collection6/mapbiomas_collection60_integration_v1',
  'c7':'projects/mapbiomas-public/assets/brazil/lulc/collection7/mapbiomas_collection70_integration_v2',
  'c71':'projects/mapbiomas-public/assets/brazil/lulc/collection7_1/mapbiomas_collection71_integration_v1',
  'c8':'projects/mapbiomas-public/assets/brazil/lulc/collection8/mapbiomas_collection80_integration_v1',
  'c9':'projects/mapbiomas-public/assets/brazil/lulc/collection9/mapbiomas_collection90_integration_v1',
  'c10':'projects/mapbiomas-public/assets/brazil/lulc/collection10/mapbiomas_brazil_collection10_integration_v2',
  'c11':'projects/mapbiomas-public/assets/brazil/lulc/collection11/mapbiomas_brazil_collection11_integration_v1'
};

// Get a list of the collection keys to iterate over.
var col_list = Object.keys(cols);

col_list = ['c11']

// --- START OUTER LOOP: Iterate over each MapBiomas collection ---
col_list.forEach(function(col_id){
  
  // Define the asset path for the reference sample points.
  var assetSamples = 'projects/mapbiomas-workspace/VALIDACAO/mapbiomas_85k_col5_points_w_edge_and_edited_v3';
  // Get the asset path for the current MapBiomas collection.
  var assetMapBiomas = cols[col_id];
  // Define a folder name for exporting results for this collection.
  var folder = 'ACC_'+col_id+'_v5_no_EDGE_github';
  
  // --- START INNER LOOP: Iterate over each year ---
  for (var Year in anos){
    var year = anos[Year];
    var ano = anos[Year]; // Using 'ano' as well for consistency with original code
    
    // Load the reference samples FeatureCollection.
    var samples = ee.FeatureCollection(assetSamples);
    
    // For years after 2022, use the 2022 reference data as a substitute.
    if (year > 2022){
      samples = samples.map(function(feat){
        // If the class for the current year is empty, use the class from 2022.
        var year_class = ee.String(feat.get('CLASS_' + ano));
        var new_class = ee.Algorithms.If(year_class.match(''), feat.get('CLASS_2022'), feat.get('CLASS_' + ano));

        // Set the properties for the future year based on the 2022 data.
        return feat
          .set('CLASS_' + ano, new_class)
          .set('BORDA_' + ano, feat.get('BORDA_2022'))
          .set('COUNT_' + ano, feat.get('COUNT_2022'));
      });
    }
    
    // Get unique keys for the strata: map sheets ('CARTA_2') and slope classes ('DECLIVIDAD').
    var cartas_unique = samples.aggregate_histogram('CARTA_2').keys();
    var declividade_strats = samples.aggregate_histogram('DECLIVIDAD').keys();
    
    // Calculate the total number of samples per stratum (map sheet + slope) BEFORE filtering.
    var carta_stratsize_total = ee.Dictionary(cartas_unique.iterate(function(carta, cartas_remade){
      return ee.Dictionary(cartas_remade).set(carta, samples.filter(ee.Filter.eq('CARTA_2', carta)).aggregate_histogram('DECLIVIDAD'));
    }, ee.Dictionary()));
    
    // Filter the samples to remove invalid classes and edge pixels.
    samples = samples.filter(ee.Filter.inList('CLASS_' + ano, excludedClasses).not())
                     .map(function (feature) {
                         // Add the numeric reference ID based on the class name.
                         return feature.set('year', year)
                                       .set('reference', classes.get(feature.get('CLASS_' + ano)));
                     })
                     .filter(ee.Filter.neq('BORDA_' + ano, 1)); // Remove edge pixels
    
    // Calculate the number of samples per stratum AFTER filtering.
    var carta_stratsize_filtered = ee.Dictionary(cartas_unique.iterate(function(carta, cartas_remade){
      return ee.Dictionary(cartas_remade).set(carta, samples.filter(ee.Filter.eq('CARTA_2', carta)).aggregate_histogram('DECLIVIDAD'));
    }, ee.Dictionary()));
    
    // Map over the filtered samples to calculate adjusted weights.
    samples = samples.map(function(feat){
      feat = ee.Feature(feat);
      
      // Get stratum identifiers for the current point.
      var carta = feat.get('CARTA_2');
      var strat = feat.get('DECLIVIDAD');

      // Calculate the initial sampling probability from the 'PESO_AMOS' property.
      var amos_weitgh = ee.Number.parse(ee.String(feat.get('PESO_AMOS')).replace(',', '.'));
      var amos_prob = ee.Number(1).divide(amos_weitgh);
      
      // Get the number of votes (interpreter agreement) for the point.
      var vote_count = ee.Number.parse(feat.get(ee.String('COUNT_').cat(ano)));
  
      // Get the stratum sizes before and after filtering.
      var strat_total_size = ee.Number.parse(ee.Dictionary(carta_stratsize_total.get(carta)).get(strat));
      var strat_filtered_size = ee.Number(ee.Dictionary(carta_stratsize_filtered.get(carta)).get(strat));
      
      // Calculate the adjusted probability and weight by correcting for the removed samples.
      var new_prob = ee.Number(amos_prob.multiply(strat_filtered_size.divide(strat_total_size)));
      var new_weight = ee.Number(amos_weitgh.multiply(strat_filtered_size.divide(strat_total_size)));
      
      // Calculate a weight based on the number of votes. More agreement = higher weight.
      var vote_weight = ee.Algorithms.If(ee.Number(vote_count).eq(1), 1,
        ee.Algorithms.If(ee.Number(vote_count).eq(2), 0.5,
          ee.Algorithms.If(vote_count.eq(3), ee.Number(1).divide(3), 1)
        )
      );
      
      var value_peso = ee.Number.parse(vote_weight);
      // Combine the sampling probability with the vote weight.
      var peso_voto = ee.Number.parse(amos_prob).multiply(ee.Number.parse(value_peso));
      
      // Return the feature with all the new weight properties added.
      return feat.set({'PROB_AMOS2':amos_prob,'NEW_PROB':new_prob,'NEW_WEIGHT':new_weight,'PESO_VOT':peso_voto, 'VAL_PESO':value_peso, 'COUNT':vote_count});
    });
    
    // Load the MapBiomas classification image for the current collection.
    var classification = ee.Image(assetMapBiomas);
  
    // Select the correct year's band, rename it, and add state and biome bands.
    var mapbiomas = classification.select('classification_'+year).rename('classification')
      .addBands([states.rename('StateNB'), biomes.rename('BioNB')]);
    
    // Extract the map pixel values at each sample point location.
    var result = mapbiomas
        .sampleRegions({
            collection: samples, 
            properties: ['CLASS_' + ano,'reference','year','BIOMA','CARTA_2','DECLIVIDAD','TARGETID', 'LON', 'LAT','PROB_AMOS','PROB_AMOS2','NEW_WEIGHT','AMOSTRAS','AMOSTRA_AM','REINSP','NEW_PROB','PESO_VOT','VAL_PESO','VOTOS'], 
            scale: 30, 
            geometries: false
        });
        
  
    // Export the resulting feature collection to a CSV file in Google Drive.
    Export.table.toDrive({
      collection: result, 
      description: 'acc_mapbiomas_' + year, 
      folder: folder,
      fileFormat: 'csv'
    });
    
  } // --- END INNER (YEAR) LOOP ---

}); // --- END OUTER (COLLECTION) LOOP ---
