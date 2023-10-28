  `%notin%` <- Negate(`%in%`)

getBinaryPhenotype <- function(subsystem, phenotype, genes) {
  ### Vitamins
  # Biotin
  if (subsystem == 'Biotin HMP' && phenotype == 'B7') {
    if ('BioA' %in% genes && 'BioB' %in% genes && 
      'BioD' %in% genes && 'BioF' %in% genes) {
      status <- 1
      rule <- 1
    } else {
      status <- 0
      rule <- 0
    }
    
  # Thiamin
  } else if (subsystem == 'Thiamin HMP' && phenotype == 'B1') {
   if (('ThiC' %in% genes && 'ThiG' %in% genes) ||
       ('ThiC' %in% genes && !('ThiG' %in% genes) && 'Thi4' %in% genes) ||
       (!('ThiC' %in% genes) && 'TMPase' %in% genes && 'ThiS' %in% genes && !('ThiE' %in% genes))
    ){
     status <- 1
   } else {
     status <- 0
   }
    
  # Riboflavin
  } else if (subsystem == 'Riboflavin HMP' && phenotype == 'B2') {
    if ('RibDr' %in% genes && 'RibA' %in% genes){
      status <- 1
    } else {
      status <- 0
    }
    
    # Pyridoxin
  } else if (subsystem == 'Pyridoxin HMP' && phenotype == 'B6') {
    if ('PdxS' %in% genes || 'PdxJ' %in% genes) {
      status <- 1
    } else {
      status <- 0
    }
    
    # Folate
  } else if (subsystem == 'Folate HMP' && phenotype == 'B9') {
    if (('FolP' %in% genes && 'FolK' %in% genes && 'FolB' %in% genes) ||
        ('FolP' %in% genes && 'FolK' %in% genes && !('FolB' %in% genes) && 'FolE1' %in% genes) ||
        ('FolP' %in% genes && 'FolK' %in% genes && !('FolB' %in% genes) && !('FolE1' %in% genes) && 'FolE2' %in% genes) ||
        ('FolP' %in% genes && !('FolK' %in% genes) && 'FolA' %in% genes) ||        
        (!('FolP' %in% genes) && 'FolQ3' %in% genes) ||
        (!('FolP' %in% genes) && !('FolQ3' %in% genes) && 'FolB' %in% genes && 'FolE1' %in% genes)
    ) {
      status <- 1
    } else {
      status <- 0
    }

    # Queuosine
  } else if (subsystem == 'Queuosine HMP' && phenotype == 'Q') {
    if (('QueE' %in% genes && 'QueC' %in% genes) ||
        (!('QueE' %in% genes) && 'QueD' %in% genes && 'QueC' %in% genes)
    ) {
      status <- 1
    } else {
      status <- 0
    }

    # CoA_Pantothenate
  } else if (subsystem == 'CoA Pantothenate HMP' && phenotype == 'B5') {
    if (('PanB' %in% genes && 'PanC' %in% genes) ||
        (!('PanB' %in% genes) && 'PanG' %in% genes && !('IlvC' %in% genes))
    ) {
      status <- 1
    } else {
      status <- 0
    }
    
    # NAD_Niacin
  } else if (subsystem == 'NAD Niacin HMP' && phenotype == 'B3') {

    if( ('NadA' %in% genes) ||
        (!('NadA' %in% genes) && 'HAD' %in% genes) ||
        (!('NadA' %in% genes) && !('HAD' %in% genes) && 'NadB' %in% genes && 'NadC' %in% genes && 'NadD' %in% genes && !('PncB1' %in% genes))
    )  {
      status <- 1
    } else {
      status <- 0
    }
    
    # B12_Cobalamin
  } else if (subsystem == 'B12 Cobalamin HMP' && phenotype == "B12") {
   if(('CbiC' %in% genes && 'CbiF' %in% genes) ||
      ('CbiC' %in% genes && !('CbiF' %in% genes) && !('CbiM' %in% genes)) ||
      (!('CbiC' %in% genes) && 'CbiQ' %in% genes)
    ) {
     status <- 1
    } else {
     status <- 0
    }

    ### Amino acids
    # Proline
  } else if (subsystem == 'Proline HMP' && phenotype == 'Pro') {
    if ('ProA' %in% genes && 'ProB' %in% genes) {
      status <- 1
    } else {
      status <- 0
    }
    
    # Threonine
  } else if (subsystem == 'Threonine HMP' && phenotype == 'Thr') {
    if ('Hom' %in% genes && 'ThrC' %in% genes) {
      status <- 1
    } else {
      status <- 0
    }
    
    # Glycine
  } else if (subsystem == 'Glycine HMP' && phenotype == "Gly") {
    if ('GlyA' %in% genes || 'GlyB' %in% genes || 'SgaA' %in% genes) {
      status <- 1
    } else {
      status <- 0
    }
    
    # Serine
  } else if (subsystem == 'Serine HMP' && phenotype == 'Ser') {
    if ('SerA' %in% genes &&
        ('SerC1' %in% genes ||
         'SerC2' %in% genes || 'SerC3' %in% genes)) {
      status <- 1
    } else {
      status <- 0
    }
    
    # Cysteine
  } else if (subsystem == 'Cysteine HMP' && phenotype == 'Cys') {
    if (('CysK' %in% genes && 'CysE' %in% genes) ||
        ('CysK' %in% genes && !('CysE' %in% genes) && 'CysE2' %in% genes) ||
        (!('CysK' %in% genes) && 'CysKB' %in% genes && 'CysE' %in% genes)
  ) {
      status <- 1
    } else {
      status <- 0
    }
    
    # Tryptophan
  } else if (subsystem == 'Tryptophan HMP' && phenotype == 'Trp') {
   if(('TrpE' %in% genes && 'TrpB' %in% genes) ||
      (!('TrpE' %in% genes) && 'TrpD' %in% genes && 'TrpG' %in% genes))
    {
      status <- 1
    } else {
      status <- 0
    }
    
    # Histidine
  } else if (subsystem == 'Histidine HMP' && phenotype == 'His') {
    if( ('HisB' %in% genes && 'HisF' %in% genes) ||
        (!('HisB' %in% genes) && 'HisG' %in% genes && 'HisE' %in% genes)
    ) {
      status <- 1
    } else {
      status <- 0
    }
    
    # Lysine
  } else if (subsystem == 'Lysine HMP' && phenotype == 'Lys') {
    if(('LysA' %in% genes && 'DapB' %in% genes) ||
       ('LysA' %in% genes && !('DapB' %in% genes) && 'DapD' %in% genes && 'DapA' %in% genes) ||
       ('LysA' %in% genes && !('DapB' %in% genes) && !('DapD' %in% genes) && 'DapA' %in% genes && !('Asd' %in% genes) && !('DapeL' %in% genes)) ||
       (!('LysA' %in% genes) && 'DapH' %in% genes && !('DapeL' %in% genes) && !('Ddh' %in% genes) && 'DapB' %in% genes && !('DapL' %in% genes))       
    ) {
      status <- 1
    } else {
      status <- 0
    }
    
    # Methionine
  } else if (subsystem == 'Methionine HMP' && phenotype == 'Met') {
    if (('MetF' %in% genes && 'MetA' %in% genes && 'Hom' %in% genes) ||
        ('MetF' %in% genes && !('MetA' %in% genes) && 'HSAT' %in% genes && 'MetE' %in% genes) ||
        ('MetF' %in% genes && !('MetA' %in% genes) && 'HSAT' %in% genes && !('MetE' %in% genes) && 'MetH' %in% genes) ||
        ('MetF' %in% genes && !('MetA' %in% genes) && 'HSAT' %in% genes && !('MetE' %in% genes) && !('MetH' %in% genes) && 'MetY' %in% genes) ||
        ('MetF' %in% genes && !('MetA' %in% genes) && !('HSAT' %in% genes) && 'CTGS' %in% genes) ||
        ('MetF' %in% genes && !('MetA' %in% genes) && !('HSAT' %in% genes) && !('CTGS' %in% genes) && 'MetY' %in% genes && 'MetH4' %in% genes) ||
        ('MetF' %in% genes && !('MetA' %in% genes) && !('HSAT' %in% genes) && !('CTGS' %in% genes) && 'MetY' %in% genes && !('MetH4' %in% genes) && 'MetE2' %in% genes) ||
        ('MetF' %in% genes && !('MetA' %in% genes) && !('HSAT' %in% genes) && !('CTGS' %in% genes) && 'MetY' %in% genes && !('MetH4' %in% genes) && !('MetE2' %in% genes) && !('MetT' %in% genes) && 'MetN' %in% genes) ||
        (!('MetF' %in% genes) && 'MetY' %in% genes && 'MetE2' %in% genes && 'Hom' %in% genes) ||
        (!('MetF' %in% genes) && 'MetY' %in% genes && !('MetE2' %in% genes) && 'MetH' %in% genes) ||
        (!('MetF' %in% genes) && 'MetY' %in% genes && !('MetE2' %in% genes) && !('MetH' %in% genes) && 'MetE' %in% genes && 'Hom' %in% genes) ||
        (!('MetF' %in% genes) && 'MetY' %in% genes && !('MetE2' %in% genes) && !('MetH' %in% genes) && !('MetE' %in% genes) && 'MetH5' %in% genes) ||
        (!('MetF' %in% genes) && 'MetY' %in% genes && !('MetE2' %in% genes) && !('MetH' %in% genes) && !('MetE' %in% genes) && !('MetH5' %in% genes) && 'MetEF' %in% genes) ||
        (!('MetF' %in% genes) && 'MetY' %in% genes && !('MetE2' %in% genes) && !('MetH' %in% genes) && !('MetE' %in% genes) && !('MetH5' %in% genes) && !('MetEF' %in% genes) && 'CTGS' %in% genes && 'MetA' %in% genes) ||
        (!('MetF' %in% genes) && 'MetY' %in% genes && !('MetE2' %in% genes) && !('MetH' %in% genes) && !('MetE' %in% genes) && !('MetH5' %in% genes) && !('MetEF' %in% genes) && 'CTGS' %in% genes && !('MetA' %in% genes) && 'BhmT' %in% genes) ||
        (!('MetF' %in% genes) && 'MetY' %in% genes && !('MetE2' %in% genes) && !('MetH' %in% genes) && !('MetE' %in% genes) && !('MetH5' %in% genes) && !('MetEF' %in% genes) && !('CTGS' %in% genes) && 'MetH4' %in% genes) ||
        (!('MetF' %in% genes) && 'MetY' %in% genes && !('MetE2' %in% genes) && !('MetH' %in% genes) && !('MetE' %in% genes) && !('MetH5' %in% genes) && !('MetEF' %in% genes) && !('CTGS' %in% genes) && !('MetH4' %in% genes) && 'MetE3' %in% genes) ||
        (!('MetF' %in% genes) && !('MetY' %in% genes) && 'MetA' %in% genes && 'Hom' %in% genes && !('MetT' %in% genes)) ||
        (!('MetF' %in% genes) && !('MetY' %in% genes) && !('MetA' %in% genes) && 'HSAT' %in% genes && 'MetH' %in% genes) ||
        (!('MetF' %in% genes) && !('MetY' %in% genes) && !('MetA' %in% genes) && 'HSAT' %in% genes && !('MetH' %in% genes) && 'CTBL2' %in% genes)
      ) {
      status <- 1
    } else {
      status <- 0
    }
    
    # Arginine
  } else if (subsystem == 'Arginine HMP' && phenotype == 'Arg') {
    if (('ArgC' %in% genes && 'ArgG' %in% genes && 'ArgB' %in% genes && 'ArgD' %in% genes) ||
        ('ArgC' %in% genes && 'ArgG' %in% genes && 'ArgB' %in% genes && !('ArgD' %in% genes) && !('ArgF3' %in% genes)) ||
        ('ArgC' %in% genes && 'ArgG' %in% genes && !('ArgB' %in% genes) && 'ArgR' %in% genes) ||
        ('ArgC' %in% genes && !('ArgG' %in% genes) && 'ArgH' %in% genes) || 
        (!('ArgC' %in% genes) && 'ArgA2' %in% genes) ||
        (!('ArgC' %in% genes) && !('ArgA2' %in% genes) && 'ArgJ' %in% genes && 'ArgB' %in% genes)
    ) {
      status <- 1
    } else {
      status <- 0
    }
    
    # Chorismate
  } else if (subsystem == 'Chorismate HMP' && phenotype == 'Chor') {
    if(('AroC' %in% genes && 'AroB' %in% genes && 'AroK1' %in% genes) ||
       ('AroC' %in% genes && 'AroB' %in% genes && !('AroK1' %in% genes) && 'AroA' %in% genes && 'AroE2' %in% genes) ||
       ('AroC' %in% genes && 'AroB' %in% genes && !('AroK1' %in% genes) && 'AroA' %in% genes && !('AroE2' %in% genes) && 'AroG1' %in% genes) ||
       ('AroC' %in% genes && !('AroB' %in% genes) && 'AroD1' %in% genes) || 
       ('AroC' %in% genes && !('AroB' %in% genes) && !('AroD1' %in% genes) && 'AroD2' %in% genes) ||
       (!('AroC' %in% genes) && 'AroK1' %in% genes && 'AroG2' %in% genes) ||
       (!('AroC' %in% genes) && 'AroK1' %in% genes && !('AroG2' %in% genes) && 'AroD2' %in% genes) ||
       (!('AroC' %in% genes) && 'AroK1' %in% genes && !('AroG2' %in% genes) && !('AroD2' %in% genes) && 'AroD1' %in% genes)
    ) {
      status <- 1
    } else {
      status <- 0
    }
    
    # Leucine
  } else if (subsystem == 'Isoleucine Valine Leucine HMP' && phenotype == 'Leu') {
    if (('LeuC' %in% genes && 'IlvG' %in% genes && 'IlvD' %in% genes) ||
        ('LeuC' %in% genes && 'IlvG' %in% genes && !('IlvD' %in% genes) && 'IlvA' %in% genes)
    )
        {
      status <- 1
    } else {
      status <- 0
    }
    
    # Isoleucine
  } else if (subsystem == 'Isoleucine Valine Leucine HMP' && phenotype == 'Ile') {
    if (('IlvG' %in% genes && 'IlvC2' %in% genes && 'LeuD' %in% genes) ||
        ('IlvG' %in% genes && 'IlvC2' %in% genes && !('LeuD' %in% genes) && !('IlvA-C' %in% genes)) ||
        ('IlvG' %in% genes && 'IlvC2' %in% genes && !('LeuD' %in% genes) && 'IlvA-C' %in% genes && 'IlvM' %in% genes) ||
        ('IlvG' %in% genes && !('IlvC2' %in% genes) && 'CimA' %in% genes)
    )
    {
      status <- 1
    } else {
      status <- 0
    }
    
    # Valine
  } else if (subsystem == 'Isoleucine Valine Leucine HMP' && phenotype == 'Val') {
    if (('IlvG' %in% genes && 'IlvC2' %in% genes && 'LeuD' %in% genes) ||
        ('IlvG' %in% genes && 'IlvC2' %in% genes && !('LeuD' %in% genes) && !('IlvA-C' %in% genes)) ||
        ('IlvG' %in% genes && 'IlvC2' %in% genes && !('LeuD' %in% genes) && 'IlvA-C' %in% genes && 'IlvM' %in% genes) ||
        ('IlvG' %in% genes && !('IlvC2' %in% genes) && 'CimA' %in% genes) 
    )
    {
      status <- 1
    } else {
      status <- 0
    }
    
    # Tyrosine
  } else if (subsystem == 'Tyrosine Phenylalanine HMP' && phenotype == 'Tyr') {
    if ('TyrAp' %in% genes ||
        'TyrAc' %in% genes ||
        'TyrAx' %in% genes ||
        'TyrC' %in% genes) {
      status <- 1
    } else {
      status <- 0
    }
    
    # Phenylalanine
  } else if (subsystem == 'Tyrosine Phenylalanine HMP' && phenotype == 'Phe') {
    if ('PheAp' %in% genes ||
        'PheAc' %in% genes) {
      status <- 1
    } else {
      status <- 0
    }
    
    # Aspartate
  } else if (subsystem == 'Aspartate Asparagine HMP' && phenotype == 'Asp') {
    if ('AspC' %in% genes ||
        'AspC1' %in% genes) {
      status <- 1
    } else {
      status <- 0
    }
    
    # Asparagine
  } else if (subsystem == 'Aspartate Asparagine HMP' && phenotype == 'Asn') {
    if (('AspC' %in% genes ||
        'AspC1' %in% genes) &&
        ('AsnA' %in% genes ||
        'AsnB1' %in% genes ||
        'AsnB' %in% genes ||
        'AsnH' %in% genes ||
        'YisO' %in% genes ||
        ('GatA' %in% genes &&
        'GatB' %in% genes)) ) {
      status <- 1
    } else {
      status <- 0
    }
    
    # Glutamate
  } else if (subsystem == 'Glutamine Glutamate HMP' && phenotype == 'Glu') {
    if('GltB' %in% genes ||
       'GltD' %in% genes ||
       'GltBa' %in% genes ||
       'GlnA2' %in% genes ||
       'Gdh1' %in% genes ||
       'Gdh2' %in% genes ||
       'Gdh3' %in% genes) {
      status <- 1
    } else {
      status <- 0
    }
    
    # Glutamine
  } else if (subsystem == "Glutamine Glutamate HMP" && phenotype == 'Gln') {
    if(('GltB' %in% genes ||
        'GltD' %in% genes ||
        'GltBa' %in% genes ||
        'GlnA2' %in% genes ||
        'Gdh1' %in% genes ||
        'Gdh2' %in% genes ||
        'Gdh3' %in% genes) &&
       ('GlnA' %in% genes ||
        'GlnA3' %in% genes ||
        'GlnA4' %in% genes ||
        'GlnN' %in% genes ||
        'GlnA6' %in% genes ||
       ('GatA' %in% genes &&
        'GatB' %in% genes))) {
      status <- 1
    } else {
      status <- 0
    }
    
  ### Sugars  
  } else if (subsystem == "Allose utilization HMP" && phenotype == 'All') {
     if(('AlsE' %in% genes && 'RpiA' %in% genes && !('AlsR3' %in% genes) && 'AlsGc' %in% genes) ||
        ('AlsE' %in% genes && 'RpiA' %in% genes && !('AlsR3' %in% genes) && !('AlsGc' %in% genes) && 'AlsK' %in% genes) ||
        ('AlsE' %in% genes && 'RpiA' %in% genes && !('AlsR3' %in% genes) && !('AlsGc' %in% genes) && !('AlsK' %in% genes) && 'RpiB' %in% genes) ||
        ('AlsE' %in% genes && 'RpiA' %in% genes && !('AlsR3' %in% genes) && !('AlsGc' %in% genes) && !('AlsK' %in% genes) && !('RpiB' %in% genes) && 'AlsGa' %in% genes) ||
        ('AlsE' %in% genes && 'RpiA' %in% genes && 'AlsR3' %in% genes && 'AlsGa' %in% genes) ||
        ('AlsE' %in% genes && 'RpiA' %in% genes && 'AlsR3' %in% genes && !('AlsGa' %in% genes) && 'AlsK' %in% genes) ||
        ('AlsE' %in% genes && !('RpiA' %in% genes) && 'AlsA' %in% genes) ||
        ('AlsE' %in% genes && !('RpiA' %in% genes) && !('AlsA' %in% genes) && 'AlsGa' %in% genes) ||
        (!('AlsE' %in% genes) && 'AlsGb' %in% genes) ||
        (!('AlsE' %in% genes) && !('AlsGb' %in% genes) && 'AlsR3' %in% genes)
     )
      {
        status <- 1
      } else {
        status <- 0
      }
  } else if (subsystem == "Fructose-asparagine utilization HMP" && phenotype == 'FruAsp') {
    if('FraA' %in% genes && 'FraD' %in% genes){
      status <- 1
    } else {
      status <- 0
    }
  } else if (subsystem == "Ethanolamine utilization HMP" && phenotype == 'Eut_ut') {
    if('EutB' %in% genes || 'EutC'  %in% genes){
      status <- 1
    } else {
      status <- 0
    }
  } else if (subsystem == "Fructose utilization HMP" && phenotype == 'Fru') {
    if( ('FruAb' %in% genes) || 
        (!('FruAb' %in% genes) && 'FruP' %in% genes) ||
        (!('FruAb' %in% genes) && !('FruP' %in% genes) && 'FruR' %in% genes && 'FruE_bif' %in% genes) ||
        (!('FruAb' %in% genes) && !('FruP' %in% genes) && 'FruR' %in% genes && !('FruE_bif' %in% genes) && 'FruA_bif' %in% genes) ||
        (!('FruAb' %in% genes) && !('FruP' %in% genes) && !('FruR' %in% genes) && 'FruAb1' %in% genes) ||
        (!('FruAb' %in% genes) && !('FruP' %in% genes) && !('FruR' %in% genes) && !('FruAb1' %in% genes) && 'FruE' %in% genes) ||
        (!('FruAb' %in% genes) && !('FruP' %in% genes) && !('FruR' %in% genes) && !('FruAb1' %in% genes) && !('FruE' %in% genes) && 'FruAa1' %in% genes)
    ) {
      status <- 1
    } else {
      status <- 0
    }
  } else if (subsystem == "Hyaluronate utilization HMP" && phenotype == 'Hyl') {
    if(('HylF' %in% genes) ||
       (!('HylF' %in% genes) && 'HylZ' %in% genes) ||
       (!('HylF' %in% genes) && !('HylZ' %in% genes) && 'HylE' %in% genes)
      ){
      status <- 1
    } else {
      status <- 0
    }
  } else if (subsystem == "Lactate utilization HMP" && phenotype == 'Lac_ut') {
    if('LldF' %in% genes ||
       'LldD' %in% genes ||
       'LctO' %in% genes ||
       'LctE' %in% genes ||
       'DldD' %in% genes ||
       'Dld' %in% genes) {
      status <- 1
    } else {
      status <- 0
    }
  } else if (subsystem == "Lipoate HMP" && phenotype == 'LA') {
    if('LipA1' %in% genes){
      status <- 1
    } else {
      status <- 0
    }
  } else if (subsystem == "Methionine degradation HMP" && phenotype == 'Met_d') {
    if('MGL' %in% genes){
      status <- 1
    } else {
      status <- 0
    }
  } else if (subsystem == "Mevalonate & Isoprenoid Biosynthesis HMP" && phenotype == 'IDX') {
    if(('DXR' %in% genes && 'DXPS' %in% genes && 'CMK' %in% genes) ||
       ('DXR' %in% genes && 'DXPS' %in% genes && !('CMK' %in% genes) && !('HMGCS' %in% genes)) ||
       ('DXR' %in% genes && !('DXPS' %in% genes) && 'CMT' %in% genes) ||
       ('DXR' %in% genes && !('DXPS' %in% genes) && !('CMT' %in% genes) && 'HDR2' %in% genes) ||
       ('DXR' %in% genes && !('DXPS' %in% genes) && !('CMT' %in% genes) && !('HDR2' %in% genes) && 'HDS' %in% genes) ||
       (!('DXR' %in% genes) && 'HDS' %in% genes) ||
       (!('DXR' %in% genes) && !('HDS' %in% genes) && 'HDS2' %in% genes)
      ){
         status <- 1
       } else {
         status <- 0
       }
  } else if (subsystem == "Mevalonate & Isoprenoid Biosynthesis HMP" && phenotype == 'IMV') {
    if('DPMVD' %in% genes){
      status <- 1
    } else {
      status <- 0
    }
  } else if (subsystem == "Proline degradation HMP" && phenotype == 'Pro_d') {
    if('PutA' %in% genes && 'PutB' %in% genes){
      status <- 1
    } else {
      status <- 0
    }
  } else if (subsystem == "Propanediol utilization HMP" && phenotype == 'PD_ut') {
    if(('PduB' %in% genes && 'PduL' %in% genes && 'PduP' %in% genes) ||
       ('PduB' %in% genes && 'PduL' %in% genes && !('PduP' %in% genes) && 'PduC' %in% genes) ||
       ('PduB' %in% genes && !('PduL' %in% genes) && 'PduC2' %in% genes) ||
       ('PduB' %in% genes && !('PduL' %in% genes) && !('PduC2' %in% genes) && 'PduC' %in% genes) ||
       (!('PduB' %in% genes) && 'PduC' %in% genes && !('PduN' %in% genes)) ||
       (!('PduB' %in% genes) && !('PduC' %in% genes) && 'PduC2' %in% genes)
    ){
      status <- 1
    } else {
      status <- 0
    }
  } else if (subsystem == "Threonine degradation HMP" && phenotype == 'Thr_d') {
    if(('Kbl' %in% genes && 'Tdh' %in% genes) ||
       (!('Kbl' %in% genes) && 'TdcC' %in% genes)){
      status <- 1
    } else {
      status <- 0
    }
  } else if (subsystem == "Tryptophan degradation HMP" && phenotype == 'Trp_d') {
    if('TnaA' %in% genes || 'IpdC' %in% genes || 'Trd' %in% genes || 'TMO' %in% genes || 'TDC' %in% genes ||
       ('TDO' %in% genes && 'KYN' %in% genes)){
      status <- 1
    } else {
      status <- 0
    }
  } else if (subsystem == "Urea degradation HMP" && phenotype == 'Urea_d') {
    if(('UreC' %in% genes) || 
       ('UC' %in% genes && 'AH' %in% genes)){
      status <- 1
    } else {
      status <- 0
    }
  } else if (subsystem == "Glucose utilization HMP" && phenotype == 'Glc') {
    if(('GlvB' %in% genes) ||
       (!('GlvB' %in% genes) && 'PtsGa' %in% genes) ||
       (!('GlvB' %in% genes) && !('PtsGa' %in% genes) && 'GlcP_1' %in% genes) ||
       (!('GlvB' %in% genes) && !('PtsGa' %in% genes) && !('GlcP_1' %in% genes) && 'PtsG_bif' %in% genes) ||
       (!('GlvB' %in% genes) && !('PtsGa' %in% genes) && !('GlcP_1' %in% genes) && !('PtsG_bif' %in% genes) && 'GlcU' %in% genes) ||
       (!('GlvB' %in% genes) && !('PtsGa' %in% genes) && !('GlcP_1' %in% genes) && !('PtsG_bif' %in% genes) && !('GlcU' %in% genes) && 'PtsGb' %in% genes) ||
       (!('GlvB' %in% genes) && !('PtsGa' %in% genes) && !('GlcP_1' %in% genes) && !('PtsG_bif' %in% genes) && !('GlcU' %in% genes) && !('PtsGb' %in% genes) && 'SS' %in% genes) ||       
       (!('GlvB' %in% genes) && !('PtsGa' %in% genes) && !('GlcP_1' %in% genes) && !('PtsG_bif' %in% genes) && !('GlcU' %in% genes) && !('PtsGb' %in% genes) && !('SS' %in% genes) && 'GlcP' %in% genes)      
    ){
      status <- 1
    } else {
      status <- 0
    }
  } else if (subsystem == "Galactose & Tagatose utilization HMP" && phenotype == 'Gal') {
    if( ('GalK' %in% genes && 'MglC' %in% genes && !('TagR1' %in% genes)) ||
        ('GalK' %in% genes && 'MglC' %in% genes && 'TagR1' %in% genes && 'Pgm' %in% genes) ||
        ('GalK' %in% genes && !('MglC' %in% genes) && 'GlcT' %in% genes) ||
        ('GalK' %in% genes && !('MglC' %in% genes) && !('GlcT' %in% genes) && 'GatA' %in% genes && 'GatC' %in% genes) ||
        ('GalK' %in% genes && !('MglC' %in% genes) && !('GlcT' %in% genes) && !('GatA' %in% genes) && 'GalP' %in% genes) ||
        ('GalK' %in% genes && !('MglC' %in% genes) && !('GlcT' %in% genes) && !('GatA' %in% genes) && !('GalP' %in% genes) && 'SgaT' %in% genes && 'GalE' %in% genes && 'Pgm' %in% genes) ||
        ('GalK' %in% genes && !('MglC' %in% genes) && !('GlcT' %in% genes) && !('GatA' %in% genes) && !('GalP' %in% genes) && 'SgaT' %in% genes && 'GalE' %in% genes && !('Pgm' %in% genes) && !('GalR2' %in% genes)) ||
        ('GalK' %in% genes && !('MglC' %in% genes) && !('GlcT' %in% genes) && !('GatA' %in% genes) && !('GalP' %in% genes) && 'SgaT' %in% genes && 'GalE' %in% genes && !('Pgm' %in% genes) && 'GalR2' %in% genes && 'MglB' %in% genes) ||
        ('GalK' %in% genes && !('MglC' %in% genes) && !('GlcT' %in% genes) && !('GatA' %in% genes) && !('GalP' %in% genes) && !('SgaT' %in% genes) && 'Gal_IID' %in% genes) ||
        ('GalK' %in% genes && !('MglC' %in% genes) && !('GlcT' %in% genes) && !('GatA' %in% genes) && !('GalP' %in% genes) && !('SgaT' %in% genes) && !('Gal_IID' %in% genes) && 'MglB' %in% genes && !('Pgm' %in% genes) && !('LacA' %in% genes)) ||
        ('GalK' %in% genes && !('MglC' %in% genes) && !('GlcT' %in% genes) && !('GatA' %in% genes) && !('GalP' %in% genes) && !('SgaT' %in% genes) && !('Gal_IID' %in% genes) && !('MglB' %in% genes) && 'LacR1' %in% genes && !('LacC' %in% genes) && !('Pgm' %in% genes)) ||
        (!('GalK' %in% genes) && 'GatA' %in% genes && 'LacR1' %in% genes) ||
        (!('GalK' %in% genes) && !('GatA' %in% genes) && 'MglC' %in% genes && 'LacA' %in% genes) ||
        (!('GalK' %in% genes) && !('GatA' %in% genes) && !('MglC' %in% genes) && 'GalM2' %in% genes)
    ) {
        status <- 1
      } else {
        status <- 0
      }
  } else if (subsystem == "Galactose & Tagatose utilization HMP" && phenotype == 'Tag') {
    if( ('TagABb' %in% genes && 'LacD' %in% genes && 'Pgm' %in% genes) ||
        ('TagABb' %in% genes && 'LacD' %in% genes && !('Pgm' %in% genes) && !('GalK' %in% genes)) ||
        ('TagABb' %in% genes && 'LacD' %in% genes && !('Pgm' %in% genes) && 'GalK' %in% genes && 'LacR1' %in% genes) ||
        ('TagABb' %in% genes && 'LacD' %in% genes && !('Pgm' %in% genes) && 'GalK' %in% genes && !('LacR1' %in% genes) && !('GalM' %in% genes)) ||
        ('TagABb' %in% genes && 'LacD' %in% genes && !('Pgm' %in% genes) && 'GalK' %in% genes && !('LacR1' %in% genes) && 'GalM' %in% genes && 'MglA' %in% genes) ||
        ('TagABb' %in% genes && !('LacD' %in% genes) && 'GalM' %in% genes) ||
        (!('TagABb' %in% genes) && 'TagR1' %in% genes && !('LacC' %in% genes)) ||
        (!('TagABb' %in% genes) && 'TagR1' %in% genes && 'LacC' %in% genes && 'TagABc' %in% genes) |
        (!('TagABb' %in% genes) && !('TagR1' %in% genes) && 'LacA' %in% genes && !('LacC' %in% genes) && 'GalM' %in% genes && !('GatA' %in% genes))        
      ) {
      status <- 1
    } else {
      status <- 0
    }
  } else if (subsystem == "Mannose and Mannosides utilization HMP" && phenotype == 'Man') {
    if( ('ManX1b' %in% genes && 'ManY1c' %in% genes) ||
        ('ManX1b' %in% genes && !('ManY1c' %in% genes) && 'ManB' %in% genes) ||
        ('ManX1b' %in% genes && !('ManY1c' %in% genes) && !('ManB' %in% genes) && 'ManZ1d' %in% genes) ||
        (!('ManX1b' %in% genes) && 'Mna_GH125' %in% genes && 'ManT' %in% genes) ||
        (!('ManX1b' %in% genes) && !('Mna_GH125' %in% genes) && 'ManP2' %in% genes) ||
        (!('ManX1b' %in% genes) && !('Mna_GH125' %in% genes) && !('ManP2' %in% genes) && 'ManP3' %in% genes && !('BmoP' %in% genes) && 'MnnA1' %in% genes) ||
        (!('ManX1b' %in% genes) && !('Mna_GH125' %in% genes) && !('ManP2' %in% genes) && 'ManP3' %in% genes && !('BmoP' %in% genes) && !('MnnA1' %in% genes) && !('ManA1' %in% genes)) ||
        (!('ManX1b' %in% genes) && !('Mna_GH125' %in% genes) && !('ManP2' %in% genes) && !('ManP3' %in% genes) && 'ManP1' %in% genes) ||
        (!('ManX1b' %in% genes) && !('Mna_GH125' %in% genes) && !('ManP2' %in% genes) && !('ManP3' %in% genes) && !('ManP1' %in% genes) && 'ManX1a' %in% genes && !('ManI' %in% genes) && 'ManB' %in% genes && !('Hxk' %in% genes)) ||
        (!('ManX1b' %in% genes) && !('Mna_GH125' %in% genes) && !('ManP2' %in% genes) && !('ManP3' %in% genes) && !('ManP1' %in% genes) && 'ManX1a' %in% genes && !('ManI' %in% genes) && !('ManB' %in% genes) && 'ManY1c' %in% genes) ||
        (!('ManX1b' %in% genes) && !('Mna_GH125' %in% genes) && !('ManP2' %in% genes) && !('ManP3' %in% genes) && !('ManP1' %in% genes) && !('ManX1a' %in% genes) && 'ManTa' %in% genes) ||
        (!('ManX1b' %in% genes) && !('Mna_GH125' %in% genes) && !('ManP2' %in% genes) && !('ManP3' %in% genes) && !('ManP1' %in% genes) && !('ManX1a' %in% genes) && !('ManTa' %in% genes) && 'ManX3b' %in% genes && !('MnnA1' %in% genes)) ||
        (!('ManX1b' %in% genes) && !('Mna_GH125' %in% genes) && !('ManP2' %in% genes) && !('ManP3' %in% genes) && !('ManP1' %in% genes) && !('ManX1a' %in% genes) && !('ManTa' %in% genes) && 'ManX3b' %in% genes && 'MnnA1' %in% genes && 'ManA' %in% genes) ||
        (!('ManX1b' %in% genes) && !('Mna_GH125' %in% genes) && !('ManP2' %in% genes) && !('ManP3' %in% genes) && !('ManP1' %in% genes) && !('ManX1a' %in% genes) && !('ManTa' %in% genes) && !('ManX3b' %in% genes) && 'ManY1c' %in% genes && 'ManA' %in% genes && 'ManA2' %in% genes) ||
        (!('ManX1b' %in% genes) && !('Mna_GH125' %in% genes) && !('ManP2' %in% genes) && !('ManP3' %in% genes) && !('ManP1' %in% genes) && !('ManX1a' %in% genes) && !('ManTa' %in% genes) && !('ManX3b' %in% genes) && 'ManY1c' %in% genes && 'ManA' %in% genes && !('ManA2' %in% genes) && !('Hxk' %in% genes) && 'ManB' %in% genes && 'ManZ1d' %in% genes) ||
        (!('ManX1b' %in% genes) && !('Mna_GH125' %in% genes) && !('ManP2' %in% genes) && !('ManP3' %in% genes) && !('ManP1' %in% genes) && !('ManX1a' %in% genes) && !('ManTa' %in% genes) && !('ManX3b' %in% genes) && !('ManY1c' %in% genes) && 'AgaM' %in% genes && 'ManA' %in% genes) ||
        (!('ManX1b' %in% genes) && !('Mna_GH125' %in% genes) && !('ManP2' %in% genes) && !('ManP3' %in% genes) && !('ManP1' %in% genes) && !('ManX1a' %in% genes) && !('ManTa' %in% genes) && !('ManX3b' %in% genes) && !('ManY1c' %in% genes) && !('AgaM' %in% genes) && 'ManK1' %in% genes && 'ManK2' %in% genes) 
      ){
      status <- 1
    } else {
      status <- 0
    }
  } else if (subsystem == "Mannose and Mannosides utilization HMP" && phenotype == '(Man)n'){
    if( ('MnnA1' %in% genes && 'ManX1b' %in% genes && !('ManI' %in% genes) && !('GmuG' %in% genes) && !('Hxk' %in% genes) && 'ManA' %in% genes && !('ManY3c' %in% genes) && !('ManX3a' %in% genes)) ||
        ('MnnA1' %in% genes && 'ManX1b' %in% genes && !('ManI' %in% genes) && !('GmuG' %in% genes) && !('Hxk' %in% genes) && 'ManA' %in% genes && !('ManY3c' %in% genes) && 'ManX3a' %in% genes && 'ManZ3d' %in% genes) ||
        ('MnnA1' %in% genes && 'ManX1b' %in% genes && !('ManI' %in% genes) && !('GmuG' %in% genes) && !('Hxk' %in% genes) && 'ManA' %in% genes && !('ManY3c' %in% genes) && 'ManX3a' %in% genes && !('ManZ3d' %in% genes) && 'MnnA2' %in% genes) ||
        ('MnnA1' %in% genes && 'ManX1b' %in% genes && !('ManI' %in% genes) && !('GmuG' %in% genes) && !('Hxk' %in% genes) && 'ManA' %in% genes && 'ManY3c' %in% genes && 'MosTa' %in% genes) ||
        ('MnnA1' %in% genes && 'ManX1b' %in% genes && !('ManI' %in% genes) && !('GmuG' %in% genes) && !('Hxk' %in% genes) && 'ManA' %in% genes && 'ManY3c' %in% genes && !('MosTa' %in% genes) && 'ManA1' %in% genes) ||
        ('MnnA1' %in% genes && 'ManX1b' %in% genes && !('ManI' %in% genes) && !('GmuG' %in% genes) && !('Hxk' %in% genes) && 'ManA' %in% genes && 'ManY3c' %in% genes && !('MosTa' %in% genes) && !('ManA1' %in% genes) && 'MnnA2' %in% genes) ||
        ('MnnA1' %in% genes && 'ManX1b' %in% genes && !('ManI' %in% genes) && !('GmuG' %in% genes) && !('Hxk' %in% genes) && !('ManA' %in% genes) && 'Mna_GH125' %in% genes) ||   
        ('MnnA1' %in% genes && 'ManX1b' %in% genes && !('ManI' %in% genes) && 'GmuG' %in% genes && 'BmgP' %in% genes) ||
        ('MnnA1' %in% genes && !('ManX1b' %in% genes) && 'BmgP' %in% genes && 'MnnA2' %in% genes && 'MnbP' %in% genes) ||
        ('MnnA1' %in% genes && !('ManX1b' %in% genes) && 'BmgP' %in% genes && 'MnnA2' %in% genes && !('MnbP' %in% genes) && 'Hxk' %in% genes) ||
        ('MnnA1' %in% genes && !('ManX1b' %in% genes) && 'BmgP' %in% genes && !('MnnA2' %in% genes) && 'MnbE' %in% genes) ||
        ('MnnA1' %in% genes && !('ManX1b' %in% genes) && !('BmgP' %in% genes) && 'BmoP' %in% genes) ||
        ('MnnA1' %in% genes && !('ManX1b' %in% genes) && !('BmgP' %in% genes) && !('BmoP' %in% genes) && 'ManK2' %in% genes && 'ManA1' %in% genes) ||
        ('MnnA1' %in% genes && !('ManX1b' %in% genes) && !('BmgP' %in% genes) && !('BmoP' %in% genes) && 'ManK2' %in% genes && !('ManA1' %in% genes) && !('MnnA2' %in% genes)) ||
        ('MnnA1' %in% genes && !('ManX1b' %in% genes) && !('BmgP' %in% genes) && !('BmoP' %in% genes) && 'ManK2' %in% genes && !('ManA1' %in% genes) && 'MnnA2' %in% genes && 'MnnA5' %in% genes) ||
        ('MnnA1' %in% genes && !('ManX1b' %in% genes) && !('BmgP' %in% genes) && !('BmoP' %in% genes) && 'ManK2' %in% genes && !('ManA1' %in% genes) && 'MnnA2' %in% genes && !('MnnA5' %in% genes) && !('MnnB1' %in% genes) && !('MnnB3' %in% genes)) ||
        ('MnnA1' %in% genes && !('ManX1b' %in% genes) && !('BmgP' %in% genes) && !('BmoP' %in% genes) && !('ManK2' %in% genes) && 'ManP2' %in% genes && !('ManA1' %in% genes)) ||
        ('MnnA1' %in% genes && !('ManX1b' %in% genes) && !('BmgP' %in% genes) && !('BmoP' %in% genes) && !('ManK2' %in% genes) && !('ManP2' %in% genes) && 'ManX1a' %in% genes) ||
        (!('MnnA1' %in% genes) && 'MnbE' %in% genes && 'MnbP' %in% genes) ||
        (!('MnnA1' %in% genes) && 'MnbE' %in% genes && !('MnbP' %in% genes) && 'MnbTa' %in% genes) ||
        (!('MnnA1' %in% genes) && !('MnbE' %in% genes) && 'GmuE' %in% genes) ||
        (!('MnnA1' %in% genes) && !('MnbE' %in% genes) && !('GmuE' %in% genes) && 'MnnB1' %in% genes && 'ManX1a' %in% genes && !('MnnA6' %in% genes) && !('MnnA2' %in% genes)) ||
        (!('MnnA1' %in% genes) && !('MnbE' %in% genes) && !('GmuE' %in% genes) && 'MnnB1' %in% genes && 'ManX1a' %in% genes && !('MnnA6' %in% genes) && 'MnnA2' %in% genes && 'ManI' %in% genes) ||
        (!('MnnA1' %in% genes) && !('MnbE' %in% genes) && !('GmuE' %in% genes) && 'MnnB1' %in% genes && !('ManX1a' %in% genes) && 'ManP2' %in% genes && 'BmgP' %in% genes) ||
        (!('MnnA1' %in% genes) && !('MnbE' %in% genes) && !('GmuE' %in% genes) && 'MnnB1' %in% genes && !('ManX1a' %in% genes) && 'ManP2' %in% genes && !('BmgP' %in% genes) && 'ManK2' %in% genes) ||
        (!('MnnA1' %in% genes) && !('MnbE' %in% genes) && !('GmuE' %in% genes) && 'MnnB1' %in% genes && !('ManX1a' %in% genes) && 'ManP2' %in% genes && !('BmgP' %in% genes) && !('ManK2' %in% genes) && 'MnnA5' %in% genes) ||
        (!('MnnA1' %in% genes) && !('MnbE' %in% genes) && !('GmuE' %in% genes) && 'MnnB1' %in% genes && !('ManX1a' %in% genes) && !('ManP2' %in% genes) && 'ManP3' %in% genes) ||
        (!('MnnA1' %in% genes) && !('MnbE' %in% genes) && !('GmuE' %in% genes) && 'MnnB1' %in% genes && !('ManX1a' %in% genes) && !('ManP2' %in% genes) && !('ManP3' %in% genes) && 'ManP1' %in% genes) ||
        (!('MnnA1' %in% genes) && !('MnbE' %in% genes) && !('GmuE' %in% genes) && 'MnnB1' %in% genes && !('ManX1a' %in% genes) && !('ManP2' %in% genes) && !('ManP3' %in% genes) && !('ManP1' %in% genes) && 'BaMan26A' %in% genes) ||
        (!('MnnA1' %in% genes) && !('MnbE' %in% genes) && !('GmuE' %in% genes) && 'MnnB1' %in% genes && !('ManX1a' %in% genes) && !('ManP2' %in% genes) && !('ManP3' %in% genes) && !('ManP1' %in% genes) && !('BaMan26A' %in% genes) && 'MnbTa' %in% genes) ||
        (!('MnnA1' %in% genes) && !('MnbE' %in% genes) && !('GmuE' %in% genes) && !('MnnB1' %in% genes) && 'MnbA' %in% genes) ||
        (!('MnnA1' %in% genes) && !('MnbE' %in% genes) && !('GmuE' %in% genes) && !('MnnB1' %in% genes) && !('MnbA' %in% genes) && 'MnbTa' %in% genes && !('Hxk' %in% genes)) ||
        (!('MnnA1' %in% genes) && !('MnbE' %in% genes) && !('GmuE' %in% genes) && !('MnnB1' %in% genes) && !('MnbA' %in% genes) && !('MnbTa' %in% genes) && 'ManG' %in% genes) ||
        (!('MnnA1' %in% genes) && !('MnbE' %in% genes) && !('GmuE' %in% genes) && !('MnnB1' %in% genes) && !('MnbA' %in% genes) && !('MnbTa' %in% genes) && !('ManG' %in% genes) && 'ManP1' %in% genes && 'MnnA2' %in% genes) ||
        (!('MnnA1' %in% genes) && !('MnbE' %in% genes) && !('GmuE' %in% genes) && !('MnnB1' %in% genes) && !('MnbA' %in% genes) && !('MnbTa' %in% genes) && !('ManG' %in% genes) && !('ManP1' %in% genes) && 'ManP3' %in% genes) ||
        (!('MnnA1' %in% genes) && !('MnbE' %in% genes) && !('GmuE' %in% genes) && !('MnnB1' %in% genes) && !('MnbA' %in% genes) && !('MnbTa' %in% genes) && !('ManG' %in% genes) && !('ManP1' %in% genes) && !('ManP3' %in% genes) && 'MnbP' %in% genes && 'ManA1' %in% genes) ||
        (!('MnnA1' %in% genes) && !('MnbE' %in% genes) && !('GmuE' %in% genes) && !('MnnB1' %in% genes) && !('MnbA' %in% genes) && !('MnbTa' %in% genes) && !('ManG' %in% genes) && !('ManP1' %in% genes) && !('ManP3' %in% genes) && !('MnbP' %in% genes) && 'ManX2a' %in% genes && 'ManX3a' %in% genes && !('ManB' %in% genes) && !('ManX1a' %in% genes)) ||
        (!('MnnA1' %in% genes) && !('MnbE' %in% genes) && !('GmuE' %in% genes) && !('MnnB1' %in% genes) && !('MnbA' %in% genes) && !('MnbTa' %in% genes) && !('ManG' %in% genes) && !('ManP1' %in% genes) && !('ManP3' %in% genes) && !('MnbP' %in% genes) && 'ManX2a' %in% genes && !('ManX3a' %in% genes) && 'ManR3' %in% genes && 'ManA1' %in% genes) ||
        (!('MnnA1' %in% genes) && !('MnbE' %in% genes) && !('GmuE' %in% genes) && !('MnnB1' %in% genes) && !('MnbA' %in% genes) && !('MnbTa' %in% genes) && !('ManG' %in% genes) && !('ManP1' %in% genes) && !('ManP3' %in% genes) && !('MnbP' %in% genes) && !('ManX2a' %in% genes) && 'MnnA2' %in% genes && 'ManX1b' %in% genes && 'ManB' %in% genes) ||
        (!('MnnA1' %in% genes) && !('MnbE' %in% genes) && !('GmuE' %in% genes) && !('MnnB1' %in% genes) && !('MnbA' %in% genes) && !('MnbTa' %in% genes) && !('ManG' %in% genes) && !('ManP1' %in% genes) && !('ManP3' %in% genes) && !('MnbP' %in% genes) && !('ManX2a' %in% genes) && 'MnnA2' %in% genes && 'ManX1b' %in% genes && !('ManB' %in% genes) && !('ManZ1d' %in% genes))
    ){
      status <- 1
    } else {
      status <- 0
    }
  } else if (subsystem == "Butyrate utilization HMP" && phenotype == 'But_ut'){
    if(('YdiP' %in% genes && 'YdiS' %in% genes) ||
       (!('YdiP' %in% genes) && 'YdiR' %in% genes)
      ) {
      status <- 1
    } else {
      status <- 0
    }
  } else if (subsystem == "HMP propionate synthesis" && phenotype == 'Propionate') {
   if(('PduP' %in% genes && !('MmdC' %in% genes) && 'PduL' %in% genes && 'PduC' %in% genes) ||
      ('PduP' %in% genes && !('MmdC' %in% genes) && 'PduL' %in% genes && !('PduC' %in% genes) && 'PduC2' %in% genes) ||
      ('PduP' %in% genes && !('MmdC' %in% genes) && 'PduL' %in% genes && !('PduC' %in% genes) && !('PduC2' %in% genes) && 'LcdA' %in% genes) ||
      ('PduP' %in% genes && !('MmdC' %in% genes) && !('PduL' %in% genes) && 'MutA' %in% genes) ||
      ('PduP' %in% genes && !('MmdC' %in% genes) && !('PduL' %in% genes) && !('MutA' %in% genes) && 'PduC' %in% genes) ||
      ('PduP' %in% genes && !('MmdC' %in% genes) && !('PduL' %in% genes) && !('MutA' %in% genes) && !('PduC' %in% genes) && 'PduC2' %in% genes) || 
      ('PduP' %in% genes && 'MmdC' %in% genes && 'PduL' %in% genes && 'PduC2' %in% genes) ||
      ('PduP' %in% genes && 'MmdC' %in% genes && 'PduL' %in% genes && !('PduC2' %in% genes) && 'Mce' %in% genes) ||
      ('PduP' %in% genes && 'MmdC' %in% genes && !('PduL' %in% genes) && 'PduC' %in% genes) ||
      (!('PduP' %in% genes) && 'Mce' %in% genes && 'MmdA' %in% genes && 'MutA' %in% genes && !('PduD' %in% genes) && !('PduL' %in% genes)) ||
      (!('PduP' %in% genes) && 'Mce' %in% genes && 'MmdA' %in% genes && 'MutA' %in% genes && !('PduD' %in% genes) && 'PduL' %in% genes && 'MutB' %in% genes) ||
      (!('PduP' %in% genes) && 'Mce' %in% genes && 'MmdA' %in% genes && 'MutA' %in% genes && !('PduD' %in% genes) && 'PduL' %in% genes && !('MutB' %in% genes) && !('MutA1' %in% genes)) ||
      (!('PduP' %in% genes) && 'Mce' %in% genes && 'MmdA' %in% genes && !('MutA' %in% genes) && 'MutA1' %in% genes) ||
      (!('PduP' %in% genes) && 'Mce' %in% genes && !('MmdA' %in% genes) && 'PST' %in% genes) ||
      (!('PduP' %in% genes) && 'Mce' %in% genes && !('MmdA' %in% genes) && !('PST' %in% genes) && 'DhaK' %in% genes) ||
      (!('PduP' %in% genes) && 'Mce' %in% genes && !('MmdA' %in% genes) && !('PST' %in% genes) && !('DhaK' %in% genes) && 'MmdB' %in% genes) ||
      (!('PduP' %in% genes) && 'Mce' %in% genes && !('MmdA' %in% genes) && !('PST' %in% genes) && !('DhaK' %in% genes) && !('MmdB' %in% genes) && 'MmdC' %in% genes) ||
      (!('PduP' %in% genes) && !('Mce' %in% genes) && 'Pct' %in% genes && 'LcdB' %in% genes) ||
      (!('PduP' %in% genes) && !('Mce' %in% genes) && 'Pct' %in% genes && !('LcdB' %in% genes) && 'PrpP' %in% genes) ||
      (!('PduP' %in% genes) && !('Mce' %in% genes) && 'Pct' %in% genes && !('LcdB' %in% genes) && !('PrpP' %in% genes) && 'PduC' %in% genes) ||
      
      (!('PduP' %in% genes) && !('Mce' %in% genes) && !('Pct' %in% genes) && 'PduC' %in% genes && 'PduL' %in% genes) ||
      (!('PduP' %in% genes) && !('Mce' %in% genes) && !('Pct' %in% genes) && !('PduC' %in% genes) && 'LcdA' %in% genes && 'PduL' %in% genes) ||
      (!('PduP' %in% genes) && !('Mce' %in% genes) && !('Pct' %in% genes) && !('PduC' %in% genes) && 'LcdA' %in% genes && !('PduL' %in% genes) && 'DhaK' %in% genes) ||
      (!('PduP' %in% genes) && !('Mce' %in% genes) && !('Pct' %in% genes) && !('PduC' %in% genes) && !('LcdA' %in% genes) && 'MutB' %in% genes && 'PduL' %in% genes) ||
      (!('PduP' %in% genes) && !('Mce' %in% genes) && !('Pct' %in% genes) && !('PduC' %in% genes) && !('LcdA' %in% genes) && !('MutB' %in% genes) && 'DhaK' %in% genes && 'MmdC' %in% genes && !('FucO' %in% genes)) 
      
   ) {
     status <- 1
   } else {
     status <- 0
   }
      
  } else if (subsystem == "Fucose utilization HMP" && phenotype == 'Fuc') {
    if(('FucI' %in% genes && !('Sp4GH98' %in% genes) && 'FucK' %in% genes && 'FucP' %in% genes) ||
       ('FucI' %in% genes && !('Sp4GH98' %in% genes) && 'FucK' %in% genes && !('FucP' %in% genes) && 'FucY' %in% genes && 'FucZ' %in% genes) ||
       ('FucI' %in% genes && !('Sp4GH98' %in% genes) && 'FucK' %in% genes && !('FucP' %in% genes) && 'FucY' %in% genes && !('FucZ' %in% genes) && 'FucR2' %in% genes) ||
       ('FucI' %in% genes && !('Sp4GH98' %in% genes) && 'FucK' %in% genes && !('FucP' %in% genes) && 'FucY' %in% genes && !('FucZ' %in% genes) && !('FucR2' %in% genes) && 'FclB' %in% genes) ||
       ('FucI' %in% genes && !('Sp4GH98' %in% genes) && 'FucK' %in% genes && !('FucP' %in% genes) && !('FucY' %in% genes) && 'FucT' %in% genes) ||
       ('FucI' %in% genes && !('Sp4GH98' %in% genes) && 'FucK' %in% genes && !('FucP' %in% genes) && !('FucY' %in% genes) && !('FucT' %in% genes) && 'FsdX' %in% genes && !('FucU' %in% genes) && 'FucR1' %in% genes) ||
       ('FucI' %in% genes && !('Sp4GH98' %in% genes) && 'FucK' %in% genes && !('FucP' %in% genes) && !('FucY' %in% genes) && !('FucT' %in% genes) && !('FsdX' %in% genes) && 'AlfA2' %in% genes) ||
       ('FucI' %in% genes && !('Sp4GH98' %in% genes) && 'FucK' %in% genes && !('FucP' %in% genes) && !('FucY' %in% genes) && !('FucT' %in% genes) && !('FsdX' %in% genes) && !('AlfA2' %in% genes) && 'FucX' %in% genes && 'FucA' %in% genes) ||
       ('FucI' %in% genes && !('Sp4GH98' %in% genes) && 'FucK' %in% genes && !('FucP' %in% genes) && !('FucY' %in% genes) && !('FucT' %in% genes) && !('FsdX' %in% genes) && !('AlfA2' %in% genes) && !('FucX' %in% genes) && 'FucZ' %in% genes && !('AlfA1' %in% genes)) ||
       
       ('FucI' %in% genes && !('Sp4GH98' %in% genes) && !('FucK' %in% genes) && 'FucX' %in% genes) ||
       ('FucI' %in% genes && !('Sp4GH98' %in% genes) && !('FucK' %in% genes) && !('FucX' %in% genes) && 'FucK2' %in% genes) ||
       ('FucI' %in% genes && !('Sp4GH98' %in% genes) && !('FucK' %in% genes) && !('FucX' %in% genes) && !('FucK2' %in% genes) && 'FucA2' %in% genes) ||
       ('FucI' %in% genes && !('Sp4GH98' %in% genes) && !('FucK' %in% genes) && !('FucX' %in% genes) && !('FucK2' %in% genes) && !('FucA2' %in% genes) && 'AlfA1' %in% genes && 'FsdX' %in% genes && !('FucA' %in% genes)) ||
       ('FucI' %in% genes && !('Sp4GH98' %in% genes) && !('FucK' %in% genes) && !('FucX' %in% genes) && !('FucK2' %in% genes) && !('FucA2' %in% genes) && 'AlfA1' %in% genes && 'FsdX' %in% genes && 'FucA' %in% genes && !('AlfA' %in% genes)) ||
       ('FucI' %in% genes && !('Sp4GH98' %in% genes) && !('FucK' %in% genes) && !('FucX' %in% genes) && !('FucK2' %in% genes) && !('FucA2' %in% genes) && 'AlfA1' %in% genes && !('FsdX' %in% genes) && 'FucA' %in% genes && 'FucP' %in% genes) ||        
       
       ('FucI' %in% genes && 'Sp4GH98' %in% genes && !('RhaW' %in% genes) && 'FucO' %in% genes) ||
       ('FucI' %in% genes && 'Sp4GH98' %in% genes && !('RhaW' %in% genes) && !('FucO' %in% genes) && !('FucK' %in% genes)) ||
       
       (!('FucI' %in% genes) && 'FclA2' %in% genes && 'FucP' %in% genes && 'FclB' %in% genes && !('FclA1' %in% genes)) ||
       (!('FucI' %in% genes) && 'FclA2' %in% genes && 'FucP' %in% genes && !('FclB' %in% genes) && 'FucU' %in% genes) ||
       (!('FucI' %in% genes) && 'FclA2' %in% genes && !('FucP' %in% genes) && 'FucZ' %in% genes) ||
       (!('FucI' %in% genes) && !('FclA2' %in% genes) && 'FclB' %in% genes && 'FucP' %in% genes && 'FclD1' %in% genes) ||
       (!('FucI' %in% genes) && !('FclA2' %in% genes) && 'FclB' %in% genes && !('FucP' %in% genes) && 'FclE2' %in% genes) ||
       (!('FucI' %in% genes) && !('FclA2' %in% genes) && !('FclB' %in% genes) && 'FucK' %in% genes && 'FucU' %in% genes)       
       ) {
      status <- 1
    } else {
      status <- 0
    }
  } else if (subsystem == "Fucose utilization HMP" && phenotype == '(Fuc)n') {
   if(('FucI' %in% genes && 'AlfA' %in% genes && 'FucX' %in% genes && !('FclB' %in% genes)) ||
      ('FucI' %in% genes && 'AlfA' %in% genes && 'FucX' %in% genes && 'FclB' %in% genes && 'FclC' %in% genes) ||
      ('FucI' %in% genes && 'AlfA' %in% genes && !('FucX' %in% genes)  && 'FucK' %in% genes && 'FucP' %in% genes) ||
      ('FucI' %in% genes && 'AlfA' %in% genes && !('FucX' %in% genes)  && 'FucK' %in% genes && !('FucP' %in% genes) && 'FsdX' %in% genes) ||
      ('FucI' %in% genes && 'AlfA' %in% genes && !('FucX' %in% genes)  && 'FucK' %in% genes && !('FucP' %in% genes) && !('FsdX' %in% genes) && 'FucT' %in% genes) ||
      ('FucI' %in% genes && 'AlfA' %in% genes && !('FucX' %in% genes)  && 'FucK' %in% genes && !('FucP' %in% genes) && !('FsdX' %in% genes) && !('FucT' %in% genes) && 'FucSa' %in% genes) ||
      ('FucI' %in% genes && 'AlfA' %in% genes && !('FucX' %in% genes)  && !('FucK' %in% genes)  && 'FsdY' %in% genes && !('FucY' %in% genes)) ||
      ('FucI' %in% genes && 'AlfA' %in% genes && !('FucX' %in% genes)  && !('FucK' %in% genes)  && !('FsdY' %in% genes) && 'FucK2' %in% genes) ||
      ('FucI' %in% genes && 'AlfA' %in% genes && !('FucX' %in% genes)  && !('FucK' %in% genes)  && !('FsdY' %in% genes) && !('FucK2' %in% genes) && 'FucA2' %in% genes) ||
      ('FucI' %in% genes && !('AlfA' %in% genes) && 'FucSd' %in% genes) ||
      ('FucI' %in% genes && !('AlfA' %in% genes) && !('FucSd' %in% genes) && 'AlfA1' %in% genes && 'FucA' %in% genes && 'FucP' %in% genes) ||
      ('FucI' %in% genes && !('AlfA' %in% genes) && !('FucSd' %in% genes) && 'AlfA1' %in% genes && 'FucA' %in% genes && !('FucP' %in% genes) && !('FucK' %in% genes)) ||
      ('FucI' %in% genes && !('AlfA' %in% genes) && !('FucSd' %in% genes) && 'AlfA1' %in% genes && !('FucA' %in% genes) && 'FucY' %in% genes) ||
      ('FucI' %in% genes && !('AlfA' %in% genes) && !('FucSd' %in% genes) && !('AlfA1' %in% genes) && 'Sp4GH98' %in% genes) ||
      (!('FucI' %in% genes) && 'FucK' %in% genes && 'FucU' %in% genes) ||    
      (!('FucI' %in% genes) && !('FucK' %in% genes) && 'AlfA_29' %in% genes && 'FucP' %in% genes)    
   ) {
     status <- 1
   } else {
     status <- 0
   }

  } else if (subsystem == "Fucose utilization HMP" && phenotype == 'FL') {
    if('FL2_B' %in% genes) {
      status <- 1
    } else {
      status <- 0
    }
  } else if (subsystem == "N-Acetylglucosamine HMP" && phenotype == 'GlcNAc') {
    if( ('NagEc' %in% genes && 'NagA' %in% genes && !('NagR3' %in% genes)) ||
        ('NagEc' %in% genes && 'NagA' %in% genes && 'NagR3' %in% genes && 'NagEa' %in% genes) ||
        ('NagEc' %in% genes && 'NagA' %in% genes && 'NagR3' %in% genes && !('NagEa' %in% genes) && 'LnbA' %in% genes) ||
        ('NagEc' %in% genes && 'NagA' %in% genes && 'NagR3' %in% genes && !('NagEa' %in% genes) && !('LnbA' %in% genes) && 'NagTp1' %in% genes) ||
        (!('NagEc' %in% genes) && 'NamLa' %in% genes && 'NagA' %in% genes && !('NagTp2' %in% genes) && 'NagR2' %in% genes) ||
        (!('NagEc' %in% genes) && 'NamLa' %in% genes && 'NagA' %in% genes && !('NagTp2' %in% genes) && !('NagR2' %in% genes) && !('Hex_old' %in% genes)) ||
        (!('NagEc' %in% genes) && !('NamLa' %in% genes) && 'NagX' %in% genes && 'NagA2' %in% genes && !('NagR2' %in% genes) && 'NAGLU' %in% genes) ||
        (!('NagEc' %in% genes) && !('NamLa' %in% genes) && 'NagX' %in% genes && 'NagA2' %in% genes && !('NagR2' %in% genes) && !('NAGLU' %in% genes) && 'NagA' %in% genes) ||
        (!('NagEc' %in% genes) && !('NamLa' %in% genes) && 'NagX' %in% genes && 'NagA2' %in% genes && !('NagR2' %in% genes) && !('NAGLU' %in% genes) && !('NagA' %in% genes) && 'ChiA' %in% genes) ||
        (!('NagEc' %in% genes) && !('NamLa' %in% genes) && 'NagX' %in% genes && 'NagA2' %in% genes && !('NagR2' %in% genes) && !('NAGLU' %in% genes) && !('NagA' %in% genes) && !('ChiA' %in% genes) && 'NagP' %in% genes) ||
        (!('NagEc' %in% genes) && !('NamLa' %in% genes) && 'NagX' %in% genes && 'NagA2' %in% genes && !('NagR2' %in% genes) && !('NAGLU' %in% genes) && !('NagA' %in% genes) && !('ChiA' %in% genes) && !('NagP' %in% genes) && !('Hex_old' %in% genes)) ||
        (!('NagEc' %in% genes) && !('NamLa' %in% genes) && 'NagX' %in% genes && 'NagA2' %in% genes && 'NagR2' %in% genes && 'NagA' %in% genes) ||
        (!('NagEc' %in% genes) && !('NamLa' %in% genes) && 'NagX' %in% genes && !('NagA2' %in% genes) && 'NagA' %in% genes && !('NagK2' %in% genes) && 'Hex_old' %in% genes) ||
        (!('NagEc' %in% genes) && !('NamLa' %in% genes) && 'NagX' %in% genes && !('NagA2' %in% genes) && 'NagA' %in% genes && !('NagK2' %in% genes) && !('Hex_old' %in% genes) && 'RokA' %in% genes) ||
        (!('NagEc' %in% genes) && !('NamLa' %in% genes) && 'NagX' %in% genes && !('NagA2' %in% genes) && !('NagA' %in% genes) && 'NagK4' %in% genes) ||        
        (!('NagEc' %in% genes) && !('NamLa' %in% genes) && !('NagX' %in% genes) && 'NagTp2' %in% genes && 'NagA' %in% genes && 'NagK' %in% genes) ||
        (!('NagEc' %in% genes) && !('NamLa' %in% genes) && !('NagX' %in% genes) && 'NagTp2' %in% genes && 'NagA' %in% genes && !('NagK' %in% genes) && 'NagK2' %in% genes) ||
        (!('NagEc' %in% genes) && !('NamLa' %in% genes) && !('NagX' %in% genes) && 'NagTp2' %in% genes && 'NagA' %in% genes && !('NagK' %in% genes) && !('NagK2' %in% genes) && 'LnbB' %in% genes) ||
        (!('NagEc' %in% genes) && !('NamLa' %in% genes) && !('NagX' %in% genes) && 'NagTp2' %in% genes && 'NagA' %in% genes && !('NagK' %in% genes) && !('NagK2' %in% genes) && !('LnbB' %in% genes) && !('NagB2' %in% genes) && 'NagR2' %in% genes && !('Hex_old' %in% genes)) ||
        (!('NagEc' %in% genes) && !('NamLa' %in% genes) && !('NagX' %in% genes) && 'NagTp2' %in% genes && 'NagA' %in% genes && !('NagK' %in% genes) && !('NagK2' %in% genes) && !('LnbB' %in% genes) && !('NagB2' %in% genes) && 'NagR2' %in% genes && 'Hex_old' %in% genes && 'Pgm3' %in% genes) ||
        (!('NagEc' %in% genes) && !('NamLa' %in% genes) && !('NagX' %in% genes) && 'NagTp2' %in% genes && 'NagA' %in% genes && !('NagK' %in% genes) && !('NagK2' %in% genes) && !('LnbB' %in% genes) && !('NagB2' %in% genes) && !('NagR2' %in% genes) && 'Hex_old' %in% genes && 'NagTp1' %in% genes && !('LnpA' %in% genes)) ||
        (!('NagEc' %in% genes) && !('NamLa' %in% genes) && !('NagX' %in% genes) && 'NagTp2' %in% genes && 'NagA' %in% genes && !('NagK' %in% genes) && !('NagK2' %in% genes) && !('LnbB' %in% genes) && !('NagB2' %in% genes) && !('NagR2' %in% genes) && 'Hex_old' %in% genes && !('NagTp1' %in% genes) && 'NagEa' %in% genes) ||
        (!('NagEc' %in% genes) && !('NamLa' %in% genes) && !('NagX' %in% genes) && 'NagTp2' %in% genes && 'NagA' %in% genes && !('NagK' %in% genes) && !('NagK2' %in% genes) && !('LnbB' %in% genes) && !('NagB2' %in% genes) && !('NagR2' %in% genes) && !('Hex_old' %in% genes) && 'LnpA' %in% genes) || 
        (!('NagEc' %in% genes) && !('NamLa' %in% genes) && !('NagX' %in% genes) && !('NagTp2' %in% genes) && 'NagEa' %in% genes && 'NagA' %in% genes && 'GlmM' %in% genes) ||
        (!('NagEc' %in% genes) && !('NamLa' %in% genes) && !('NagX' %in% genes) && !('NagTp2' %in% genes) && !('NagEa' %in% genes) && 'NagP' %in% genes && 'NagB' %in% genes && !('NagKH' %in% genes)) ||
        (!('NagEc' %in% genes) && !('NamLa' %in% genes) && !('NagX' %in% genes) && !('NagTp2' %in% genes) && !('NagEa' %in% genes) && !('NagP' %in% genes) && 'LNB2_A' %in% genes && 'LNB1_A' %in% genes && 'GlmM' %in% genes) ||
        (!('NagEc' %in% genes) && !('NamLa' %in% genes) && !('NagX' %in% genes) && !('NagTp2' %in% genes) && !('NagEa' %in% genes) && !('NagP' %in% genes) && !('LNB2_A' %in% genes) && 'LnbA' %in% genes && 'NagTp1' %in% genes && !('NagR4' %in% genes)) ||        
        (!('NagEc' %in% genes) && !('NamLa' %in% genes) && !('NagX' %in% genes) && !('NagTp2' %in% genes) && !('NagEa' %in% genes) && !('NagP' %in% genes) && !('LNB2_A' %in% genes) && 'LnbA' %in% genes && !('NagTp1' %in% genes) && 'Hex_old' %in% genes && !('LnpC' %in% genes) && !('NagR2' %in% genes) && !('LnpA2' %in% genes)) ||
        (!('NagEc' %in% genes) && !('NamLa' %in% genes) && !('NagX' %in% genes) && !('NagTp2' %in% genes) && !('NagEa' %in% genes) && !('NagP' %in% genes) && !('LNB2_A' %in% genes) && !('LnbA' %in% genes) && 'ChbP2' %in% genes && 'ChbP' %in% genes) ||         
        (!('NagEc' %in% genes) && !('NamLa' %in% genes) && !('NagX' %in% genes) && !('NagTp2' %in% genes) && !('NagEa' %in% genes) && !('NagP' %in% genes) && !('LNB2_A' %in% genes) && !('LnbA' %in% genes) && !('ChbP2' %in% genes) && 'NAGLU' %in% genes && 'ChiA' %in% genes)         
    ) {
      status <- 1
    } else {
      status <- 0
    }
  
  } else if (subsystem == "N-Acetylglucosamine HMP" && phenotype == 'Chb') {
    if( ('ChbC' %in% genes) ||
        (!('ChbC' %in% genes) && 'NagX' %in% genes && !('NagEb' %in% genes) && 'NAGLU' %in% genes && 'Hex_old' %in% genes) ||
        (!('ChbC' %in% genes) && 'NagX' %in% genes && !('NagEb' %in% genes) && 'NAGLU' %in% genes && !('Hex_old' %in% genes) && 'NagA2' %in% genes && 'NagP' %in% genes) ||
        (!('ChbC' %in% genes) && 'NagX' %in% genes && !('NagEb' %in% genes) && 'NAGLU' %in% genes && !('Hex_old' %in% genes) && 'NagA2' %in% genes && !('NagP' %in% genes) && 'ChiA' %in% genes) ||
        (!('ChbC' %in% genes) && 'NagX' %in% genes && !('NagEb' %in% genes) && 'NAGLU' %in% genes && !('Hex_old' %in% genes) && !('NagA2' %in% genes) && 'NagA' %in% genes) ||        
        (!('ChbC' %in% genes) && 'NagX' %in% genes && !('NagEb' %in% genes) && !('NAGLU' %in% genes) && !('NagR2' %in% genes) && 'NagA2' %in% genes && 'RokA' %in% genes && 'Hex_old' %in% genes && 'NagA' %in% genes) ||
        (!('ChbC' %in% genes) && 'NagX' %in% genes && !('NagEb' %in% genes) && !('NAGLU' %in% genes) && !('NagR2' %in% genes) && 'NagA2' %in% genes && 'RokA' %in% genes && 'Hex_old' %in% genes && !('NagA' %in% genes) && 'ChiA' %in% genes) ||
        (!('ChbC' %in% genes) && 'NagX' %in% genes && !('NagEb' %in% genes) && !('NAGLU' %in% genes) && !('NagR2' %in% genes) && 'NagA2' %in% genes && 'RokA' %in% genes && 'Hex_old' %in% genes && !('NagA' %in% genes) && !('ChiA' %in% genes) && 'NagP' %in% genes) ||
        (!('ChbC' %in% genes) && 'NagX' %in% genes && !('NagEb' %in% genes) && !('NAGLU' %in% genes) && !('NagR2' %in% genes) && 'NagA2' %in% genes && 'RokA' %in% genes && !('Hex_old' %in% genes) && !('NagA' %in% genes)) ||
        (!('ChbC' %in% genes) && 'NagX' %in% genes && !('NagEb' %in% genes) && !('NAGLU' %in% genes) && !('NagR2' %in% genes) && 'NagA2' %in% genes && 'RokA' %in% genes && !('Hex_old' %in% genes) && 'NagA' %in% genes && 'NagP' %in% genes) ||
        (!('ChbC' %in% genes) && 'NagX' %in% genes && !('NagEb' %in% genes) && !('NAGLU' %in% genes) && !('NagR2' %in% genes) && !('NagA2' %in% genes) && 'ChiA' %in% genes) ||
        (!('ChbC' %in% genes) && 'NagX' %in% genes && !('NagEb' %in% genes) && !('NAGLU' %in% genes) && !('NagR2' %in% genes) && !('NagA2' %in% genes) && !('ChiA' %in% genes) && 'NagTs' %in% genes) ||
        (!('ChbC' %in% genes) && 'NagX' %in% genes && !('NagEb' %in% genes) && !('NAGLU' %in% genes) && !('NagR2' %in% genes) && !('NagA2' %in% genes) && !('ChiA' %in% genes) && !('NagTs' %in% genes) && 'NagP' %in% genes && 'NagA' %in% genes) ||
        (!('ChbC' %in% genes) && !('NagX' %in% genes) && 'LnbB' %in% genes && 'Hex_old' %in% genes && 'Pgm3' %in% genes && 'LnbA' %in% genes && 'NagEb' %in% genes && 'NagK2' %in% genes) ||
        (!('ChbC' %in% genes) && !('NagX' %in% genes) && 'LnbB' %in% genes && 'Hex_old' %in% genes && 'Pgm3' %in% genes && 'LnbA' %in% genes && 'NagEb' %in% genes && !('NagK2' %in% genes) && 'NagTs' %in% genes) ||
        (!('ChbC' %in% genes) && !('NagX' %in% genes) && 'LnbB' %in% genes && 'Hex_old' %in% genes && 'Pgm3' %in% genes && 'LnbA' %in% genes && 'NagEb' %in% genes && !('NagK2' %in% genes) && !('NagTs' %in% genes) && !('NagTp1' %in% genes)) ||
        (!('ChbC' %in% genes) && !('NagX' %in% genes) && 'LnbB' %in% genes && 'Hex_old' %in% genes && 'Pgm3' %in% genes && 'LnbA' %in% genes && !('NagEb' %in% genes) && 'NagTs' %in% genes) ||         
        (!('ChbC' %in% genes) && !('NagX' %in% genes) && 'LnbB' %in% genes && 'Hex_old' %in% genes && 'Pgm3' %in% genes && !('LnbA' %in% genes) && !('NagK2' %in% genes)) ||        
        (!('ChbC' %in% genes) && !('NagX' %in% genes) && 'LnbB' %in% genes && 'Hex_old' %in% genes && !('Pgm3' %in% genes) && 'LnpD' %in% genes) ||
        (!('ChbC' %in% genes) && !('NagX' %in% genes) && 'LnbB' %in% genes && 'Hex_old' %in% genes && !('Pgm3' %in% genes) && !('LnpD' %in% genes) && !('NAGLU' %in% genes) && !('NagR2' %in% genes) && 'NagEb' %in% genes) ||
        (!('ChbC' %in% genes) && !('NagX' %in% genes) && 'LnbB' %in% genes && 'Hex_old' %in% genes && !('Pgm3' %in% genes) && !('LnpD' %in% genes) && !('NAGLU' %in% genes) && !('NagR2' %in% genes) && !('NagEb' %in% genes) && 'NagK' %in% genes) ||
        (!('ChbC' %in% genes) && !('NagX' %in% genes) && 'LnbB' %in% genes && 'Hex_old' %in% genes && !('Pgm3' %in% genes) && !('LnpD' %in% genes) && !('NAGLU' %in% genes) && !('NagR2' %in% genes) && !('NagEb' %in% genes) && !('NagK' %in% genes) && 'ChbP2' %in% genes) ||
        (!('ChbC' %in% genes) && !('NagX' %in% genes) && 'LnbB' %in% genes && 'Hex_old' %in% genes && !('Pgm3' %in% genes) && !('LnpD' %in% genes) && !('NAGLU' %in% genes) && !('NagR2' %in% genes) && !('NagEb' %in% genes) && !('NagK' %in% genes) && !('ChbP2' %in% genes) && 'NagTp1' %in% genes && 'LnpA' %in% genes) ||
        (!('ChbC' %in% genes) && !('NagX' %in% genes) && 'LnbB' %in% genes && 'Hex_old' %in% genes && !('Pgm3' %in% genes) && !('LnpD' %in% genes) && !('NAGLU' %in% genes) && 'NagR2' %in% genes && !('NagR4' %in% genes) && 'ChiA' %in% genes) ||
        (!('ChbC' %in% genes) && !('NagX' %in% genes) && 'LnbB' %in% genes && 'Hex_old' %in% genes && !('Pgm3' %in% genes) && !('LnpD' %in% genes) && !('NAGLU' %in% genes) && 'NagR2' %in% genes && !('NagR4' %in% genes) && !('ChiA' %in% genes) && !('NagB2' %in% genes) && 'LnpC' %in% genes) ||
        (!('ChbC' %in% genes) && !('NagX' %in% genes) && 'LnbB' %in% genes && 'Hex_old' %in% genes && !('Pgm3' %in% genes) && !('LnpD' %in% genes) && !('NAGLU' %in% genes) && 'NagR2' %in% genes && !('NagR4' %in% genes) && !('ChiA' %in% genes) && !('NagB2' %in% genes) && !('LnpC' %in% genes) && 'NagKH' %in% genes) ||
        (!('ChbC' %in% genes) && !('NagX' %in% genes) && 'LnbB' %in% genes && !('Hex_old' %in% genes) && 'ChiX' %in% genes) ||
        (!('ChbC' %in% genes) && !('NagX' %in% genes) && !('LnbB' %in% genes) && 'NagP' %in% genes && 'Hex_old' %in% genes && !('NagKH' %in% genes)) ||   
        (!('ChbC' %in% genes) && !('NagX' %in% genes) && !('LnbB' %in% genes) && 'NagP' %in% genes && !('Hex_old' %in% genes) && 'NAGLU' %in% genes) ||
        (!('ChbC' %in% genes) && !('NagX' %in% genes) && !('LnbB' %in% genes) && !('NagP' %in% genes) && 'Chi_bifA' %in% genes) ||
        (!('ChbC' %in% genes) && !('NagX' %in% genes) && !('LnbB' %in% genes) && !('NagP' %in% genes) && !('Chi_bifA' %in% genes) && 'Chi_bifX' %in% genes) ||
        (!('ChbC' %in% genes) && !('NagX' %in% genes) && !('LnbB' %in% genes) && !('NagP' %in% genes) && !('Chi_bifA' %in% genes) && !('Chi_bifX' %in% genes) && 'NagTa' %in% genes && 'NagB2' %in% genes && !('NagK' %in% genes)) ||
        (!('ChbC' %in% genes) && !('NagX' %in% genes) && !('LnbB' %in% genes) && !('NagP' %in% genes) && !('Chi_bifA' %in% genes) && !('Chi_bifX' %in% genes) && !('NagTa' %in% genes) && 'LnbC' %in% genes && 'Hex_old' %in% genes) ||        
        (!('ChbC' %in% genes) && !('NagX' %in% genes) && !('LnbB' %in% genes) && !('NagP' %in% genes) && !('Chi_bifA' %in% genes) && !('Chi_bifX' %in% genes) && !('NagTa' %in% genes) && !('LnbC' %in% genes) && 'ChbB' %in% genes && 'ChbA' %in% genes) ||
        (!('ChbC' %in% genes) && !('NagX' %in% genes) && !('LnbB' %in% genes) && !('NagP' %in% genes) && !('Chi_bifA' %in% genes) && !('Chi_bifX' %in% genes) && !('NagTa' %in% genes) && !('LnbC' %in% genes) && 'ChbB' %in% genes && !('ChbA' %in% genes) && 'ChiA' %in% genes) ||
        (!('ChbC' %in% genes) && !('NagX' %in% genes) && !('LnbB' %in% genes) && !('NagP' %in% genes) && !('Chi_bifA' %in% genes) && !('Chi_bifX' %in% genes) && !('NagTa' %in% genes) && !('LnbC' %in% genes) && !('ChbB' %in% genes) && 'LnbA' %in% genes) ||
        (!('ChbC' %in% genes) && !('NagX' %in% genes) && !('LnbB' %in% genes) && !('NagP' %in% genes) && !('Chi_bifA' %in% genes) && !('Chi_bifX' %in% genes) && !('NagTa' %in% genes) && !('LnbC' %in% genes) && !('ChbB' %in% genes) && !('LnbA' %in% genes) && 'ChiG2' %in% genes && 'Pgm3' %in% genes) ||
        (!('ChbC' %in% genes) && !('NagX' %in% genes) && !('LnbB' %in% genes) && !('NagP' %in% genes) && !('Chi_bifA' %in% genes) && !('Chi_bifX' %in% genes) && !('NagTa' %in% genes) && !('LnbC' %in% genes) && !('ChbB' %in% genes) && !('LnbA' %in% genes) && !('ChiG2' %in% genes) && 'NAGLU' %in% genes && 'ChiA' %in% genes) ||
        (!('ChbC' %in% genes) && !('NagX' %in% genes) && !('LnbB' %in% genes) && !('NagP' %in% genes) && !('Chi_bifA' %in% genes) && !('Chi_bifX' %in% genes) && !('NagTa' %in% genes) && !('LnbC' %in% genes) && !('ChbB' %in% genes) && !('LnbA' %in% genes) && !('ChiG2' %in% genes) && 'NAGLU' %in% genes && !('ChiA' %in% genes) && 'NagEa' %in% genes && !('NagEb' %in% genes)) ||
        (!('ChbC' %in% genes) && !('NagX' %in% genes) && !('LnbB' %in% genes) && !('NagP' %in% genes) && !('Chi_bifA' %in% genes) && !('Chi_bifX' %in% genes) && !('NagTa' %in% genes) && !('LnbC' %in% genes) && !('ChbB' %in% genes) && !('LnbA' %in% genes) && !('ChiG2' %in% genes) && !('NAGLU' %in% genes) && 'LnpA' %in% genes && 'LNB1_A' %in% genes && 'NagB2' %in% genes) ||        
        (!('ChbC' %in% genes) && !('NagX' %in% genes) && !('LnbB' %in% genes) && !('NagP' %in% genes) && !('Chi_bifA' %in% genes) && !('Chi_bifX' %in% genes) && !('NagTa' %in% genes) && !('LnbC' %in% genes) && !('ChbB' %in% genes) && !('LnbA' %in% genes) && !('ChiG2' %in% genes) && !('NAGLU' %in% genes) && 'LnpA' %in% genes && !('LNB1_A' %in% genes) && 'NagR3' %in% genes && 'NagTp2' %in% genes) ||        
        (!('ChbC' %in% genes) && !('NagX' %in% genes) && !('LnbB' %in% genes) && !('NagP' %in% genes) && !('Chi_bifA' %in% genes) && !('Chi_bifX' %in% genes) && !('NagTa' %in% genes) && !('LnbC' %in% genes) && !('ChbB' %in% genes) && !('LnbA' %in% genes) && !('ChiG2' %in% genes) && !('NAGLU' %in% genes) && !('LnpA' %in% genes) && 'NagEa' %in% genes && !('NagEc' %in% genes) && 'Hex_old' %in% genes && 'NagA' %in% genes && !('NagK' %in% genes) && 'GlmM' %in% genes && !('NagR2' %in% genes))        
        ) {
          status <- 1
        } else {
          status <- 0
        }
       
  } else if (subsystem == "N-Acetylglucosamine HMP" && phenotype == 'Lnb') {
    if(('LnpC' %in% genes && 'LnpA2' %in% genes) ||
       ('LnpC' %in% genes && !('LnpA2' %in% genes) && 'LnpA' %in% genes && 'LnbA' %in% genes) ||
       ('LnpC' %in% genes && !('LnpA2' %in% genes) && 'LnpA' %in% genes && !('LnbA' %in% genes) && 'LnpD' %in% genes && !('ChiA' %in% genes)) ||
       ('LnpC' %in% genes && !('LnpA2' %in% genes) && !('LnpA' %in% genes) && 'LnbA' %in% genes) ||
       (!('LnpC' %in% genes) && 'LnbC' %in% genes && 'NagA' %in% genes && !('NagX' %in% genes) && 'LnbA' %in% genes && !('NagR3' %in% genes) && !('NagTp2' %in% genes)) ||
       (!('LnpC' %in% genes) && 'LnbC' %in% genes && 'NagA' %in% genes && !('NagX' %in% genes) && 'LnbA' %in% genes && !('NagR3' %in% genes) && 'NagTp2' %in% genes && 'NagK2' %in% genes) || 
       (!('LnpC' %in% genes) && 'LnbC' %in% genes && 'NagA' %in% genes && !('NagX' %in% genes) && 'LnbA' %in% genes && !('NagR3' %in% genes) && 'NagTp2' %in% genes && !('NagK2' %in% genes) && !('NagEa' %in% genes)) ||
       (!('LnpC' %in% genes) && 'LnbC' %in% genes && 'NagA' %in% genes && !('NagX' %in% genes) && 'LnbA' %in% genes && 'NagR3' %in% genes && !('NagTp1' %in% genes)) ||       
       (!('LnpC' %in% genes) && 'LnbC' %in% genes && 'NagA' %in% genes && !('NagX' %in% genes) && !('LnbA' %in% genes) && !('NagK2' %in% genes)) ||
       (!('LnpC' %in% genes) && 'LnbC' %in% genes && 'NagA' %in% genes && 'NagX' %in% genes && 'NagTs' %in% genes) ||
       (!('LnpC' %in% genes) && !('LnbC' %in% genes) && 'LnbA' %in% genes && 'NagK2' %in% genes) ||
       (!('LnpC' %in% genes) && !('LnbC' %in% genes) && !('LnbA' %in% genes) && 'NagR3' %in% genes && !('NagEb' %in% genes) && !('NagK' %in% genes))       
    ) {
      status <- 1
    } else {
      status <- 0
    }

  } else if (subsystem == "Arabinose and arabinosides utilization HMP" && phenotype == 'Ara') {
    if( ('AraA' %in% genes && 'AraF' %in% genes && !('AbfA2' %in% genes) && !('AbfA5' %in% genes)) ||
        ('AraA' %in% genes && 'AraF' %in% genes && !('AbfA2' %in% genes) && 'AbfA5' %in% genes && !('AbfA1' %in% genes)) ||
        ('AraA' %in% genes && 'AraF' %in% genes && 'AbfA2' %in% genes && !('AbfA1' %in% genes) &&'AraG' %in% genes && !('HypBA1' %in% genes)) ||
        ('AraA' %in% genes && !('AraF' %in% genes) && 'AraE' %in% genes && !('AbnA' %in% genes) && !('Abf2_GH43' %in% genes) && !('TTT' %in% genes)) ||
        ('AraA' %in% genes && !('AraF' %in% genes) && 'AraE' %in% genes && !('AbnA' %in% genes) && !('Abf2_GH43' %in% genes) && 'TTT' %in% genes && 'AbfA1' %in% genes) ||
        ('AraA' %in% genes && !('AraF' %in% genes) && 'AraE' %in% genes && 'AbnA' %in% genes && 'AraN' %in% genes && 'Abf_GH51' %in% genes) ||
        ('AraA' %in% genes && !('AraF' %in% genes) && 'AraE' %in% genes && 'AbnA' %in% genes && !('AraN' %in% genes) && !('Abf2/Xsa_GH51' %in% genes)) ||
        ('AraA' %in% genes && !('AraF' %in% genes) && !('AraE' %in% genes) && 'AraM' %in% genes && !('AraN' %in% genes) && 'AbfA1' %in% genes && !('AbfA5' %in% genes)) ||
        ('AraA' %in% genes && !('AraF' %in% genes) && !('AraE' %in% genes) && 'AraM' %in% genes && !('AraN' %in% genes) && 'AbfA1' %in% genes && 'AbfA5' %in% genes && 'AbfA3' %in% genes) ||
        ('AraA' %in% genes && !('AraF' %in% genes) && !('AraE' %in% genes) && 'AraM' %in% genes && !('AraN' %in% genes) && !('AbfA1' %in% genes) && !('Abf2/Xsa_GH51' %in% genes) && 'AraW' %in% genes) ||
        ('AraA' %in% genes && !('AraF' %in% genes) && !('AraE' %in% genes) && 'AraM' %in% genes && !('AraN' %in% genes) && !('AbfA1' %in% genes) && !('Abf2/Xsa_GH51' %in% genes) && !('AraW' %in% genes) && 'AbfA2' %in% genes) ||
        ('AraA' %in% genes && !('AraF' %in% genes) && !('AraE' %in% genes) && !('AraM' %in% genes) && 'AraW' %in% genes && !('Abf_GH43' %in% genes)) ||
        ('AraA' %in% genes && !('AraF' %in% genes) && !('AraE' %in% genes) && !('AraM' %in% genes) && 'AraW' %in% genes && 'Abf_GH43' %in% genes && 'Abf_GH51' %in% genes) ||
        ('AraA' %in% genes && !('AraF' %in% genes) && !('AraE' %in% genes) && !('AraM' %in% genes) && !('AraW' %in% genes) && 'TTT' %in% genes && 'AraB' %in% genes) ||
        ('AraA' %in% genes && !('AraF' %in% genes) && !('AraE' %in% genes) && !('AraM' %in% genes) && !('AraW' %in% genes) && !('TTT' %in% genes) && 'AraF_old' %in% genes) ||
        ('AraA' %in% genes && !('AraF' %in% genes) && !('AraE' %in% genes) && !('AraM' %in% genes) && !('AraW' %in% genes) && !('TTT' %in% genes) && !('AraF_old' %in% genes) && 'AraN' %in% genes && 'AbfA1' %in% genes && !('Abf_GH51' %in% genes) && !('AbnA' %in% genes) && !('Abf2/Xsa_GH51' %in% genes) && !('AraG' %in% genes) && !('AraZ' %in% genes) && !('HypBA1' %in% genes)) ||
        ('AraA' %in% genes && !('AraF' %in% genes) && !('AraE' %in% genes) && !('AraM' %in% genes) && !('AraW' %in% genes) && !('TTT' %in% genes) && !('AraF_old' %in% genes) && !('AraN' %in% genes) && 'AraG' %in% genes) ||
        ('AraA' %in% genes && !('AraF' %in% genes) && !('AraE' %in% genes) && !('AraM' %in% genes) && !('AraW' %in% genes) && !('TTT' %in% genes) && !('AraF_old' %in% genes) && !('AraN' %in% genes) && !('AraG' %in% genes) && 'AbfA2_put' %in% genes && 'HypBA1' %in% genes && !('Abf_GH43' %in% genes)) ||
        (!('AraA' %in% genes) && 'AraY' %in% genes && !('AbfA3' %in% genes)) ||
        (!('AraA' %in% genes) && !('AraY' %in% genes) && 'AbfA3' %in% genes && 'AraF' %in% genes && !('Abf_GH51' %in% genes))
    ) {
      status <- 1
    } else {
      status <- 0
    }
  } else if (subsystem == "Arabinose and arabinosides utilization HMP" && phenotype == 'aAOS') {
    if( ('AraA' %in% genes && 'AbnA' %in% genes && !('AraF_old' %in% genes) && 'Abf2/Xsa_GH51' %in% genes && !('AbfA3' %in% genes)) ||
        ('AraA' %in% genes && 'AbnA' %in% genes && !('AraF_old' %in% genes) && 'Abf2/Xsa_GH51' %in% genes && 'AbfA3' %in% genes && 'AbfA1' %in% genes) ||
        ('AraA' %in% genes && 'AbnA' %in% genes && !('AraF_old' %in% genes) && 'Abf2/Xsa_GH51' %in% genes && 'AbfA3' %in% genes && !('AbfA1' %in% genes) && 'AraF' %in% genes) ||
        ('AraA' %in% genes && 'AbnA' %in% genes && !('AraF_old' %in% genes) && !('Abf2/Xsa_GH51' %in% genes) && 'AraP_SSS' %in% genes) ||
        ('AraA' %in% genes && 'AbnA' %in% genes && !('AraF_old' %in% genes) && !('Abf2/Xsa_GH51' %in% genes) && !('AraP_SSS' %in% genes) && !('AraR1' %in% genes) && 'AraN' %in% genes) ||
        ('AraA' %in% genes && 'AbnA' %in% genes && !('AraF_old' %in% genes) && !('Abf2/Xsa_GH51' %in% genes) && !('AraP_SSS' %in% genes) && !('AraR1' %in% genes) && !('AraN' %in% genes) && 'Abf_GH43' %in% genes) ||
        ('AraA' %in% genes && !('AbnA' %in% genes) && 'AbfA3' %in% genes && 'AbfT_old' %in% genes) ||
        ('AraA' %in% genes && !('AbnA' %in% genes) && 'AbfA3' %in% genes && !('AbfT_old' %in% genes) && 'AbfA1' %in% genes) ||
        ('AraA' %in% genes && !('AbnA' %in% genes) && 'AbfA3' %in% genes && !('AbfT_old' %in% genes) && !('AbfA1' %in% genes) && 'AraN' %in% genes) ||
        ('AraA' %in% genes && !('AbnA' %in% genes) && 'AbfA3' %in% genes && !('AbfT_old' %in% genes) && !('AbfA1' %in% genes) && !('AraN' %in% genes) && 'AauA' %in% genes) ||
        ('AraA' %in% genes && !('AbnA' %in% genes) && !('AbfA3' %in% genes) && 'Abf_GH43' %in% genes) ||
        ('AraA' %in% genes && !('AbnA' %in% genes) && !('AbfA3' %in% genes) && !('Abf_GH43' %in% genes) && 'AraU' %in% genes && 'Abf_GH51' %in% genes) ||
        ('AraA' %in% genes && !('AbnA' %in% genes) && !('AbfA3' %in% genes) && !('Abf_GH43' %in% genes) && 'AraU' %in% genes && !('Abf_GH51' %in% genes) && 'AbfA1' %in% genes) ||
        ('AraA' %in% genes && !('AbnA' %in% genes) && !('AbfA3' %in% genes) && !('Abf_GH43' %in% genes) && !('AraU' %in% genes) && 'AraP_SSS' %in% genes) ||
        ('AraA' %in% genes && !('AbnA' %in% genes) && !('AbfA3' %in% genes) && !('Abf_GH43' %in% genes) && !('AraU' %in% genes) && !('AraP_SSS' %in% genes) && 'AraN' %in% genes) ||
        ('AraA' %in% genes && !('AbnA' %in% genes) && !('AbfA3' %in% genes) && !('Abf_GH43' %in% genes) && !('AraU' %in% genes) && !('AraP_SSS' %in% genes) && !('AraN' %in% genes) && 'AbfT_old' %in% genes) ||
        ('AraA' %in% genes && !('AbnA' %in% genes) && !('AbfA3' %in% genes) && !('Abf_GH43' %in% genes) && !('AraU' %in% genes) && !('AraP_SSS' %in% genes) && !('AraN' %in% genes) && !('AbfT_old' %in% genes) && 'Abf2/Xsa_GH51' %in% genes && 'AbfA1' %in% genes) ||
        ('AraA' %in% genes && !('AbnA' %in% genes) && !('AbfA3' %in% genes) && !('Abf_GH43' %in% genes) && !('AraU' %in% genes) && !('AraP_SSS' %in% genes) && !('AraN' %in% genes) && !('AbfT_old' %in% genes) && !('Abf2/Xsa_GH51' %in% genes) && 'AbfT_OHS' %in% genes) ||
        ('AraA' %in% genes && !('AbnA' %in% genes) && !('AbfA3' %in% genes) && !('Abf_GH43' %in% genes) && !('AraU' %in% genes) && !('AraP_SSS' %in% genes) && !('AraN' %in% genes) && !('AbfT_old' %in% genes) && !('Abf2/Xsa_GH51' %in% genes) && !('AbfT_OHS' %in% genes) && 'BauA' %in% genes) ||
        ('AraA' %in% genes && !('AbnA' %in% genes) && !('AbfA3' %in% genes) && !('Abf_GH43' %in% genes) && !('AraU' %in% genes) && !('AraP_SSS' %in% genes) && !('AraN' %in% genes) && !('AbfT_old' %in% genes) && !('Abf2/Xsa_GH51' %in% genes) && !('AbfT_OHS' %in% genes) && !('BauA' %in% genes) && 'AraQ' %in% genes && 'Abf_GH51' %in% genes) ||
        ('AraA' %in% genes && !('AbnA' %in% genes) && !('AbfA3' %in% genes) && !('Abf_GH43' %in% genes) && !('AraU' %in% genes) && !('AraP_SSS' %in% genes) && !('AraN' %in% genes) && !('AbfT_old' %in% genes) && !('Abf2/Xsa_GH51' %in% genes) && !('AbfT_OHS' %in% genes) && !('BauA' %in% genes) && !('AraQ' %in% genes) && 'TTT' %in% genes && 'AraF' %in% genes) ||
        ('AraA' %in% genes && !('AbnA' %in% genes) && !('AbfA3' %in% genes) && !('Abf_GH43' %in% genes) && !('AraU' %in% genes) && !('AraP_SSS' %in% genes) && !('AraN' %in% genes) && !('AbfT_old' %in% genes) && !('Abf2/Xsa_GH51' %in% genes) && !('AbfT_OHS' %in% genes) && !('BauA' %in% genes) && !('AraQ' %in% genes) && !('TTT' %in% genes) && 'AraX' %in% genes && 'AraF' %in% genes && !('AraH' %in% genes)) ||
        (!('AraA' %in% genes) && 'Abf_GH51' %in% genes && 'AbfA3' %in% genes) ||
        (!('AraA' %in% genes) && 'Abf_GH51' %in% genes && !('AbfA3' %in% genes) && !('AraB' %in% genes))
    ) {
      status <- 1
    } else {
      status <- 0
    }
  } else if (subsystem == "Arabinose and arabinosides utilization HMP" && phenotype == 'bAOS') {
    if( ('HypBA1' %in% genes && 'AraB' %in% genes && !('AbfA3' %in% genes)) ||
        ('HypBA1' %in% genes && 'AraB' %in% genes && 'AbfA3' %in% genes && 'BauT' %in% genes)
    ) {
      status <- 1
    } else {
      status <- 0
    }
      
  } else if (subsystem == "Sucrose and Fructosides utilization HMP" && phenotype == 'Scr'){
    if( ('ScrR' %in% genes && 'ScrAb' %in% genes && !('BfrG' %in% genes)) ||
        ('ScrR' %in% genes && 'ScrAb' %in% genes && 'BfrG' %in% genes && !('BfrR2' %in% genes)) ||
        ('ScrR' %in% genes && 'ScrAb' %in% genes && 'BfrG' %in% genes && 'BfrR2' %in% genes && !('ScrP' %in% genes)) ||
        ('ScrR' %in% genes && 'ScrAb' %in% genes && 'BfrG' %in% genes && 'BfrR2' %in% genes && 'ScrP' %in% genes && 'ScrB1' %in% genes) ||
        ('ScrR' %in% genes && 'ScrAb' %in% genes && 'BfrG' %in% genes && 'BfrR2' %in% genes && 'ScrP' %in% genes && !('ScrB1' %in% genes) && 'ScrX2' %in% genes) ||
        ('ScrR' %in% genes && 'ScrAb' %in% genes && 'BfrG' %in% genes && 'BfrR2' %in% genes && 'ScrP' %in% genes && !('ScrB1' %in% genes) && !('ScrX2' %in% genes) && !('BfrA_old' %in% genes)) ||
        ('ScrR' %in% genes && !('ScrAb' %in% genes) && 'CsrR' %in% genes) ||
        ('ScrR' %in% genes && !('ScrAb' %in% genes) && !('CsrR' %in% genes) && 'ScrAb1' %in% genes) ||
        ('ScrR' %in% genes && !('ScrAb' %in% genes) && !('CsrR' %in% genes) && !('ScrAb1' %in% genes) && 'ScrX' %in% genes && !('BfrE' %in% genes) && 'ScrB2' %in% genes) ||
        ('ScrR' %in% genes && !('ScrAb' %in% genes) && !('CsrR' %in% genes) && !('ScrAb1' %in% genes) && 'ScrX' %in% genes && !('BfrE' %in% genes) && !('ScrB2' %in% genes) && 'BfrD' %in% genes) ||
        ('ScrR' %in% genes && !('ScrAb' %in% genes) && !('CsrR' %in% genes) && !('ScrAb1' %in% genes) && 'ScrX' %in% genes && !('BfrE' %in% genes) && !('ScrB2' %in% genes) && !('BfrD' %in% genes) && 'AmyS' %in% genes && 'ScrB1' %in% genes) ||
        ('ScrR' %in% genes && !('ScrAb' %in% genes) && !('CsrR' %in% genes) && !('ScrAb1' %in% genes) && 'ScrX' %in% genes && !('BfrE' %in% genes) && !('ScrB2' %in% genes) && !('BfrD' %in% genes) && 'AmyS' %in% genes && !('ScrB1' %in% genes) && 'ScrP' %in% genes) ||
        ('ScrR' %in% genes && !('ScrAb' %in% genes) && !('CsrR' %in% genes) && !('ScrAb1' %in% genes) && 'ScrX' %in% genes && !('BfrE' %in% genes) && !('ScrB2' %in% genes) && !('BfrD' %in% genes) && !('AmyS' %in% genes) && !('ScrP' %in% genes)) ||
        ('ScrR' %in% genes && !('ScrAb' %in% genes) && !('CsrR' %in% genes) && !('ScrAb1' %in% genes) && !('ScrX' %in% genes) && 'CscA2' %in% genes) ||
        ('ScrR' %in% genes && !('ScrAb' %in% genes) && !('CsrR' %in% genes) && !('ScrAb1' %in% genes) && !('ScrX' %in% genes) && !('CscA2' %in% genes) && 'BfrA2' %in% genes) ||
        ('ScrR' %in% genes && !('ScrAb' %in% genes) && !('CsrR' %in% genes) && !('ScrAb1' %in% genes) && !('ScrX' %in% genes) && !('CscA2' %in% genes) && !('BfrA2' %in% genes) && 'SacC' %in% genes) ||
        ('ScrR' %in% genes && !('ScrAb' %in% genes) && !('CsrR' %in% genes) && !('ScrAb1' %in% genes) && !('ScrX' %in% genes) && !('CscA2' %in% genes) && !('BfrA2' %in% genes) && !('SacC' %in% genes) && 'ScrT' %in% genes) ||
        ('ScrR' %in% genes && !('ScrAb' %in% genes) && !('CsrR' %in% genes) && !('ScrAb1' %in% genes) && !('ScrX' %in% genes) && !('CscA2' %in% genes) && !('BfrA2' %in% genes) && !('SacC' %in% genes) && !('ScrT' %in% genes) && 'BfrH' %in% genes && !('AmyS' %in% genes)) ||
        ('ScrR' %in% genes && !('ScrAb' %in% genes) && !('CsrR' %in% genes) && !('ScrAb1' %in% genes) && !('ScrX' %in% genes) && !('CscA2' %in% genes) && !('BfrA2' %in% genes) && !('SacC' %in% genes) && !('ScrT' %in% genes) && !('BfrH' %in% genes) && 'ScrX2' %in% genes && !('BfrA_old' %in% genes)) ||  
        ('ScrR' %in% genes && !('ScrAb' %in% genes) && !('CsrR' %in% genes) && !('ScrAb1' %in% genes) && !('ScrX' %in% genes) && !('CscA2' %in% genes) && !('BfrA2' %in% genes) && !('SacC' %in% genes) && !('ScrT' %in% genes) && !('BfrH' %in% genes) && !('ScrX2' %in% genes) && 'ScrB1' %in% genes && 'CscB' %in% genes) ||  
        (!('ScrR' %in% genes) && 'SusC_fos' %in% genes) ||
        (!('ScrR' %in% genes) && !('SusC_fos' %in% genes) && 'ScrAb' %in% genes && !('BfrA_old' %in% genes) && 'ScrK' %in% genes) ||
        (!('ScrR' %in% genes) && !('SusC_fos' %in% genes) && 'ScrAb' %in% genes && !('BfrA_old' %in% genes) && !('ScrK' %in% genes) && 'ScrAa' %in% genes && 'ScrB1' %in% genes) ||
        (!('ScrR' %in% genes) && !('SusC_fos' %in% genes) && 'ScrAb' %in% genes && !('BfrA_old' %in% genes) && !('ScrK' %in% genes) && 'ScrAa' %in% genes && !('ScrB1' %in% genes) && 'ScrB2' %in% genes) ||
        (!('ScrR' %in% genes) && !('SusC_fos' %in% genes) && 'ScrAb' %in% genes && !('BfrA_old' %in% genes) && !('ScrK' %in% genes) && !('ScrAa' %in% genes) && 'ScrB2' %in% genes) ||
        (!('ScrR' %in% genes) && !('SusC_fos' %in% genes) && 'ScrAb' %in% genes && 'BfrA_old' %in% genes && 'ScrB2' %in% genes) ||
        (!('ScrR' %in% genes) && !('SusC_fos' %in% genes) && !('ScrAb' %in% genes) && 'CscB' %in% genes && 'CsrR' %in% genes) ||
        (!('ScrR' %in% genes) && !('SusC_fos' %in% genes) && !('ScrAb' %in% genes) && 'CscB' %in% genes && !('CsrR' %in% genes) && 'ScrP' %in% genes) ||
        (!('ScrR' %in% genes) && !('SusC_fos' %in% genes) && !('ScrAb' %in% genes) && 'CscB' %in% genes && !('CsrR' %in% genes) && !('ScrP' %in% genes) && 'ScrB1' %in% genes) ||
        (!('ScrR' %in% genes) && !('SusC_fos' %in% genes) && !('ScrAb' %in% genes) && 'CscB' %in% genes && !('CsrR' %in% genes) && !('ScrP' %in% genes) && !('ScrB1' %in% genes) && 'BfrA_old' %in% genes) ||
        (!('ScrR' %in% genes) && !('SusC_fos' %in% genes) && !('ScrAb' %in% genes) && !('CscB' %in% genes) && 'ScrZ2' %in% genes && 'ScrP' %in% genes && !('BfrA2' %in% genes)) ||  
        (!('ScrR' %in% genes) && !('SusC_fos' %in% genes) && !('ScrAb' %in% genes) && !('CscB' %in% genes) && !('ScrZ2' %in% genes) && 'FruP' %in% genes) ||
        (!('ScrR' %in% genes) && !('SusC_fos' %in% genes) && !('ScrAb' %in% genes) && !('CscB' %in% genes) && !('ScrZ2' %in% genes) && !('FruP' %in% genes) && 'ScrAb1' %in% genes) ||
        (!('ScrR' %in% genes) && !('SusC_fos' %in% genes) && !('ScrAb' %in% genes) && !('CscB' %in% genes) && !('ScrZ2' %in% genes) && !('FruP' %in% genes) && !('ScrAb1' %in% genes) && 'ScrX' %in% genes) 
     ) {
      status <- 1
    } else {
      status <- 0
    }

  } else if(subsystem == "Sucrose and Fructosides utilization HMP" && phenotype == 'FOS') {
    if( ('FruP' %in% genes && !('ScrT' %in% genes)) ||
        ('FruP' %in% genes && 'ScrT' %in% genes && 'CscA2' %in% genes) ||
        (!('FruP' %in% genes) && 'CscA2' %in% genes && 'CscB' %in% genes) ||
        (!('FruP' %in% genes) && 'CscA2' %in% genes && !('CscB' %in% genes) && 'BfrA' %in% genes) ||
        (!('FruP' %in% genes) && 'CscA2' %in% genes && !('CscB' %in% genes) && !('BfrA' %in% genes) && 'BfrA2' %in% genes) ||
        (!('FruP' %in% genes) && !('CscA2' %in% genes) && 'BfrE' %in% genes && !('BfrA2' %in% genes) && !('AmyS' %in% genes) && !('ScrAb' %in% genes)) ||
        (!('FruP' %in% genes) && !('CscA2' %in% genes) && 'BfrE' %in% genes && !('BfrA2' %in% genes) && !('AmyS' %in% genes) && 'ScrAb' %in% genes && 'ScrR' %in% genes && 'ScrB2' %in% genes) ||
        (!('FruP' %in% genes) && !('CscA2' %in% genes) && 'BfrE' %in% genes && !('BfrA2' %in% genes) && !('AmyS' %in% genes) && 'ScrAb' %in% genes && 'ScrR' %in% genes && !('ScrB2' %in% genes) && !('ScrP' %in% genes)) ||
        (!('FruP' %in% genes) && !('CscA2' %in% genes) && 'BfrE' %in% genes && !('BfrA2' %in% genes) && !('AmyS' %in% genes) && 'ScrAb' %in% genes && 'ScrR' %in% genes && !('ScrB2' %in% genes) && 'ScrP' %in% genes && 'ScrAa' %in% genes && !('BfrR2' %in% genes)) ||
        (!('FruP' %in% genes) && !('CscA2' %in% genes) && 'BfrE' %in% genes && !('BfrA2' %in% genes) && !('AmyS' %in% genes) && 'ScrAb' %in% genes && 'ScrR' %in% genes && !('ScrB2' %in% genes) && 'ScrP' %in% genes && 'ScrAa' %in% genes && 'BfrR2' %in% genes && 'CscB' %in% genes) ||
        (!('FruP' %in% genes) && !('CscA2' %in% genes) && 'BfrE' %in% genes && !('BfrA2' %in% genes) && !('AmyS' %in% genes) && 'ScrAb' %in% genes && !('ScrR' %in% genes) && 'ScrP' %in% genes) ||
        (!('FruP' %in% genes) && !('CscA2' %in% genes) && 'BfrE' %in% genes && !('BfrA2' %in% genes) && 'AmyS' %in% genes && 'BfrF' %in% genes && !('ScrAa' %in% genes) && 'ScrB2' %in% genes) ||
        (!('FruP' %in% genes) && !('CscA2' %in% genes) && 'BfrE' %in% genes && !('BfrA2' %in% genes) && 'AmyS' %in% genes && 'BfrF' %in% genes && !('ScrAa' %in% genes) && !('ScrB2' %in% genes) && !('BfrG' %in% genes)) ||
        (!('FruP' %in% genes) && !('CscA2' %in% genes) && 'BfrE' %in% genes && !('BfrA2' %in% genes) && 'AmyS' %in% genes && 'BfrF' %in% genes && !('ScrAa' %in% genes) && !('ScrB2' %in% genes) && 'BfrG' %in% genes && 'ScrP' %in% genes) || 
        (!('FruP' %in% genes) && !('CscA2' %in% genes) && 'BfrE' %in% genes && !('BfrA2' %in% genes) && 'AmyS' %in% genes && 'BfrF' %in% genes && !('ScrAa' %in% genes) && !('ScrB2' %in% genes) && 'BfrG' %in% genes && !('ScrP' %in% genes) && !('BfrR2' %in% genes)) ||
        (!('FruP' %in% genes) && !('CscA2' %in% genes) && 'BfrE' %in% genes && !('BfrA2' %in% genes) && 'AmyS' %in% genes && 'BfrF' %in% genes && 'ScrAa' %in% genes && !('ScrP' %in% genes)) ||
        (!('FruP' %in% genes) && !('CscA2' %in% genes) && 'BfrE' %in% genes && 'BfrA2' %in% genes && 'ScrP' %in% genes) ||
        (!('FruP' %in% genes) && !('CscA2' %in% genes) && !('BfrE' %in% genes) && 'BfrC' %in% genes && !('ScrX2' %in% genes) && !('SacC' %in% genes) && !('BfrA_old' %in% genes)) ||
        (!('FruP' %in% genes) && !('CscA2' %in% genes) && !('BfrE' %in% genes) && 'BfrC' %in% genes && !('ScrX2' %in% genes) && !('SacC' %in% genes) && 'BfrA_old' %in% genes && !('ScrB1' %in% genes)) ||
        (!('FruP' %in% genes) && !('CscA2' %in% genes) && !('BfrE' %in% genes) && 'BfrC' %in% genes && !('ScrX2' %in% genes) && !('SacC' %in% genes) && 'BfrA_old' %in% genes && 'ScrB1' %in% genes && 'ScrB2' %in% genes) ||
        (!('FruP' %in% genes) && !('CscA2' %in% genes) && !('BfrE' %in% genes) && 'BfrC' %in% genes && !('ScrX2' %in% genes) && !('SacC' %in% genes) && 'BfrA_old' %in% genes && 'ScrB1' %in% genes && !('ScrB2' %in% genes) && !('ScrAa' %in% genes)) ||
        (!('FruP' %in% genes) && !('CscA2' %in% genes) && !('BfrE' %in% genes) && 'BfrC' %in% genes && !('ScrX2' %in% genes) && !('SacC' %in% genes) && 'BfrA_old' %in% genes && 'ScrB1' %in% genes && !('ScrB2' %in% genes) && 'ScrAa' %in% genes && 'BfrX' %in% genes) ||
        (!('FruP' %in% genes) && !('CscA2' %in% genes) && !('BfrE' %in% genes) && 'BfrC' %in% genes && !('ScrX2' %in% genes) && 'SacC' %in% genes && !('ScrP' %in% genes)) ||
        (!('FruP' %in% genes) && !('CscA2' %in% genes) && !('BfrE' %in% genes) && 'BfrC' %in% genes && 'ScrX2' %in% genes && 'ScrZ2' %in% genes) ||  
        (!('FruP' %in% genes) && !('CscA2' %in% genes) && !('BfrE' %in% genes) && !('BfrC' %in% genes) && 'SusC_fos' %in% genes) ||
        (!('FruP' %in% genes) && !('CscA2' %in% genes) && !('BfrE' %in% genes) && !('BfrC' %in% genes) && !('SusC_fos' %in% genes) && 'BfrX' %in% genes) ||
        (!('FruP' %in% genes) && !('CscA2' %in% genes) && !('BfrE' %in% genes) && !('BfrC' %in% genes) && !('SusC_fos' %in% genes) && !('BfrX' %in% genes) && 'BfrB' %in% genes && !('AmyS' %in% genes)) ||
        (!('FruP' %in% genes) && !('CscA2' %in% genes) && !('BfrE' %in% genes) && !('BfrC' %in% genes) && !('SusC_fos' %in% genes) && !('BfrX' %in% genes) && !('BfrB' %in% genes) && 'ScrY' %in% genes && 'BfrA_old' %in% genes && !('ScrP' %in% genes)) 
    ) {
      status <- 1
    } else {
      status <- 0
    }

  } else if(subsystem == "HMP Butyrate synthesis" && phenotype == 'Butyrate') {
    if( ('EtfB2' %in% genes && 'Hbd' %in% genes && 'Bcd' %in% genes) ||
        ('EtfB2' %in% genes && 'Hbd' %in% genes && !('Bcd' %in% genes) && 'Thl' %in% genes) ||
        ('EtfB2' %in% genes && !('Hbd' %in% genes) && 'AbfH' %in% genes) ||
        ('EtfB2' %in% genes && !('Hbd' %in% genes) && !('AbfH' %in% genes) && 'CtfA' %in% genes && !('Ptb' %in% genes)) ||
        (!('EtfB2' %in% genes) && 'AbfD' %in% genes && 'Thl' %in% genes) ||
        (!('EtfB2' %in% genes) && !('AbfD' %in% genes) && 'Buk' %in% genes && 'Bcd' %in% genes && 'Thl' %in% genes)
    ) {
      status <- 1
    } else {
      status <- 0
    }
  } else if(subsystem == "Ribose utilization HMP" && phenotype == 'Rbs') {
    if(('RbsD' %in% genes && 'RbsC' %in% genes) ||
       ('RbsD' %in% genes && !('RbsC' %in% genes) && !('UP' %in% genes) && !('NupA' %in% genes) && 'RbsK' %in% genes && 'RbsU' %in% genes) ||
       ('RbsD' %in% genes && !('RbsC' %in% genes) && !('UP' %in% genes) && !('NupA' %in% genes) && 'RbsK' %in% genes && !('RbsU' %in% genes) && 'RbsT' %in% genes) ||
       ('RbsD' %in% genes && !('RbsC' %in% genes) && !('UP' %in% genes) && !('NupA' %in% genes) && 'RbsK' %in% genes && !('RbsU' %in% genes) && !('RbsT' %in% genes) && 'NupC' %in% genes) ||
       ('RbsD' %in% genes && !('RbsC' %in% genes) && !('UP' %in% genes) && !('NupA' %in% genes) && 'RbsK' %in% genes && !('RbsU' %in% genes) && !('RbsT' %in% genes) && !('NupC' %in% genes) && 'RbsB' %in% genes) ||
       ('RbsD' %in% genes && !('RbsC' %in% genes) && 'UP' %in% genes && 'RbsU' %in% genes) ||
       ('RbsD' %in% genes && !('RbsC' %in% genes) && 'UP' %in% genes && !('RbsU' %in% genes) && 'RbsB' %in% genes && !('NupB' %in% genes)) ||
       ('RbsD' %in% genes && !('RbsC' %in% genes) && 'UP' %in% genes && !('RbsU' %in% genes) && !('RbsB' %in% genes) && 'RbsT' %in% genes) ||
       (!('RbsD' %in% genes) && 'FruE_bif' %in% genes) ||
       (!('RbsD' %in% genes) && !('FruE_bif' %in% genes) && 'RbsC' %in% genes && 'RbsK' %in% genes && 'RbsA' %in% genes) ||
       (!('RbsD' %in% genes) && !('FruE_bif' %in% genes) && 'RbsC' %in% genes && 'RbsK' %in% genes && !('RbsA' %in% genes) && 'RbsB' %in% genes) ||
       (!('RbsD' %in% genes) && !('FruE_bif' %in% genes) && 'RbsC' %in% genes && 'RbsK' %in% genes && !('RbsA' %in% genes) && !('RbsB' %in% genes) && 'IunH' %in% genes) ||
       (!('RbsD' %in% genes) && !('FruE_bif' %in% genes) && !('RbsC' %in% genes) && 'RusT' %in% genes) ||
       (!('RbsD' %in% genes) && !('FruE_bif' %in% genes) && !('RbsC' %in% genes) && !('RusT' %in% genes) && 'RbsU' %in% genes) ||
       (!('RbsD' %in% genes) && !('FruE_bif' %in% genes) && !('RbsC' %in% genes) && !('RusT' %in% genes) && !('RbsU' %in% genes) && 'RbsT' %in% genes) ||
       (!('RbsD' %in% genes) && !('FruE_bif' %in% genes) && !('RbsC' %in% genes) && !('RusT' %in% genes) && !('RbsU' %in% genes) && !('RbsT' %in% genes) && 'RbsB' %in% genes && 'RbsK' %in% genes)
    ) {
      status <- 1
    } else {
      status <- 0
    }
    
  } else if (subsystem == "Gluconate utilization HMP" && phenotype == 'Gnt') {
    if(('GntK' %in% genes && 'GntU' %in% genes) ||
       ('GntK' %in% genes && !('GntU' %in% genes) && 'GntP2' %in% genes) ||
       ('GntK' %in% genes && !('GntU' %in% genes) && !('GntP2' %in% genes) && 'GntT2' %in% genes) ||
       ('GntK' %in% genes && !('GntU' %in% genes) && !('GntP2' %in% genes) && !('GntT2' %in% genes) && 'GntT' %in% genes) ||
       ('GntK' %in% genes && !('GntU' %in% genes) && !('GntP2' %in% genes) && !('GntT2' %in% genes) && !('GntT' %in% genes) && 'GntP' %in% genes) ||
       ('GntK' %in% genes && !('GntU' %in% genes) && !('GntP2' %in% genes) && !('GntT2' %in% genes) && !('GntT' %in% genes) && !('GntP' %in% genes) && 'TRAPlp' %in% genes)
    ) {
      status <- 1
    } else {
      status <- 0
    }
  } else if (subsystem == "Maltose and Maltodextrin utilization HMP" && phenotype == '(Mal)n') {
    if( ('MalF' %in% genes && 'MalE' %in% genes && !('MalR7' %in% genes) && 'MalQ' %in% genes && !('MxmX' %in% genes)) ||
        ('MalF' %in% genes && 'MalE' %in% genes && !('MalR7' %in% genes) && 'MalQ' %in% genes && 'MxmX' %in% genes && 'GalM' %in% genes) ||
        ('MalF' %in% genes && 'MalE' %in% genes && !('MalR7' %in% genes) && 'MalQ' %in% genes && 'MxmX' %in% genes && !('GalM' %in% genes) && 'MalR' %in% genes) ||
        ('MalF' %in% genes && 'MalE' %in% genes && !('MalR7' %in% genes) && 'MalQ' %in% genes && 'MxmX' %in% genes && !('GalM' %in% genes) && !('MalR' %in% genes) && 'NplT' %in% genes) ||
        ('MalF' %in% genes && 'MalE' %in% genes && !('MalR7' %in% genes) && !('MalQ' %in% genes) && 'NplT' %in% genes && !('MalS' %in% genes) && 'GlvB' %in% genes) ||  
        ('MalF' %in% genes && 'MalE' %in% genes && !('MalR7' %in% genes) && !('MalQ' %in% genes) && 'NplT' %in% genes && !('MalS' %in% genes) && !('GlvB' %in% genes) && 'AglA' %in% genes) ||
        ('MalF' %in% genes && 'MalE' %in% genes && !('MalR7' %in% genes) && !('MalQ' %in% genes) && 'NplT' %in% genes && !('MalS' %in% genes) && !('GlvB' %in% genes) && !('AglA' %in% genes) && !('DexB' %in% genes)) ||
        ('MalF' %in% genes && 'MalE' %in% genes && !('MalR7' %in% genes) && !('MalQ' %in% genes) && 'NplT' %in% genes && !('MalS' %in% genes) && !('GlvB' %in% genes) && !('AglA' %in% genes) && 'DexB' %in% genes && 'MxmX' %in% genes) ||
        ('MalF' %in% genes && 'MalE' %in% genes && !('MalR7' %in% genes) && !('MalQ' %in% genes) && 'NplT' %in% genes && 'MalS' %in% genes && !('GalM' %in% genes)) ||
        ('MalF' %in% genes && 'MalE' %in% genes && !('MalR7' %in% genes) && !('MalQ' %in% genes) && !('NplT' %in% genes) && 'GlgP' %in% genes) ||
        ('MalF' %in% genes && 'MalE' %in% genes && !('MalR7' %in% genes) && !('MalQ' %in% genes) && !('NplT' %in% genes) && !('GlgP' %in% genes) && 'MalZ' %in% genes) ||
        ('MalF' %in% genes && 'MalE' %in% genes && !('MalR7' %in% genes) && !('MalQ' %in% genes) && !('NplT' %in% genes) && !('GlgP' %in% genes) && !('MalZ' %in% genes) && 'MalP' %in% genes && 'GalM' %in% genes) ||
        ('MalF' %in% genes && 'MalE' %in% genes && !('MalR7' %in% genes) && !('MalQ' %in% genes) && !('NplT' %in% genes) && !('GlgP' %in% genes) && !('MalZ' %in% genes) && 'MalP' %in% genes && !('GalM' %in% genes) && !('MxmX' %in% genes)) ||
        ('MalF' %in% genes && 'MalE' %in% genes && !('MalR7' %in% genes) && !('MalQ' %in% genes) && !('NplT' %in% genes) && !('GlgP' %in% genes) && !('MalZ' %in% genes) && !('MalP' %in% genes) && 'AmyA' %in% genes) ||
        ('MalF' %in% genes && 'MalE' %in% genes && !('MalR7' %in% genes) && !('MalQ' %in% genes) && !('NplT' %in% genes) && !('GlgP' %in% genes) && !('MalZ' %in% genes) && !('MalP' %in% genes) && !('AmyA' %in% genes) && 'AglA' %in% genes && !('GalM' %in% genes)) ||
        ('MalF' %in% genes && 'MalE' %in% genes && 'MalR7' %in% genes && !('GlvB' %in% genes)) ||
        ('MalF' %in% genes && 'MalE' %in% genes && 'MalR7' %in% genes && 'GlvB' %in% genes && 'Pgm' %in% genes) ||
        ('MalF' %in% genes && !('MalE' %in% genes) && 'MalK' %in% genes && 'MalQ' %in% genes) ||
        ('MalF' %in% genes && !('MalE' %in% genes) && 'MalK' %in% genes && !('MalQ' %in% genes) && !('MalG' %in% genes)) ||
        ('MalF' %in% genes && !('MalE' %in% genes) && !('MalK' %in% genes) && 'NplT' %in% genes && !('AmyA' %in% genes) && !('GlvB' %in% genes)) ||     
        ('MalF' %in% genes && !('MalE' %in% genes) && !('MalK' %in% genes) && 'NplT' %in% genes && !('AmyA' %in% genes) && 'GlvB' %in% genes && 'GalM' %in% genes && 'GlvA' %in% genes) ||
        ('MalF' %in% genes && !('MalE' %in% genes) && !('MalK' %in% genes) && 'NplT' %in% genes && !('AmyA' %in% genes) && 'GlvB' %in% genes && 'GalM' %in% genes && !('GlvA' %in% genes) && 'MalY' %in% genes) ||
        (!('MalF' %in% genes) && 'SusCm' %in% genes && 'SusDm' %in% genes && 'PulA' %in% genes) ||
        (!('MalF' %in% genes) && 'SusCm' %in% genes && 'SusDm' %in% genes && !('PulA' %in% genes) && !('Cga' %in% genes)) ||
        (!('MalF' %in% genes) && 'SusCm' %in% genes && 'SusDm' %in% genes && !('PulA' %in% genes) && 'Cga' %in% genes && 'MalT' %in% genes) ||
        (!('MalF' %in% genes) && 'SusCm' %in% genes && 'SusDm' %in% genes && !('PulA' %in% genes) && 'Cga' %in% genes && !('MalT' %in% genes) && 'MalP' %in% genes) ||
        (!('MalF' %in% genes) && 'SusCm' %in% genes && !('SusDm' %in% genes) && 'SusB2' %in% genes) ||        
        (!('MalF' %in% genes) && !('SusCm' %in% genes) && 'MdxE' %in% genes && !('MalK' %in% genes)) ||
        (!('MalF' %in% genes) && !('SusCm' %in% genes) && 'MdxE' %in% genes && 'MalK' %in% genes && 'GlgP' %in% genes) ||
        (!('MalF' %in% genes) && !('SusCm' %in% genes) && !('MdxE' %in% genes) && 'MalE' %in% genes && 'MalR' %in% genes) ||
        (!('MalF' %in% genes) && !('SusCm' %in% genes) && !('MdxE' %in% genes) && 'MalE' %in% genes && !('MalR' %in% genes) && 'MalZ' %in% genes && 'Pgm' %in% genes) ||
        (!('MalF' %in% genes) && !('SusCm' %in% genes) && !('MdxE' %in% genes) && 'MalE' %in% genes && !('MalR' %in% genes) && 'MalZ' %in% genes && !('Pgm' %in% genes) && 'MalY' %in% genes) ||
        (!('MalF' %in% genes) && !('SusCm' %in% genes) && !('MdxE' %in% genes) && 'MalE' %in% genes && !('MalR' %in% genes) && !('MalZ' %in% genes) && 'MalK' %in% genes) ||
        (!('MalF' %in% genes) && !('SusCm' %in% genes) && !('MdxE' %in% genes) && 'MalE' %in% genes && !('MalR' %in% genes) && !('MalZ' %in% genes) && !('MalK' %in% genes) && 'MalP' %in% genes && 'GalM' %in% genes) ||
        (!('MalF' %in% genes) && !('SusCm' %in% genes) && !('MdxE' %in% genes) && !('MalE' %in% genes) && 'SusDm' %in% genes && 'MalT' %in% genes) ||
        (!('MalF' %in% genes) && !('SusCm' %in% genes) && !('MdxE' %in% genes) && !('MalE' %in% genes) && !('SusDm' %in% genes) && 'Agl3' %in% genes) ||
        (!('MalF' %in% genes) && !('SusCm' %in% genes) && !('MdxE' %in% genes) && !('MalE' %in% genes) && !('SusDm' %in% genes) && !('Agl3' %in% genes) && 'SusB2' %in% genes && 'Gde2' %in% genes && !('MalP' %in% genes) && !('MalT' %in% genes)) ||
        (!('MalF' %in% genes) && !('SusCm' %in% genes) && !('MdxE' %in% genes) && !('MalE' %in% genes) && !('SusDm' %in% genes) && !('Agl3' %in% genes) && 'SusB2' %in% genes && 'Gde2' %in% genes && !('MalP' %in% genes) && 'MalT' %in% genes && 'NplT' %in% genes) ||
        (!('MalF' %in% genes) && !('SusCm' %in% genes) && !('MdxE' %in% genes) && !('MalE' %in% genes) && !('SusDm' %in% genes) && !('Agl3' %in% genes) && !('SusB2' %in% genes) && 'MxmX' %in% genes && 'MalL' %in% genes && 'MdxF' %in% genes && 'GalM' %in% genes) ||
        (!('MalF' %in% genes) && !('SusCm' %in% genes) && !('MdxE' %in% genes) && !('MalE' %in% genes) && !('SusDm' %in% genes) && !('Agl3' %in% genes) && !('SusB2' %in% genes) && 'MxmX' %in% genes && 'MalL' %in% genes && !('MdxF' %in% genes) && 'MalG' %in% genes) ||
        (!('MalF' %in% genes) && !('SusCm' %in% genes) && !('MdxE' %in% genes) && !('MalE' %in% genes) && !('SusDm' %in% genes) && !('Agl3' %in% genes) && !('SusB2' %in% genes) && 'MxmX' %in% genes && !('MalL' %in% genes) && 'Gde2' %in% genes && 'GlvB' %in% genes && !('Pgm' %in% genes)) ||
        (!('MalF' %in% genes) && !('SusCm' %in% genes) && !('MdxE' %in% genes) && !('MalE' %in% genes) && !('SusDm' %in% genes) && !('Agl3' %in% genes) && !('SusB2' %in% genes) && 'MxmX' %in% genes && !('MalL' %in% genes) && !('Gde2' %in% genes) && 'MalQ' %in% genes && 'GlvA' %in% genes) ||
        (!('MalF' %in% genes) && !('SusCm' %in% genes) && !('MdxE' %in% genes) && !('MalE' %in% genes) && !('SusDm' %in% genes) && !('Agl3' %in% genes) && !('SusB2' %in% genes) && 'MxmX' %in% genes && !('MalL' %in% genes) && !('Gde2' %in% genes) && 'MalQ' %in% genes && !('GlvA' %in% genes) && 'MalK' %in% genes && 'GalM' %in% genes) ||
        (!('MalF' %in% genes) && !('SusCm' %in% genes) && !('MdxE' %in% genes) && !('MalE' %in% genes) && !('SusDm' %in% genes) && !('Agl3' %in% genes) && !('SusB2' %in% genes) && !('MxmX' %in% genes) && 'SusA' %in% genes && !('SusB' %in% genes)) ||        
        (!('MalF' %in% genes) && !('SusCm' %in% genes) && !('MdxE' %in% genes) && !('MalE' %in% genes) && !('SusDm' %in% genes) && !('Agl3' %in% genes) && !('SusB2' %in% genes) && !('MxmX' %in% genes) && !('SusA' %in% genes) && 'PulA' %in% genes && 'MalZ' %in% genes && 'AglA' %in% genes && !('MalL' %in% genes))        
    ) {
      status <- 1
    } else {
      status <- 0
    }
  } else if (subsystem == "Maltose and Maltodextrin utilization HMP" && phenotype == 'Mal') {
    if(('MalF' %in% genes && 'MalQ' %in% genes && 'MalE' %in% genes) ||
       ('MalF' %in% genes && 'MalQ' %in% genes && !('MalE' %in% genes) && !('MdxF' %in% genes) && !('PulA' %in% genes)) ||
       ('MalF' %in% genes && 'MalQ' %in% genes && !('MalE' %in% genes) && !('MdxF' %in% genes) && 'PulA' %in% genes && 'GlvB' %in% genes) ||
       ('MalF' %in% genes && 'MalQ' %in% genes && !('MalE' %in% genes) && !('MdxF' %in% genes) && 'PulA' %in% genes && !('GlvB' %in% genes) && 'Pgm' %in% genes) ||
       ('MalF' %in% genes && !('MalQ' %in% genes) && 'NplT' %in% genes && !('MalS' %in% genes) && 'GlvB' %in% genes) ||
       ('MalF' %in% genes && !('MalQ' %in% genes) && 'NplT' %in% genes && !('MalS' %in% genes) && !('GlvB' %in% genes) && 'AglA' %in% genes) ||
       ('MalF' %in% genes && !('MalQ' %in% genes) && 'NplT' %in% genes && !('MalS' %in% genes) && !('GlvB' %in% genes) && !('AglA' %in% genes) && !('DexB' %in% genes)) ||
       ('MalF' %in% genes && !('MalQ' %in% genes) && 'NplT' %in% genes && !('MalS' %in% genes) && !('GlvB' %in% genes) && !('AglA' %in% genes) && 'DexB' %in% genes && 'MxmX' %in% genes) ||
       ('MalF' %in% genes && !('MalQ' %in% genes) && 'NplT' %in% genes && 'MalS' %in% genes && !('GalM' %in% genes)) ||    
       ('MalF' %in% genes && !('MalQ' %in% genes) && !('NplT' %in% genes) && 'AglA' %in% genes) ||       
       ('MalF' %in% genes && !('MalQ' %in% genes) && !('NplT' %in% genes) && !('AglA' %in% genes) && 'GlgP' %in% genes) ||
       ('MalF' %in% genes && !('MalQ' %in% genes) && !('NplT' %in% genes) && !('AglA' %in% genes) && !('GlgP' %in% genes) && 'MalL' %in% genes && 'GalM' %in% genes) ||
       ('MalF' %in% genes && !('MalQ' %in% genes) && !('NplT' %in% genes) && !('AglA' %in% genes) && !('GlgP' %in% genes) && !('MalL' %in% genes) && 'GlvB' %in% genes) ||
       ('MalF' %in% genes && !('MalQ' %in% genes) && !('NplT' %in% genes) && !('AglA' %in% genes) && !('GlgP' %in% genes) && !('MalL' %in% genes) && !('GlvB' %in% genes) && 'AmyA' %in% genes) ||
       ('MalF' %in% genes && !('MalQ' %in% genes) && !('NplT' %in% genes) && !('AglA' %in% genes) && !('GlgP' %in% genes) && !('MalL' %in% genes) && !('GlvB' %in% genes) && !('AmyA' %in% genes) && 'MalZ' %in% genes && 'MalE' %in% genes) ||
       (!('MalF' %in% genes) && 'GlvB' %in% genes && !('Cga' %in% genes) && 'GlvA' %in% genes && !('MalL' %in% genes)) ||
       (!('MalF' %in% genes) && 'GlvB' %in% genes && !('Cga' %in% genes) && 'GlvA' %in% genes && 'MalL' %in% genes && !('GlgX' %in% genes)) ||
       (!('MalF' %in% genes) && 'GlvB' %in% genes && !('Cga' %in% genes) && 'GlvA' %in% genes && 'MalL' %in% genes && 'GlgX' %in% genes && 'MdxE' %in% genes) ||
       (!('MalF' %in% genes) && 'GlvB' %in% genes && !('Cga' %in% genes) && !('GlvA' %in% genes) && 'AglA' %in% genes && !('NplT' %in% genes) && !('MalQ' %in% genes) && !('Pgm' %in% genes)) ||
       (!('MalF' %in% genes) && 'GlvB' %in% genes && !('Cga' %in% genes) && !('GlvA' %in% genes) && 'AglA' %in% genes && !('NplT' %in% genes) && !('MalQ' %in% genes) && 'Pgm' %in% genes && !('AmyA' %in% genes)) ||
       (!('MalF' %in% genes) && 'GlvB' %in% genes && !('Cga' %in% genes) && !('GlvA' %in% genes) && 'AglA' %in% genes && !('NplT' %in% genes) && !('MalQ' %in% genes) && 'Pgm' %in% genes && 'AmyA' %in% genes && 'GalM' %in% genes) ||
       (!('MalF' %in% genes) && 'GlvB' %in% genes && !('Cga' %in% genes) && !('GlvA' %in% genes) && 'AglA' %in% genes && !('NplT' %in% genes) && !('MalQ' %in% genes) && 'Pgm' %in% genes && 'AmyA' %in% genes && !('GalM' %in% genes) && 'MalK' %in% genes) ||
       (!('MalF' %in% genes) && 'GlvB' %in% genes && !('Cga' %in% genes) && !('GlvA' %in% genes) && 'AglA' %in% genes && !('NplT' %in% genes) && 'MalQ' %in% genes && !('PulA' %in% genes)) ||
       (!('MalF' %in% genes) && 'GlvB' %in% genes && !('Cga' %in% genes) && !('GlvA' %in% genes) && 'AglA' %in% genes && !('NplT' %in% genes) && 'MalQ' %in% genes && 'PulA' %in% genes && 'MxmX' %in% genes) ||
       (!('MalF' %in% genes) && 'GlvB' %in% genes && !('Cga' %in% genes) && !('GlvA' %in% genes) && 'AglA' %in% genes && 'NplT' %in% genes && !('MxmX' %in% genes)) ||
       (!('MalF' %in% genes) && 'GlvB' %in% genes && !('Cga' %in% genes) && !('GlvA' %in% genes) && 'AglA' %in% genes && 'NplT' %in% genes && 'MxmX' %in% genes && 'MdxE' %in% genes) ||
       (!('MalF' %in% genes) && 'GlvB' %in% genes && !('Cga' %in% genes) && !('GlvA' %in% genes) && 'AglA' %in% genes && 'NplT' %in% genes && 'MxmX' %in% genes && !('MdxE' %in% genes) && !('PulA' %in% genes) && 'GalM' %in% genes && !('AmyA' %in% genes)) ||
       (!('MalF' %in% genes) && 'GlvB' %in% genes && !('Cga' %in% genes) && !('GlvA' %in% genes) && !('AglA' %in% genes) && 'MalQ' %in% genes) ||
       (!('MalF' %in% genes) && 'GlvB' %in% genes && !('Cga' %in% genes) && !('GlvA' %in% genes) && !('AglA' %in% genes) && !('MalQ' %in% genes) && 'MalL' %in% genes && !('AmyAc' %in% genes)) ||
       (!('MalF' %in% genes) && 'GlvB' %in% genes && !('Cga' %in% genes) && !('GlvA' %in% genes) && !('AglA' %in% genes) && !('MalQ' %in% genes) && !('MalL' %in% genes) && 'GlvG' %in% genes) || 
       (!('MalF' %in% genes) && 'GlvB' %in% genes && 'Cga' %in% genes && !('Pgm' %in% genes)) ||
       (!('MalF' %in% genes) && !('GlvB' %in% genes) && 'MalT' %in% genes) ||
       (!('MalF' %in% genes) && !('GlvB' %in% genes) && !('MalT' %in% genes) && 'SusCm' %in% genes && 'SusDm' %in% genes && !('Cga' %in% genes)) ||
       (!('MalF' %in% genes) && !('GlvB' %in% genes) && !('MalT' %in% genes) && 'SusCm' %in% genes && 'SusDm' %in% genes && 'Cga' %in% genes && 'PulA' %in% genes) ||
       (!('MalF' %in% genes) && !('GlvB' %in% genes) && !('MalT' %in% genes) && 'SusCm' %in% genes && 'SusDm' %in% genes && 'Cga' %in% genes && !('PulA' %in% genes) && 'MalP' %in% genes) ||
       (!('MalF' %in% genes) && !('GlvB' %in% genes) && !('MalT' %in% genes) && 'SusCm' %in% genes && !('SusDm' %in% genes) && 'MalP' %in% genes) ||
       (!('MalF' %in% genes) && !('GlvB' %in% genes) && !('MalT' %in% genes) && !('SusCm' %in% genes) && 'MdxF' %in% genes && 'AglA' %in% genes) ||       
       (!('MalF' %in% genes) && !('GlvB' %in% genes) && !('MalT' %in% genes) && !('SusCm' %in% genes) && 'MdxF' %in% genes && !('AglA' %in% genes) && 'MdxE' %in% genes) ||
       (!('MalF' %in% genes) && !('GlvB' %in% genes) && !('MalT' %in% genes) && !('SusCm' %in% genes) && !('MdxF' %in% genes) && 'MalE' %in% genes && !('AglA' %in% genes)) ||
       (!('MalF' %in% genes) && !('GlvB' %in% genes) && !('MalT' %in% genes) && !('SusCm' %in% genes) && !('MdxF' %in% genes) && 'MalE' %in% genes && 'AglA' %in% genes && 'AmyA' %in% genes) ||
       (!('MalF' %in% genes) && !('GlvB' %in% genes) && !('MalT' %in% genes) && !('SusCm' %in% genes) && !('MdxF' %in% genes) && !('MalE' %in% genes) && 'Agl3' %in% genes) ||
       (!('MalF' %in% genes) && !('GlvB' %in% genes) && !('MalT' %in% genes) && !('SusCm' %in% genes) && !('MdxF' %in% genes) && !('MalE' %in% genes) && !('Agl3' %in% genes) && 'SusB2' %in% genes && 'Gde2' %in% genes && 'MalZ' %in% genes) ||
       (!('MalF' %in% genes) && !('GlvB' %in% genes) && !('MalT' %in% genes) && !('SusCm' %in% genes) && !('MdxF' %in% genes) && !('MalE' %in% genes) && !('Agl3' %in% genes) && !('SusB2' %in% genes) && 'PulA' %in% genes && 'AmyA' %in% genes && 'AglA' %in% genes && !('SusA' %in% genes)) ||
       (!('MalF' %in% genes) && !('GlvB' %in% genes) && !('MalT' %in% genes) && !('SusCm' %in% genes) && !('MdxF' %in% genes) && !('MalE' %in% genes) && !('Agl3' %in% genes) && !('SusB2' %in% genes) && !('PulA' %in% genes) && 'MalK' %in% genes && 'GalM' %in% genes && 'MxmX' %in% genes) 
    ) {
      status <- 1
    } else {
      status <- 0
    }
  } else if (subsystem == "Trehalose utilization HMP" && phenotype == 'Tre') {
    if(('TreB_b' %in% genes && 'TreR1' %in% genes) ||
       ('TreB_b' %in% genes && !('TreR1' %in% genes) && 'TreC' %in% genes) ||
       ('TreB_b' %in% genes && !('TreR1' %in% genes) && !('TreC' %in% genes) && 'TP' %in% genes) ||
       (!('TreB_b' %in% genes) && 'TreP2' %in% genes) ||
       (!('TreB_b' %in% genes) && !('TreP2' %in% genes) && 'TreP1' %in% genes) ||
       (!('TreB_b' %in% genes) && !('TreP2' %in% genes) && !('TreP1' %in% genes) && 'TreR1' %in% genes && 'TP' %in% genes)
    ) {
      status <- 1
    } else {
      status <- 0
    }
  } else if (subsystem == "N-Acetyl-Galactosamine and Galactosamine Utilization HMP" && phenotype == 'GalNAc') {
    if(('AgaA' %in% genes && 'AgaS' %in% genes && 'GalNAc_a' %in% genes) ||
       ('AgaA' %in% genes && 'AgaS' %in% genes && !('GalNAc_a' %in% genes) && 'GalNAc_b' %in% genes) ||
       ('AgaA' %in% genes && 'AgaS' %in% genes && !('GalNAc_a' %in% genes) && !('GalNAc_b' %in% genes) && 'GalN2_b' %in% genes) ||
       ('AgaA' %in% genes && 'AgaS' %in% genes && !('GalNAc_a' %in% genes) && !('GalNAc_b' %in% genes) && !('GalN2_b' %in% genes) && !('AgaY' %in% genes)) ||
       ('AgaA' %in% genes && 'AgaS' %in% genes && !('GalNAc_a' %in% genes) && !('GalNAc_b' %in% genes) && !('GalN2_b' %in% genes) && 'AgaY' %in% genes && 'GalN_b' %in% genes) ||
       ('AgaA' %in% genes && 'AgaS' %in% genes && !('GalNAc_a' %in% genes) && !('GalNAc_b' %in% genes) && !('GalN2_b' %in% genes) && 'AgaY' %in% genes && !('GalN_b' %in% genes) && 'GalNAc_d' %in% genes && !('GalNAc_c' %in% genes)) ||
       (!('AgaA' %in% genes) && 'AgaR3' %in% genes && 'AgaY' %in% genes && 'GalNAc_b' %in% genes && !('GalN_c' %in% genes)) 
    ) {
      status <- 1
    } else {
      status <- 0
    }
  } else if (subsystem == "N-Acetyl-Galactosamine and Galactosamine Utilization HMP" && phenotype == 'GalN') {
    if(('AgaS' %in% genes && 'GalNAc_c' %in% genes && 'GalNAc_a' %in% genes) ||
       ('AgaS' %in% genes && 'GalNAc_c' %in% genes && !('GalNAc_a' %in% genes) && 'GalNAc_b' %in% genes) ||
       ('AgaS' %in% genes && 'GalNAc_c' %in% genes && !('GalNAc_a' %in% genes) && !('GalNAc_b' %in% genes) && !('AgaA' %in% genes)) ||
       ('AgaS' %in% genes && !('GalNAc_c' %in% genes) && 'GalN_b' %in% genes && 'AgaZ' %in% genes) ||
       ('AgaS' %in% genes && !('GalNAc_c' %in% genes) && 'GalN_b' %in% genes && !('AgaZ' %in% genes) && 'AgaY' %in% genes) ||
       ('AgaS' %in% genes && !('GalNAc_c' %in% genes) && !('GalN_b' %in% genes) && 'GalN2_c' %in% genes) ||
       ('AgaS' %in% genes && !('GalNAc_c' %in% genes) && !('GalN_b' %in% genes) && !('GalN2_c' %in% genes) && 'GalN_c' %in% genes) ||
       ('AgaS' %in% genes && !('GalNAc_c' %in% genes) && !('GalN_b' %in% genes) && !('GalN2_c' %in% genes) && !('GalN_c' %in% genes) && 'GalNAc_d' %in% genes) ||
       ('AgaS' %in% genes && !('GalNAc_c' %in% genes) && !('GalN_b' %in% genes) && !('GalN2_c' %in% genes) && !('GalN_c' %in% genes) && !('GalNAc_d' %in% genes) && 'GalN2_d' %in% genes) ||
       ('AgaS' %in% genes && !('GalNAc_c' %in% genes) && !('GalN_b' %in% genes) && !('GalN2_c' %in% genes) && !('GalN_c' %in% genes) && !('GalNAc_d' %in% genes) && !('GalN2_d' %in% genes) && 'GalN2_b' %in% genes) ||
       ('AgaS' %in% genes && !('GalNAc_c' %in% genes) && !('GalN_b' %in% genes) && !('GalN2_c' %in% genes) && !('GalN_c' %in% genes) && !('GalNAc_d' %in% genes) && !('GalN2_d' %in% genes) && !('GalN2_b' %in% genes) && 'GalNAc_b' %in% genes && 'AgaA' %in% genes) ||
       (!('AgaS' %in% genes) && 'GalNAc_c' %in% genes && !('AgaA' %in% genes) && 'AgaZ' %in% genes && !('GalN_c' %in% genes) && !('AgaI' %in% genes)) ||
       (!('AgaS' %in% genes) && 'GalNAc_c' %in% genes && !('AgaA' %in% genes) && 'AgaZ' %in% genes && !('GalN_c' %in% genes) && 'AgaI' %in% genes && !('GalNAc_b' %in% genes)) ||
       (!('AgaS' %in% genes) && !('GalNAc_c' %in% genes) && 'GalN_b' %in% genes && 'AgaY' %in% genes) 
    ) {
      status <- 1
    } else {
      status <- 0
    }
  } else if (subsystem == "Sialic acids utilization HMP" && phenotype == 'ManNAc') {
    if(('ManTb' %in% genes && 'NanE1' %in% genes && 'ManTc' %in% genes && !('TRAP(nana)l' %in% genes)) ||
       ('ManTb' %in% genes && 'NanE1' %in% genes && 'ManTc' %in% genes && 'TRAP(nana)l' %in% genes && 'TRAP(nana)s' %in% genes) ||
       ('ManTb' %in% genes && 'NanE1' %in% genes && !('ManTc' %in% genes) && !('NanX1' %in% genes) && !('NanT3' %in% genes)) ||
       ('ManTb' %in% genes && 'NanE1' %in% genes && !('ManTc' %in% genes) && !('NanX1' %in% genes) && 'NanT3' %in% genes && 'NanK' %in% genes) ||
       ('ManTb' %in% genes && !('NanE1' %in% genes) && 'NanA' %in% genes) ||       
       (!('ManTb' %in% genes) && 'ManZ' %in% genes && !('NanX1' %in% genes)) ||
       (!('ManTb' %in% genes) && !('ManZ' %in% genes) && 'ManTa' %in% genes && 'NanE1' %in% genes) ||
       (!('ManTb' %in% genes) && !('ManZ' %in% genes) && !('ManTa' %in% genes) && 'ManTc' %in% genes) ||
       (!('ManTb' %in% genes) && !('ManZ' %in% genes) && !('ManTa' %in% genes) && !('ManTc' %in% genes) && 'ManY' %in% genes && !('NanR7' %in% genes) && 'NanX1' %in% genes) ||
       (!('ManTb' %in% genes) && !('ManZ' %in% genes) && !('ManTa' %in% genes) && !('ManTc' %in% genes) && 'ManY' %in% genes && !('NanR7' %in% genes) && !('NanX1' %in% genes) && 'NanE1' %in% genes && !('YjgK' %in% genes))
    ) {
      status <- 1
    } else {
      status <- 0
    }
  } else if (subsystem == "Sialic acids utilization HMP" && phenotype == 'NANA') {
    if(('NanA' %in% genes && 'NagB' %in% genes && 'YjgK' %in% genes && 'NanK' %in% genes) ||
       ('NanA' %in% genes && 'NagB' %in% genes && 'YjgK' %in% genes && !('NanK' %in% genes) && !('NanT3' %in% genes) && !('NanE1' %in% genes)) ||
       ('NanA' %in% genes && 'NagB' %in% genes && 'YjgK' %in% genes && !('NanK' %in% genes) && !('NanT3' %in% genes) && 'NanE1' %in% genes && 'NanX1' %in% genes) ||
       ('NanA' %in% genes && 'NagB' %in% genes && !('YjgK' %in% genes) && !('ManZ' %in% genes) && !('NanR1' %in% genes) && 'NanT2' %in% genes) ||
       ('NanA' %in% genes && 'NagB' %in% genes && !('YjgK' %in% genes) && !('ManZ' %in% genes) && !('NanR1' %in% genes) && !('NanT2' %in% genes) && 'NanE1' %in% genes && 'NanX1' %in% genes) ||
       ('NanA' %in% genes && 'NagB' %in% genes && !('YjgK' %in% genes) && !('ManZ' %in% genes) && !('NanR1' %in% genes) && !('NanT2' %in% genes) && 'NanE1' %in% genes && !('NanX1' %in% genes) && 'NanT3' %in% genes) ||
       ('NanA' %in% genes && 'NagB' %in% genes && !('YjgK' %in% genes) && !('ManZ' %in% genes) && !('NanR1' %in% genes) && !('NanT2' %in% genes) && 'NanE1' %in% genes && !('NanX1' %in% genes) && !('NanT3' %in% genes) && 'NanT1' %in% genes) ||
       ('NanA' %in% genes && 'NagB' %in% genes && !('YjgK' %in% genes) && !('ManZ' %in% genes) && !('NanR1' %in% genes) && !('NanT2' %in% genes) && 'NanE1' %in% genes && !('NanX1' %in% genes) && !('NanT3' %in% genes) && !('NanT1' %in% genes) && 'TRAP(nana)p' %in% genes) ||
       ('NanA' %in% genes && 'NagB' %in% genes && !('YjgK' %in% genes) && !('ManZ' %in% genes) && !('NanR1' %in% genes) && !('NanT2' %in% genes) && 'NanE1' %in% genes && !('NanX1' %in% genes) && !('NanT3' %in% genes) && !('NanT1' %in% genes) && !('TRAP(nana)p' %in% genes) && 'NanB' %in% genes) ||
       ('NanA' %in% genes && 'NagB' %in% genes && !('YjgK' %in% genes) && !('ManZ' %in% genes) && !('NanR1' %in% genes) && !('NanT2' %in% genes) && 'NanE1' %in% genes && !('NanX1' %in% genes) && !('NanT3' %in% genes) && !('NanT1' %in% genes) && !('TRAP(nana)p' %in% genes) && !('NanB' %in% genes) && 'NanY' %in% genes) ||
       ('NanA' %in% genes && 'NagB' %in% genes && !('YjgK' %in% genes) && !('ManZ' %in% genes) && !('NanR1' %in% genes) && !('NanT2' %in% genes) && 'NanE1' %in% genes && !('NanX1' %in% genes) && !('NanT3' %in% genes) && !('NanT1' %in% genes) && !('TRAP(nana)p' %in% genes) && !('NanB' %in% genes) && !('NanY' %in% genes) && 'NanS' %in% genes) ||
       ('NanA' %in% genes && 'NagB' %in% genes && !('YjgK' %in% genes) && !('ManZ' %in% genes) && !('NanR1' %in% genes) && !('NanT2' %in% genes) && !('NanE1' %in% genes) && 'NanE2' %in% genes && 'NanT3' %in% genes) ||
       ('NanA' %in% genes && 'NagB' %in% genes && !('YjgK' %in% genes) && !('ManZ' %in% genes) && !('NanR1' %in% genes) && !('NanT2' %in% genes) && !('NanE1' %in% genes) && 'NanE2' %in% genes && !('NanT3' %in% genes) && 'NanX1' %in% genes) ||
       ('NanA' %in% genes && 'NagB' %in% genes && !('YjgK' %in% genes) && !('ManZ' %in% genes) && !('NanR1' %in% genes) && !('NanT2' %in% genes) && !('NanE1' %in% genes) && 'NanE2' %in% genes && !('NanT3' %in% genes) && !('NanX1' %in% genes) && 'TRAP(nana)s' %in% genes) ||
       ('NanA' %in% genes && 'NagB' %in% genes && !('YjgK' %in% genes) && !('ManZ' %in% genes) && 'NanR1' %in% genes && 'NanK' %in% genes) ||
       ('NanA' %in% genes && 'NagB' %in% genes && !('YjgK' %in% genes) && 'ManZ' %in% genes && 'NanE2' %in% genes) ||
       ('NanA' %in% genes && !('NagB' %in% genes) && 'NanK' %in% genes) ||
       (!('NanA' %in% genes) && 'NanX1' %in% genes && 'NanK' %in% genes)       
    ) {
      status <- 1
    } else {
      status <- 0
    }
  } else if (subsystem == "Unsaturated glucuronate utilization HMP" && phenotype == 'ddGlcA') {
    if(('KdgT' %in% genes && 'KdgK1' %in% genes && 'KdgA' %in% genes) ||
       ('KdgT' %in% genes && 'KdgK1' %in% genes && !('KdgA' %in% genes) && !('KduI' %in% genes))
    ) {
      status <- 1
    } else {
      status <- 0
    }
  } else if (subsystem == "Rhamnose and rhamnosides utilization HMP" && phenotype == 'Rha') {
    if(('RhaT' %in% genes && 'RhaA' %in% genes) ||
       ('RhaT' %in% genes && !('RhaA' %in% genes) && 'LraM' %in% genes) ||
       (!('RhaT' %in% genes) && 'RhaG' %in% genes && 'RhaF' %in% genes && 'RhaH' %in% genes) ||
       (!('RhaT' %in% genes) && !('RhaG' %in% genes) && 'RhaY' %in% genes && 'RhaA' %in% genes) ||
       (!('RhaT' %in% genes) && !('RhaG' %in% genes) && 'RhaY' %in% genes && !('RhaA' %in% genes) && 'RhaE' %in% genes) ||
       (!('RhaT' %in% genes) && !('RhaG' %in% genes) && !('RhaY' %in% genes) && 'RhaH' %in% genes && !('RhaR1' %in% genes)) ||
       (!('RhaT' %in% genes) && !('RhaG' %in% genes) && !('RhaY' %in% genes) && !('RhaH' %in% genes) && 'RhaM1' %in% genes && !('RhiN' %in% genes) && !('RamA' %in% genes) && 'RhaB' %in% genes)
    ) {
      status <- 1
    } else {
      status <- 0
    }
  } else if (subsystem == "Rhamnose and rhamnosides utilization HMP" && phenotype == 'Rhi') {
    if(('RhiN' %in% genes && 'RhaA' %in% genes && 'RhaR1' %in% genes && !('RamX2' %in% genes) && !('RhaY' %in% genes) && !('RamV2' %in% genes)) ||
       ('RhiN' %in% genes && 'RhaA' %in% genes && 'RhaR1' %in% genes && !('RamX2' %in% genes) && !('RhaY' %in% genes) && 'RamV2' %in% genes && 'RhiG' %in% genes) ||
       ('RhiN' %in% genes && 'RhaA' %in% genes && 'RhaR1' %in% genes && !('RamX2' %in% genes) && 'RhaY' %in% genes && 'RhiA' %in% genes) ||
       ('RhiN' %in% genes && 'RhaA' %in% genes && 'RhaR1' %in% genes && !('RamX2' %in% genes) && 'RhaY' %in% genes && !('RhiA' %in% genes) && 'YesX' %in% genes) ||
       ('RhiN' %in% genes && 'RhaA' %in% genes && 'RhaR1' %in% genes && 'RamX2' %in% genes && 'RhiG' %in% genes) ||
       ('RhiN' %in% genes && 'RhaA' %in% genes && 'RhaR1' %in% genes && 'RamX2' %in% genes && !('RhiG' %in% genes) && 'RhiZ' %in% genes) ||
       ('RhiN' %in% genes && 'RhaA' %in% genes && !('RhaR1' %in% genes) && 'RhiT' %in% genes) ||
       ('RhiN' %in% genes && 'RhaA' %in% genes && !('RhaR1' %in% genes) && !('RhiT' %in% genes) && 'YesX' %in% genes) ||
       ('RhiN' %in% genes && 'RhaA' %in% genes && !('RhaR1' %in% genes) && !('RhiT' %in% genes) && !('YesX' %in% genes) && 'RhiC' %in% genes) ||
       ('RhiN' %in% genes && 'RhaA' %in% genes && !('RhaR1' %in% genes) && !('RhiT' %in% genes) && !('YesX' %in% genes) && !('RhiC' %in% genes) && !('RhaW' %in% genes) && !('RhaW1' %in% genes) && !('RhaR2' %in% genes) && 'RamA' %in% genes) ||
       ('RhiN' %in% genes && 'RhaA' %in% genes && !('RhaR1' %in% genes) && !('RhiT' %in% genes) && !('YesX' %in% genes) && !('RhiC' %in% genes) && !('RhaW' %in% genes) && !('RhaW1' %in% genes) && !('RhaR2' %in% genes) && !('RamA' %in% genes) && !('RhaM1' %in% genes)) ||
       ('RhiN' %in% genes && 'RhaA' %in% genes && !('RhaR1' %in% genes) && !('RhiT' %in% genes) && !('YesX' %in% genes) && !('RhiC' %in% genes) && !('RhaW' %in% genes) && !('RhaW1' %in% genes) && !('RhaR2' %in% genes) && !('RamA' %in% genes) && 'RhaM1' %in% genes && !('RhaT' %in% genes)) ||
       ('RhiN' %in% genes && 'RhaA' %in% genes && !('RhaR1' %in% genes) && !('RhiT' %in% genes) && !('YesX' %in% genes) && !('RhiC' %in% genes) && !('RhaW' %in% genes) && !('RhaW1' %in% genes) && 'RhaR2' %in% genes && 'RhiG' %in% genes) ||
       ('RhiN' %in% genes && !('RhaA' %in% genes) && 'LraM' %in% genes && 'RhiB' %in% genes) ||
       ('RhiN' %in% genes && !('RhaA' %in% genes) && !('LraM' %in% genes) && 'RhaE' %in% genes) ||
       ('RhiN' %in% genes && !('RhaA' %in% genes) && !('LraM' %in% genes) && !('RhaE' %in% genes) && 'RhaM2' %in% genes) ||
       (!('RhiN' %in% genes) && 'RamA' %in% genes && 'RhaT' %in% genes) ||
       (!('RhiN' %in% genes) && 'RamA' %in% genes && !('RhaT' %in% genes) && 'RhaA' %in% genes && 'RamT' %in% genes && !('RhaW1' %in% genes)) ||
       (!('RhiN' %in% genes) && 'RamA' %in% genes && !('RhaT' %in% genes) && 'RhaA' %in% genes && !('RamT' %in% genes) && 'RhiT' %in% genes && 'RhaD' %in% genes && !('RhaW' %in% genes) && !('RhaF' %in% genes)) ||
       (!('RhiN' %in% genes) && 'RamA' %in% genes && !('RhaT' %in% genes) && 'RhaA' %in% genes && !('RamT' %in% genes) && 'RhiT' %in% genes && 'RhaD' %in% genes && !('RhaW' %in% genes) && 'RhaF' %in% genes && 'RhaM1' %in% genes) ||
       (!('RhiN' %in% genes) && 'RamA' %in% genes && !('RhaT' %in% genes) && 'RhaA' %in% genes && !('RamT' %in% genes) && 'RhiT' %in% genes && 'RhaD' %in% genes && !('RhaW' %in% genes) && 'RhaF' %in% genes && !('RhaM1' %in% genes) && 'RhiG' %in% genes) ||
       (!('RhiN' %in% genes) && 'RamA' %in% genes && !('RhaT' %in% genes) && 'RhaA' %in% genes && !('RamT' %in% genes) && 'RhiT' %in% genes && 'RhaD' %in% genes && 'RhaW' %in% genes && 'RamU' %in% genes) ||
       (!('RhiN' %in% genes) && 'RamA' %in% genes && !('RhaT' %in% genes) && 'RhaA' %in% genes && !('RamT' %in% genes) && !('RhiT' %in% genes) && 'RhiG' %in% genes) ||
       (!('RhiN' %in% genes) && 'RamA' %in% genes && !('RhaT' %in% genes) && 'RhaA' %in% genes && !('RamT' %in% genes) && !('RhiT' %in% genes) && !('RhiG' %in% genes) && 'RhaM2' %in% genes) ||
       (!('RhiN' %in% genes) && 'RamA' %in% genes && !('RhaT' %in% genes) && 'RhaA' %in% genes && !('RamT' %in% genes) && !('RhiT' %in% genes) && !('RhiG' %in% genes) && !('RhaM2' %in% genes) && 'RamU' %in% genes) ||
       (!('RhiN' %in% genes) && 'RamA' %in% genes && !('RhaT' %in% genes) && 'RhaA' %in% genes && !('RamT' %in% genes) && !('RhiT' %in% genes) && !('RhiG' %in% genes) && !('RhaM2' %in% genes) && !('RamU' %in% genes) && 'RamX2' %in% genes && 'RhaM1' %in% genes) ||
       (!('RhiN' %in% genes) && !('RamA' %in% genes) && 'RhiT' %in% genes && 'RhaT' %in% genes)
    ) {
      status <- 1
    } else {
      status <- 0
    }
  } else if (subsystem == "N-acetylmuramic acid HMP" && phenotype == 'MurNac') {
    if(('MurY' %in% genes && 'MurQ' %in% genes) ||
       (!('MurY' %in% genes) && 'MurT' %in% genes) ||
       (!('MurY' %in% genes) && !('MurT' %in% genes) && 'MurA' %in% genes) ||
       (!('MurY' %in% genes) && !('MurT' %in% genes) && !('MurA' %in% genes) && 'MurO' %in% genes && !('MurQ' %in% genes) && 'MurK2' %in% genes) 
    ) {
      status <- 1
    } else {
      status <- 0
    }
  } else if (subsystem == "Beta-galactosides utilization HMP" && phenotype == 'Lac') {
    if(('LacG' %in% genes && 'LacEb' %in% genes) ||
       ('LacG' %in% genes && !('LacEb' %in% genes) && 'LacL' %in% genes) ||
       ('LacG' %in% genes && !('LacEb' %in% genes) && !('LacL' %in% genes) && 'LacS' %in% genes) ||
       ('LacG' %in% genes && !('LacEb' %in% genes) && !('LacL' %in% genes) && !('LacS' %in% genes) && 'LacF' %in% genes) ||
       (!('LacG' %in% genes) && 'LacS' %in% genes && 'Bga2A' %in% genes) ||
       (!('LacG' %in% genes) && 'LacS' %in% genes && !('Bga2A' %in% genes) && 'LacZ' %in% genes) ||
       (!('LacG' %in% genes) && 'LacS' %in% genes && !('Bga2A' %in% genes) && !('LacZ' %in% genes) && 'LacL' %in% genes) ||
       (!('LacG' %in% genes) && 'LacS' %in% genes && !('Bga2A' %in% genes) && !('LacZ' %in% genes) && !('LacL' %in% genes) && 'LacZ_old' %in% genes) ||
       (!('LacG' %in% genes) && 'LacS' %in% genes && !('Bga2A' %in% genes) && !('LacZ' %in% genes) && !('LacL' %in% genes) && !('LacZ_old' %in% genes) && 'Bga42A' %in% genes) ||
       (!('LacG' %in% genes) && 'LacS' %in% genes && !('Bga2A' %in% genes) && !('LacZ' %in% genes) && !('LacL' %in% genes) && !('LacZ_old' %in% genes) && !('Bga42A' %in% genes) && 'EbgA' %in% genes) ||
       (!('LacG' %in% genes) && !('LacS' %in% genes) && 'LacY' %in% genes && !('LacZ_old' %in% genes) && !('BgaT' %in% genes)) ||
       (!('LacG' %in% genes) && !('LacS' %in% genes) && 'LacY' %in% genes && !('LacZ_old' %in% genes) && 'BgaT' %in% genes && 'BgaX' %in% genes) ||
       (!('LacG' %in% genes) && !('LacS' %in% genes) && 'LacY' %in% genes && !('LacZ_old' %in% genes) && 'BgaT' %in% genes && !('BgaX' %in% genes) && !('LacL' %in% genes)) ||
       (!('LacG' %in% genes) && !('LacS' %in% genes) && 'LacY' %in% genes && 'LacZ_old' %in% genes && 'LacI' %in% genes) ||
       (!('LacG' %in% genes) && !('LacS' %in% genes) && 'LacY' %in% genes && 'LacZ_old' %in% genes && !('LacI' %in% genes) && 'LacL' %in% genes) ||
       (!('LacG' %in% genes) && !('LacS' %in% genes) && 'LacY' %in% genes && 'LacZ_old' %in% genes && !('LacI' %in% genes) && !('LacL' %in% genes) && 'BgaX' %in% genes) ||
       (!('LacG' %in% genes) && !('LacS' %in% genes) && !('LacY' %in% genes) && 'SusC_bga' %in% genes) ||
       (!('LacG' %in% genes) && !('LacS' %in% genes) && !('LacY' %in% genes) && !('SusC_bga' %in% genes) && 'BgaZ' %in% genes && 'EbgR' %in% genes && !('EbgA' %in% genes))
    ) {
      status <- 1
    } else {
      status <- 0
    }
  } else if (subsystem == "Beta-galactosides utilization HMP" && phenotype == 'GOS') {
    if(('Bga2A' %in% genes) ||
       (!('Bga2A' %in% genes) && 'BgaX' %in% genes && 'BgaY' %in% genes && 'BgaZ' %in% genes) ||
       (!('Bga2A' %in% genes) && 'BgaX' %in% genes && 'BgaY' %in% genes && !('BgaZ' %in% genes) && !('LacS' %in% genes) && !('BguR3' %in% genes)) ||
       (!('Bga2A' %in% genes) && 'BgaX' %in% genes && !('BgaY' %in% genes) && !('LacY' %in% genes)) ||
       (!('Bga2A' %in% genes) && 'BgaX' %in% genes && !('BgaY' %in% genes) && 'LacY' %in% genes && 'EbgA' %in% genes && 'BgaZ' %in% genes) ||
       (!('Bga2A' %in% genes) && !('BgaX' %in% genes) && 'SusC_bga' %in% genes) ||
       (!('Bga2A' %in% genes) && !('BgaX' %in% genes) && !('SusC_bga' %in% genes) && 'BgaT' %in% genes && !('LacY' %in% genes)) ||
       (!('Bga2A' %in% genes) && !('BgaX' %in% genes) && !('SusC_bga' %in% genes) && 'BgaT' %in% genes && 'LacY' %in% genes && 'LacL' %in% genes) ||
       (!('Bga2A' %in% genes) && !('BgaX' %in% genes) && !('SusC_bga' %in% genes) && !('BgaT' %in% genes) && 'Bga42B' %in% genes) ||
       (!('Bga2A' %in% genes) && !('BgaX' %in% genes) && !('SusC_bga' %in% genes) && !('BgaT' %in% genes) && !('Bga42B' %in% genes) && 'BgaY' %in% genes && 'LacS' %in% genes)
    ) {
      status <- 1
    } else {
      status <- 0
    }
  } else if (subsystem == "Histidine degradation HMP" && phenotype == 'His_d') {
    if(('HutI' %in% genes && 'HutH' %in% genes && 'HutU' %in% genes && 'HutG' %in% genes) ||
       ('HutI' %in% genes && 'HutH' %in% genes && 'HutU' %in% genes && !('HutG' %in% genes) && !('HutT2' %in% genes) && 'GluF' %in% genes) ||
       ('HutI' %in% genes && 'HutH' %in% genes && 'HutU' %in% genes && !('HutG' %in% genes) && !('HutT2' %in% genes) && !('GluF' %in% genes) && 'HutF' %in% genes && 'NfoD2' %in% genes) ||
       ('HutI' %in% genes && 'HutH' %in% genes && 'HutU' %in% genes && !('HutG' %in% genes) && !('HutT2' %in% genes) && !('GluF' %in% genes) && 'HutF' %in% genes && !('NfoD2' %in% genes) && 'NfoD' %in% genes) ||
       ('HutI' %in% genes && 'HutH' %in% genes && 'HutU' %in% genes && !('HutG' %in% genes) && !('HutT2' %in% genes) && !('GluF' %in% genes) && !('HutF' %in% genes) && 'HutP' %in% genes) ||
       ('HutI' %in% genes && 'HutH' %in% genes && 'HutU' %in% genes && !('HutG' %in% genes) && 'HutT2' %in% genes && 'ForC' %in% genes) ||
       ('HutI' %in% genes && 'HutH' %in% genes && 'HutU' %in% genes && !('HutG' %in% genes) && 'HutT2' %in% genes && !('ForC' %in% genes) && 'NfoD' %in% genes) ||
       ('HutI' %in% genes && !('HutH' %in% genes) && 'HisM' %in% genes) ||       
       (!('HutI' %in% genes) && 'HutU' %in% genes && 'ForC' %in% genes)
    ) {
      status <- 1
    } else {
      status <- 0
    }
  } else if (subsystem == "Lysine degradation HMP" && phenotype == 'Lys_d') {
    if(('PatA' %in% genes && 'LysP' %in% genes && 'Prr2' %in% genes && 'LDC2' %in% genes) ||
       ('PatA' %in% genes && 'LysP' %in% genes && 'Prr2' %in% genes && !('LDC2' %in% genes) && 'CadA' %in% genes) ||
       ('PatA' %in% genes && 'LysP' %in% genes && !('Prr2' %in% genes) && 'KamA' %in% genes) ||
       ('PatA' %in% genes && 'LysP' %in% genes && !('Prr2' %in% genes) && !('KamA' %in% genes) && 'LDC2' %in% genes) ||
       ('PatA' %in% genes && !('LysP' %in% genes) && 'DavB' %in% genes) ||
       (!('PatA' %in% genes) && 'YodS' %in% genes && 'YodQ' %in% genes) ||
       (!('PatA' %in% genes) && 'YodS' %in% genes && !('YodQ' %in% genes) && 'KamD' %in% genes) ||
       (!('PatA' %in% genes) && !('YodS' %in% genes) && 'KDD' %in% genes && 'KamE' %in% genes) ||
       (!('PatA' %in% genes) && !('YodS' %in% genes) && !('KDD' %in% genes) && 'DavB' %in% genes && 'DavA' %in% genes) ||
       (!('PatA' %in% genes) && !('YodS' %in% genes) && !('KDD' %in% genes) && !('DavB' %in% genes) && 'KamE' %in% genes && 'KCE' %in% genes)
    ) {
     status <- 1
    } else {
     status <- 0
    }
  } else if (subsystem == "Beta-glucosides utilization HMP" && phenotype == 'Bgl') {
    if(('AscB' %in% genes && 'CelA' %in% genes) ||
       ('AscB' %in% genes && !('CelA' %in% genes) && 'BglFb' %in% genes) ||
       ('AscB' %in% genes && !('CelA' %in% genes) && !('BglFb' %in% genes) && 'AscFc' %in% genes) ||
       ('AscB' %in% genes && !('CelA' %in% genes) && !('BglFb' %in% genes) && !('AscFc' %in% genes) && 'CelD' %in% genes && 'CbpA' %in% genes) ||
       ('AscB' %in% genes && !('CelA' %in% genes) && !('BglFb' %in% genes) && !('AscFc' %in% genes) && 'CelD' %in% genes && !('CbpA' %in% genes) && 'CelB1' %in% genes) ||
       ('AscB' %in% genes && !('CelA' %in% genes) && !('BglFb' %in% genes) && !('AscFc' %in% genes) && 'CelD' %in% genes && !('CbpA' %in% genes) && !('CelB1' %in% genes) && 'CelB2' %in% genes) ||
       ('AscB' %in% genes && !('CelA' %in% genes) && !('BglFb' %in% genes) && !('AscFc' %in% genes) && !('CelD' %in% genes) && 'Bgl1' %in% genes) ||
       ('AscB' %in% genes && !('CelA' %in% genes) && !('BglFb' %in% genes) && !('AscFc' %in% genes) && !('CelD' %in% genes) && !('Bgl1' %in% genes) && 'BglR' %in% genes && !('BglA' %in% genes)) ||
       ('AscB' %in% genes && !('CelA' %in% genes) && !('BglFb' %in% genes) && !('AscFc' %in% genes) && !('CelD' %in% genes) && !('Bgl1' %in% genes) && !('BglR' %in% genes) && 'BglG' %in% genes && 'BglA3' %in% genes) ||
       (!('AscB' %in% genes) && 'BglX' %in% genes && !('BglA2' %in% genes)) ||
       (!('AscB' %in% genes) && 'BglX' %in% genes && 'BglA2' %in% genes && 'BglA' %in% genes) ||
       (!('AscB' %in% genes) && !('BglX' %in% genes) && 'SusC_bgl' %in% genes && 'LamA' %in% genes) ||
       (!('AscB' %in% genes) && !('BglX' %in% genes) && 'SusC_bgl' %in% genes && !('LamA' %in% genes) && 'BglA3' %in% genes) ||
       (!('AscB' %in% genes) && !('BglX' %in% genes) && !('SusC_bgl' %in% genes) && 'BglFb' %in% genes && 'BglA' %in% genes) ||
       (!('AscB' %in% genes) && !('BglX' %in% genes) && !('SusC_bgl' %in% genes) && 'BglFb' %in% genes && !('BglA' %in% genes) && 'BglA2' %in% genes) ||
       (!('AscB' %in% genes) && !('BglX' %in% genes) && !('SusC_bgl' %in% genes) && 'BglFb' %in% genes && !('BglA' %in% genes) && !('BglA2' %in% genes) && 'CelD' %in% genes && 'BglG' %in% genes && !('CelB1' %in% genes)) ||
       (!('AscB' %in% genes) && !('BglX' %in% genes) && !('SusC_bgl' %in% genes) && !('BglFb' %in% genes) && 'CldE' %in% genes) ||
       (!('AscB' %in% genes) && !('BglX' %in% genes) && !('SusC_bgl' %in% genes) && !('BglFb' %in% genes) && !('CldE' %in% genes) && 'CelB1' %in% genes && 'BglA' %in% genes && 'CelD' %in% genes) ||
       (!('AscB' %in% genes) && !('BglX' %in% genes) && !('SusC_bgl' %in% genes) && !('BglFb' %in% genes) && !('CldE' %in% genes) && !('CelB1' %in% genes) && 'SusD_bgl' %in% genes) ||
       (!('AscB' %in% genes) && !('BglX' %in% genes) && !('SusC_bgl' %in% genes) && !('BglFb' %in% genes) && !('CldE' %in% genes) && !('CelB1' %in% genes) && !('SusD_bgl' %in% genes) && 'BglX2' %in% genes) ||
       (!('AscB' %in% genes) && !('BglX' %in% genes) && !('SusC_bgl' %in% genes) && !('BglFb' %in% genes) && !('CldE' %in% genes) && !('CelB1' %in% genes) && !('SusD_bgl' %in% genes) && !('BglX2' %in% genes) && 'CelB2' %in% genes && 'BglA' %in% genes)
    ) {
      status <- 1
    } else {
      status <- 0
    }
  } else if (subsystem == "HMP Inositol" && phenotype == "Ino") {
    if(('IolB' %in% genes && !('ThuA' %in% genes) && 'IolE' %in% genes && 'IolD' %in% genes && 'IolC' %in% genes && 'IolR2' %in% genes && !('IolR' %in% genes)) ||
       ('IolB' %in% genes && !('ThuA' %in% genes) && 'IolE' %in% genes && 'IolD' %in% genes && 'IolC' %in% genes && 'IolR2' %in% genes && 'IolR' %in% genes && 'IolF' %in% genes) ||
       ('IolB' %in% genes && !('ThuA' %in% genes) && 'IolE' %in% genes && 'IolD' %in% genes && 'IolC' %in% genes && 'IolR2' %in% genes && 'IolR' %in% genes && !('IolF' %in% genes) && 'InoX' %in% genes) ||
       ('IolB' %in% genes && !('ThuA' %in% genes) && 'IolE' %in% genes && 'IolD' %in% genes && 'IolC' %in% genes && 'IolR2' %in% genes && 'IolR' %in% genes && !('IolF' %in% genes) && !('InoX' %in% genes) && !('IolG' %in% genes)) ||
       ('IolB' %in% genes && !('ThuA' %in% genes) && 'IolE' %in% genes && 'IolD' %in% genes && 'IolC' %in% genes && !('IolR2' %in% genes) && 'IolT' %in% genes) ||
       ('IolB' %in% genes && !('ThuA' %in% genes) && 'IolE' %in% genes && 'IolD' %in% genes && 'IolC' %in% genes && !('IolR2' %in% genes) && !('IolT' %in% genes) && 'NaIT' %in% genes) ||
       ('IolB' %in% genes && !('ThuA' %in% genes) && 'IolE' %in% genes && 'IolD' %in% genes && 'IolC' %in% genes && !('IolR2' %in% genes) && !('IolT' %in% genes) && !('NaIT' %in% genes) && 'IolT2' %in% genes) ||
       ('IolB' %in% genes && !('ThuA' %in% genes) && 'IolE' %in% genes && 'IolD' %in% genes && 'IolC' %in% genes && !('IolR2' %in% genes) && !('IolT' %in% genes) && !('NaIT' %in% genes) && !('IolT2' %in% genes) && 'InoX' %in% genes) ||
       ('IolB' %in% genes && !('ThuA' %in% genes) && 'IolE' %in% genes && 'IolD' %in% genes && 'IolC' %in% genes && !('IolR2' %in% genes) && !('IolT' %in% genes) && !('NaIT' %in% genes) && !('IolT2' %in% genes) && !('InoX' %in% genes) && 'InoY' %in% genes) ||
       ('IolB' %in% genes && !('ThuA' %in% genes) && 'IolE' %in% genes && 'IolD' %in% genes && 'IolC' %in% genes && !('IolR2' %in% genes) && !('IolT' %in% genes) && !('NaIT' %in% genes) && !('IolT2' %in% genes) && !('InoX' %in% genes) && !('InoY' %in% genes) && 'IolQ' %in% genes) ||
       ('IolB' %in% genes && !('ThuA' %in% genes) && 'IolE' %in% genes && 'IolD' %in% genes && !('IolC' %in% genes) && 'IolG' %in% genes && 'IolA' %in% genes) ||
       ('IolB' %in% genes && !('ThuA' %in% genes) && 'IolE' %in% genes && 'IolD' %in% genes && !('IolC' %in% genes) && 'IolG' %in% genes && !('IolA' %in% genes) && 'IolI' %in% genes) ||
       ('IolB' %in% genes && !('ThuA' %in% genes) && 'IolE' %in% genes && 'IolD' %in% genes && !('IolC' %in% genes) && 'IolG' %in% genes && !('IolA' %in% genes) && !('IolI' %in% genes) && 'IolT2' %in% genes) ||
       ('IolB' %in% genes && !('ThuA' %in% genes) && 'IolE' %in% genes && !('IolD' %in% genes) && 'IolG' %in% genes && !('IolG1' %in% genes)) ||
       ('IolB' %in% genes && !('ThuA' %in% genes) && 'IolE' %in% genes && !('IolD' %in% genes) && 'IolG' %in% genes && 'IolG1' %in% genes && 'IolT2' %in% genes) ||
       ('IolB' %in% genes && !('ThuA' %in% genes) && 'IolE' %in% genes && !('IolD' %in% genes) && 'IolG' %in% genes && 'IolG1' %in% genes && !('IolT2' %in% genes) && 'InoX' %in% genes) ||
       ('IolB' %in% genes && !('ThuA' %in% genes) && 'IolE' %in% genes && !('IolD' %in% genes) && 'IolG' %in% genes && 'IolG1' %in% genes && !('IolT2' %in% genes) && !('InoX' %in% genes) && !('IolC' %in% genes)) ||
       ('IolB' %in% genes && !('ThuA' %in% genes) && 'IolE' %in% genes && !('IolD' %in% genes) && 'IolG' %in% genes && 'IolG1' %in% genes && !('IolT2' %in% genes) && !('InoX' %in% genes) && 'IolC' %in% genes && 'IolR2' %in% genes) ||
       ('IolB' %in% genes && !('ThuA' %in% genes) && 'IolE' %in% genes && !('IolD' %in% genes) && !('IolG' %in% genes) && 'IolG2' %in% genes) || 
       ('IolB' %in% genes && !('ThuA' %in% genes) && !('IolE' %in% genes) && 'InoX' %in% genes) ||       
       ('IolB' %in% genes && 'ThuA' %in% genes && 'IolG1' %in% genes && 'IolR' %in% genes) ||
       ('IolB' %in% genes && 'ThuA' %in% genes && 'IolG1' %in% genes && !('IolR' %in% genes) && 'InoY' %in% genes) ||
       ('IolB' %in% genes && 'ThuA' %in% genes && 'IolG1' %in% genes && !('IolR' %in% genes) && !('InoY' %in% genes) && 'IolJ' %in% genes) ||
       (!('IolB' %in% genes) && 'InoZ' %in% genes && 'IolG' %in% genes) ||
       (!('IolB' %in% genes) && 'InoZ' %in% genes && !('IolG' %in% genes) && 'IolG1' %in% genes && 'InoY' %in% genes) ||
       (!('IolB' %in% genes) && 'InoZ' %in% genes && !('IolG' %in% genes) && 'IolG1' %in% genes && !('InoY' %in% genes) && 'IolE' %in% genes) ||
       (!('IolB' %in% genes) && !('InoZ' %in% genes) && 'InoF' %in% genes) ||
       (!('IolB' %in% genes) && !('InoZ' %in% genes) && !('InoF' %in% genes) && 'IolQ' %in% genes) ||
       (!('IolB' %in% genes) && !('InoZ' %in% genes) && !('InoF' %in% genes) && !('IolQ' %in% genes) && 'InoY' %in% genes && 'IolG' %in% genes && 'IolI' %in% genes) ||
       (!('IolB' %in% genes) && !('InoZ' %in% genes) && !('InoF' %in% genes) && !('IolQ' %in% genes) && 'InoY' %in% genes && 'IolG' %in% genes && !('IolI' %in% genes) && 'IolE' %in% genes) ||
       (!('IolB' %in% genes) && !('InoZ' %in% genes) && !('InoF' %in% genes) && !('IolQ' %in% genes) && !('InoY' %in% genes) && 'IolJ2' %in% genes) ||
       (!('IolB' %in% genes) && !('InoZ' %in% genes) && !('InoF' %in% genes) && !('IolQ' %in% genes) && !('InoY' %in% genes) && !('IolJ2' %in% genes) && 'IolD' %in% genes && 'IolG1' %in% genes) ||
       (!('IolB' %in% genes) && !('InoZ' %in% genes) && !('InoF' %in% genes) && !('IolQ' %in% genes) && !('InoY' %in% genes) && !('IolJ2' %in% genes) && !('IolD' %in% genes) && 'ThuA' %in% genes && !('IolG' %in% genes)) ||
       (!('IolB' %in% genes) && !('InoZ' %in% genes) && !('InoF' %in% genes) && !('IolQ' %in% genes) && !('InoY' %in% genes) && !('IolJ2' %in% genes) && !('IolD' %in% genes) && !('ThuA' %in% genes) && 'IolT' %in% genes && 'IolG1' %in% genes && 'IolI' %in% genes)
    ) {
      status <- 1
    } else {
      status <- 0
    }
  } else if (subsystem == "Xylose and xylosides utilization HMP" && phenotype == 'Xyl') {
    if(('XylA' %in% genes && !('XynPTSb' %in% genes) && 'XylG' %in% genes && !('AXH' %in% genes)) ||
       ('XylA' %in% genes && !('XynPTSb' %in% genes) && 'XylG' %in% genes && 'AXH' %in% genes && !('XylA2' %in% genes)) ||
       ('XylA' %in% genes && !('XynPTSb' %in% genes) && !('XylG' %in% genes) && 'XylE' %in% genes && 'XylB' %in% genes) ||
       ('XylA' %in% genes && !('XynPTSb' %in% genes) && !('XylG' %in% genes) && !('XylE' %in% genes) && 'FruE_bif' %in% genes && !('RexA' %in% genes)) ||
       ('XylA' %in% genes && !('XynPTSb' %in% genes) && !('XylG' %in% genes) && !('XylE' %in% genes) && !('FruE_bif' %in% genes) && 'XylT' %in% genes) ||
       ('XylA' %in% genes && !('XynPTSb' %in% genes) && !('XylG' %in% genes) && !('XylE' %in% genes) && !('FruE_bif' %in% genes) && !('XylT' %in% genes) && 'XylH' %in% genes) ||
       ('XylA' %in% genes && !('XynPTSb' %in% genes) && !('XylG' %in% genes) && !('XylE' %in% genes) && !('FruE_bif' %in% genes) && !('XylT' %in% genes) && !('XylH' %in% genes) && 'XynA2' %in% genes) ||
       ('XylA' %in% genes && !('XynPTSb' %in% genes) && !('XylG' %in% genes) && !('XylE' %in% genes) && !('FruE_bif' %in% genes) && !('XylT' %in% genes) && !('XylH' %in% genes) && !('XynA2' %in% genes) && 'XylU' %in% genes && !('AguA' %in% genes)) ||
       ('XylA' %in% genes && !('XynPTSb' %in% genes) && !('XylG' %in% genes) && !('XylE' %in% genes) && !('FruE_bif' %in% genes) && !('XylT' %in% genes) && !('XylH' %in% genes) && !('XynA2' %in% genes) && !('XylU' %in% genes) && 'XosR' %in% genes && !('XylC' %in% genes) && 'XylS' %in% genes) ||
       (!('XylA' %in% genes) && 'XylH' %in% genes && 'XylA2' %in% genes) ||
       (!('XylA' %in% genes) && !('XylH' %in% genes) && 'XylE' %in% genes && 'XylA2' %in% genes) ||
       (!('XylA' %in% genes) && !('XylH' %in% genes) && 'XylE' %in% genes && !('XylA2' %in% genes) && 'XynB2' %in% genes) ||
       (!('XylA' %in% genes) && !('XylH' %in% genes) && !('XylE' %in% genes) && 'XylT' %in% genes && 'XylB' %in% genes) ||
       (!('XylA' %in% genes) && !('XylH' %in% genes) && !('XylE' %in% genes) && !('XylT' %in% genes) && 'XylF' %in% genes && 'XynA' %in% genes) 
    ) {
      status <- 1
    } else {
      status <- 0
    }
  } else if (subsystem == "Xylose and xylosides utilization HMP" && phenotype == 'XOS') {
    if(('XylA' %in% genes && 'XynB' %in% genes && 'XylB' %in% genes && !('XynlR3' %in% genes) && 'XynT' %in% genes) ||
       ('XylA' %in% genes && 'XynB' %in% genes && 'XylB' %in% genes && !('XynlR3' %in% genes) && !('XynT' %in% genes) && 'XylE' %in% genes) ||
       ('XylA' %in% genes && 'XynB' %in% genes && 'XylB' %in% genes && !('XynlR3' %in% genes) && !('XynT' %in% genes) && !('XylE' %in% genes) && 'XylR2' %in% genes && !('AxeA' %in% genes) && !('XynV' %in% genes)) ||
       ('XylA' %in% genes && 'XynB' %in% genes && 'XylB' %in% genes && !('XynlR3' %in% genes) && !('XynT' %in% genes) && !('XylE' %in% genes) && 'XylR2' %in% genes && !('AxeA' %in% genes) && 'XynV' %in% genes && 'XynU' %in% genes) ||
       ('XylA' %in% genes && 'XynB' %in% genes && 'XylB' %in% genes && !('XynlR3' %in% genes) && !('XynT' %in% genes) && !('XylE' %in% genes) && 'XylR2' %in% genes && 'AxeA' %in% genes && 'AguA' %in% genes) ||
       ('XylA' %in% genes && 'XynB' %in% genes && 'XylB' %in% genes && !('XynlR3' %in% genes) && !('XynT' %in% genes) && !('XylE' %in% genes) && !('XylR2' %in% genes) && 'XynY' %in% genes) ||
       ('XylA' %in% genes && 'XynB' %in% genes && 'XylB' %in% genes && !('XynlR3' %in% genes) && !('XynT' %in% genes) && !('XylE' %in% genes) && !('XylR2' %in% genes) && !('XynY' %in% genes) && 'XynU' %in% genes && !('XylS' %in% genes)) ||
       ('XylA' %in% genes && 'XynB' %in% genes && 'XylB' %in% genes && !('XynlR3' %in% genes) && !('XynT' %in% genes) && !('XylE' %in% genes) && !('XylR2' %in% genes) && !('XynY' %in% genes) && !('XynU' %in% genes) && 'XylS' %in% genes && !('XynA' %in% genes)) ||
       ('XylA' %in% genes && 'XynB' %in% genes && 'XylB' %in% genes && !('XynlR3' %in% genes) && !('XynT' %in% genes) && !('XylE' %in% genes) && !('XylR2' %in% genes) && !('XynY' %in% genes) && !('XynU' %in% genes) && 'XylS' %in% genes && 'XynA' %in% genes && 'AguA' %in% genes) ||
       ('XylA' %in% genes && !('XynB' %in% genes) && 'XosC' %in% genes) ||
       ('XylA' %in% genes && !('XynB' %in% genes) && !('XosC' %in% genes) && 'XynT' %in% genes && 'XylF' %in% genes && 'XylR' %in% genes && 'XylS' %in% genes) ||
       ('XylA' %in% genes && !('XynB' %in% genes) && !('XosC' %in% genes) && 'XynT' %in% genes && 'XylF' %in% genes && !('XylR' %in% genes) && 'XynB2' %in% genes) ||
       ('XylA' %in% genes && !('XynB' %in% genes) && !('XosC' %in% genes) && 'XynT' %in% genes && !('XylF' %in% genes) && 'XylE' %in% genes && 'XylS' %in% genes) ||
       ('XylA' %in% genes && !('XynB' %in% genes) && !('XosC' %in% genes) && 'XynT' %in% genes && !('XylF' %in% genes) && 'XylE' %in% genes && !('XylS' %in% genes) && 'XynB2' %in% genes) ||
       ('XylA' %in% genes && !('XynB' %in% genes) && !('XosC' %in% genes) && 'XynT' %in% genes && !('XylF' %in% genes) && !('XylE' %in% genes) && 'XynB2' %in% genes) ||
       ('XylA' %in% genes && !('XynB' %in% genes) && !('XosC' %in% genes) && 'XynT' %in% genes && !('XylF' %in% genes) && !('XylE' %in% genes) && !('XynB2' %in% genes) && 'XylR2' %in% genes && 'XylT' %in% genes && 'XylS' %in% genes) ||
       ('XylA' %in% genes && !('XynB' %in% genes) && !('XosC' %in% genes) && !('XynT' %in% genes) && 'XynPTSa' %in% genes) ||
       ('XylA' %in% genes && !('XynB' %in% genes) && !('XosC' %in% genes) && !('XynT' %in% genes) && !('XynPTSa' %in% genes) && 'XynU' %in% genes) ||
       ('XylA' %in% genes && !('XynB' %in% genes) && !('XosC' %in% genes) && !('XynT' %in% genes) && !('XynPTSa' %in% genes) && !('XynU' %in% genes) && 'XylE' %in% genes && 'XynA' %in% genes && !('XylR2' %in% genes)) ||
       ('XylA' %in% genes && !('XynB' %in% genes) && !('XosC' %in% genes) && !('XynT' %in% genes) && !('XynPTSa' %in% genes) && !('XynU' %in% genes) && 'XylE' %in% genes && !('XynA' %in% genes) && 'XylR4' %in% genes && !('XynB2' %in% genes)) || 
       ('XylA' %in% genes && !('XynB' %in% genes) && !('XosC' %in% genes) && !('XynT' %in% genes) && !('XynPTSa' %in% genes) && !('XynU' %in% genes) && 'XylE' %in% genes && !('XynA' %in% genes) && !('XylR4' %in% genes) && 'AxuA' %in% genes) ||
       ('XylA' %in% genes && !('XynB' %in% genes) && !('XosC' %in% genes) && !('XynT' %in% genes) && !('XynPTSa' %in% genes) && !('XynU' %in% genes) && !('XylE' %in% genes) && 'AguA' %in% genes) ||
       ('XylA' %in% genes && !('XynB' %in% genes) && !('XosC' %in% genes) && !('XynT' %in% genes) && !('XynPTSa' %in% genes) && !('XynU' %in% genes) && !('XylE' %in% genes) && !('AguA' %in% genes) && 'XynX' %in% genes) ||
       ('XylA' %in% genes && !('XynB' %in% genes) && !('XosC' %in% genes) && !('XynT' %in% genes) && !('XynPTSa' %in% genes) && !('XynU' %in% genes) && !('XylE' %in% genes) && !('AguA' %in% genes) && !('XynX' %in% genes) && 'XylT' %in% genes && 'XylS' %in% genes) ||
       (!('XylA' %in% genes) && 'XylA2' %in% genes && 'XynT' %in% genes && 'XynA' %in% genes) ||
       (!('XylA' %in% genes) && 'XylA2' %in% genes && 'XynT' %in% genes && !('XynA' %in% genes) && 'XynB' %in% genes) ||
       (!('XylA' %in% genes) && 'XylA2' %in% genes && 'XynT' %in% genes && !('XynA' %in% genes) && !('XynB' %in% genes) && 'XynB2' %in% genes) ||
       (!('XylA' %in% genes) && 'XylA2' %in% genes && 'XynT' %in% genes && !('XynA' %in% genes) && !('XynB' %in% genes) && !('XynB2' %in% genes) && 'AxuB' %in% genes) ||
       (!('XylA' %in% genes) && 'XylA2' %in% genes && !('XynT' %in% genes) && 'XynV' %in% genes) ||
       (!('XylA' %in% genes) && 'XylA2' %in% genes && !('XynT' %in% genes) && !('XynV' %in% genes) && 'XylE' %in% genes) ||
       (!('XylA' %in% genes) && 'XylA2' %in% genes && !('XynT' %in% genes) && !('XynV' %in% genes) && !('XylE' %in% genes) && 'XynX' %in% genes) ||
       (!('XylA' %in% genes) && 'XylA2' %in% genes && !('XynT' %in% genes) && !('XynV' %in% genes) && !('XylE' %in% genes) && !('XynX' %in% genes) && 'XynY' %in% genes) ||
       (!('XylA' %in% genes) && 'XylA2' %in% genes && !('XynT' %in% genes) && !('XynV' %in% genes) && !('XylE' %in% genes) && !('XynX' %in% genes) && !('XynY' %in% genes) && 'XylR' %in% genes && 'XynB' %in% genes) ||
       (!('XylA' %in% genes) && !('XylA2' %in% genes) && 'XynB2' %in% genes && 'XylS' %in% genes && 'XylB' %in% genes)
    ) {
      status <- 1
    } else {
      status <- 0
    }
  } else if (subsystem == "Xylose and xylosides utilization HMP" && phenotype == 'aXyl') {
    if(('XylS' %in% genes && !('FruE_bif' %in% genes) && 'XylA' %in% genes && !('XynY' %in% genes) && 'XynT' %in% genes && !('XynU' %in% genes) && 'XylF' %in% genes) ||
       ('XylS' %in% genes && !('FruE_bif' %in% genes) && 'XylA' %in% genes && !('XynY' %in% genes) && 'XynT' %in% genes && !('XynU' %in% genes) && !('XylF' %in% genes) && 'XynB' %in% genes) ||
       ('XylS' %in% genes && !('FruE_bif' %in% genes) && 'XylA' %in% genes && !('XynY' %in% genes) && 'XynT' %in% genes && !('XynU' %in% genes) && !('XylF' %in% genes) && !('XynB' %in% genes) && 'XynB2' %in% genes) ||
       ('XylS' %in% genes && !('FruE_bif' %in% genes) && 'XylA' %in% genes && !('XynY' %in% genes) && 'XynT' %in% genes && !('XynU' %in% genes) && !('XylF' %in% genes) && !('XynB' %in% genes) && !('XynB2' %in% genes) && 'XylR' %in% genes && !('XynA' %in% genes)) ||
       ('XylS' %in% genes && !('FruE_bif' %in% genes) && 'XylA' %in% genes && !('XynY' %in% genes) && 'XynT' %in% genes && !('XynU' %in% genes) && !('XylF' %in% genes) && !('XynB' %in% genes) && !('XynB2' %in% genes) && 'XylR' %in% genes && 'XynA' %in% genes && 'XylE' %in% genes) ||
       ('XylS' %in% genes && !('FruE_bif' %in% genes) && 'XylA' %in% genes && !('XynY' %in% genes) && 'XynT' %in% genes && !('XynU' %in% genes) && !('XylF' %in% genes) && !('XynB' %in% genes) && !('XynB2' %in% genes) && !('XylR' %in% genes) && 'XynA' %in% genes) ||
       ('XylS' %in% genes && !('FruE_bif' %in% genes) && 'XylA' %in% genes && !('XynY' %in% genes) && 'XynT' %in% genes && !('XynU' %in% genes) && !('XylF' %in% genes) && !('XynB' %in% genes) && !('XynB2' %in% genes) && !('XylR' %in% genes) && !('XynA' %in% genes) && !('XylE' %in% genes)) ||
       ('XylS' %in% genes && !('FruE_bif' %in% genes) && 'XylA' %in% genes && !('XynY' %in% genes) && 'XynT' %in% genes && 'XynU' %in% genes && 'AxuA' %in% genes) ||
       ('XylS' %in% genes && !('FruE_bif' %in% genes) && 'XylA' %in% genes && !('XynY' %in% genes) && !('XynT' %in% genes) && 'XylE' %in% genes) ||
       ('XylS' %in% genes && !('FruE_bif' %in% genes) && 'XylA' %in% genes && !('XynY' %in% genes) && !('XynT' %in% genes) && !('XylE' %in% genes) && 'XynPTSc' %in% genes) ||
       ('XylS' %in% genes && !('FruE_bif' %in% genes) && 'XylA' %in% genes && !('XynY' %in% genes) && !('XynT' %in% genes) && !('XylE' %in% genes) && !('XynPTSc' %in% genes) && 'AxuA' %in% genes) ||
       ('XylS' %in% genes && !('FruE_bif' %in% genes) && 'XylA' %in% genes && !('XynY' %in% genes) && !('XynT' %in% genes) && !('XylE' %in% genes) && !('XynPTSc' %in% genes) && !('AxuA' %in% genes) && 'SusCx' %in% genes) ||
       ('XylS' %in% genes && !('FruE_bif' %in% genes) && 'XylA' %in% genes && !('XynY' %in% genes) && !('XynT' %in% genes) && !('XylE' %in% genes) && !('XynPTSc' %in% genes) && !('AxuA' %in% genes) && !('SusCx' %in% genes) && 'XylT' %in% genes) ||
       ('XylS' %in% genes && !('FruE_bif' %in% genes) && 'XylA' %in% genes && !('XynY' %in% genes) && !('XynT' %in% genes) && !('XylE' %in% genes) && !('XynPTSc' %in% genes) && !('AxuA' %in% genes) && !('SusCx' %in% genes) && !('XylT' %in% genes) && 'XynU' %in% genes && !('AguA' %in% genes)) ||
       ('XylS' %in% genes && !('FruE_bif' %in% genes) && 'XylA' %in% genes && 'XynY' %in% genes && 'AxuA' %in% genes) ||
       ('XylS' %in% genes && !('FruE_bif' %in% genes) && 'XylA' %in% genes && 'XynY' %in% genes && !('AxuA' %in% genes) && !('XynB' %in% genes)) ||
       ('XylS' %in% genes && !('FruE_bif' %in% genes) && 'XylA' %in% genes && 'XynY' %in% genes && !('AxuA' %in% genes) && 'XynB' %in% genes && 'XynT' %in% genes) ||
       ('XylS' %in% genes && !('FruE_bif' %in% genes) && 'XylA' %in% genes && 'XynY' %in% genes && !('AxuA' %in% genes) && 'XynB' %in% genes && !('XynT' %in% genes) && 'AguA' %in% genes && !('XylR2' %in% genes)) ||
       ('XylS' %in% genes && !('FruE_bif' %in% genes) && !('XylA' %in% genes) && 'AxuB' %in% genes) ||
       ('XylS' %in% genes && !('FruE_bif' %in% genes) && !('XylA' %in% genes) && !('AxuB' %in% genes) && 'AguA' %in% genes && 'XynT' %in% genes) ||
       ('XylS' %in% genes && !('FruE_bif' %in% genes) && !('XylA' %in% genes) && !('AxuB' %in% genes) && 'AguA' %in% genes && !('XynT' %in% genes) && 'XylE' %in% genes) ||
       ('XylS' %in% genes && !('FruE_bif' %in% genes) && !('XylA' %in% genes) && !('AxuB' %in% genes) && !('AguA' %in% genes) && 'XylS2' %in% genes) ||
       ('XylS' %in% genes && !('FruE_bif' %in% genes) && !('XylA' %in% genes) && !('AxuB' %in% genes) && !('AguA' %in% genes) && !('XylS2' %in% genes) && 'XylE' %in% genes && 'XynB2' %in% genes) ||
       ('XylS' %in% genes && !('FruE_bif' %in% genes) && !('XylA' %in% genes) && !('AxuB' %in% genes) && !('AguA' %in% genes) && !('XylS2' %in% genes) && !('XylE' %in% genes) && 'XynT' %in% genes && 'XynB' %in% genes && !('XylR' %in% genes) && !('XylR2' %in% genes) && !('XynB2' %in% genes) && !('XylF' %in% genes)) ||
       ('XylS' %in% genes && 'FruE_bif' %in% genes && 'AxuA' %in% genes) ||
       (!('XylS' %in% genes) && 'AxuA' %in% genes) ||
       (!('XylS' %in% genes) && !('AxuA' %in% genes) && 'AguA' %in% genes && 'XylE' %in% genes && !('XynB' %in% genes)) ||
       (!('XylS' %in% genes) && !('AxuA' %in% genes) && !('AguA' %in% genes) && 'XylE' %in% genes && 'XylF' %in% genes && !('XynB' %in% genes))
  ) {
      status <- 1
    } else {
      status <- 0
    }
  } else if (subsystem == "Hexuronates utilization HMP" && phenotype == "GlcA"){
    if( ('ExuT' %in% genes && 'UxuB' %in% genes && !('UxuE' %in% genes) && 'UxuA' %in% genes && 'UxaC' %in% genes) ||
        ('ExuT' %in% genes && 'UxuB' %in% genes && !('UxuE' %in% genes) && 'UxuA' %in% genes && !('UxaC' %in% genes) && 'UxaA' %in% genes) ||
        ('ExuT' %in% genes && 'UxuB' %in% genes && !('UxuE' %in% genes) && !('UxuA' %in% genes) && !('UxaB' %in% genes)) ||
        ('ExuT' %in% genes && !('UxuB' %in% genes) && 'Udh' %in% genes) ||
        ('ExuT' %in% genes && !('UxuB' %in% genes) && !('Udh' %in% genes) && 'KdgF' %in% genes && 'UxuA' %in% genes) ||
        (!('ExuT' %in% genes) && 'UxuM' %in% genes && !('TogA' %in% genes) && 'UxuQ' %in% genes && !('ExuR' %in% genes) && !('LplD' %in% genes) && 'KdgF' %in% genes) ||
        (!('ExuT' %in% genes) && 'UxuM' %in% genes && !('TogA' %in% genes) && 'UxuQ' %in% genes && !('ExuR' %in% genes) && !('LplD' %in% genes) && !('KdgF' %in% genes) && !('PelX' %in% genes) && !('HexR' %in% genes) && !('RhiN' %in% genes) && 'UxaC' %in% genes) ||
        (!('ExuT' %in% genes) && 'UxuM' %in% genes && !('TogA' %in% genes) && 'UxuQ' %in% genes && !('ExuR' %in% genes) && !('LplD' %in% genes) && !('KdgF' %in% genes) && !('PelX' %in% genes) && !('HexR' %in% genes) && !('RhiN' %in% genes) && !('UxaC' %in% genes) && 'Udh' %in% genes) ||
        (!('ExuT' %in% genes) && 'UxuM' %in% genes && !('TogA' %in% genes) && 'UxuQ' %in% genes && !('ExuR' %in% genes) && !('LplD' %in% genes) && !('KdgF' %in% genes) && !('PelX' %in% genes) && !('HexR' %in% genes) && !('RhiN' %in% genes) && !('UxaC' %in% genes) && !('Udh' %in% genes) && 'UxaB' %in% genes) ||
        (!('ExuT' %in% genes) && 'UxuM' %in% genes && !('TogA' %in% genes) && 'UxuQ' %in% genes && !('ExuR' %in% genes) && !('LplD' %in% genes) && !('KdgF' %in% genes) && !('PelX' %in% genes) && !('HexR' %in% genes) && 'RhiN' %in% genes && 'UxaB' %in% genes && 'YesX' %in% genes) ||
        (!('ExuT' %in% genes) && 'UxuM' %in% genes && !('TogA' %in% genes) && 'UxuQ' %in% genes && !('ExuR' %in% genes) && !('LplD' %in% genes) && !('KdgF' %in% genes) && !('PelX' %in% genes) && !('HexR' %in% genes) && 'RhiN' %in% genes && 'UxaB' %in% genes && !('YesX' %in% genes) && 'UidA' %in% genes) ||
        (!('ExuT' %in% genes) && 'UxuM' %in% genes && !('TogA' %in% genes) && 'UxuQ' %in% genes && !('ExuR' %in% genes) && !('LplD' %in% genes) && !('KdgF' %in% genes) && !('PelX' %in% genes) && !('HexR' %in% genes) && 'RhiN' %in% genes && 'UxaB' %in% genes && !('YesX' %in% genes) && !('UidA' %in% genes) && 'TogMN' %in% genes) ||
        (!('ExuT' %in% genes) && 'UxuM' %in% genes && !('TogA' %in% genes) && 'UxuQ' %in% genes && !('ExuR' %in% genes) && !('LplD' %in% genes) && !('KdgF' %in% genes) && !('PelX' %in% genes) && !('HexR' %in% genes) && 'RhiN' %in% genes && 'UxaB' %in% genes && !('YesX' %in% genes) && !('UidA' %in% genes) && !('TogMN' %in% genes) && 'RhiX' %in% genes) ||
        (!('ExuT' %in% genes) && 'UxuM' %in% genes && !('TogA' %in% genes) && 'UxuQ' %in% genes && !('ExuR' %in% genes) && !('LplD' %in% genes) && !('KdgF' %in% genes) && !('PelX' %in% genes) && 'HexR' %in% genes && 'UxuB' %in% genes) ||
        (!('ExuT' %in% genes) && 'UxuM' %in% genes && !('TogA' %in% genes) && 'UxuQ' %in% genes && !('ExuR' %in% genes) && !('LplD' %in% genes) && !('KdgF' %in% genes) && 'PelX' %in% genes && !('UidA' %in% genes)) ||
        (!('ExuT' %in% genes) && 'UxuM' %in% genes && !('TogA' %in% genes) && 'UxuQ' %in% genes && !('ExuR' %in% genes) && 'LplD' %in% genes && 'HexR' %in% genes) ||
        (!('ExuT' %in% genes) && 'UxuM' %in% genes && !('TogA' %in% genes) && 'UxuQ' %in% genes && !('ExuR' %in% genes) && 'LplD' %in% genes && !('HexR' %in% genes) && 'TogB' %in% genes && !('KdgF' %in% genes)) ||
        (!('ExuT' %in% genes) && 'UxuM' %in% genes && !('TogA' %in% genes) && 'UxuQ' %in% genes && 'ExuR' %in% genes && 'UxaC' %in% genes) ||
        (!('ExuT' %in% genes) && 'UxuM' %in% genes && !('TogA' %in% genes) && !('UxuQ' %in% genes) && !('UidA' %in% genes) && 'UxuA' %in% genes && 'UxaC' %in% genes) ||      
        (!('ExuT' %in% genes) && !('UxuM' %in% genes) && 'ExuT2' %in% genes && 'UxuB' %in% genes) ||
        (!('ExuT' %in% genes) && !('UxuM' %in% genes) && !('ExuT2' %in% genes) && 'TogT' %in% genes && 'UxaA' %in% genes && 'UxuA' %in% genes) ||
        (!('ExuT' %in% genes) && !('UxuM' %in% genes) && !('ExuT2' %in% genes) && 'TogT' %in% genes && !('UxaA' %in% genes) && !('UidA' %in% genes) && 'UxuA' %in% genes) ||
        (!('ExuT' %in% genes) && !('UxuM' %in% genes) && !('ExuT2' %in% genes) && !('TogT' %in% genes) && 'GntP' %in% genes && 'UxuB' %in% genes) ||
        (!('ExuT' %in% genes) && !('UxuM' %in% genes) && !('ExuT2' %in% genes) && !('TogT' %in% genes) && !('GntP' %in% genes) && 'UxuL' %in% genes) ||
        (!('ExuT' %in% genes) && !('UxuM' %in% genes) && !('ExuT2' %in% genes) && !('TogT' %in% genes) && !('GntP' %in% genes) && !('UxuL' %in% genes) && 'HexR' %in% genes && 'UidB2' %in% genes) ||
        (!('ExuT' %in% genes) && !('UxuM' %in% genes) && !('ExuT2' %in% genes) && !('TogT' %in% genes) && !('GntP' %in% genes) && !('UxuL' %in% genes) && !('HexR' %in% genes) && 'PelB' %in% genes && 'UxuA' %in% genes) ||
        (!('ExuT' %in% genes) && !('UxuM' %in% genes) && !('ExuT2' %in% genes) && !('TogT' %in% genes) && !('GntP' %in% genes) && !('UxuL' %in% genes) && !('HexR' %in% genes) && !('PelB' %in% genes) && 'UidB2' %in% genes && 'UidB1' %in% genes && !('UidA' %in% genes)) 
    ) {
      status <- 1
    } else {
      status <- 0
    }
  } else if (subsystem == "Hexuronates utilization HMP" && phenotype == "GalA"){
    if(('UxaB' %in% genes && 'ExuT' %in% genes && 'UxaA' %in% genes && 'UxaC' %in% genes) ||
       ('UxaB' %in% genes && 'ExuT' %in% genes && 'UxaA' %in% genes && !('UxaC' %in% genes) && !('UxuB' %in% genes)) ||
       ('UxaB' %in% genes && !('ExuT' %in% genes) && 'UxuM' %in% genes && !('TogA' %in% genes) && !('LplD' %in% genes) && !('PelX' %in% genes) && 'TogMN' %in% genes) ||
       ('UxaB' %in% genes && !('ExuT' %in% genes) && 'UxuM' %in% genes && !('TogA' %in% genes) && !('LplD' %in% genes) && !('PelX' %in% genes) && !('TogMN' %in% genes) && !('Pgl' %in% genes)) ||
       ('UxaB' %in% genes && !('ExuT' %in% genes) && 'UxuM' %in% genes && !('TogA' %in% genes) && !('LplD' %in% genes) && !('PelX' %in% genes) && !('TogMN' %in% genes) && 'Pgl' %in% genes && 'RhiX' %in% genes) ||
       ('UxaB' %in% genes && !('ExuT' %in% genes) && 'UxuM' %in% genes && !('TogA' %in% genes) && !('LplD' %in% genes) && !('PelX' %in% genes) && !('TogMN' %in% genes) && 'Pgl' %in% genes && !('RhiX' %in% genes) && 'KdgF' %in% genes) ||
       ('UxaB' %in% genes && !('ExuT' %in% genes) && 'UxuM' %in% genes && !('TogA' %in% genes) && !('LplD' %in% genes) && !('PelX' %in% genes) && !('TogMN' %in% genes) && 'Pgl' %in% genes && !('RhiX' %in% genes) && !('KdgF' %in% genes) && 'UidA' %in% genes) ||
       ('UxaB' %in% genes && !('ExuT' %in% genes) && 'UxuM' %in% genes && !('TogA' %in% genes) && !('LplD' %in% genes) && 'PelX' %in% genes && !('UidA' %in% genes)) ||
       ('UxaB' %in% genes && !('ExuT' %in% genes) && 'UxuM' %in% genes && !('TogA' %in% genes) && !('LplD' %in% genes) && 'PelX' %in% genes && 'UidA' %in% genes && 'UidB2' %in% genes) ||
       ('UxaB' %in% genes && !('ExuT' %in% genes) && 'UxuM' %in% genes && !('TogA' %in% genes) && 'LplD' %in% genes && 'UxuQ' %in% genes && 'HexR' %in% genes) ||
       ('UxaB' %in% genes && !('ExuT' %in% genes) && 'UxuM' %in% genes && !('TogA' %in% genes) && 'LplD' %in% genes && 'UxuQ' %in% genes && !('HexR' %in% genes) && 'TogB' %in% genes && !('KdgF' %in% genes)) ||
       ('UxaB' %in% genes && !('ExuT' %in% genes) && !('UxuM' %in% genes) && 'TogT' %in% genes && 'UxuB' %in% genes) ||
       ('UxaB' %in% genes && !('ExuT' %in% genes) && !('UxuM' %in% genes) && !('TogT' %in% genes) && 'ExuT2' %in% genes && !('UidB2' %in% genes)) ||
       ('UxaB' %in% genes && !('ExuT' %in% genes) && !('UxuM' %in% genes) && !('TogT' %in% genes) && !('ExuT2' %in% genes) && 'GntP' %in% genes) ||
       ('UxaB' %in% genes && !('ExuT' %in% genes) && !('UxuM' %in% genes) && !('TogT' %in% genes) && !('ExuT2' %in% genes) && !('GntP' %in% genes) && 'UidB2' %in% genes && !('UidA' %in% genes) && 'RhiN' %in% genes) ||
       ('UxaB' %in% genes && !('ExuT' %in% genes) && !('UxuM' %in% genes) && !('TogT' %in% genes) && !('ExuT2' %in% genes) && !('GntP' %in% genes) && !('UidB2' %in% genes) && 'PelB' %in% genes) || 
       (!('UxaB' %in% genes) && 'Udh' %in% genes) ||
       (!('UxaB' %in% genes) && !('Udh' %in% genes) && 'UxaE' %in% genes && 'ExuT2' %in% genes) ||
       (!('UxaB' %in% genes) && !('Udh' %in% genes) && !('UxaE' %in% genes) && 'LgoT' %in% genes) ||
       (!('UxaB' %in% genes) && !('Udh' %in% genes) && !('UxaE' %in% genes) && !('LgoT' %in% genes) && 'PelX' %in% genes && 'UxaA' %in% genes) ||
       (!('UxaB' %in% genes) && !('Udh' %in% genes) && !('UxaE' %in% genes) && !('LgoT' %in% genes) && !('PelX' %in% genes) && 'UidB2' %in% genes && 'UxaA' %in% genes)    
   ) {
     status <- 1
   } else {
     status <- 0
   }
  } else if (subsystem == "Hexuronates utilization HMP" && phenotype == "(GlcA)n"){
    if(('UidA' %in% genes && 'UidB1' %in% genes && !('RhiX' %in% genes)) ||
       ('UidA' %in% genes && 'UidB1' %in% genes && 'RhiX' %in% genes && 'UxuP' %in% genes) ||
       ('UidA' %in% genes && !('UidB1' %in% genes) && 'UidB2' %in% genes && !('UxuQ' %in% genes)) ||
       ('UidA' %in% genes && !('UidB1' %in% genes) && 'UidB2' %in% genes && 'UxuQ' %in% genes && 'TogB' %in% genes) ||
       ('UidA' %in% genes && !('UidB1' %in% genes) && !('UidB2' %in% genes) && 'UidB3' %in% genes && !('TogMN' %in% genes) && !('RhiT' %in% genes)) ||
       (!('UidA' %in% genes) && 'UidB2' %in% genes && 'Pgl' %in% genes && !('UxuP' %in% genes) && !('TogT' %in% genes)) ||
       (!('UidA' %in% genes) && !('UidB2' %in% genes) && 'RgdR' %in% genes && 'KdgF' %in% genes && !('UxuP' %in% genes) && !('ExuT2' %in% genes)) ||
       (!('UidA' %in% genes) && !('UidB2' %in% genes) && !('RgdR' %in% genes) && 'Pgl' %in% genes && !('UxaA' %in% genes) && !('UxuP' %in% genes) && 'UxaC' %in% genes && !('RhiN' %in% genes))          
    ) {
      status <- 1
    } else {
      status <- 0
    }
  } else if (subsystem == "Hexuronates utilization HMP" && phenotype == "(GalA)n"){
   if(('Pgl' %in% genes && 'UxaB' %in% genes && 'UxuA' %in% genes && 'UxaC' %in% genes && !('UidB2' %in% genes) && 'RhiN' %in% genes && !('PelA' %in% genes) && !('PelL' %in% genes) && !('RgdR' %in% genes) && !('RhiT' %in% genes)) ||
      ('Pgl' %in% genes && 'UxaB' %in% genes && 'UxuA' %in% genes && 'UxaC' %in% genes && !('UidB2' %in% genes) && 'RhiN' %in% genes && !('PelA' %in% genes) && !('PelL' %in% genes) && !('RgdR' %in% genes) && 'RhiT' %in% genes && 'Ogl' %in% genes) ||
      ('Pgl' %in% genes && 'UxaB' %in% genes && 'UxuA' %in% genes && 'UxaC' %in% genes && !('UidB2' %in% genes) && 'RhiN' %in% genes && !('PelA' %in% genes) && !('PelL' %in% genes) && !('RgdR' %in% genes) && 'RhiT' %in% genes && !('Ogl' %in% genes) && !('ExuT' %in% genes)) ||
      ('Pgl' %in% genes && 'UxaB' %in% genes && 'UxuA' %in% genes && 'UxaC' %in% genes && !('UidB2' %in% genes) && 'RhiN' %in% genes && !('PelA' %in% genes) && !('PelL' %in% genes) && !('RgdR' %in% genes) && 'RhiT' %in% genes && !('Ogl' %in% genes) && 'ExuT' %in% genes && 'TogT' %in% genes) ||
      ('Pgl' %in% genes && 'UxaB' %in% genes && 'UxuA' %in% genes && 'UxaC' %in% genes && !('UidB2' %in% genes) && 'RhiN' %in% genes && !('PelA' %in% genes) && !('PelL' %in% genes) && !('RgdR' %in% genes) && 'RhiT' %in% genes && !('Ogl' %in% genes) && 'ExuT' %in% genes && !('TogT' %in% genes) && 'UxuF' %in% genes) ||
      ('Pgl' %in% genes && 'UxaB' %in% genes && 'UxuA' %in% genes && 'UxaC' %in% genes && !('UidB2' %in% genes) && 'RhiN' %in% genes && !('PelA' %in% genes) && !('PelL' %in% genes) && !('RgdR' %in% genes) && 'RhiT' %in% genes && !('Ogl' %in% genes) && 'ExuT' %in% genes && !('TogT' %in% genes) && !('UxuF' %in% genes) && 'AltT' %in% genes) ||
      ('Pgl' %in% genes && 'UxaB' %in% genes && 'UxuA' %in% genes && 'UxaC' %in% genes && !('UidB2' %in% genes) && 'RhiN' %in% genes && !('PelA' %in% genes) && !('PelL' %in% genes) && !('RgdR' %in% genes) && 'RhiT' %in% genes && !('Ogl' %in% genes) && 'ExuT' %in% genes && !('TogT' %in% genes) && !('UxuF' %in% genes) && !('AltT' %in% genes) && 'UxuR' %in% genes) ||
      ('Pgl' %in% genes && 'UxaB' %in% genes && 'UxuA' %in% genes && 'UxaC' %in% genes && !('UidB2' %in% genes) && 'RhiN' %in% genes && !('PelA' %in% genes) && !('PelL' %in% genes) && 'RgdR' %in% genes && !('UidA' %in% genes)) ||     
      ('Pgl' %in% genes && 'UxaB' %in% genes && 'UxuA' %in% genes && 'UxaC' %in% genes && !('UidB2' %in% genes) && 'RhiN' %in% genes && !('PelA' %in% genes) && 'PelL' %in% genes && !('UxuP' %in% genes)) ||
      ('Pgl' %in% genes && 'UxaB' %in% genes && 'UxuA' %in% genes && 'UxaC' %in% genes && !('UidB2' %in% genes) && 'RhiN' %in% genes && !('PelA' %in% genes) && 'PelL' %in% genes && 'UxuP' %in% genes && 'UxuQ' %in% genes) ||
      ('Pgl' %in% genes && 'UxaB' %in% genes && 'UxuA' %in% genes && 'UxaC' %in% genes && !('UidB2' %in% genes) && 'RhiN' %in% genes && 'PelA' %in% genes && 'TogMN' %in% genes) ||  
      ('Pgl' %in% genes && 'UxaB' %in% genes && 'UxuA' %in% genes && 'UxaC' %in% genes && !('UidB2' %in% genes) && !('RhiN' %in% genes) && 'ExuT' %in% genes) ||      
      ('Pgl' %in% genes && 'UxaB' %in% genes && 'UxuA' %in% genes && 'UxaC' %in% genes && 'UidB2' %in% genes && 'UxuP' %in% genes) ||
      ('Pgl' %in% genes && 'UxaB' %in% genes && 'UxuA' %in% genes && 'UxaC' %in% genes && 'UidB2' %in% genes && !('UxuP' %in% genes) && !('UidA' %in% genes) && !('UidB1' %in% genes)) ||
      ('Pgl' %in% genes && 'UxaB' %in% genes && !('UxuA' %in% genes) && !('RhiT' %in% genes) && 'ExuT' %in% genes) ||
      ('Pgl' %in% genes && 'UxaB' %in% genes && !('UxuA' %in% genes) && !('RhiT' %in% genes) && !('ExuT' %in% genes) && 'AltT' %in% genes) ||
      ('Pgl' %in% genes && 'UxaB' %in% genes && !('UxuA' %in% genes) && !('RhiT' %in% genes) && !('ExuT' %in% genes) && !('AltT' %in% genes) && 'UxuB' %in% genes) ||
      ('Pgl' %in% genes && 'UxaB' %in% genes && !('UxuA' %in% genes) && !('RhiT' %in% genes) && !('ExuT' %in% genes) && !('AltT' %in% genes) && !('UxuB' %in% genes) && 'RhiA' %in% genes) ||
      ('Pgl' %in% genes && 'UxaB' %in% genes && !('UxuA' %in% genes) && !('RhiT' %in% genes) && !('ExuT' %in% genes) && !('AltT' %in% genes) && !('UxuB' %in% genes) && !('RhiA' %in% genes) && 'UxuR2' %in% genes) ||
      ('Pgl' %in% genes && !('UxaB' %in% genes) && 'UxaE' %in% genes && !('UidA' %in% genes)) ||
      ('Pgl' %in% genes && !('UxaB' %in% genes) && !('UxaE' %in% genes) && 'UxaC' %in% genes && !('UxuP' %in% genes) && !('RhiN' %in% genes) && 'UxuA' %in% genes) ||      
      (!('Pgl' %in% genes) && 'Ogl' %in% genes && 'UxaC' %in% genes && !('AltT' %in% genes)) ||
      (!('Pgl' %in% genes) && !('Ogl' %in% genes) && 'TogB' %in% genes && !('UxuB' %in% genes)) ||
      (!('Pgl' %in% genes) && !('Ogl' %in% genes) && 'TogB' %in% genes && 'UxuB' %in% genes && !('RhiN' %in% genes) && 'UxuA' %in% genes && !('UxuP' %in% genes)) ||
      (!('Pgl' %in% genes) && !('Ogl' %in% genes) && !('TogB' %in% genes) && 'KdgM' %in% genes) ||
      (!('Pgl' %in% genes) && !('Ogl' %in% genes) && !('TogB' %in% genes) && !('KdgM' %in% genes) && 'ExuT2' %in% genes && 'KdgF' %in% genes && 'UxaB' %in% genes) ||
      (!('Pgl' %in% genes) && !('Ogl' %in% genes) && !('TogB' %in% genes) && !('KdgM' %in% genes) && 'ExuT2' %in% genes && !('KdgF' %in% genes) && 'UxaA' %in% genes && !('ExuT' %in% genes) && !('UxuP' %in% genes) && !('UidB2' %in% genes) && 'UxuB' %in% genes) ||
      (!('Pgl' %in% genes) && !('Ogl' %in% genes) && !('TogB' %in% genes) && !('KdgM' %in% genes) && 'ExuT2' %in% genes && !('KdgF' %in% genes) && 'UxaA' %in% genes && !('ExuT' %in% genes) && !('UxuP' %in% genes) && !('UidB2' %in% genes) && !('UxuB' %in% genes) && 'PelA' %in% genes) ||
      (!('Pgl' %in% genes) && !('Ogl' %in% genes) && !('TogB' %in% genes) && !('KdgM' %in% genes) && !('ExuT2' %in% genes) && 'TogT' %in% genes && 'AltT' %in% genes) ||
      (!('Pgl' %in% genes) && !('Ogl' %in% genes) && !('TogB' %in% genes) && !('KdgM' %in% genes) && !('ExuT2' %in% genes) && 'TogT' %in% genes && !('AltT' %in% genes) && 'UxaB' %in% genes && !('ExuT' %in% genes)) ||
      (!('Pgl' %in% genes) && !('Ogl' %in% genes) && !('TogB' %in% genes) && !('KdgM' %in% genes) && !('ExuT2' %in% genes) && !('TogT' %in% genes) && 'HexR' %in% genes && 'UidB1' %in% genes) ||      
      (!('Pgl' %in% genes) && !('Ogl' %in% genes) && !('TogB' %in% genes) && !('KdgM' %in% genes) && !('ExuT2' %in% genes) && !('TogT' %in% genes) && !('HexR' %in% genes) && 'UxuQ' %in% genes && 'UxaB' %in% genes && !('ExuT' %in% genes) && 'UidB1' %in% genes && 'UidA' %in% genes)     
   ) {
     status <- 1
   } else {
     status <- 0
   }
  } else if (subsystem == "Sugar Alcohols utilization HMP" && phenotype == "Atl"){
    if(('AtlD' %in% genes && 'AtlPc' %in% genes) ||
       (!('AtlD' %in% genes) && 'DalD' %in% genes && 'RbtT' %in% genes) ||
       (!('AtlD' %in% genes) && 'DalD' %in% genes && !('RbtT' %in% genes) && 'MtlE' %in% genes) ||
       (!('AtlD' %in% genes) && !('DalD' %in% genes) && 'RbtT' %in% genes && 'MtlR2' %in% genes && !('GatPc' %in% genes) && !('RbtD' %in% genes)) ||
       (!('AtlD' %in% genes) && !('DalD' %in% genes) && 'RbtT' %in% genes && !('MtlR2' %in% genes) && 'SrlD' %in% genes && !('MtlD1' %in% genes)) ||
       (!('AtlD' %in% genes) && !('DalD' %in% genes) && !('RbtT' %in% genes) && 'RbtK' %in% genes && 'GatD' %in% genes && !('ScrK' %in% genes))
    ) {
      status <- 1
    } else {
      status <- 0
    }
  } else if (subsystem == "Sugar Alcohols utilization HMP" && phenotype == "Mtl"){
   if(('MtlA_IIB' %in% genes && 'MtlD1' %in% genes) ||
      ('MtlA_IIB' %in% genes && !('MtlD1' %in% genes) && 'MtlD2' %in% genes && 'MtlA_IIC' %in% genes && !('MtlR3' %in% genes)) ||
      ('MtlA_IIB' %in% genes && !('MtlD1' %in% genes) && 'MtlD2' %in% genes && 'MtlA_IIC' %in% genes && 'MtlR3' %in% genes && 'XylB' %in% genes) ||
      ('MtlA_IIB' %in% genes && !('MtlD1' %in% genes) && 'MtlD2' %in% genes && !('MtlA_IIC' %in% genes) && 'SrlE' %in% genes) ||
      ('MtlA_IIB' %in% genes && !('MtlD1' %in% genes) && !('MtlD2' %in% genes) && !('ScrK' %in% genes) && 'XylB' %in% genes) ||      
      (!('MtlA_IIB' %in% genes) && 'MtlD' %in% genes && 'MtlF' %in% genes) ||
      (!('MtlA_IIB' %in% genes) && 'MtlD' %in% genes && !('MtlF' %in% genes) && 'MtlP1' %in% genes) ||
      (!('MtlA_IIB' %in% genes) && 'MtlD' %in% genes && !('MtlF' %in% genes) && !('MtlP1' %in% genes) && 'FruK' %in% genes && 'XylB' %in% genes && 'ScrK' %in% genes && !('XylD' %in% genes) && !('GatPc' %in% genes)) ||
      (!('MtlA_IIB' %in% genes) && !('MtlD' %in% genes) && 'MtlP2' %in% genes) ||
      (!('MtlA_IIB' %in% genes) && !('MtlD' %in% genes) && !('MtlP2' %in% genes) && 'MtlD1' %in% genes && 'MtlA_IIA' %in% genes) ||
      (!('MtlA_IIB' %in% genes) && !('MtlD' %in% genes) && !('MtlP2' %in% genes) && 'MtlD1' %in% genes && !('MtlA_IIA' %in% genes) && !('FruK' %in% genes) && 'ScrK' %in% genes) ||
      (!('MtlA_IIB' %in% genes) && !('MtlD' %in% genes) && !('MtlP2' %in% genes) && 'MtlD1' %in% genes && !('MtlA_IIA' %in% genes) && 'FruK' %in% genes && 'LacC' %in% genes && !('GatY' %in% genes) && !('ScrK' %in% genes) && !('GatPa' %in% genes))
   ) {
     status <- 1
     } else {
     status <- 0
     }
  } else if (subsystem == "Sugar Alcohols utilization HMP" && phenotype == "Rtl"){
    if(('RbtD' %in% genes && 'XylB' %in% genes) ||
       ('RbtD' %in% genes && !('XylB' %in% genes) && 'MtlE' %in% genes) ||
       (!('RbtD' %in% genes) && 'RbtT' %in% genes && 'MtlR2' %in% genes && !('MtlA_IIC' %in% genes) && !('GatD' %in% genes) && !('GatY' %in% genes))
    ) {
      status <- 1
    } else {
      status <- 0
    }
  } else if (subsystem == "Sugar Alcohols utilization HMP" && phenotype == "Xlt"){
   if(('XylD' %in% genes && 'RbtT' %in% genes && 'XylB' %in% genes) ||
      ('XylD' %in% genes && !('RbtT' %in% genes) && 'XltC' %in% genes && !('SrlX' %in% genes)) ||
      ('XylD' %in% genes && !('RbtT' %in% genes) && 'XltC' %in% genes && 'SrlX' %in% genes && 'DalD' %in% genes) ||
      ('XylD' %in% genes && !('RbtT' %in% genes) && 'XltC' %in% genes && 'SrlX' %in% genes && !('DalD' %in% genes) && 'GatD' %in% genes && !('SrlD' %in% genes)) ||
      ('XylD' %in% genes && !('RbtT' %in% genes) && !('XltC' %in% genes) && 'MtlK' %in% genes && !('GatPa' %in% genes)) ||       
      (!('XylD' %in% genes) && 'RbtT' %in% genes && 'MtlD' %in% genes && 'MtlD1' %in% genes) ||
      (!('XylD' %in% genes) && 'RbtT' %in% genes && !('MtlD' %in% genes) && 'GutB' %in% genes && 'LacC' %in% genes && 'GatD' %in% genes) ||
      (!('XylD' %in% genes) && 'RbtT' %in% genes && !('MtlD' %in% genes) && 'GutB' %in% genes && 'LacC' %in% genes && !('GatD' %in% genes) && 'SrlD' %in% genes && !('MtlD1' %in% genes)) ||
      (!('XylD' %in% genes) && !('RbtT' %in% genes) && 'DalD' %in% genes && 'SrlD' %in% genes) ||
      (!('XylD' %in% genes) && !('RbtT' %in% genes) && !('DalD' %in% genes) && 'MtlK' %in% genes && 'MtlD1' %in% genes && 'MtlD' %in% genes)
   ) {
     status <- 1
   } else {
     status <- 0
   }
  } else if (subsystem == "Sugar Alcohols utilization HMP" && phenotype == "Gtl"){
    if(('GatD' %in% genes && 'GatPc' %in% genes && 'GatY' %in% genes && 'GatPb' %in% genes && !('MtlD2' %in% genes) && 'GatPa' %in% genes && !('RbtD' %in% genes)) ||
       ('GatD' %in% genes && 'GatPc' %in% genes && 'GatY' %in% genes && 'GatPb' %in% genes && !('MtlD2' %in% genes) && 'GatPa' %in% genes && 'RbtD' %in% genes && !('LacC' %in% genes)) ||
       ('GatD' %in% genes && 'GatPc' %in% genes && 'GatY' %in% genes && 'GatPb' %in% genes && !('MtlD2' %in% genes) && 'GatPa' %in% genes && 'RbtD' %in% genes && 'LacC' %in% genes && !('MtlA_IIC' %in% genes)) ||
       ('GatD' %in% genes && 'GatPc' %in% genes && 'GatY' %in% genes && 'GatPb' %in% genes && !('MtlD2' %in% genes) && 'GatPa' %in% genes && 'RbtD' %in% genes && 'LacC' %in% genes && 'MtlA_IIC' %in% genes && 'XylD' %in% genes) ||
       ('GatD' %in% genes && 'GatPc' %in% genes && 'GatY' %in% genes && 'GatPb' %in% genes && !('MtlD2' %in% genes) && !('GatPa' %in% genes) && 'LacC' %in% genes) ||
       ('GatD' %in% genes && 'GatPc' %in% genes && 'GatY' %in% genes && 'GatPb' %in% genes && !('MtlD2' %in% genes) && !('GatPa' %in% genes) && !('LacC' %in% genes) && 'GatZ' %in% genes) ||
       ('GatD' %in% genes && 'GatPc' %in% genes && 'GatY' %in% genes && 'GatPb' %in% genes && 'MtlD2' %in% genes && 'MtlA_IIC' %in% genes) ||    
       ('GatD' %in% genes && 'GatPc' %in% genes && 'GatY' %in% genes && !('GatPb' %in% genes) && 'MtlA_IIC' %in% genes && !('DalD' %in% genes)) ||
       ('GatD' %in% genes && 'GatPc' %in% genes && !('GatY' %in% genes) && !('XylD' %in% genes) && !('XltR2' %in% genes) && !('RbtD' %in% genes) && 'GatPa' %in% genes && 'AtlD' %in% genes) ||
       ('GatD' %in% genes && 'GatPc' %in% genes && !('GatY' %in% genes) && !('XylD' %in% genes) && !('XltR2' %in% genes) && !('RbtD' %in% genes) && 'GatPa' %in% genes && !('AtlD' %in% genes) && !('SrlR' %in% genes)) ||
       ('GatD' %in% genes && 'GatPc' %in% genes && !('GatY' %in% genes) && !('XylD' %in% genes) && !('XltR2' %in% genes) && !('RbtD' %in% genes) && !('GatPa' %in% genes) &&  !('XylB' %in% genes)) ||
       
       ('GatD' %in% genes && !('GatPc' %in% genes) && 'GatZ' %in% genes && !('ScrK' %in% genes)) ||       
       (!('GatD' %in% genes) && 'GatPb' %in% genes && 'SrlD' %in% genes && !('GatPa' %in% genes) && !('XylD' %in% genes) && 'MtlR2' %in% genes) ||
       (!('GatD' %in% genes) && 'GatPb' %in% genes && 'SrlD' %in% genes && !('GatPa' %in% genes) && !('XylD' %in% genes) && !('MtlR2' %in% genes) && 'MtlD' %in% genes) ||
       (!('GatD' %in% genes) && 'GatPb' %in% genes && 'SrlD' %in% genes && !('GatPa' %in% genes) && !('XylD' %in% genes) && !('MtlR2' %in% genes) && !('MtlD' %in% genes) && 'MtlR1' %in% genes) ||
       (!('GatD' %in% genes) && 'GatPb' %in% genes && 'SrlD' %in% genes && 'GatPa' %in% genes && 'RbtK' %in% genes) ||
       (!('GatD' %in% genes) && 'GatPb' %in% genes && 'SrlD' %in% genes && 'GatPa' %in% genes && !('RbtK' %in% genes) && 'MtlR1' %in% genes) ||
       (!('GatD' %in% genes) && 'GatPb' %in% genes && 'SrlD' %in% genes && 'GatPa' %in% genes && !('RbtK' %in% genes) && !('MtlR1' %in% genes) && 'XylB' %in% genes && 'GatY' %in% genes && !('SrlR' %in% genes) && 'MtlA_IIA' %in% genes) ||
       (!('GatD' %in% genes) && 'GatPb' %in% genes && 'SrlD' %in% genes && 'GatPa' %in% genes && !('RbtK' %in% genes) && !('MtlR1' %in% genes) && 'XylB' %in% genes && 'GatY' %in% genes && 'SrlR' %in% genes && !('ScrK' %in% genes)) ||
       (!('GatD' %in% genes) && 'GatPb' %in% genes && !('SrlD' %in% genes) && 'GatR' %in% genes) ||
       (!('GatD' %in% genes) && 'GatPb' %in% genes && !('SrlD' %in% genes) && !('GatR' %in% genes) && 'MtlA_IIA' %in% genes && !('MtlA_IIB' %in% genes) && 'XylB' %in% genes && 'LacC' %in% genes) ||
       (!('GatD' %in% genes) && !('GatPb' %in% genes) && 'RbtD' %in% genes && 'MtlD' %in% genes && !('XylD' %in% genes))       
    ) {
      status <- 1
    } else {
      status <- 0
    }
  } else if (subsystem == "Sugar Alcohols utilization HMP" && phenotype == "Srl"){
   if(('SrlE' %in% genes && !('SrlR2' %in% genes) && 'SrlD' %in% genes && !('AtlR1' %in% genes) && !('DalD' %in% genes)) ||
      ('SrlE' %in% genes && !('SrlR2' %in% genes) && 'SrlD' %in% genes && !('AtlR1' %in% genes) && 'DalD' %in% genes && 'MtlA_IIC' %in% genes) ||
      ('SrlE' %in% genes && !('SrlR2' %in% genes) && 'SrlD' %in% genes && !('AtlR1' %in% genes) && 'DalD' %in% genes && !('MtlA_IIC' %in% genes) && 'RbtD' %in% genes) ||
      ('SrlE' %in% genes && !('SrlR2' %in% genes) && 'SrlD' %in% genes && !('AtlR1' %in% genes) && 'DalD' %in% genes && !('MtlA_IIC' %in% genes) && !('RbtD' %in% genes) && 'GatD' %in% genes) ||
      ('SrlE' %in% genes && !('SrlR2' %in% genes) && 'SrlD' %in% genes && 'AtlR1' %in% genes && 'MtlA_IIA' %in% genes) ||     
      ('SrlE' %in% genes && !('SrlR2' %in% genes) && !('SrlD' %in% genes) && 'MtlA_IIC' %in% genes && !('MtlR2' %in% genes)) ||
      ('SrlE' %in% genes && !('SrlR2' %in% genes) && !('SrlD' %in% genes) && 'MtlA_IIC' %in% genes && 'MtlR2' %in% genes && 'GatD' %in% genes && !('AtlD' %in% genes)) ||
      ('SrlE' %in% genes && !('SrlR2' %in% genes) && !('SrlD' %in% genes) && !('MtlA_IIC' %in% genes) && !('SrlR' %in% genes) && 'GatY' %in% genes) ||
      (!('SrlE' %in% genes) && 'SrlP1' %in% genes && !('MtlG' %in% genes) && !('MtlA_IIA' %in% genes)) ||
      (!('SrlE' %in% genes) && 'SrlP1' %in% genes && !('MtlG' %in% genes) && 'MtlA_IIA' %in% genes && 'MtlD2' %in% genes) ||
      (!('SrlE' %in% genes) && !('SrlP1' %in% genes) && 'GutB' %in% genes && 'MtlF' %in% genes) ||
      (!('SrlE' %in% genes) && !('SrlP1' %in% genes) && 'GutB' %in% genes && !('MtlF' %in% genes) && 'GutA' %in% genes) ||
      (!('SrlE' %in% genes) && !('SrlP1' %in% genes) && 'GutB' %in% genes && !('MtlF' %in% genes) && !('GutA' %in% genes) && 'SrlY' %in% genes && !('MtlD1' %in% genes) && !('SrlD' %in% genes) && 'LacC' %in% genes) ||
      (!('SrlE' %in% genes) && !('SrlP1' %in% genes) && 'GutB' %in% genes && !('MtlF' %in% genes) && !('GutA' %in% genes) && 'SrlY' %in% genes && !('MtlD1' %in% genes) && !('SrlD' %in% genes) && !('LacC' %in% genes) && !('XylD' %in% genes)) ||
      (!('SrlE' %in% genes) && !('SrlP1' %in% genes) && 'GutB' %in% genes && !('MtlF' %in% genes) && !('GutA' %in% genes) && 'SrlY' %in% genes && !('MtlD1' %in% genes) && !('SrlD' %in% genes) && !('LacC' %in% genes) && 'XylD' %in% genes && 'DalD' %in% genes) ||
      (!('SrlE' %in% genes) && !('SrlP1' %in% genes) && 'GutB' %in% genes && !('MtlF' %in% genes) && !('GutA' %in% genes) && !('SrlY' %in% genes) && 'RbtD' %in% genes && 'ScrK' %in% genes && !('GatPc' %in% genes)) ||
      (!('SrlE' %in% genes) && !('SrlP1' %in% genes) && 'GutB' %in% genes && !('MtlF' %in% genes) && !('GutA' %in% genes) && !('SrlY' %in% genes) && 'RbtD' %in% genes && 'ScrK' %in% genes && 'GatPc' %in% genes && 'MtlD1' %in% genes) ||
      (!('SrlE' %in% genes) && !('SrlP1' %in% genes) && 'GutB' %in% genes && !('MtlF' %in% genes) && !('GutA' %in% genes) && !('SrlY' %in% genes) && !('RbtD' %in% genes) && 'MtlE' %in% genes && 'DalD' %in% genes) ||
      (!('SrlE' %in% genes) && !('SrlP1' %in% genes) && 'GutB' %in% genes && !('MtlF' %in% genes) && !('GutA' %in% genes) && !('SrlY' %in% genes) && !('RbtD' %in% genes) && !('MtlE' %in% genes) && 'RbtT' %in% genes && !('MtlD1' %in% genes) && !('SrlD' %in% genes)) ||
      (!('SrlE' %in% genes) && !('SrlP1' %in% genes) && !('GutB' %in% genes) && 'SrlR' %in% genes) ||
      (!('SrlE' %in% genes) && !('SrlP1' %in% genes) && !('GutB' %in% genes) && !('SrlR' %in% genes) && 'GatZ' %in% genes && !('RbtK' %in% genes)) ||
      (!('SrlE' %in% genes) && !('SrlP1' %in% genes) && !('GutB' %in% genes) && !('SrlR' %in% genes) && !('GatZ' %in% genes) && 'SrlB' %in% genes && 'XylB' %in% genes && !('GatPc' %in% genes) && 'MtlD1' %in% genes && 'ScrK' %in% genes) ||
      (!('SrlE' %in% genes) && !('SrlP1' %in% genes) && !('GutB' %in% genes) && !('SrlR' %in% genes) && !('GatZ' %in% genes) && !('SrlB' %in% genes) && 'GatPa' %in% genes && 'XylB' %in% genes && 'MtlA_IIA' %in% genes && !('MtlA_IIB' %in% genes) && 'LacC' %in% genes)  
   ) {
     status <- 1
   } else {
     status <- 0
   }
  } else if(subsystem == "Fructoselysine and Glucoselysine utilization HMP" && phenotype == "FruLys"){
    if(('FrlB' %in% genes && 'GfrA' %in% genes && !('FrlD' %in% genes)) ||
       ('FrlB' %in% genes && 'GfrA' %in% genes && 'FrlD' %in% genes && 'GfrF' %in% genes) ||
       ('FrlB' %in% genes && 'GfrA' %in% genes && 'FrlD' %in% genes && !('GfrF' %in% genes) && !('GfrR' %in% genes)) ||
       ('FrlB' %in% genes && 'GfrA' %in% genes && 'FrlD' %in% genes && !('GfrF' %in% genes) && 'GfrR' %in% genes && !('FrlC' %in% genes)) ||
       ('FrlB' %in% genes && !('GfrA' %in% genes) && 'FrlR' %in% genes && 'FrlC' %in% genes) ||
       ('FrlB' %in% genes && !('GfrA' %in% genes) && 'FrlR' %in% genes && !('FrlC' %in% genes) && 'FrlN' %in% genes) ||
       ('FrlB' %in% genes && !('GfrA' %in% genes) && 'FrlR' %in% genes && !('FrlC' %in% genes) && !('FrlN' %in% genes) && 'GfrB' %in% genes) ||
       ('FrlB' %in% genes && !('GfrA' %in% genes) && 'FrlR' %in% genes && !('FrlC' %in% genes) && !('FrlN' %in% genes) && !('GfrB' %in% genes) && 'FrlA' %in% genes) ||
       ('FrlB' %in% genes && !('GfrA' %in% genes) && 'FrlR' %in% genes && !('FrlC' %in% genes) && !('FrlN' %in% genes) && !('GfrB' %in% genes) && !('FrlA' %in% genes) && 'FrlX' %in% genes) ||
       ('FrlB' %in% genes && !('GfrA' %in% genes) && 'FrlR' %in% genes && !('FrlC' %in% genes) && !('FrlN' %in% genes) && !('GfrB' %in% genes) && !('FrlA' %in% genes) && !('FrlX' %in% genes) && 'FrlT2' %in% genes) ||
       ('FrlB' %in% genes && !('GfrA' %in% genes) && 'FrlR' %in% genes && !('FrlC' %in% genes) && !('FrlN' %in% genes) && !('GfrB' %in% genes) && !('FrlA' %in% genes) && !('FrlX' %in% genes) && !('FrlT2' %in% genes) && 'GfrF' %in% genes) ||
       ('FrlB' %in% genes && !('GfrA' %in% genes) && !('FrlR' %in% genes) && 'FrlX' %in% genes) ||
       ('FrlB' %in% genes && !('GfrA' %in% genes) && !('FrlR' %in% genes) && !('FrlX' %in% genes) && 'GfrD' %in% genes && !('GfrR' %in% genes) && 'GfrB' %in% genes) ||
       ('FrlB' %in% genes && !('GfrA' %in% genes) && !('FrlR' %in% genes) && !('FrlX' %in% genes) && 'GfrD' %in% genes && !('GfrR' %in% genes) && !('GfrB' %in% genes) && 'FrlD' %in% genes) ||
       ('FrlB' %in% genes && !('GfrA' %in% genes) && !('FrlR' %in% genes) && !('FrlX' %in% genes) && !('GfrD' %in% genes) && 'FrlA' %in% genes) ||
       ('FrlB' %in% genes && !('GfrA' %in% genes) && !('FrlR' %in% genes) && !('FrlX' %in% genes) && !('GfrD' %in% genes) && !('FrlA' %in% genes) && 'FrlO' %in% genes) ||
       ('FrlB' %in% genes && !('GfrA' %in% genes) && !('FrlR' %in% genes) && !('FrlX' %in% genes) && !('GfrD' %in% genes) && !('FrlA' %in% genes) && !('FrlO' %in% genes) && 'FrlT4' %in% genes) ||
       (!('FrlB' %in% genes) && 'GfrF' %in% genes) ||
       (!('FrlB' %in% genes) && !('GfrF' %in% genes) && 'FrlD' %in% genes)
    ) {
      status <- 1
    } else {
      status <- 0
    }
  } else if(subsystem == "Fructoselysine and Glucoselysine utilization HMP" && phenotype == "GlcLys"){
   if('GfrF' %in% genes
   ) {
     status <- 1
   } else {
     status <- 0
   }
  } else if(subsystem == "Fructoselysine and Glucoselysine utilization HMP" && phenotype == "PsiLys"){
   if(('FrlA' %in% genes && 'FrlD' %in% genes) ||
      (!('FrlA' %in% genes) && 'FrlV' %in% genes && !('FrlR' %in% genes)) ||
      (!('FrlA' %in% genes) && !('FrlV' %in% genes) && 'FrlC' %in% genes && !('GfrD' %in% genes) && !('FrlO' %in% genes) && !('FrlX' %in% genes) && 'FrlR' %in% genes && !('FrlT3' %in% genes))
   ) {
     status <- 1
   } else {
     status <- 0
   }
  } else if(subsystem == "Branched chain amino acid degradation HMP" && phenotype == "Ile_d"){
    if(('IvdC' %in% genes && 'BCDH_E2b' %in% genes && !('AacS2' %in% genes)) ||
       ('IvdC' %in% genes && 'BCDH_E2b' %in% genes && 'AacS2' %in% genes && 'LiuC' %in% genes) ||
       ('IvdC' %in% genes && 'BCDH_E2b' %in% genes && 'AacS2' %in% genes && !('LiuC' %in% genes) && !('EtfD' %in% genes)) ||
       ('IvdC' %in% genes && !('BCDH_E2b' %in% genes) && 'BCDH_E1a' %in% genes) ||
       ('IvdC' %in% genes && !('BCDH_E2b' %in% genes) && !('BCDH_E1a' %in% genes) && 'BCDH_E3' %in% genes) ||
       ('IvdC' %in% genes && !('BCDH_E2b' %in% genes) && !('BCDH_E1a' %in% genes) && !('BCDH_E3' %in% genes) && 'IvdE_Val' %in% genes && !('LiuR' %in% genes)) ||
       (!('IvdC' %in% genes) && 'IvdE_Val' %in% genes && 'SCOTB' %in% genes && !('MCCC2' %in% genes) && 'EtfD' %in% genes)       
    ) {
      status <- 1
    } else {
      status <- 0
    }
  } else if(subsystem == "Branched chain amino acid degradation HMP" && phenotype == "Leu_d"){
   if(('LiuA' %in% genes && 'BCDH_E2b' %in% genes && 'MCCC2' %in% genes && 'BCAT' %in% genes && 'BCDH_E1a' %in% genes) ||
      ('LiuA' %in% genes && 'BCDH_E2b' %in% genes && 'MCCC2' %in% genes && 'BCAT' %in% genes && !('BCDH_E1a' %in% genes) && 'IvdF' %in% genes) ||
      ('LiuA' %in% genes && 'BCDH_E2b' %in% genes && 'MCCC2' %in% genes && 'BCAT' %in% genes && !('BCDH_E1a' %in% genes) && !('IvdF' %in% genes) && 'Ldh' %in% genes) ||
      ('LiuA' %in% genes && 'BCDH_E2b' %in% genes && 'MCCC2' %in% genes && 'BCAT' %in% genes && !('BCDH_E1a' %in% genes) && !('IvdF' %in% genes) && !('Ldh' %in% genes) && 'BCDH_E3' %in% genes) ||
      ('LiuA' %in% genes && 'BCDH_E2b' %in% genes && 'MCCC2' %in% genes && 'BCAT' %in% genes && !('BCDH_E1a' %in% genes) && !('IvdF' %in% genes) && !('Ldh' %in% genes) && !('BCDH_E3' %in% genes) && !('MCCC1' %in% genes)) ||
      ('LiuA' %in% genes && 'BCDH_E2b' %in% genes && 'MCCC2' %in% genes && !('BCAT' %in% genes) && 'Ldh' %in% genes) ||
      ('LiuA' %in% genes && !('BCDH_E2b' %in% genes) && 'BCDH_E1a' %in% genes) ||
      ('LiuA' %in% genes && !('BCDH_E2b' %in% genes) && !('BCDH_E1a' %in% genes) && 'BCDH_E3' %in% genes) ||
      ('LiuA' %in% genes && !('BCDH_E2b' %in% genes) && !('BCDH_E1a' %in% genes) && !('BCDH_E3' %in% genes) && 'IvdC' %in% genes && 'IvdE_Val' %in% genes && !('Ldh' %in% genes)) ||
      (!('LiuA' %in% genes) && 'AacS2' %in% genes) ||
      (!('LiuA' %in% genes) && !('AacS2' %in% genes) && 'LiuR' %in% genes && 'BCDH_E1a' %in% genes && 'EtfD' %in% genes)
   ) {
     status <- 1
   } else {
     status <- 0
   }
  } else if(subsystem == "Branched chain amino acid degradation HMP" && phenotype == "Val_d"){
    if(('IvdC' %in% genes && 'BCDH_E2b' %in% genes && 'IvdB' %in% genes) ||
       ('IvdC' %in% genes && 'BCDH_E2b' %in% genes && !('IvdB' %in% genes) && !('BCDH_E3' %in% genes)) ||
       ('IvdC' %in% genes && !('BCDH_E2b' %in% genes) && 'BCDH_E3' %in% genes) ||
       ('IvdC' %in% genes && !('BCDH_E2b' %in% genes) && !('BCDH_E3' %in% genes) && 'BCDH_E1a' %in% genes && 'IvdD' %in% genes) ||
       ('IvdC' %in% genes && !('BCDH_E2b' %in% genes) && !('BCDH_E3' %in% genes) && !('BCDH_E1a' %in% genes) && 'IvdE_Val' %in% genes && !('LiuR' %in% genes))
    ) {
      status <- 1
    } else {
      status <- 0
    }
  } else if(subsystem == "Alpha-galactosides utilization HMP" && phenotype == "Raf"){
    if(('RafD_bif' %in% genes) ||
       (!('RafD_bif' %in% genes) && 'RafB' %in% genes)
    ) {
      status <- 1
    } else {
      status <- 0
    }
  } else if(subsystem == "Alpha-galactosides utilization HMP" && phenotype == "Aga"){
   if(('AgaF' %in% genes && 'AgaG' %in% genes) ||
      ('AgaF' %in% genes && !('AgaG' %in% genes) && 'AgaL' %in% genes) ||
      (!('AgaF' %in% genes) && 'AgaE' %in% genes) 
   ) {
     status <- 1
   } else {
     status <- 0
   }
  } else if(subsystem == "Alpha-galactosides utilization HMP" && phenotype == "Mel"){
   if(('MelB' %in% genes && 'AgaL' %in% genes) ||
      (!('MelB' %in% genes) && 'RafD_bif' %in% genes)
   ) {
     status <- 1
   } else {
     status <- 0
   }
  } else if(subsystem == "Menaquinone biosynthesis HMP" && phenotype == "MQ"){
   if(('MenD' %in% genes && 'MenB' %in% genes) ||
      (!('MenD' %in% genes) && 'MenF' %in% genes) ||
      (!('MenD' %in% genes) && !('MenF' %in% genes) && 'MenC' %in% genes && 'MenA' %in% genes && 'MenB' %in% genes)
   ) {
     status <- 1
   } else {
     status <- 0
   }
  } else if(subsystem == "Bile acids HMP" && phenotype == 'BA_t') {
    if (('7aHSDH' %in% genes && !('BacT' %in% genes)) ||
        (!('7aHSDH' %in% genes) && 'BaiO' %in% genes && !('BclB' %in% genes)) ||
        (!('7aHSDH' %in% genes) && 'BaiO' %in% genes && 'BclB' %in% genes && '12aHSDH' %in% genes) ||
        (!('7aHSDH' %in% genes) && !('BaiO' %in% genes) && '12aHSDH' %in% genes) ||
        (!('7aHSDH' %in% genes) && !('BaiO' %in% genes) && !('12aHSDH' %in% genes) && '3aHSDH' %in% genes) ||
        (!('7aHSDH' %in% genes) && !('BaiO' %in% genes) && !('12aHSDH' %in% genes) && !('3aHSDH' %in% genes) && 'BaiCD' %in% genes && !('BSH' %in% genes) && 'BaiH' %in% genes) ||
        (!('7aHSDH' %in% genes) && !('BaiO' %in% genes) && !('12aHSDH' %in% genes) && !('3aHSDH' %in% genes) && 'BaiCD' %in% genes && !('BSH' %in% genes) && !('BaiH' %in% genes) && !('BmpR' %in% genes) && !('BtrR' %in% genes)) ||
        (!('7aHSDH' %in% genes) && !('BaiO' %in% genes) && !('12aHSDH' %in% genes) && !('3aHSDH' %in% genes) && !('BaiCD' %in% genes) && 'BaiG' %in% genes) ||
        (!('7aHSDH' %in% genes) && !('BaiO' %in% genes) && !('12aHSDH' %in% genes) && !('3aHSDH' %in% genes) && !('BaiCD' %in% genes) && !('BaiG' %in% genes) && 'BaiH' %in% genes && 'BaiA' %in% genes) 
    ) {
      status <- 1
    } else {
      status <- 0
    }
  } else if(subsystem == "Bile acids HMP" && phenotype == 'CA_d') {
    if (('BSH' %in% genes) ||
        ('BmpR' %in% genes && 'BaiR' %in% genes && !('BaiH' %in% genes))
    ) {
      status <- 1
    } else {
      status <- 0
    }
  } else if (subsystem == 'HMP lactate-acetate-formate fermentation' && phenotype == 'Acetate'){
    if ( ('Pta' %in% genes && 'AckA' %in% genes)
    ) {
      status <- 1
    } else {
      status <- 0
    }
  } else if (subsystem == 'HMP lactate-acetate-formate fermentation' && phenotype == 'D-Lactate'){
    if ( ('LdhA-D' %in% genes) 
    ) {
      status <- 1
    } else {
      status <- 0
    }
  } else if (subsystem == 'HMP lactate-acetate-formate fermentation' && phenotype == 'L-Lactate'){
    if ( ('LdhA-L' %in% genes) 
    ) {
      status <- 1
    } else {
      status <- 0
    }
  } else if (subsystem == 'HMP lactate-acetate-formate fermentation' && phenotype == 'Ethanol'){
    if ( ('AADH' %in% genes && 'ADH' %in% genes) ||
         (!('AADH' %in% genes) && 'AADH2' %in% genes && 'ADH' %in% genes)
    ) {
      status <- 1
    } else {
      status <- 0
    }
  } else if (subsystem == 'HMP lactate-acetate-formate fermentation' && phenotype == 'Formate'){
    if ( ('PflA' %in% genes && 'PflB' %in% genes)
    ) {
      status <- 1
    } else {
      status <- 0
    }
  }
  
  return(status)
}
