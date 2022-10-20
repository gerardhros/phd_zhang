# designing a meta-regression model

# require packages
require(readxl);require(data.table); require(metafor)

# read in database from excel file 
d1 <- as.data.table(read_xlsx('../02 data/220621 dataset_s1.xlsx',sheet = 'Dataset_s1',col_types = "text"))

# read in the covariates from soil and weather
s1 <- fread('products/220709 covariates metaanalysis.csv')
s1[,c('lon','lat') := NULL]

# columns that are character or numeric
cols.c <- colnames(d1)[grepl('paper|treatm|prec|temp|exp|altit|name|type|unit|code|source',colnames(d1))]
cols.n <- colnames(d1)[!colnames(d1) %in% cols.c]

# ensure that columns are the correct type
d1[,c(cols.c) := lapply(.SD,as.character),.SDcols = cols.c]
d1[,c(cols.n) := lapply(.SD,as.numeric),.SDcols = cols.n]

# remove columns that are not relevant for the meta-analysis
cols.rem <- c('paper_title','paper_author','paper_year','paper_journal')
d1[,c(cols.rem) := NULL]

# merge the spatial coveriates
d2 <- merge(d1,s1,by= 'rank',all.x = TRUE)

# --- data checks ---- 

  # do checks on data, transform where needed, and standardize

  # soil texture classes
  d2[soil_texture == 3 | soil_sand > 65, mv_soiltexture :="sand"]
  d2[soil_texture == 2 | soil_loam > 50, mv_soiltexture :="loam"]
  d2[soil_texture == 1 | soil_clay > 40, mv_soiltexture :="clay"]
  
  # estimate missing soil texture class based on soil type
  d2[,soil_type := tolower(soil_type)]
  d2[is.na(mv_soiltexture) & grepl('ferrosol',soil_type), mv_soiltexture := 'clay']
  d2[is.na(mv_soiltexture) & grepl('haplustox|kandiudox|ultisol',soil_type), mv_soiltexture := 'clay']
  d2[is.na(mv_soiltexture) & grepl('podzol|sandstone|kurosol|udorthent|brunisolic',soil_type), mv_soiltexture := 'sand']
  d2[is.na(mv_soiltexture) & grepl('loess|fragipan|garret',soil_type), mv_soiltexture := 'loam']
  
  # estimate missing soil texture based on ISRIC data?
  d2[is.na(mv_soiltexture), mv_soiltexture := "unknown"]
  
  # get soil type
  d2[soil_type_code == 1, mv_soiltype := 'ferralsols']
  d2[soil_type_code == 2, mv_soiltype := 'luvisols']
  d2[soil_type_code == 3, mv_soiltype := 'anthrosols']
  d2[soil_type_code == 4, mv_soiltype := 'fluvisols']
  d2[soil_type_code == 5, mv_soiltype := 'cambisols & regosols']
  d2[soil_type_code == 6, mv_soiltype := 'phaozems']
  d2[is.na(mv_soiltype), mv_soiltype := "unknown"]
  
  # transform and normalize covariates ISRIC, CNU
  d2[,mv_clay := scale(log(clay_mean_0_5))]
  d2[,mv_soc := scale(log(soc_mean_0_5))]
  d2[,mv_sand := scale(log(sand_mean_0_5))]
  d2[,mv_loam := scale(log(silt_mean_0_5))]
  d2[,mv_ntot := scale(log(ntot_mean_0_5))]
  d2[,mv_phw := scale(log(phw_mean_0_5))]
  d2[,mv_bdod := scale(log(bdod_mean_0_5))]
  d2[,mv_cec := scale(log(cec_mean_0_5))]
  d2[,mv_prec_mean := scale(pre_mean)]
  d2[,mv_prec_sd := scale(pre_sd)]
  d2[,mv_temp_mean := scale(tmp_mean)]
  d2[,mv_temp_sd := scale(tmp_sd)]
  d2[,mv_pet_mean := scale(pet_mean)]
  d2[,mv_pet_sd := scale(pet_sd)]
  
  # get initial soil properties, and replace missing ones by isric estimates or median
  d2[,mv_sp_ph := scale(soil_ph_water)]
  d2[,mv_sp_cec := scale(soil_cec)]
  d2[,mv_sp_om := scale(soil_som)]
  d2[,mv_sp_bs := scale(soil_bs)]
  d2[is.na(mv_sp_ph), mv_sp_ph := mv_phw]
  d2[is.na(mv_sp_cec), mv_sp_cec := mv_cec]
  d2[is.na(mv_sp_om), mv_sp_om := mv_soc]
  d2[is.na(mv_sp_bs), mv_sp_bs := median(d2$mv_sp_bs,na.rm = T)]
  
  # set initial soil properties into classes
  d2[,mv_sp_phc := as.character(cut(soil_ph_water,c(-100,4.5,5,5.5,6,6.5,100)))]
  d2[is.na(mv_sp_phc), mv_sp_phc := 'unknown']
  d2[,mv_sp_cecc := as.character(cut(soil_cec,c(-100,5,10,20,100)))]
  d2[is.na(mv_sp_cecc), mv_sp_cecc := 'unknown']
  d2[,mv_sp_omc := as.character(cut(soil_som,c(-100,6,12,20,30,100)))]
  d2[is.na(mv_sp_omc), mv_sp_omc := 'unknown']
  
  # simplify cropping system
  d2[,crop_name := tolower(crop_name)]
  d2[grepl('potato|sugarc|cane',crop_name),mv_cropping := 'rootcrops']
  d2[is.na(mv_cropping) & grepl('grass|pasture|lucern',crop_name),mv_cropping := 'grass_fodder']
  d2[is.na(mv_cropping) &grepl('barley|grain|wheat|canol|rapese',crop_name),mv_cropping := 'cereal']
  d2[is.na(mv_cropping) &grepl('corn|maize',crop_name),mv_cropping := 'maize']
  d2[is.na(mv_cropping) &grepl('vegetable|cirtus|citrus|lettuce|melon|pean|tomat|cabbag|brassi',crop_name),mv_cropping := 'vegetable']
  d2[is.na(mv_cropping) &grepl('soybean|bean|cowpea|pea',crop_name),mv_cropping := 'nfixating']
  d2[is.na(mv_cropping) &grepl('rice',crop_name),mv_cropping := 'nfixating']
  d2[is.na(mv_cropping), mv_cropping := 'other']
  
  # experiment and treatment code
  d2[,mv_exp_type := tolower(exp_type)]
  d2[id_treatment_code == 1, mv_type := 'lime']
  d2[id_treatment_code == 2, mv_type := 'biochar']
  d2[id_treatment_code == 3, mv_type := 'byproduct']
  d2[id_treatment_code == 4, mv_type := 'manure']
  d2[id_treatment_code == 5, mv_type := 'straw']
  d2[id_treatment_code == 6, mv_type := 'combination']
  
  # experimental duration
  d2[,mv_duration := as.numeric(gsub("[[:alpha:]]","",exp_duration))]
  d2[,mv_durationc := as.character(cut(mv_duration,c(-100,24,72,1000)))]
  d2[is.na(mv_durationc), mv_durationc := 'unknown']
  d2[is.na(mv_duration) & grepl('pot',mv_exp_type), mv_duration := median(d2$mv_duration[d2$mv_exp_type=='pot'],na.rm=T)]
  d2[is.na(mv_duration) & grepl('field',mv_exp_type), mv_duration := median(d2$mv_duration[d2$mv_exp_type=='field'],na.rm=T)]
  
  # rate of application  (kg / ha/ yr)
  d2[,mv_rate_lime := scale(log(lime_rate))]
  d2[,mv_rate_manure := scale(log(manure_rate2))]
  d2[,mv_rate_biochar := scale(log(biochar_rate3))]
  d2[,mv_rate_straw := scale(log(straw_rate3))]
  d2[,mv_rate_byproduct := scale(log(bp_rate3))]
  d2[,mv_rate_combi := scale(log(combination_rate))]
  
  # add climate zone
  d2[,mv_climzone := GEnS]
  
  
# ---- ma for yield ----

  # select only relevant columns for doing meta-regression
  cols.sel <- colnames(d2)[grepl('^ma_|^mv_|^id_liter|n_rep',colnames(d2))]
  
  # subset the data.table
  d3 <- d2[,mget(cols.sel)]
  
  # subset the database for only the yield
  d3.yield <- d3[!is.na(ma_yield_lnrr)]
  
  # calculate effect size yield
  d3.yield.rr <- escalc(measure = "ROM", data = d3.yield, 
                        m1i = ma_yield_mean_t, sd1i = ma_yield_sd_t, n1i = n_rep,
                        m2i = ma_yield_mean_c, sd2i = ma_yield_sd_c, n2i = n_rep )
  
  # make a general model without moderators
  m1 <- rma.mv(yi = yi,V = vi,data = d3.yield.rr,
               random = list(~ 1|id_literature), 
               method="REML",sparse = TRUE)

  # transform data for moderator values and
  m1 <- rma.mv(yi = yi,V = vi,data = d3.yield.rr,
               mods = ~ mv_type-1,
               random = list(~ 1|id_literature), 
               method="REML",sparse = TRUE) 
  
  m1 <- rma.mv(yi = yi,V = vi,data = d3.yield.rr,
               mods = ~ mv_sp_phc-1,
               random = list(~ 1|id_literature), 
               method="REML",sparse = TRUE) 
  
  
  m1c <- (exp(coefficients(m1))-1) * 100
  m1p <- c(4.5,4.75,5.25,5.75,6.25,7,3.5)
require(ggplot2)
  ggplot(data = data.frame(m1c,m1p),aes(x=m1p,y=m1c)) + geom_point(size=6) + theme_bw() +
    xlim(3,8) + ylab('effect on yield (%)') + xlab('initial pH (mean of class)')
  
summary(m1)  

contr <- 5
treat <- 12.5
lnRR <- log(treat/contr)

