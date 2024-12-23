Age of Acquisition of 696 Words in Mandarin (Taiwan)
====================================================

The distributions of ages at which words are produced are shown in the figures below. The first figure plots the empirical age distributions of the data collected by by Liu & Tsao (2010). The second figure plots the model-based estimates of the words' age of acquisition.

<img src="made/distribution_per_word.png" width="80%">

下圖為696個華語詞彙產出的年齡分配。這些分配是根據 Liu 與 Tsao (2010) 所收集之 MCDI 資料並透過模型估計而得。原始資料來源為 Wordbank 專案 (Frank 等，2017): <https://github.com/langcog/wordbank>。

<img src="made/AoA-all.png" width="60%">



Data
----

Liu & Tsao (2010)'s data is available from the Wordbank project (Frank et al., 2017) at <https://wordbank.stanford.edu/data/>.
Raw data can be found in `raw/`, and the age distribution data is available in `made/MCDI.age-production.csv`. The model-based estimates are found in `made/MCDI.age.m-produciton.csv`, and the code for fitting the model is found in `model_based_analysis.R` and `model.stan`.


References
----------

Frank, M. J., Braginsky, M., Yurovsky, D., & Marchman, V. A. (2017). Wordbank: An open repository for developmental vocabulary data. Journal of Child Language, 44(3), 677–694. Cambridge Core. https://doi.org/10.1017/S0305000916000209

劉惠美(Huei-Mei Liu) & 曹峰銘(Feng-Ming Tsao). (2010). 華語嬰幼兒溝通發展量表之編製與應用(The Standardization and Application of
Mandarin-Chinese Communicative Developmental Inventory for Infants and
Toddlers). 中華心理衛生學刊(Formosa Journal of Mental Health), 23(4), 503–534. https://doi.org/10.30074/FJMH.201012_23(4).0001
