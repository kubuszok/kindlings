
### AnonVsLambdaCodecReadBenchmark
method                                              2.13         3
anonSimpleCC                                       11.0M     10.9M
factorySimpleCC                                    10.7M     10.3M

### AnonVsLambdaCodecWriteBenchmark
method                                              2.13         3
anonSimpleCC                                       58.8M     64.6M
factorySimpleCC                                    60.1M     65.1M

### AnonVsLambdaCreationBenchmark
method                                              2.13         3
anonCodecCreation                                 335.5M    329.2M
anonHashCreation                                  326.7M    328.9M
anonShowCreation                                  329.0M    331.8M
factoryCodecCreation                              844.0M    874.7M
factoryHashCreation                               843.9M    862.0M
factoryShowCreation                               996.6M      1.0B

### AnonVsLambdaEncoderBenchmark
method                                              2.13         3
anonPerson                                          3.7M      3.7M
anonSimpleCC                                       34.1M     31.5M
factoryPerson                                       3.6M      3.8M
factorySimpleCC                                    33.3M     32.0M

### AnonVsLambdaFunctorBenchmark
method                                              2.13         3
anonMapSimpleCCBox                                273.6M    277.9M
factoryMapSimpleCCBox                             273.3M    272.6M

### AnonVsLambdaHashBenchmark
method                                              2.13         3
anonEqvPerson                                       3.7B      3.7B
anonEqvSimpleCC                                     1.3B      1.3B
anonHashPerson                                     16.5M     18.9M
anonHashSimpleCC                                    3.7B      3.8B
factoryEqvPerson                                    3.8B      3.7B
factoryEqvSimpleCC                                  1.3B      1.3B
factoryHashPerson                                  16.2M     18.4M
factoryHashSimpleCC                                 3.8B      3.8B

### AnonVsLambdaShowBenchmark
method                                              2.13         3
anonEvent                                           2.7M      2.8M
anonPerson                                          3.0M      3.1M
anonSimpleCC                                      124.7M    125.9M
factoryEvent                                        2.8M      2.8M
factoryPerson                                       3.1M      3.1M
factorySimpleCC                                   125.6M    124.7M

### AvroDecodeBenchmark
method                                              2.13         3
kindlingsEvent                                      8.5M      8.6M
kindlingsPerson                                     9.8M      9.3M
kindlingsSimpleADT                                168.0M    167.0M
kindlingsSimpleCC                                 119.2M    127.2M
originalAutoPerson                                  3.7M      4.4M
originalAutoSimpleCC                               17.7M     83.9M
originalSemiAutoPerson                                 —      3.1M
originalSemiAutoSimpleCC                               —     26.0M

### AvroEncodeBenchmark
method                                              2.13         3
kindlingsEvent                                     17.4M     18.0M
kindlingsPerson                                    19.5M     19.1M
kindlingsSimpleADT                                364.3M    378.5M
kindlingsSimpleCC                                 272.5M    277.4M
originalAutoPerson                                  4.5M      5.8M
originalAutoSimpleCC                               44.6M     50.5M
originalSemiAutoPerson                                 —      5.8M
originalSemiAutoSimpleCC                               —     48.7M

### CatsEmptyBenchmark
method                                              2.13         3
kindlingsEmpty                                      1.6B      1.8B
originalSemiAutoEmpty                               1.6B      1.1B

### CatsEqBenchmark
method                                              2.13         3
kindlingsSimpleCCEqual                            100.2M    102.3M
kindlingsSimpleCCNotEqual                         549.8M    561.4M
originalAutoSimpleCCEqual                          45.5M     92.2M
originalSemiAutoSimpleCCEqual                      46.1M     86.5M

### CatsFoldableBenchmark
method                                              2.13         3
kindlingsSimpleCCBoxFoldLeft                        1.6B      1.6B

### CatsFoldableKittensBenchmark
method                                              2.13         3
kittensFoldLeft                                        —    109.6M

### CatsFunctorBenchmark
method                                              2.13         3
kindlingsSimpleCCBoxMap                           277.3M    275.9M
originalSemiAutoSimpleCCBoxMap                      5.7M     65.2M

### CatsHashBenchmark
method                                              2.13         3
kindlingsSimpleCC                                 824.9M    828.3M
originalAutoSimpleCC                               27.4M    108.3M
originalSemiAutoSimpleCC                           27.2M    110.0M

### CatsMonoidBenchmark
method                                              2.13         3
kindlingsCombine                                  192.8M    193.9M
kindlingsEmpty                                      3.6B      3.7B
originalSemiAutoCombine                            49.0M    119.5M
originalSemiAutoEmpty                               1.7B      1.0B

### CatsOrderBenchmark
method                                              2.13         3
kindlingsSimpleCCCompare                          424.8M    429.7M
originalAutoSimpleCCCompare                       387.8M    347.6M
originalSemiAutoSimpleCCCompare                   389.7M    313.9M

### CatsSemigroupBenchmark
method                                              2.13         3
kindlingsCombine                                  194.1M    193.6M
originalSemiAutoCombine                            54.3M    146.3M

### CatsShowBenchmark
method                                              2.13         3
kindlingsEvent                                      1.9M      1.5M
kindlingsPerson                                     2.0M      1.6M
kindlingsSimpleADT                                 86.0M     72.8M
kindlingsSimpleCC                                  38.2M     27.2M
originalAutoEvent                                   620K      1.2M
originalAutoPerson                                  804K      1.4M
originalAutoSimpleADT                              10.4M     52.8M
originalAutoSimpleCC                                7.0M     19.9M
originalSemiAutoEvent                               602K      535K
originalSemiAutoSimpleADT                          16.2M     51.9M
originalSemiAutoSimpleCC                            7.5M     19.6M

### CatsShowPrettyBenchmark
method                                              2.13         3
kindlingsFastShowPrettyPerson                       1.2M      1.3M
kindlingsFastShowPrettySimpleCC                    13.5M     18.0M
kindlingsShowPerson                                 2.0M      1.7M
kindlingsShowPrettyPerson                           1.9M      1.8M
kindlingsShowPrettySimpleCC                        34.4M     34.3M
kindlingsShowSimpleCC                              39.1M     27.0M

### CatsShowPrettyKittensBenchmark
method                                              2.13         3
kittensShowPrettyPerson                                —      557K
kittensShowPrettySimpleCC                              —      5.4M

### CatsTraverseBenchmark
method                                              2.13         3
kindlingsSimpleCCBoxTraverse                      170.8M    164.2M

### CatsTraverseKittensBenchmark
method                                              2.13         3
kittensTraverse                                        —     18.7M

### CirceBoosterDecodeBenchmark
method                                              2.13         3
kindlingsBoosterEvent                               1.0M      996K
kindlingsBoosterPerson                              1.3M      1.3M
kindlingsBoosterSimpleADT                          11.2M     10.9M
kindlingsBoosterSimpleCC                            9.3M      8.8M
kindlingsNoBoosterEvent                             736K      836K
kindlingsNoBoosterPerson                            918K      1.1M
kindlingsNoBoosterSimpleADT                         8.7M      9.8M
kindlingsNoBoosterSimpleCC                          6.1M      7.1M
originalBoosterEvent                                906K      825K
originalBoosterPerson                               1.1M      1.0M
originalBoosterSimpleADT                            9.1M      9.2M
originalBoosterSimpleCC                             8.1M      6.6M
originalNoBoosterEvent                              724K      703K
originalNoBoosterPerson                             879K      874K
originalNoBoosterSimpleADT                          7.4M      8.5M
originalNoBoosterSimpleCC                           5.9M      5.9M

### CirceBoosterEncodeBenchmark
method                                              2.13         3
kindlingsBoosterEvent                               1.3M      1.4M
kindlingsBoosterPerson                              1.6M      1.7M
kindlingsBoosterSimpleADT                          14.3M     15.6M
kindlingsBoosterSimpleCC                           13.9M     15.5M
kindlingsNoBoosterEvent                             831K      939K
kindlingsNoBoosterPerson                            985K      1.1M
kindlingsNoBoosterSimpleADT                         7.8M      8.1M
kindlingsNoBoosterSimpleCC                          6.8M      7.2M
originalBoosterEvent                                1.1M      1.2M
originalBoosterPerson                               1.4M      1.5M
originalBoosterSimpleADT                            8.1M     11.7M
originalBoosterSimpleCC                            10.5M     12.0M
originalNoBoosterEvent                              764K      805K
originalNoBoosterPerson                             882K      964K
originalNoBoosterSimpleADT                          5.9M      6.9M
originalNoBoosterSimpleCC                           5.4M      6.7M

### CirceDecodeBenchmark
method                                              2.13         3
kindlingsAutoEvent                                  3.5M      3.3M
kindlingsAutoPerson                                 5.3M      5.4M
kindlingsAutoSimpleADT                             55.9M     54.6M
kindlingsAutoSimpleCC                              93.2M     92.1M
kindlingsSemiAutoEvent                              3.3M      3.5M
kindlingsSemiAutoPerson                             5.4M      5.5M
kindlingsSemiAutoSimpleADT                         56.3M     58.3M
kindlingsSemiAutoSimpleCC                          88.3M     91.9M
originalAutoEvent                                   2.7M      2.2M
originalAutoPerson                                  3.6M      2.6M
originalAutoSimpleADT                              25.7M     28.0M
originalAutoSimpleCC                               42.6M     21.2M
originalSemiAutoEvent                               2.7M      2.1M
originalSemiAutoPerson                              3.5M      2.7M
originalSemiAutoSimpleADT                          25.0M     27.9M
originalSemiAutoSimpleCC                           42.0M     20.5M

### CirceEncodeBenchmark
method                                              2.13         3
kindlingsAutoEvent                                  3.4M      3.4M
kindlingsAutoPerson                                 4.5M      4.5M
kindlingsAutoSimpleADT                             27.1M     25.7M
kindlingsAutoSimpleCC                              30.9M     31.2M
kindlingsSemiAutoEvent                              3.4M      3.3M
kindlingsSemiAutoPerson                             4.5M      4.4M
kindlingsSemiAutoSimpleADT                         27.5M     26.8M
kindlingsSemiAutoSimpleCC                          30.3M     31.2M
originalAutoEvent                                   2.4M      2.3M
originalAutoPerson                                  3.1M      3.2M
originalAutoSimpleADT                              13.9M     27.1M
originalAutoSimpleCC                               19.0M     20.9M
originalSemiAutoEvent                               2.3M      2.4M
originalSemiAutoPerson                              3.0M      3.1M
originalSemiAutoSimpleADT                          13.4M     26.6M
originalSemiAutoSimpleCC                           18.8M     21.8M

### FastShowPrettyBenchmark
method                                              2.13         3
kindlingsEvent                                      854K      972K
kindlingsPerson                                     1.2M      1.3M
kindlingsSimpleADT                                 15.6M     14.6M
kindlingsSimpleCC                                  13.5M     17.9M

### JsoniterReadBenchmark
method                                              2.13         3
kindlingsAutoEvent                                  3.3M      3.3M
kindlingsAutoPerson                                 3.6M      3.7M
kindlingsAutoSimpleADT                             16.8M     15.7M
kindlingsAutoSimpleCC                              35.8M     36.4M
kindlingsSemiAutoEvent                              3.3M      3.3M
kindlingsSemiAutoPerson                             3.6M      3.6M
kindlingsSemiAutoSimpleADT                         15.5M     15.8M
kindlingsSemiAutoSimpleCC                          36.4M     36.4M
originalSemiAutoPerson                              3.8M      3.8M
originalSemiAutoSimpleCC                           35.5M     35.1M

### JsoniterWriteBenchmark
method                                              2.13         3
kindlingsAutoEvent                                  4.5M      4.8M
kindlingsAutoPerson                                 4.7M      5.4M
kindlingsAutoSimpleADT                             62.7M     65.7M
kindlingsAutoSimpleCC                              59.3M     63.9M
kindlingsSemiAutoEvent                              4.5M      4.8M
kindlingsSemiAutoPerson                             4.8M      5.5M
kindlingsSemiAutoSimpleADT                         62.2M     65.9M
kindlingsSemiAutoSimpleCC                          61.1M     63.6M
originalSemiAutoEvent                               4.3M      4.9M
originalSemiAutoPerson                              4.7M      5.4M
originalSemiAutoSimpleADT                          69.2M     71.9M
originalSemiAutoSimpleCC                           60.8M     63.8M

### OpticsBenchmark
method                                              2.13         3
handWrittenDeepName                                98.8M     93.2M
handWrittenEachSalary                              21.4M     21.9M
kindlingsDeepName                                  99.0M     99.2M
kindlingsEachSalary                                21.8M     21.2M
quicklensDeepName                                  98.7M     99.1M
quicklensEachSalary                                20.1M     20.8M

### PureconfigReadBenchmark
method                                              2.13         3
kindlingsPerson                                     1.0M      1.0M
kindlingsSimpleCC                                  17.2M     17.2M
originalSemiAutoPerson                              216K      197K
originalSemiAutoSimpleCC                            1.4M      1.4M

### PureconfigWriteBenchmark
method                                              2.13         3
kindlingsPerson                                     1.2M      1.2M
kindlingsSimpleCC                                  11.0M     11.1M
originalSemiAutoPerson                              205K      248K
originalSemiAutoSimpleCC                            1.2M      1.7M

### ScalacheckArbitraryBenchmark
method                                              2.13         3
kindlingsPerson                                       4K        4K
kindlingsSimpleADT                                  1.9M      1.9M
kindlingsSimpleCC                                   779K      741K

### ScalacheckShrinkBenchmark
method                                              2.13         3
kindlingsPerson                                     5.8M      5.2M
kindlingsSimpleCC                                   5.8M      6.9M

### SconfigReadBenchmark
method                                              2.13         3
kindlingsEvent                                      2.6M      2.8M
kindlingsPerson                                     5.0M      4.7M
kindlingsSimpleADT                                 12.0M     11.8M
kindlingsSimpleCC                                  72.4M     60.9M

### SconfigWriteBenchmark
method                                              2.13         3
kindlingsEvent                                      630K      648K
kindlingsPerson                                     1.2M      1.3M
kindlingsSimpleADT                                  5.2M      6.3M
kindlingsSimpleCC                                  10.9M     10.9M

### TapirOpenApiJsoniterBenchmark
method                                              2.13         3
circeDecode                                          12K        5K
circeEncode                                          24K       24K
kindlingsDecode                                      26K       21K
kindlingsEncode                                      70K       56K

### TapirSchemaBenchmark
method                                              2.13         3
kindlingsEvent                                      3.9B      3.8B
kindlingsPerson                                     3.9B      3.8B
kindlingsSimpleCC                                   3.8B      3.8B
originalAutoEvent                                   3.8B      3.8B
originalAutoPerson                                  3.8B      3.8B
originalAutoSimpleCC                                3.8B      3.8B
originalSemiAutoEvent                               3.8B      3.8B
originalSemiAutoPerson                              3.8B      3.7B
originalSemiAutoSimpleCC                            3.8B      3.8B

### UbjsonReadBenchmark
method                                              2.13         3
kindlingsEvent                                      1.3M      1.2M
kindlingsPerson                                     1.4M      1.4M
kindlingsSimpleADT                                 13.8M     13.6M
kindlingsSimpleCC                                  11.3M     10.4M

### UbjsonWriteBenchmark
method                                              2.13         3
kindlingsEvent                                      1.3M      1.2M
kindlingsPerson                                     1.4M      1.4M
kindlingsSimpleADT                                 13.4M     13.2M
kindlingsSimpleCC                                  11.2M     11.0M

### XmlDecodeBenchmark
method                                              2.13         3
kindlingsAddress                                    3.2M      3.4M
kindlingsSimpleCC                                   4.8M      5.0M

### XmlEncodeBenchmark
method                                              2.13         3
kindlingsAddress                                   38.6M     38.8M
kindlingsSimpleCC                                  46.2M     45.2M

### YamlDecodeBenchmark
method                                              2.13         3
kindlingsEvent                                      721K      629K
kindlingsPerson                                     774K      763K
kindlingsSimpleADT                                 98.6M    104.1M
kindlingsSimpleCC                                   9.4M      7.0M

### YamlEncodeBenchmark
method                                              2.13         3
kindlingsEvent                                      136K      148K
kindlingsPerson                                     147K      164K
kindlingsSimpleADT                                  2.2M      2.3M
kindlingsSimpleCC                                   1.4M      1.4M
