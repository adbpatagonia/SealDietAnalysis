weightregs <- function(preycode,group="Front",pMVmm,preylength){
   wregids <- c(836, 838, 693, 700, 193, 150, 150, 810, 808, 829, 832, 820, 821, 819, 813, 721, 849, 844, 451, 438, 438, 439, 454, 441, 449, 443, 444, 447, 426, 428, 672, 857, 481, 474, 478, 432, 272, 187, 187, 188, 320, 890, 889, 891, 887, 895, 892, 173, 572, 792, 792, 711, 716, 717, 727, 732, 729, 726, 730, 744)
   wregpvids <- c(838, 693, 193, 808, 451, 439, 426, 428, 672, 173, 572, 150, 792, 187)
   wregplids <-c(836,711,717,716,729,726,727,732,744,730,700,810,829,832,820,821,819,813,721,849,844,438,454,441,449,443,444,447,857,481,474,478,432,272,188,320,890,889,887,891,895,892,150,792,187)   
   
   if(!preycode %in% wregids){predweight <- NA}
   if(preycode %in% wregpvids){
      if(preycode == 838  ) { #  Aspidophoroides monopterygius(Common Alligatorfish)
          predweight <- 0.5532 * pMVmm ^ 2 + 3.0997 * pMVmm - 4.8008}
      if(preycode == 693  ) { #  Ammodytes sp (Sand Launce)
          predweight <- 0.371 * (pMVmm ^ 3.89)}
      if(preycode == 193  ) { #  Argentina silus (Atlantic argentine)
          predweight <- 0.5592 * (pMVmm ^ 3.173)}
      if(preycode == 808  ) { #  Cottidae (Sculpin)
          predweight <- 1.565 * pMVmm}
      if(preycode == 451  ) { #  Boreogadus saida (Arctic Cod)
          predweight <- 0.2 * (pMVmm ^ 2.64)}
      if(preycode == 439  ) { #  Gadus ogac (Rock cod)
          predweight <- 0.0101 * pMVmm ^ 4.0995}
      if(preycode == 426  ) { #  Gasterosteus aculeatus (Threespine Stickleback)
          predweight <- (2.02 * pMVmm) ^ 4.28}
      if(preycode == 428  ) { #  Pungitius pungitius (Ninespine Stickleback)
          predweight <- 1.54 * pMVmm ^ 2.62 }
      if(preycode == 672  ) { #  Tautogolabrus adspersus (Cunner)      predweight<-HpMVmmly ) { # s?
          predweight <- 19.635 * (pMVmm) ^ 1.7165  }
      if(preycode == 173  ) { #  Salmo salar (Salmon)
          predweight <- 16.78 * (pMVmm ^ 2.45)}
      if(preycode == 572  ) { #  Scomber scombrus (Atlantic Mackerel)
          predweight <- 1.094 * (pMVmm ^ 4.039) }
     if(preycode == 150  ) { #  Clupea harengus (Atlantic Herring)
         if (group!="Gulf"){
            predweight <- 1.48 * (pMVmm ^ 3.08)}
         if (group=="Gulf"){
            predweight <- NA}
      }
      if(preycode == 792  ) { #  Sebastes sp (Redfish)
         if (group!="Gulf"){
            predweight <- 0.13 * (pMVmm ^ 3.12)}
         if (group=="Gulf"){
            predweight <- NA}
       }
       
      if(preycode == 187  ) { #  Mallotus villosus (Capelin)
         if (group=="Gulf"){
           predweight <- 1.31383 * (pMVmm ^ 2.46456)}
         if (group!="Gulf"){
            predweight <- NA}
         }
      }

   if(preycode %in% wregplids){       
#   if(!(is.na(preylength))){    
      if(preycode == 836  ) { #  Agonus decagonus (Atlantic poacher)
          predweight <- 0.0032 * preylength ^ 3.0667}
      if(preycode == 711  ) { #  Eumesogrammus praecisus (Fourline Snakeblenny)
          predweight <- 0.0044 * preylength ^ 3.2216}
      if(preycode == 717 | preycode== 716  ) { #  Lumpenus maculatus (Daubed Shanny)
          predweight <- 0.0009 * preylength ^ 3.4609 }
      if(preycode == 729  ) { #  Lycodes       predweighticulatus (Arctic Eelpout)
          predweight <- 0.004 * preylength ^ 3.0911 }
      if(preycode == 726 | preycode== 727 | preycode== 732 | preycode== 744  ) { #  Lycodes sp (Eelpout)
          predweight <- 0.0049 * preylength ^ 2.9696}
      if(preycode == 730  ) { #  Lycodes vahlii (Vahl ) { # s Eelpout)
          predweight <- 0.0047 * preylength ^ 0.9385}
      if(preycode == 700  ) { #  Anarhichas lupus (Striped wolffish)
          predweight <- 0.0088 * preylength ^ 2.9758}
      if(preycode == 810  ) { #  Artediellus atlanticus (Hookear Sculpin)
          predweight <- 0.2345 * preylength ^ 2 - 0.9733 * preylength}
      if(preycode == 829  ) { #  Cottunculus microps (Polar Sculpin)
          predweight <- 1.5298 * preylength ^ 2 - 39.726 * preylength + 337.18}
      if(preycode == 832  ) { #  Icelus spatula (Spatulate Sculpin)
          predweight <- 0.0059 * preylength ^ 3.2476}
      if(preycode == 820  ) { #  Myoxocephalus Octodecemspinosus (Longhorn Sculpin)
          predweight <- 0.5962 * preylength^2 - 9.9759 * preylength + 52.168}
      if(preycode == 821  ) { #  Myoxocephalus quadricornis (Fourhorn Sculpin)
          predweight <- 0.0092 * preylength ^ 3.0848}
      if(preycode == 819  ) { #  Myoxocephalus scorpius (Shorthorn Sculpin)
          predweight <- 0.0069 * preylength ^ 3.2435}
      if(preycode == 813  ) { #  Triglops sp. (Mailed Sculpin)
          predweight <- 0.0019 * preylength ^ 3.5581 }
      if(preycode == 721  ) { #  Cryptacanthodes maculatus (Wrymouth)
          predweight <- 0.000071 * (preylength ^ 3.8569) }
      if(preycode == 849  ) { #  Cyclopterus lumpus (Common Lumpfish)
          predweight <- (32.71 * preylength) - 1.305}
      if(preycode == 844  ) { #  Eumicrotremus spinosus (Atlantic Spiny Lumpsucker)
          predweight <- 0.0362 * preylength ^ 3.173}               
      if(preycode == 438  ) { #  Gadus morhua (Atlantic Cod)
         if (group=="Gulf"){
            predweight <- 0.0032 * (preylength ^ 3.2644)
      }else{         
            predweight <- 10 ^ (-5.2106 + 3.0879 * ((log(preylength) / log(10)))) * 1000}      
   } 

      if(preycode == 454  ) { #  Gaidropsarus ensis (Threebeard Rockling)
          predweight <- 0.0017 * preylength ^ 3.4352}
      if(preycode == 441  ) { #  Melanogrammus aeglefinus (Haddock)
          predweight <- 0.0077 * (preylength ^ 3.073)}
      if(preycode == 449  ) { #  Merluccius bilinearis (Silver Hake) From Bowen and Harrison 1994 and Hunt 1992
          predweight <- 0.0059 * ((preylength)^3.05)}
      if(preycode == 443  ) { #  Pollachius virens (Pollock)
          predweight <- 0.0134 * (preylength ^ 2.94)}
      if(preycode == 444  ) { #  Urophysis chesteri (Longfin  Hake)
          predweight <- 0.0057 * (preylength ^ 2.944)}
      if(preycode == 447  ) { #  Urophysis tenuis (White Hake)
          predweight <- 0.003998 * (preylength ^ 3.1718)}
      if(preycode == 857  ) { #  Liparis sp (Snailfish)
          predweight <- 0.0065 * preylength ^ 3.1802 }
      if(preycode == 481  ) { #  Coryphaenoides rupestris (Roundnose Grenadier)
          predweight <- 1.1672 * exp(0.1387 * preylength)}
      if(preycode == 474  ) { #  Macrourus berglax (Roughhead Grenadier)
          predweight <- 0.0011 * preylength ^ 3.3695}
      if(preycode == 478  ) { #  Nezumia bairdi (Marlin Spike or Common Grenadier)
          predweight <- 0.0014 * preylength ^ 3.2134}
      if(preycode == 432  ) { #  Antimora rostrata (Blue hake)
          predweight <- 0.0016 * preylength ^ 3.3617}
      if(preycode == 272  ) { #  Myctophidae (Lanterfishes)
          predweight <- 0.1538 * preylength ^ 2 - 1.1205 * preylength + 3.3602}
      if(preycode == 188  ) { #  Osmerus mordax (Smelt)
          predweight <- 0.0026 * (preylength ^ 3.3001)}
      if(preycode == 320  ) { #  Notolepis rissoi (White Barracudina)
          predweight <- 0.0002 * preylength ^ 3.5982}
      if(preycode == 890  ) { #  Glyptocephalus cynoglossus (Witch Flounder)
          predweight <- 0.0021 * preylength ^ 3.3098}
      if(preycode == 889 | preycode== 887  ) { #  Hipploglossoides platessoides (American Plaice)
          predweight <- 0.0044 * preylength ^ 3.1983}
      if(preycode == 891  ) { #  Limanda ferrugina (Yellowtail Flounder)
          predweight <- 0.0076 * preylength ^ 3.0467}
      if(preycode == 895  ) { #  Pseudopleuronectes americanus (Winter Flounder)
          predweight <- 0.0079 * (preylength ^ 3.12)}
      if(preycode == 892  ) { #  Reinhardtius hippoglossoides (Greenland Halibut)
          predweight <- 0.0025 * preylength ^ 3.3399 }
          
          
      if(preycode == 150  ) { #  Clupea harengus (Atlantic Herring)
         if (group=="Gulf"){
            predweight <- 0.00509 * (preylength ^ 3.16138)} 
           }
      if(preycode == 792  ) { #  Sebastes sp (Redfish)
         if (group=="Gulf"){
            predweight <- 0.009465 * (preylength ^ 3.1235)}                      
           }
      if(preycode == 187  ) { #  Mallotus villosus (Capelin)
         if (group!="Gulf"){
           predweight <- exp((log(preylength) * 3.808) - 7.63)}
         }       
      }
    return(predweight)    
   }         