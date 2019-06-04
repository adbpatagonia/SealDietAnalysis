lengthregs <- function(preycode,group="Front",pMVmm){
#if((is.na(pMVmm))){
#    predlength <- NA   
#    return(predlength) }   
#if(!(is.na(pMVmm))){
lregids <- c(836, 838, 693, 700, 193, 150, 150, 150, 810, 808, 829, 832, 820, 821, 819, 813, 721, 849, 844, 451, 438, 438, 439, 454, 441, 449, 443, 444, 447, 426, 428, 672, 857, 481, 474, 478, 432, 272, 187, 187, 188, 320, 890, 889, 891, 887, 895, 892, 173, 572, 792, 792, 711, 716, 717, 727, 732, 729, 726, 730, 744)
if(preycode ==  836) { # Agonus decagonus (Atlantic poacher)
    predlength <- 4.5148 * pMVmm ^ 0.9471
}
if(preycode ==  838) { #  Aspidophoroides monopterygius(Common Alligatorfish)
    predlength <- -4.3009 * pMVmm + 27.497 * pMVmm - 26.665
}
if(preycode ==  693 ) { #  Ammodytes sp (Sand Launce)
    predlength <- ((76.454 * pMVmm) - 13.547) / 10
}
if(preycode ==  700 ) { #  Anarhichas lupus (Striped wolffish)
    predlength <- 4.5264 * pMVmm ^ 2 - 4.8657 * pMVmm + 9.1141
}
if(preycode ==  193 ) { #  Argentina silus (Atlantic argentine)
    predlength <- (10.466 + (40.03 * pMVmm)) / 10
}
if(preycode ==  150 ) { #  Clupea harengus (Atlantic Herring)
     if (group=="Gulf"){
        predlength <- (5.6553 * pMVmm) + 0.4795
        }else{
           if(pMVmm < 4){
              predlength <- (15.627 + (57.86 * pMVmm)) / 10
           }else{
              predlength <- 5.55 * pMVmm + 0.04
              }
           }
        }

if(preycode ==  810 ) { #  Artediellus atlanticus (Hookear Sculpin)
    predlength <- -0.1261 * pMVmm ^ 2 + 2.9693 * pMVmm
}
if(preycode ==  808 ) { #  Cottidae (Sculpin)
    predlength <- (27 * pMVmm) / 10
}
if(preycode ==  829 ) { #  Cottunculus microps (Polar Sculpin)
    predlength <- 0.7142 * pMVmm ^ 2 - 0.3812 * pMVmm
}
if(preycode ==  832 ) { #  Icelus spatula (Spatulate Sculpin)
    predlength <- 0.2956 * pMVmm ^ 2 + 1.7028 * pMVmm
}
if(preycode ==  820 ) { #  Myoxocephalus Octodecemspinosus (Longhorn Sculpin)
    predlength <- 2.7269 * pMVmm ^ 1.1626
}
if(preycode ==  821 ) { #  Myoxocephalus quadricornis (Fourhorn Sculpin)
    predlength <- 0.277 * pMVmm ^ 2 + 2.5086 * pMVmm
}
if(preycode ==  819 ) { #  Myoxocephalus scorpius (Shorthorn Sculpin)
    predlength <- 0.2024 * pMVmm + 2.574 * pMVmm
}
if(preycode ==  813 ) { #  Triglops sp. (Mailed Sculpin)
    predlength <- 0.5445 * pMVmm ^ 2 + 1.1569 * pMVmm + 2.9606
}
if(preycode ==  721 ) { #  Cryptacanthodes maculatus (Wrymouth)
    predlength <- 2.8132 * (pMVmm ^ 1.46)
}
if(preycode ==  849 ) { #  Cyclopterus lumpus (Common Lumpfish)
    predlength <- (6 * pMVmm) + 20.8
}
if(preycode ==  844 ) { #  Eumicrotremus spinosus (Atlantic Spiny Lumpsucker)
    predlength <- 0.5661 * exp(2.3303 *pMVmm)
}
if(preycode ==  451 ) { #  Boreogadus saida (Arctic Cod)
    predlength <- (19.433 + (18.612 * pMVmm) + 0.546 * (pMVmm ^ 2)) / 10
}
if(preycode ==  438 ) { #  Gadus morhua (Atlantic Cod)
     if (group== "Gulf"){
        predlength <- 6.152 + (0.7341 * pMVmm) + (0.1323 * (pMVmm ^ 2))
     }else{
        predlength <- 4.4986 + 0.1184 * (pMVmm) + 0.1997 * (pMVmm) ^ 2}
}
if(preycode ==  439 ) { #  Gadus ogac (Rock cod)
    predlength <- 0.1001 * pMVmm ^ 2 + 0.9985 * pMVmm + 2.6473
}
if(preycode ==  454 ) { #  Gaidropsarus ensis (Threebeard Rockling)
    predlength <- 4.42241 * pMVmm ^ 1.4878
}
if(preycode ==  441 ) { #  Melanogrammus aeglefinus (Haddock)
    predlength <- exp(2.0479 + (1.0087 * pMVmm) / 10)
}
if(preycode ==  449 ) { #  Merluccius bilinearis (Silver Hake) From Bowen and Harrison 1994 and Hunt 1992
    predlength <- exp(3.01115 + 1.02758 * log(pMVmm / 10))
}
if(preycode ==  443 ) { #  Pollachius virens (Pollock)
    predlength <- exp(3.251 + 1.6251 * log(pMVmm / 10))
}
if(preycode ==  444 ) { #  Urophysis chesteri (Longfin  Hake)
    predlength <- 1.6163 * (pMVmm ^ 1.2118)
}
if(preycode ==  447 ) { #  Urophysis tenuis (White Hake)
    predlength <- 1.52504 * (pMVmm ^ 1.1456)
}
if(preycode ==  426 ) { #  Gasterosteus aculeatus (Threespine Stickleback)
    predlength <- -2.58 + 14.84 * pMVmm
}
if(preycode ==  428 ) { #  Pungitius pungitius (Ninespine Stickleback)
    predlength <- 7.21 * pMVmm
}
if(preycode ==  672  ) { #  Tautogolabrus adspersus (Cunner)ret<-Holly) { # s?
    predlength <- 7.7254 * (pMVmm) ^ 0.8399
}
if(preycode ==  857 ) { #  Liparis sp (Snailfish)
    predlength <- 5.7414 * pMVmm ^ 1.3634
}
if(preycode ==  481 ) { #  Coryphaenoides rupestris (Roundnose Grenadier)
    predlength <- -0.1934 * pMVmm ^ 2 + 7.894 * pMVmm - 16.738
}
if(preycode ==  474 ) { #  Macrourus berglax (Roughhead Grenadier)
    predlength <- 0.1218 * pMVmm ^ 2 + 1.9965 * pMVmm + 3.0626
}
if(preycode ==  478 ) { #  Nezumia bairdi (Marlin Spike or Common Grenadier)
    predlength <- 0.0453 * pMVmm ^ 2 + 3.0707 * pMVmm + 0.5186
}
if(preycode ==  432 ) { #  Antimora rostrata (Blue hake)
    predlength <- 0.3866 * pMVmm ^ 1.7851
}
if(preycode ==  272 ) { #  Myctophidae (Lanterfishes)
    predlength <- 0.3437 * pMVmm ^ 2 + 0.6088 * pMVmm + 3.6332
}
if(preycode ==  187 ) { #  Mallotus villosus (Capelin)
     if (group=="Gulf"){
        predlength <- 5.2997 * (pMVmm ^ 1.01921)
     }else{
         predlength <- ((215.741 * pMVmm) - (176.657 * (pMVmm ^ 2)) + (71.062 * pMVmm ^ 3) - (9.449 * pMVmm ^ 4) - 23.151) / 10}
}


if(preycode ==  188 ) { #  Osmerus mordax (Smelt)
    predlength <- 2.8571 * pMVmm ^ 1.131
}
 if(preycode ==  320 ) { #  Notolepis rissoi (White Barracudina)
    predlength <- 2.9949 * pMVmm ^ 1.6366
}
 if(preycode ==  890 ) { #  Glyptocephalus cynoglossus (Witch Flounder)
    predlength <- 5.0015 * pMVmm ^ 1.1197
}
 if(preycode ==  889 | preycode== 887 ) { #  Hipploglossoides platessoides (American Plaice)
    predlength <- 4.0964 * pMVmm ^ 1.1816
}
 if(preycode ==  891 ) { #  Limanda ferrugina (Yellowtail Flounder)
    predlength <- 3.0814 * pMVmm ^ 1.4412
}
 if(preycode ==  895 ) { #  Pseudopleuronectes americanus (Winter Flounder)
    predlength <- (8.389 * pMVmm) - 8.559
}
 if(preycode ==  892 ) { #  Reinhardtius hippoglossoides (Greenland Halibut)
    predlength <- 0.0009 * pMVmm ^ 2 + 4.962 * pMVmm
}
 if(preycode ==  173 ) { #  Salmo salar (Salmon)
    predlength <- (8.84 * pMVmm) - 4.51
}
 if(preycode ==  572 ) { #  Scomber scombrus (Atlantic Mackerel)
    predlength <- (7.33 * pMVmm) + 0.37
}
 if(preycode ==  792 ) { #  Sebastes sp (Redfish)
     if (group=="Gulf"){
        predlength <- 1.9595 * (pMVmm ^ 1.07223)
     }else{
        predlength <- (0.12 * (pMVmm ^ 2)) + 9.82}
}

if(preycode ==  711 ) { #  Eumesogrammus praecisus (Fourline Snakeblenny)
    predlength <- 3.4394 * pMVmm ^ 0.982
}
if(preycode ==  717 | preycode== 716 ) { #  Lumpenus maculatus (Daubed Shanny)
    predlength <- 9.2666 * pMVmm ^ 0.6212
}
if(preycode ==  729 ) { #  Lycodes reticulatus (Arctic Eelpout)
    predlength <- 2.5241 * pMVmm ^ 1.6747
}
if(preycode ==  726 | preycode== 727 | preycode== 732 | preycode== 744 ) { #  Lycodes sp (Eelpout)
    predlength <- 2.6478 * pMVmm ^ 1.5958
}
if(preycode ==  730 ) { #  Lycodes vahlii (Vahl) { # s Eelpout)
    predlength <- 2.8705 * pMVmm ^ 1.4875
    }
    
    
if(preycode ==  709 ) { #  Pricklebacks (Ns)
    predlength <- 100
    }
if(preycode ==  845 ) { #  Spiny Lumpsucker
    predlength <- 100
    }

if(preycode ==  999 ) { #  unidentified fish
    predlength <- 100
    }     
if(preycode ==  425 ) { #  Stickleback (Ns) Gas.Sp.
    predlength <- 100
    }  


if(preycode ==  436 ) { #  Gadoids (Ns)
     if (group== "Gulf"){
        predlength <- 6.152 + (0.7341 * pMVmm) + (0.1323 * (pMVmm ^ 2))
     }else{
        predlength <- 4.4986 + 0.1184 * (pMVmm) + 0.1997 * (pMVmm) ^ 2}
}      
if(preycode ==  437 ) { #  Gadus sp
     if (group== "Gulf"){
        predlength <- 6.152 + (0.7341 * pMVmm) + (0.1323 * (pMVmm ^ 2))
     }else{
        predlength <- 4.4986 + 0.1184 * (pMVmm) + 0.1997 * (pMVmm) ^ 2}
}    

if(!preycode %in% lregids){ predlength <- NA}            
    return(predlength)
    
 
    
    }