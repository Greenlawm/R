process.hagfish.logbook = function(){
  
p = setup.parameters()
l = hagfish.logbook.db(DS="logbook")
h.z <- load.shapefile(DS = 'hagfish.z')
l = intersect.sp(l, h.z, var = 'zones')

#Pre 2009
#120 lb per barrel or 925 lbs per wharf box winter (1 Nov - 30 April)
#80 lbs per barrel or 616.8 lbs per wharf box summer (1 may - 31 october)

s = l[which(l$yr < '2009' & (l$month >= 11| l$month <= 4)), 7]
l[which(l$yr < '2009' & (l$month >= 11| l$month <= 4)), 7] <-  s * 1.15 #(1061 lbs per wharf box/925 lbs per wharf box)

s = l[which(l$yr < '2009' & (l$month > 4| l$month < 11)), 7]
l[which(l$yr < '2009' & (l$month > 4| l$month < 11)), 7] <- s * 1.72


#2009-1015
#146 lbs per barrel or 1126 lbs per warf box, 7.71 barrels per box

s = l[which(l$yr >= '2009'| l$yr <= '2015'), 7]
l[which(l$yr >= '2009'| l$yr <= '2015'), 7] <- s * 0.943

#2016-continuing
#1062 lbs warf box (3/4 full 797, ½ full 531, ¼ full 266)

#CPUE for change from 1/2" to 9/16" hole size
l$traps_retrieved = as.numeric(l$traps_retrieved)
l$cpue = l$traps_retrieved
s <- l[which(is.finite(l$traps_retrieved)), ] 
s$cpue = s$landings/s$traps_retrieved

l[which(is.finite(l$traps_retrieved)), 19] <- s$cpue

s = l[which(l$yr >= '2014' & (l$zones == 'Gully'| l$zones == 'Slope'| l$zones == '4Vn Slope') & is.finite(l$cpue)), 19 ]
l[which(l$yr >= '2014'& (l$zones == 'Gully'| l$zones == 'Slope'| l$zones == '4Vn Slope') & is.finite(l$cpue)), 19] <- s + 12.5

s = l[which(l$yr >= '2014' & (l$zones == 'Mid-Shore' | l$zones == 'Jordan' | l$zones == 'NE Channel' | l$zones == 'Georges Basin'| 
                                l$zones == '4vn'| l$zones == 'Other') & is.finite(l$cpue)), 19 ]
l[which(l$yr >= '2014'& (l$zones == 'Mid-Shore' | l$zones == 'Jordan' | l$zones == 'NE Channel' | l$zones == 'Georges Basin'| 
                           l$zones == '4vn'| l$zones == 'Other') & is.finite(l$cpue)), 19] <- s + 5

return(l)
}

