test_gf = data.frame(Filenames=rep('fake_filename', 40),
                     Genotype=rep(c('WT','KO'), each=20),
                     Treatment=rep(rep(c('RAPA','VEH'), each=10), 2),
                     RawAge=31:70,
                     Sex=c(rep(c('M','F'), each=10), rep('M', 20)),
                     Background=c(rep(rep(c('C57Bl6', 'FVB'), each=5), 2), rep(c('CD1', 'Bl6/129'), each=10)))

# 39 files to match the number of files collected for Tsai mice
test_gf = test_gf[-40,]  

write.table(x=test_gf, file='gf_test.txt')