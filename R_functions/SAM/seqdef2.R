seqdef2 <-
function(x, cols=eps, len) {
# This sequence definition includes all the important parameters that need to be set to generate our trees. If data is missing it is automatically recognized in case missing data has format 'missing','-9999', 'NA', 'NaN', 'nan','NULL'. With len you can easy set the length of the sequence (i.e. including variables or not).

# x is a dataframe with sequences svv
# len defines the lenght of the sequence
s.states <- data.frame(s.alphabet=c(seqstatl(x[,1:len], format='STS')), stringsAsFactors=FALSE)
eps_l <- as.data.frame(merge(s.states,eps,type='left',by='s.alphabet'), stringsAsFactors=FALSE)
### Sequence definition only including sequences
if ('xx' %in% eps_l$s.alphabet | 'missing' %in% eps_l$s.alphabet | 'NULL' %in% eps_l$s.alphabet | 'NA' %in% eps_l$s.alphabet | 'NaN' %in% eps_l$s.alphabet | '-9999' %in% eps_l$s.alphabet | 'nan' %in% eps_l$s.alphabet){
seq_rand_50 <- seqdef(x, 1:len, id = rownames(x), alphabet = eps_l$s.alphabet, states = eps_l$s.scodes, labels = eps_l$s.labels, xtstep = fixes, cpal = eps_l$s.palet, missing = 'MV', left = "MV", right = "MV", nr = "*")
} else {
seq_rand_50 <- seqdef(x, 1:len, id = rownames(x), alphabet = eps_l$s.alphabet, states = eps_l$s.scodes, labels = eps_l$s.labels, xtstep = fixes, cpal = eps_l$s.palet)
}
}
