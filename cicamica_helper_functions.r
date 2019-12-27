# see CELEXHungarian-key
magyar2 <- function(string, only_first=F){
  
  # this is not hungarian
  string <- str_replace_all(string, 'w', 'v')
  # vow
  string <- str_replace_all(string, 'o":', 'W')
  string <- str_replace_all(string, 'u":', 'Y')
  string <- str_replace_all(string, 'a:', 'A')
  string <- str_replace_all(string, 'e:', 'E')
  string <- str_replace_all(string, 'i:', 'I')
  string <- str_replace_all(string, 'o:', 'O')
  string <- str_replace_all(string, 'u:', 'U')
  string <- str_replace_all(string, 'u"', 'y')
  string <- str_replace_all(string, 'o"', 'w')
  # vow vow
  string <- str_replace_all(string, 'á', 'A')
  string <- str_replace_all(string, 'é', 'E')
  string <- str_replace_all(string, 'í', 'I')
  string <- str_replace_all(string, 'ó', 'O')
  string <- str_replace_all(string, 'ú', 'U')
  string <- str_replace_all(string, 'ü', 'y')
  string <- str_replace_all(string, 'ö', 'w')
  string <- str_replace_all(string, 'õ', 'W')
  string <- str_replace_all(string, 'ő', 'W')
  string <- str_replace_all(string, 'û', 'Y')
  string <- str_replace_all(string, 'ű', 'Y')
  # s
  string <- str_replace_all(string, '(?<![cz])s(?!(z|sz))', 'S')
  # long
  string <- str_replace_all(string, 'ccs', 'TT')
  string <- str_replace_all(string, 'ddzs', 'DD')
  string <- str_replace_all(string, 'ss(?!z)', 'SS')
  string <- str_replace_all(string, 'ssz', 'ss')
  string <- str_replace_all(string, 'zzs', 'ZZ')
  string <- str_replace_all(string, 'tty', 'KK')
  string <- str_replace_all(string, 'ggy', 'GG')
  string <- str_replace_all(string, 'nny', 'NN')
  string <- str_replace_all(string, 'lly', 'jj')
  
  # short
  string <- str_replace_all(string, 'cs', 'T')
  string <- str_replace_all(string, 'dzs', 'D')
  string <- str_replace_all(string, 'sz', 's')
  string <- str_replace_all(string, 'zs', 'Z')
  string <- str_replace_all(string, 'ty', 'K')
  string <- str_replace_all(string, 'gy', 'G')
  string <- str_replace_all(string, 'ny', 'N')
  string <- str_replace_all(string, 'ly', 'j')
  # voicing
  string <- str_replace_all(string, 'p(?=[bdgzDGZ])', 'b')
  string <- str_replace_all(string, 'f(?=[bdgzDGZ])', 'v')
  string <- str_replace_all(string, 's(?=[bdgzDGZ])', 'z')
  string <- str_replace_all(string, 'S(?=[bdgzDGZ])', 'Z')
  string <- str_replace_all(string, 't(?=[bdgzDGZ])', 'd')
  string <- str_replace_all(string, 'T(?=[bdgzDGZ])', 'D')
  string <- str_replace_all(string, 'k(?=[bdgzDGZ])', 'g')
  string <- str_replace_all(string, 'K(?=[bdgzDGZ])', 'G')
  # devoicing
  string <- str_replace_all(string, 'b(?=[ptksTKSf])', 'p')
  string <- str_replace_all(string, 'v(?=[ptksTKSf])', 'f')
  string <- str_replace_all(string, 'z(?=[ptksTKSf])', 's')
  string <- str_replace_all(string, 'Z(?=[ptksTKSf])', 'S')
  string <- str_replace_all(string, 'd(?=[ptksTKSf])', 't')
  string <- str_replace_all(string, 'D(?=[ptksTKSf])', 'T')
  string <- str_replace_all(string, 'g(?=[ptksTKSf])', 'k')
  string <- str_replace_all(string, 'G(?=[ptksTKSf])', 'K')
  if (only_first) {
    string <- substr(string, 1, 1)
  }
  return(string)
}
