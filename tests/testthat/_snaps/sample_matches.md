# format snapshot is stable for high mode (duplicates, n=3)

    Code
      cat(format(res), sep = "\n")
    Output
      <joinery::Match_Sample>
        mode : high
        n    : 3
      
      rows: 3 row(s)
      
           duplicate_group     id score  rank
                     <int> <char> <num> <int>
        1:               1      a  0.95     1
        2:               1      b  0.90     2
        3:               1      c  0.85     3

