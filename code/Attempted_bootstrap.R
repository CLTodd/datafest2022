#play <- fread(file="players.csv")

sample_corr = cor(players$player_id, players$count)

my_data_perms <- players

sample_corr_perms <- replicate(n=10000,{
  
  # In each permutation, keep EntraceExam the same but permute GPA
  my_data_perms$count <- players$count[sample(1:dim(players)[1])]
  
  # Compute sample correlation on the GPA-permuted dataset
  sample_corr(my_data_perms)
})



sample_corr_boot <- replicate(n=10000,{
  
  # Sample with replacement from the given dataset to create the bootstrap dataset
  my_data_boot <- play[sample(1:dim(play)[1], size=dim(play)[1], replace=TRUE), ]
 
   # Compute sample correlation
  sample_corr(my_data_boot)
})
