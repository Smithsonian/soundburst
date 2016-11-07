pop <- function(vector.name)
{
  eval.parent(parse(text = paste(vector.name, ' <- ', 
                                 vector.name, '[-length(', vector.name, ')]',
                                 sep = '')),
              n = 1)
}