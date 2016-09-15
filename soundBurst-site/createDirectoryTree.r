create_directory_tree = function(root) {
  tree <- list()
  files <- list.files(root, all.files=F, recursive=T, include.dirs=T, pattern = "\\.wav$")
  print(root)
  
  walk_directory = function(tree, path) {
    fp <- file.path(root, path)
    is_dir <- file.info(fp)$isdir
    folders <- str_split(path, "/")[[1]]
    if (is_dir) {
      txt <- paste("tree", paste("$'", folders, "'", sep="", collapse=""), " = numeric(0)", sep="")
    } else {
      txt <- paste("tree", paste("$'", folders, "'", sep="", collapse=""), " = structure('', sticon='file')", sep="")
    }
    eval(parse(text = txt))
    return(tree)
  }
  
  for (i in 1:length(files)) {
    tree = walk_directory(tree, files[i])
  }
  save(tree, file="www/dir_tree.Rdata")
}