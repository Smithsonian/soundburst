# * Copyright 2015-2016 Smithsonian Institution.
# *
# * Licensed under the Apache License, Version 2.0 (the "License"); you may not
# * use this file except in compliance with the License.You may obtain a copy of
# * the License at: http://www.apache.org/licenses/
# *
#   * This software and accompanying documentation is supplied without
# * warranty of any kind. The copyright holder and the Smithsonian Institution:
# * (1) expressly disclaim any warranties, express or implied, including but not
# * limited to any implied warranties of merchantability, fitness for a
# * particular purpose, title or non-infringement; (2) do not assume any legal
# * liability or responsibility for the accuracy, completeness, or usefulness of
# * the software; (3) do not represent that use of the software would not
# * infringe privately owned rights; (4) do not warrant that the software
# * is error-free or will be maintained, supported, updated or enhanced;
# * (5) will not be liable for any indirect, incidental, consequential special
# * or punitive damages of any kind or nature, including but not limited to lost
# * profits or loss of data, on any basis arising from contract, tort or
# * otherwise, even if any of the parties has been warned of the possibility of
# * such loss or damage.
# *
#   * This distribution includes several third-party libraries, each with their own
# * license terms. For a complete copy of all copyright and license terms, including
# * those of third-party libraries, please see the product release notes.
# *

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