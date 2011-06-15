build_swat_basic <-
function(){
data(swat_general)
tmpdir=readline("Please enter a temp directory where you want to build your run...\n")
dir.create(tmpdir)
setwd(tmpdir)
for (file in names(swat_general)){print(file); cat(unlist(swat_general[file]),file=file,sep="\n")}
}

