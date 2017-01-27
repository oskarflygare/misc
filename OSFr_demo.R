#Working with the Open Science Framework directly from R#


install_github("chartgerink/osfr")
library(osfr)

login()
#Create a project on OSF
main <- create_project(title = "R stuff", description = "Read and write from R", public = FALSE)
main

#Create a subcomponent on OSF
sub <- create_component (id = main,
                         title ="Subcomponent",
                         category = "hypothesis")
sub
#Upload a file to OSF
x <- rnorm(1000, 2, 10)
write.csv(x, "test.csv")
df <- upload_file(id = sub,
                  filename = "test.csv",
                  upload_revision = TRUE)

#Upload your output to OSF (didnt work for me)
pdf("density.pdf")
plot(density(x))
dev.off()
fig <- upload_file(id = main,
                   filename = "density.pdf")
fig

move_file(from = df, to = main) #Moving a file on the OSF (didnt work for me)

delete(id = sub) #Delete stuff, CAREFUL!
sub

download(id = ) #download from OSF

comment_osf(id = main, txt = "This is a test comment") #Comment on OSF (didnt work for me)

logout() #logout from OSF
cleanup() #if you use an open computer, use this to keep your content safe from intruders