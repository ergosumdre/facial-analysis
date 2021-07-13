library(shiny)
library(shinysense)
library(tidyverse)
library("Rvision")
library("magick")
library(shiny)
library("devtools")
library("ROpenCVLite")

# installOpenCV()
library("Rvision")
library("magick")
library("paws")
# library("keras")


### Must use these versions ###
# conda_create("r-reticulate")
# use_virtualenv("r-reticulate")
# install_version("xtable", version = "1.8.4", repos = "http://cran.us.r-project.org", upgrade = "never")
# install_version("R6", version = "2.5.0", repos = "http://cran.us.r-project.org", upgrade = "never")
# install_version("rlang", version = "0.4.10", repos = "http://cran.us.r-project.org", upgrade = "never")
# install_version("fastmap", version = "1.1.0", repos = "http://cran.us.r-project.org", upgrade = "never")
# install_version("png", version = "0.1-7", repos = "http://cran.us.r-project.org", upgrade = "never")
# install_version("jquerylib", version = "0.1.3", repos = "http://cran.us.r-project.org", upgrade = "never")
# install_version("withr", version = "2.4.1", repos = "http://cran.us.r-project.org", upgrade = "never")
# install_version("htmltools", version = "0.5.1.1", repos = "http://cran.us.r-project.org", upgrade = "never")
# install_version("ellipsis", version = "0.3.1", repos = "http://cran.us.r-project.org", upgrade = "never")
# install_version("yaml", version = "2.2.1", repos = "http://cran.us.r-project.org", upgrade = "never")
# install_version("digest", version = "0.6.27", repos = "http://cran.us.r-project.org", upgrade = "never")
# install_version("lifecycle", version = "0.2.0", repos = "http://cran.us.r-project.org", upgrade = "never")
# install_version("later", version = "1.1.0.1", repos = "http://cran.us.r-project.org", upgrade = "never")
# install_version("base64enc", version = "0.1-3", repos = "http://cran.us.r-project.org", upgrade = "never")
# install_version("sass", version = "0.3.1", repos = "http://cran.us.r-project.org", upgrade = "never")
# install_version("htmlwidgets", version = "1.5.3", repos = "http://cran.us.r-project.org", upgrade = "never")
# install_version("promises", version = "1.1.1", repos = "http://cran.us.r-project.org", upgrade = "never")
# install_version("cachem", version = "1.0.3", repos = "http://cran.us.r-project.org", upgrade = "never")
# install_version("mime", version = "0.9", repos = "http://cran.us.r-project.org", upgrade = "never")
# install_version("bslib", version = "0.2.4", repos = "http://cran.us.r-project.org", upgrade = "never")
# install_version("r2d3", version = "0.2.3", repos = "http://cran.us.r-project.org", upgrade = "never")
# install_version("jsonlite", version = "1.7.2", repos = "http://cran.us.r-project.org", upgrade = "never")
# installOpenCV(batch = T)
# library(reticulate)


# instantiate the model
ui <- fluidPage(
    tags$head(includeHTML(("google-analytics.html"))),
    theme = shinythemes::shinytheme("flatly"),
    titlePanel("Facial Analysis"),
    fluidRow(
        column(width = 7, h3("Webcam"),
               shinyviewr_UI("myCamera", height = '250px') )
    ),
    h3("Results"),
    imageOutput("Results", height = '750px')
)
server <- function(input, output) {
    #server side call of the viewr module
    myCamera <- callModule(
        shinyviewr, "myCamera" )
    # Watch for photos being taken
    observeEvent(myCamera(), {
        photo <- myCamera()
        file_location <- paste0("/home/shiny/",Sys.time(), "_cam.png")
        file_location <- gsub(" ", "_", file_location)
        png(filename=as.character(file_location))
        plot(as.raster(photo)) # plot photo
        dev.off()

        grp.photo = file_location

        # Read the photo using magick
        img = image_read(grp.photo)

        # Get basic informatino about the photo that will be useful for annotating
        inf = image_info(img)

        # Detect the faces in the image and pull all attributes associated with faces
        Sys.setenv(
            AWS_ACCESS_KEY_ID = "XXXXXXXX",
            AWS_SECRET_ACCESS_KEY = "XXXXXX/XXXXXXX",
            AWS_REGION="us-west-2"
        )
        svc <- rekognition()

        o = svc$detect_faces(Image=list(Bytes=grp.photo), Attributes="ALL")


        # Just get the face details
        all_faces = o$FaceDetails
        length(all_faces)

        # Loop through the faces, one by one. For each face, draw a rectangle around it, add the kid's name, and emotions

        # Duplicate the original image to have something to annotate and output
        new.img = img

        for(face in all_faces) {

            # Prepare a label that collapses across the emotions data provided by rekognition. Give the type of
            # emotion and the confidence that AWS has in its expression.
            emo.label = ""
            for(emo in face$Emotions) {
                emo.label = paste(emo.label,emo$Type, " = ", round(emo$Confidence, 2), "\n", sep="")
            }

            # Identify the coordinates of the face. Note that AWS returns percentage values of the total image size. This is
            # why the image info object above is needed
            box = face$BoundingBox
            image_width=inf$width
            image_height=inf$height
            x1 = box$Left*image_width
            y1 = box$Top*image_height
            x2 = x1 + box$Width*image_width
            y2 = y1 + box$Height*image_height

            # Create a subset image in memory that is just cropped around the focal face
            img.crop = image_crop(img, paste(box$Width*image_width,"x",box$Height*image_height,"+",x1,"+",y1, sep=""))
            img.crop = image_write(img.crop, path = NULL, format = "png")

            # Search in a specified collection to see if we can label the identity of the face is in this crop
            o = svc$search_faces_by_image(CollectionId="family-r",Image=list(Bytes=img.crop), FaceMatchThreshold=70)

            # Create a graphics device version of the larger photo that we can annotate
            new.img = image_draw(new.img)

            # If the face matches something in the collection, then add the name to the image
            if(length(o$FaceMatches) > 0) {
                faceName = o$FaceMatches[[1]]$Face$ExternalImageId
                faceConfidence = round(o$FaceMatches[[1]]$Face$Confidence,3)
                print(paste("Detected: ",faceName, sep=""))
                # Annotate with the name of the person
                text(x=x1+(box$Width*image_width)/2, y=y1,faceName, adj=0.5, cex=3, col="green")
            }

            # Draw a rectangle around the face
            rect(x1,y1,x2,y2, border="green", lty="dashed", lwd=5)

            # Annotate the photo with the emotions information
            text(x=x1+(box$Width*image_width)/2, y=y1+50,emo.label, pos=1, cex=1.5, col="red")

            dev.off()
        }
        # show photo
        output$Results <- renderPlot({
            plot(new.img)
        })

    })
}
# Run the application
shinyApp(ui = ui, server = server)
