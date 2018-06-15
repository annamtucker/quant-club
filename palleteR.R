# use palleteR to make a custom color pallete based on any picture
# https://datascienceplus.com/how-to-use-paletter-to-automagically-build-palettes-from-pictures/

# you will need to have devtools installed
devtools::install_github("andreacirilloac/paletter")

require(paletter)

# specify image path, number of colors, and type of pallete
img = "C:/Users/Anna/Desktop/sanderling.jpg"
ncol = 8
type = "continuous"

pal_sand = create_palette(image_path = img,
                     number_of_colors = ncol,
                     type_of_variable = type)

img = "C:/Users/Anna/Desktop/ruddy-turnstone.jpg"

pal_rutu = create_palette(image_path = img,
                          number_of_colors = ncol,
                          type_of_variable = type)
