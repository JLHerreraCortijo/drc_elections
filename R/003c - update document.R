#### IMPORTANT NOTE #####
# This script is not meant to be run as a stand alone script, but sourced from 
# 003 - update manuscript.R

# Open the document
doc <- officer::read_docx(document_path) # officer::read_docx opens the Word document specified by document_path

##### Figure 1 #####
doc <- officer::cursor_bookmark(doc,"Fig1") # officer::cursor_bookmark finds the bookmark "Fig1" in the document
doc <- officer::body_add_img(doc, 
                             src = here::here("manuscript/figures/Figure1.png"), # here::here constructs the path to the image file
                             pos = "after", # Position the image after the bookmark
                             width = fig1_width, # Set the width of the image
                             height = fig1_height) # Set the height of the image
doc <- officer::cursor_forward(doc) # officer::cursor_forward moves the cursor to the next element
doc <- officer::body_remove(doc) # officer::body_remove removes the previous version of the figure

##### Figure 2 #####
doc <- officer::cursor_bookmark(doc,"Fig2") # Find the bookmark "Fig2" in the document
doc <- officer::body_add_img(doc, 
                             src = here::here("manuscript/figures/Figure2.png"), # Construct the path to the image file
                             pos = "after", # Position the image after the bookmark
                             width = fig2_width, # Set the width of the image
                             height = fig2_height) # Set the height of the image
doc <- officer::cursor_forward(doc) # Move the cursor to the next element
doc <- officer::body_remove(doc) # Remove the previous version of the figure

##### Figure 3 #####
doc <- officer::cursor_bookmark(doc,"Fig3") # Find the bookmark "Fig3" in the document
doc <- officer::body_add_img(doc, 
                             src = here::here("manuscript/figures/Figure3.png"), # Construct the path to the image file
                             pos = "after", # Position the image after the bookmark
                             width = fig3_width, # Set the width of the image
                             height = fig3_height) # Set the height of the image
doc <- officer::cursor_forward(doc) # Move the cursor to the next element
doc <- officer::body_remove(doc) # Remove the previous version of the figure

##### Figure 4 #####
doc <- officer::cursor_bookmark(doc,"Fig4") # Find the bookmark "Fig4" in the document
doc <- officer::body_add_img(doc, 
                             src = here::here("manuscript/figures/Figure4.png"), # Construct the path to the image file
                             pos = "after", # Position the image after the bookmark
                             width = fig4_width, # Set the width of the image
                             height = fig4_height) # Set the height of the image
doc <- officer::cursor_forward(doc) # Move the cursor to the next element
doc <- officer::body_remove(doc) # Remove the previous version of the figure

##### Figure 5 #####
doc <- officer::cursor_bookmark(doc,"Fig5") # Find the bookmark "Fig5" in the document
doc <- officer::body_add_img(doc, 
                             src = here::here("manuscript/figures/Figure5.png"), # Construct the path to the image file
                             pos = "after", # Position the image after the bookmark
                             width = fig5_width, # Set the width of the image
                             height = fig5_height) # Set the height of the image
doc <- officer::cursor_forward(doc) # Move the cursor to the next element
doc <- officer::body_remove(doc) # Remove the previous version of the figure

##### Figure 6 #####
doc <- officer::cursor_bookmark(doc,"Fig6") # Find the bookmark "Fig6" in the document
doc <- officer::body_add_img(doc, 
                             src = here::here("manuscript/figures/Figure6.png"), # Construct the path to the image file
                             pos = "after", # Position the image after the bookmark
                             width = fig6_width, # Set the width of the image
                             height = fig6_height) # Set the height of the image
doc <- officer::cursor_forward(doc) # Move the cursor to the next element
doc <- officer::body_remove(doc) # Remove the previous version of the figure

##### Figure 7 #####
doc <- officer::cursor_bookmark(doc,"Fig7") # Find the bookmark "Fig7" in the document
doc <- officer::body_add_img(doc, 
                             src = here::here("manuscript/figures/Figure7.png"), # Construct the path to the image file
                             pos = "after", # Position the image after the bookmark
                             width = fig7_width, # Set the width of the image
                             height = fig7_height) # Set the height of the image
doc <- officer::cursor_forward(doc) # Move the cursor to the next element
doc <- officer::body_remove(doc) # Remove the previous version of the figure

##### Figure 8 #####
doc <- officer::cursor_bookmark(doc,"Fig8") # Find the bookmark "Fig8" in the document
doc <- officer::body_add_img(doc, 
                             src = here::here("manuscript/figures/Figure8.png"), # Construct the path to the image file
                             pos = "after", # Position the image after the bookmark
                             width = fig8_width, # Set the width of the image
                             height = fig8_height) # Set the height of the image
doc <- officer::cursor_forward(doc) # Move the cursor to the next element
doc <- officer::body_remove(doc) # Remove the previous version of the figure

##### Figure A1 #####
doc <- officer::cursor_bookmark(doc,"FigA1") # Find the bookmark "FigA1" in the document
doc <- officer::body_add_img(doc, 
                             src = here::here("manuscript/figures/FigureA1.png"), # Construct the path to the image file
                             pos = "after", # Position the image after the bookmark
                             width = figA1_width, # Set the width of the image
                             height = figA1_height) # Set the height of the image
doc <- officer::cursor_forward(doc) # Move the cursor to the next element
doc <- officer::body_remove(doc) # Remove the previous version of the figure

##### Figure A1b #####
doc <- officer::cursor_bookmark(doc,"FigA1b") # Find the bookmark "FigA1b" in the document
doc <- officer::body_add_img(doc, 
                             src = here::here("manuscript/figures/FigureA1b.png"), # Construct the path to the image file
                             pos = "after", # Position the image after the bookmark
                             width = figA1b_width, # Set the width of the image
                             height = figA1b_height) # Set the height of the image
doc <- officer::cursor_forward(doc) # Move the cursor to the next element
doc <- officer::body_remove(doc) # Remove the previous version of the figure


##### Figure A2 #####
doc <- officer::cursor_bookmark(doc,"FigA2") # Find the bookmark "FigA2" in the document
doc <- officer::body_add_img(doc, 
                             src = here::here("manuscript/figures/FigureA2.png"), # Construct the path to the image file
                             pos = "after", # Position the image after the bookmark
                             width = figA2_width, # Set the width of the image
                             height = figA2_height) # Set the height of the image
doc <- officer::cursor_forward(doc) # Move the cursor to the next element
doc <- officer::body_remove(doc) # Remove the previous version of the figure

##### Figure A3 #####
doc <- officer::cursor_bookmark(doc,"FigA3") # Find the bookmark "FigA3" in the document
doc <- officer::body_add_img(doc, 
                             src = here::here("manuscript/figures/FigureA3.png"), # Construct the path to the image file
                             pos = "after", # Position the image after the bookmark
                             width = figA3_width, # Set the width of the image
                             height = figA3_height) # Set the height of the image
doc <- officer::cursor_forward(doc) # Move the cursor to the next element
doc <- officer::body_remove(doc) # Remove the previous version of the figure

##### Figure A4 #####
doc <- officer::cursor_bookmark(doc,"FigA4") # Find the bookmark "FigA4" in the document
doc <- officer::body_add_img(doc, 
                             src = here::here("manuscript/figures/FigureA4.png"), # Construct the path to the image file
                             pos = "after", # Position the image after the bookmark
                             width = figA4_width, # Set the width of the image
                             height = figA4_height) # Set the height of the image
doc <- officer::cursor_forward(doc) # Move the cursor to the next element
doc <- officer::body_remove(doc) # Remove the previous version of the figure

##### Figure A5 #####
doc <- officer::cursor_bookmark(doc,"FigA5") # Find the bookmark "FigA5" in the document
doc <- officer::body_add_img(doc, 
                             src = here::here("manuscript/figures/FigureA5.png"), # Construct the path to the image file
                             pos = "after", # Position the image after the bookmark
                             width = figA5_width, # Set the width of the image
                             height = figA5_height) # Set the height of the image
doc <- officer::cursor_forward(doc) # Move the cursor to the next element
doc <- officer::body_remove(doc) # Remove the previous version of the figure

##### Figure A6 #####
doc <- officer::cursor_bookmark(doc,"FigA6") # Find the bookmark "FigA6" in the document
doc <- officer::body_add_img(doc, 
                             src = here::here("manuscript/figures/FigureA6.png"), # Construct the path to the image file
                             pos = "after", # Position the image after the bookmark
                             width = figA6_width, # Set the width of the image
                             height = figA6_height) # Set the height of the image
doc <- officer::cursor_forward(doc) # Move the cursor to the next element
doc <- officer::body_remove(doc) # Remove the previous version of the figure

##### Figure A7 #####
doc <- officer::cursor_bookmark(doc,"FigA7") # Find the bookmark "FigA7" in the document
doc <- officer::body_add_img(doc, 
                             src = here::here("manuscript/figures/FigureA7.png"), # Construct the path to the image file
                             pos = "after", # Position the image after the bookmark
                             width = figA7_width, # Set the width of the image
                             height = figA7_height) # Set the height of the image
doc <- officer::cursor_forward(doc) # Move the cursor to the next element
doc <- officer::body_remove(doc) # Remove the previous version of the figure

##### Figure A8 #####
doc <- officer::cursor_bookmark(doc,"FigA8") # Find the bookmark "FigA8" in the document
doc <- officer::body_add_img(doc, 
                             src = here::here("manuscript/figures/FigureA8.png"), # Construct the path to the image file
                             pos = "after", # Position the image after the bookmark
                             width = figA8_width, # Set the width of the image
                             height = figA8_height) # Set the height of the image
doc <- officer::cursor_forward(doc) # Move the cursor to the next element
doc <- officer::body_remove(doc) # Remove the previous version of the figure

##### Figure A9 #####
doc <- officer::cursor_bookmark(doc,"FigA9") # Find the bookmark "FigA9" in the document
doc <- officer::body_add_img(doc, 
                             src = here::here("manuscript/figures/FigureA9.png"), # Construct the path to the image file
                             pos = "after", # Position the image after the bookmark
                             width = figA9_width, # Set the width of the image
                             height = figA9_height) # Set the height of the image
doc <- officer::cursor_forward(doc) # Move the cursor to the next element
doc <- officer::body_remove(doc) # Remove the previous version of the figure

##### Figure A10 #####
doc <- officer::cursor_bookmark(doc,"FigA10") # Find the bookmark "FigA10" in the document
doc <- officer::body_add_img(doc, 
                             src = here::here("manuscript/figures/FigureA10.png"), # Construct the path to the image file
                             pos = "after", # Position the image after the bookmark
                             width = figA10_width, # Set the width of the image
                             height = figA10_height) # Set the height of the image
doc <- officer::cursor_forward(doc) # Move the cursor to the next element
doc <- officer::body_remove(doc) # Remove the previous version of the figure

##### Figure A11 #####
doc <- officer::cursor_bookmark(doc,"FigA11") # Find the bookmark "FigA11" in the document
doc <- officer::body_add_img(doc, 
                             src = here::here("manuscript/figures/FigureA11.png"), # Construct the path to the image file
                             pos = "after", # Position the image after the bookmark
                             width = figA11_width, # Set the width of the image
                             height = figA11_height) # Set the height of the image
doc <- officer::cursor_forward(doc) # Move the cursor to the next element
doc <- officer::body_remove(doc) # Remove the previous version of the figure

##### Figure A12 #####
doc <- officer::cursor_bookmark(doc,"FigA12") # Find the bookmark "FigA12" in the document
doc <- officer::body_add_img(doc, 
                             src = here::here("manuscript/figures/FigureA12.png"), # Construct the path to the image file
                             pos = "after", # Position the image after the bookmark
                             width = figA12_width, # Set the width of the image
                             height = figA12_height) # Set the height of the image
doc <- officer::cursor_forward(doc) # Move the cursor to the next element
doc <- officer::body_remove(doc) # Remove the previous version of the figure

##### Table 1 #####
doc <- officer::cursor_bookmark(doc, "Table1") # Find the bookmark "Table1" in the document
doc %<>% flextable::body_add_flextable(Table_1, align = "center") # Add Table_1 as a flextable, centered in the document
doc <- officer::cursor_forward(doc) # Move the cursor to the next element
doc <- officer::body_remove(doc) # Remove the previous version of the table

##### Table 1 v2 #####
doc <- officer::cursor_bookmark(doc, "Table1v2") # Find the bookmark "Table1v2" in the document
doc %<>% flextable::body_add_flextable(Table_1_v2, align = "center") # Add Table_1_v2 as a flextable, centered in the document
doc <- officer::cursor_forward(doc) # Move the cursor to the next element
doc <- officer::body_remove(doc) # Remove the previous version of the table

##### Table 3a #####
doc <- officer::cursor_bookmark(doc, "Table3a") # Find the bookmark "Table3a" in the document
doc %<>% flextable::body_add_flextable(Table_3a, align = "center", split = TRUE) # Add Table_3a as a flextable, centered, and split across pages if needed
doc <- officer::cursor_forward(doc) # Move the cursor to the next element
doc <- officer::body_remove(doc) # Remove the previous version of the table

##### Table 3b #####
doc <- officer::cursor_bookmark(doc, "Table3b") # Find the bookmark "Table3b" in the document
doc %<>% flextable::body_add_flextable(Table_3b, align = "center", split = TRUE) # Add Table_3b as a flextable, centered, and split across pages if needed
doc <- officer::cursor_forward(doc) # Move the cursor to the next element
doc <- officer::body_remove(doc) # Remove the previous version of the table

##### Table A2 #####
doc <- officer::cursor_bookmark(doc, "TableA2") # Find the bookmark "TableA2" in the document
doc %<>% flextable::body_add_flextable(Table_A2, align = "center") # Add Table_A2 as a flextable, centered in the document
doc <- officer::cursor_forward(doc) # Move the cursor to the next element
doc <- officer::body_remove(doc) # Remove the previous version of the table

##### Table A2b #####
doc <- officer::cursor_bookmark(doc, "TableA2b") # Find the bookmark "TableA2b" in the document
doc %<>% flextable::body_add_flextable(Table_A2b, align = "center") # Add Table_A2b as a flextable, centered in the document
doc <- officer::cursor_forward(doc) # Move the cursor to the next element
doc <- officer::body_remove(doc) # Remove the previous version of the table

##### Table A2c #####
doc <- officer::cursor_bookmark(doc, "TableA2c") # Find the bookmark "TableA2c" in the document
doc %<>% flextable::body_add_flextable(Table_A2c, align = "center") # Add Table_A2c as a flextable, centered in the document
doc <- officer::cursor_forward(doc) # Move the cursor to the next element
doc <- officer::body_remove(doc) # Remove the previous version of the table

##### Table A2d #####
doc <- officer::cursor_bookmark(doc, "TableA2d") # Find the bookmark "TableA2d" in the document
doc %<>% flextable::body_add_flextable(Table_A2d, align = "center") # Add Table_A2d as a flextable, centered in the document
doc <- officer::cursor_forward(doc) # Move the cursor to the next element
doc <- officer::body_remove(doc) # Remove the previous version of the table

##### Table A3 #####
doc <- officer::cursor_bookmark(doc, "TableA3") # Find the bookmark "TableA3" in the document
doc %<>% flextable::body_add_flextable(Table_A3, align = "center") # Add Table_A3 as a flextable, centered in the document
doc <- officer::cursor_forward(doc) # Move the cursor to the next element
doc <- officer::body_remove(doc) # Remove the previous version of the table



##### Table A4 #####
doc <- officer::cursor_bookmark(doc, "TableA4") # Find the bookmark "TableA4" in the document
doc %<>% flextable::body_add_flextable(Table_A4, align = "center") # Add Table_A4 as a flextable, centered in the document
doc <- officer::cursor_forward(doc) # Move the cursor to the next element
doc <- officer::body_remove(doc) # Remove the previous version of the table

##### Table A5 #####
doc <- officer::cursor_bookmark(doc, "TableA5") # Find the bookmark "TableA5" in the document
doc %<>% flextable::body_add_flextable(Table_A5, align = "center") # Add Table_A5 as a flextable, centered in the document
doc <- officer::cursor_forward(doc) # Move the cursor to the next element
doc <- officer::body_remove(doc) # Remove the previous version of the table

##### Table A6i #####
doc <- officer::cursor_bookmark(doc, "TableA6i") # Find the bookmark "TableA6i" in the document
doc %<>% flextable::body_add_flextable(Table_A6i, align = "center") # Add Table_A6i as a flextable, centered in the document
doc <- officer::cursor_forward(doc) # Move the cursor to the next element
doc <- officer::body_remove(doc) # Remove the previous version of the table

##### Table A6ii #####
doc <- officer::cursor_bookmark(doc, "TableA6ii") # Find the bookmark "TableA6ii" in the document
doc %<>% flextable::body_add_flextable(Table_A6ii, align = "center") # Add Table_A6ii as a flextable, centered in the document
doc <- officer::cursor_forward(doc) # Move the cursor to the next element
doc <- officer::body_remove(doc) # Remove the previous version of the table

##### Table A6iii #####
doc <- officer::cursor_bookmark(doc, "TableA6iii") # Find the bookmark "TableA6iii" in the document
doc %<>% flextable::body_add_flextable(Table_A6iii, align = "center") # Add Table_A6iii as a flextable, centered in the document
doc <- officer::cursor_forward(doc) # Move the cursor to the next element
doc <- officer::body_remove(doc) # Remove the previous version of the table



##### Table A8i #####
doc <- officer::cursor_bookmark(doc, "TableA8i") # Find the bookmark "TableA8i" in the document
doc %<>% flextable::body_add_flextable(Table_A8i, align = "center") # Add Table_A8i as a flextable, centered in the document
doc <- officer::cursor_forward(doc) # Move the cursor to the next element
doc <- officer::body_remove(doc) # Remove the previous version of the table

##### Table A8ib #####
doc <- officer::cursor_bookmark(doc, "TableA8ib") # Find the bookmark "TableA8ib" in the document
doc %<>% flextable::body_add_flextable(Table_A8ib, align = "center", split = TRUE) # Add Table A8ib as a flextable, centered, and split across pages if needed
doc <- officer::cursor_forward(doc) # Move the cursor to the next element
doc <- officer::body_remove(doc) # Remove the previous version of the table

##### Table A8ic #####
doc <- officer::cursor_bookmark(doc, "TableA8ic") # Find the bookmark "TableA8ic" in the document
doc %<>% flextable::body_add_flextable(Table_A8ic, align = "center", split = TRUE) # Add Table A8ic as a flextable, centered, and split across pages if needed
doc <- officer::cursor_forward(doc) # Move the cursor to the next element
doc <- officer::body_remove(doc) # Remove the previous version of the table

##### Table A8id #####
doc <- officer::cursor_bookmark(doc, "TableA8id") # Find the bookmark "TableA8id" in the document
doc %<>% flextable::body_add_flextable(Table_A8id, align = "center", split = TRUE) # Add Table A8id as a flextable, centered, and split across pages if needed
doc <- officer::cursor_forward(doc) # Move the cursor to the next element
doc <- officer::body_remove(doc) # Remove the previous version of the table

##### Table A8ii #####
doc <- officer::cursor_bookmark(doc, "TableA8ii") # Find the bookmark "TableA8ii" in the document
doc %<>% flextable::body_add_flextable(Table_A8ii, align = "center") # Add Table_A8ii as a flextable, centered in the document
doc <- officer::cursor_forward(doc) # Move the cursor to the next element
doc <- officer::body_remove(doc) # Remove the previous version of the table

##### Table A8iii #####
doc <- officer::cursor_bookmark(doc, "TableA8iii") # Find the bookmark "TableA8iii" in the document
doc %<>% flextable::body_add_flextable(Table_A8iii, align = "center") # Add Table_A8iii as a flextable, centered in the document
doc <- officer::cursor_forward(doc) # Move the cursor to the next element
doc <- officer::body_remove(doc) # Remove the previous version of the table

##### Table A9i #####
doc <- officer::cursor_bookmark(doc, "TableA9i") # Find the bookmark "TableA9i" in the document
doc %<>% flextable::body_add_flextable(Table_A9i, align = "center") # Add Table_A9i as a flextable, centered in the document
doc <- officer::cursor_forward(doc) # Move the cursor to the next element
doc <- officer::body_remove(doc) # Remove the previous version of the table

##### Table A9ii #####
doc <- officer::cursor_bookmark(doc, "TableA9ii") # Find the bookmark "TableA9ii" in the document
doc %<>% flextable::body_add_flextable(Table_A9ii, align = "center") # Add Table_A9ii as a flextable, centered in the document
doc <- officer::cursor_forward(doc) # Move the cursor to the next element
doc <- officer::body_remove(doc) # Remove the previous version of the table

##### Table A9iii #####
doc <- officer::cursor_bookmark(doc, "TableA9iii") # Find the bookmark "TableA9iii" in the document
doc %<>% flextable::body_add_flextable(Table_A9iii, align = "center") # Add Table_A9iii as a flextable, centered in the document
doc <- officer::cursor_forward(doc) # Move the cursor to the next element
doc <- officer::body_remove(doc) # Remove the previous version of the table

##### Table A11 #####
doc <- officer::cursor_bookmark(doc, "TableA11") # Find the bookmark "TableA11" in the document
doc %<>% flextable::body_add_flextable(Table_A11, align = "center", split = TRUE) # Add Table_A11 as a flextable, centered, and split across pages if needed
doc <- officer::cursor_forward(doc) # Move the cursor to the next element
doc <- officer::body_remove(doc) # Remove the previous version of the table


##### Table A12 #####
doc <- officer::cursor_bookmark(doc, "TableA12") # Find the bookmark "TableA12" in the document
doc %<>% flextable::body_add_flextable(Table_A12, align = "center", split = TRUE) # Add Table_A12 as a flextable, centered, and split across pages if needed
doc <- officer::cursor_forward(doc) # Move the cursor to the next element
doc <- officer::body_remove(doc) # Remove the previous version of the table

##### Table A13 #####
doc <- officer::cursor_bookmark(doc, "TableA13") # Find the bookmark "TableA13" in the document
doc %<>% flextable::body_add_flextable(Table_A13, align = "center", split = TRUE) # Add Table_A13 as a flextable, centered, and split across pages if needed
doc <- officer::cursor_forward(doc) # Move the cursor to the next element
doc <- officer::body_remove(doc) # Remove the previous version of the table

##### Table A14 #####
doc <- officer::cursor_bookmark(doc, "TableA14") # Find the bookmark "TableA14" in the document
doc %<>% flextable::body_add_flextable(Table_A14, align = "center", split = TRUE) # Add Table_A14 as a flextable, centered, and split across pages if needed
doc <- officer::cursor_forward(doc) # Move the cursor to the next element
doc <- officer::body_remove(doc) # Remove the previous version of the table

##### Table A15 #####
doc <- officer::cursor_bookmark(doc, "TableA15") # Find the bookmark "TableA15" in the document
doc %<>% flextable::body_add_flextable(Table_A15, align = "center", split = TRUE) # Add Table_A15 as a flextable, centered, and split across pages if needed
doc <- officer::cursor_forward(doc) # Move the cursor to the next element
doc <- officer::body_remove(doc) # Remove the previous version of the table

##### Table A16 #####
doc <- officer::cursor_bookmark(doc, "TableA16") # Find the bookmark "TableA16" in the document
doc %<>% flextable::body_add_flextable(Table_A16, align = "center", split = TRUE) # Add Table_A16 as a flextable, centered, and split across pages if needed
doc <- officer::cursor_forward(doc) # Move the cursor to the next element
doc <- officer::body_remove(doc) # Remove the previous version of the table

##### Table A17 #####
doc <- officer::cursor_bookmark(doc, "TableA17") # Find the bookmark "TableA17" in the document
doc %<>% flextable::body_add_flextable(Table_A17, align = "center", split = TRUE) # Add Table_A17 as a flextable, centered, and split across pages if needed
doc <- officer::cursor_forward(doc) # Move the cursor to the next element
doc <- officer::body_remove(doc) # Remove the previous version of the table


##### Table A18 #####
doc <- officer::cursor_bookmark(doc, "TableA18") # Find the bookmark "TableA18" in the document
doc %<>% flextable::body_add_flextable(Table_A18, align = "center", split = TRUE) # Add Table A18 as a flextable, centered, and split across pages if needed
doc <- officer::cursor_forward(doc) # Move the cursor to the next element
doc <- officer::body_remove(doc) # Remove the previous version of the table



##### Table A19i #####
doc <- officer::cursor_bookmark(doc, "TableA19i") # Find the bookmark "TableA19i" in the document
doc %<>% flextable::body_add_flextable(Table_A19i, align = "center") # Add Table A19i as a flextable, centered in the document
doc <- officer::cursor_forward(doc) # Move the cursor to the next element
doc <- officer::body_remove(doc) # Remove the previous version of the table

##### Table A19ii #####
doc <- officer::cursor_bookmark(doc, "TableA19ii") # Find the bookmark "TableA19ii" in the document
doc %<>% flextable::body_add_flextable(Table_A19ii, align = "center") # Add Table A19ii as a flextable, centered in the document
doc <- officer::cursor_forward(doc) # Move the cursor to the next element
doc <- officer::body_remove(doc) # Remove the previous version of the table

##### Table A19iii #####
doc <- officer::cursor_bookmark(doc, "TableA19iii") # Find the bookmark "TableA19iii" in the document
doc %<>% flextable::body_add_flextable(Table_A19iii, align = "center") # Add Table A19iii as a flextable, centered in the document
doc <- officer::cursor_forward(doc) # Move the cursor to the next element
doc <- officer::body_remove(doc) # Remove the previous version of the table


##### Table A10 #####
doc <- officer::cursor_bookmark(doc, "TableA10") # Find the bookmark "TableA10" in the document
doc %<>% flextable::body_add_flextable(Table_A10, align = "center", split = TRUE) # Add Table A10 as a flextable, centered, and split across pages if needed
doc <- officer::cursor_forward(doc) # Move the cursor to the next element
doc <- officer::body_remove(doc) # Remove the previous version of the table

##### Backup document #####
back_timestamp <- Sys.time() %>% 
  format("%Y-%m-%dT%H:%M:%S", tz = "UTC") # Create a timestamp for the backup
back_path <- file.path(dirname(document_path), "backup", paste0(back_timestamp, "_backup_", basename(document_path))) # Create the backup file path
file.copy(document_path, back_path, overwrite = FALSE) # Copy the current document to the backup path

##### Save document updated #####

print(doc, target = document_path) # Save the updated document
print("docx updated") # Print confirmation message
