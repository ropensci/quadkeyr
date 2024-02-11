library(shinytest2)

test_that("{shinytest2} recording: app_tab_quadkey", {
  app <- AppDriver$new(name = "app_tab_quadkey",
                       height = 855,
                       width = 1333)
  app$set_inputs(qk = "2103")
  app$click("search")
  app$set_inputs(qkvalues_rows_current = c(1, 2, 3, 4, 5, 6, 7), 
                 allow_no_input_binding_ = TRUE)
  app$set_inputs(qkvalues_rows_all = c(1, 2, 3, 4, 5, 6, 7),
                 allow_no_input_binding_ = TRUE)
  app$set_inputs(qkvalues_state = c(1705624197530, 0, 10, 
                                    "", TRUE, FALSE, TRUE, 
      c(TRUE, "", TRUE, FALSE, TRUE), 
      c(TRUE, "", TRUE, FALSE, TRUE)), 
      allow_no_input_binding_ = TRUE)
  app$expect_values()
})


test_that("{shinytest2} recording: app_tab_grid", {
  app <- AppDriver$new(name = "app_tab_grid",
                       height = 855, 
                       width = 1333)
  app$set_inputs(xmin = "-")
  app$set_inputs(xmin = "-59")
  app$set_inputs(xmax = "-")
  app$set_inputs(xmax = "-57")
  app$set_inputs(ymin = "-")
  app$set_inputs(ymin = "-64")
  app$set_inputs(ymin = "-44")
  app$set_inputs(ymin = "-4")
  app$set_inputs(ymin = "-34")
  app$set_inputs(ymax = "-")
  app$set_inputs(ymax = "-3")
  app$set_inputs(ymax = "-35")
   app$set_inputs(levelofdetail = 12)
  app$click("grid")
  app$set_inputs(mapgrid_groups = "qks", 
                 allow_no_input_binding_ = TRUE)
  app$expect_values()
})
