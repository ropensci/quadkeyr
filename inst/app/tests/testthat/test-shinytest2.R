library(shinytest2)

test_that("{shinytest2} recording: qkmap_app_tests", {
  app <- AppDriver$new(variant = platform_variant(),
                       name = "qkmap_app_tests",
                       height = 654, 
      width = 1035)
# test initial UI
  app$expect_screenshot()
# test QuadKey tab
  app$set_inputs(qk = "2121")
  app$click("search")
  app$set_inputs(qkvalues_rows_current = c(1, 2, 3, 4, 5, 6, 7),
                 allow_no_input_binding_ = TRUE)
  app$set_inputs(qkvalues_rows_all = c(1, 2, 3, 4, 5, 6, 7),
                 allow_no_input_binding_ = TRUE)
  app$set_inputs(qkvalues_state = 
      c(1701982608460, 0, 10, "", TRUE, FALSE, TRUE, 
      c(TRUE, "", TRUE, FALSE, TRUE), 
      c(TRUE, "", TRUE, FALSE, TRUE)),
      allow_no_input_binding_ = TRUE)
  app$expect_values()
  app$expect_screenshot()
  app$set_inputs(mapqk_bounds = c(-40.9964840143779, -67.47802734375, 
                                  -40.9964840143779, 
      -67.47802734375), allow_no_input_binding_ = TRUE)
  app$set_inputs(mapqk_center = c(-67.47802734375, -40.9964840143779), 
                 allow_no_input_binding_ = TRUE)
  app$set_window_size(width = 1035, height = 654)
  app$expect_screenshot()
# test grid tab
  app$set_inputs(xmin = "1")
  app$set_inputs(xmax = "2")
  app$set_inputs(ymin = "3")
  app$set_inputs(ymax = "4")
  app$set_inputs(levelofdetail = 10)
  app$click("grid")
  app$set_inputs(mapgrid_groups = "qks", 
                 allow_no_input_binding_ = TRUE)
  app$set_inputs(mapgrid_shape_mouseover = c("1222220212", 
                                             "0.0254409860956357", 
      "qks", "3.70675060864357", "0.762901371796838"),
      allow_no_input_binding_ = TRUE)
  app$set_inputs(mapgrid_shape_mouseout = c("1222220212",
                                            "0.336074123868211", "qks", 
      "3.87665636400335", "0.908385981305031"), 
      allow_no_input_binding_ = TRUE)
  app$set_inputs(mapgrid_shape_mouseover = c("1222220210",
                                             "0.663687345304377", "qks", 
      "3.87665636400335", "0.908385981305031"),
      allow_no_input_binding_ = TRUE)
  app$set_inputs(mapgrid_shape_click = c("1222220210",
                                         "0.892733229043701", "qks", 
      "4.05474668699563", "0.889171032879408"),
      allow_no_input_binding_ = TRUE)
  app$set_inputs(mapgrid_shape_mouseout = c("1222220210", 
                                            "0.576854928748893", "qks", 
      "4.05474668699563", "0.889171032879408"), 
      allow_no_input_binding_ = TRUE)
  app$set_inputs(mapgrid_shape_mouseover = c("1222220210",
                                             "0.662321790432951", "qks", 
      "4.07940229378728", "0.971520811846336"),
      allow_no_input_binding_ = TRUE)
  app$set_inputs(mapgrid_shape_mouseout = c("1222220210",
                                            "0.885617833859076", "qks", 
      "4.09583894489224", "1.01544069396202"),
      allow_no_input_binding_ = TRUE)
  app$set_inputs(mapgrid_shape_mouseover = c("1222220211",
                                             "0.727444880183187", "qks", 
      "4.1506253358272", "1.11426042872229"), 
      allow_no_input_binding_ = TRUE)
  app$set_inputs(mapgrid_shape_mouseout = c("1222220211", 
                                            "0.302726398195518", "qks", 
      "4.24649234286565", "1.33385983930066"), 
      allow_no_input_binding_ = TRUE)
  app$expect_values()
  app$expect_screenshot()
})
