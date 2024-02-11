library(shinytest2)

test_that("{shinytest2} recording: qk_tab", {
  app <-
    AppDriver$new(
      variant = platform_variant(),
      name = "qk_tab",
      height = 853,
      width = 1333
    )
  app$set_inputs(qk = "2103")
  app$click("search")
  app$set_inputs(
    qkvalues_rows_current = c(1, 2, 3, 4, 5, 6, 7),
    allow_no_input_binding_ = TRUE
  )
  app$set_inputs(
    qkvalues_rows_all = c(1, 2, 3, 4, 5, 6, 7),
    allow_no_input_binding_ = TRUE
  )
  app$set_inputs(
    qkvalues_state = c(
      1707689467564,
      0,
      10,
      "",
      TRUE,
      FALSE,
      TRUE,
      c(TRUE, "", TRUE, FALSE, TRUE),
      c(TRUE, "", TRUE, FALSE, TRUE)
    ),
    allow_no_input_binding_ = TRUE
  )
  app$set_inputs(mapqk_groups = "polygon",
                 allow_no_input_binding_ = TRUE)
  app$expect_values()
  app$expect_screenshot()
})


test_that("{shinytest2} recording: grid_tab", {
  app <-
    AppDriver$new(
      variant = platform_variant(),
      name = "grid_tab",
      height = 853,
      width = 1333
    )
  app$set_inputs(xmin = "1")
  app$set_inputs(xmax = "2")
  app$set_inputs(ymax = "4")
  app$set_inputs(ymin = "3")
  app$set_inputs(zoom = 12)
  app$click("grid")
  app$set_inputs(mapgrid_groups = "qks",
                 allow_no_input_binding_ = TRUE)
  app$set_inputs(
    mapgrid_shape_mouseover = c(
      "122222030123",
      "0.620790028481112",
      "qks",
      "3.92748328042044",
      "1.86385457026756"
    ),
    allow_no_input_binding_ = TRUE
  )
  app$set_inputs(
    mapgrid_shape_mouseout = c(
      "122222030123",
      "0.663459122676247",
      "qks",
      "3.86995567714617",
      "1.95725752012749"
    ),
    allow_no_input_binding_ = TRUE
  )
  app$set_inputs(
    mapgrid_shape_mouseover = c(
      "122222030132",
      "0.670432163673524",
      "qks",
      "3.86995567714617",
      "1.95725752012749"
    ),
    allow_no_input_binding_ = TRUE
  )
  app$set_inputs(
    mapgrid_shape_mouseout = c(
      "122222030132",
      "0.911623706621292",
      "qks",
      "3.85899759362869",
      "1.98335540317661"
    ),
    allow_no_input_binding_ = TRUE
  )
  app$set_inputs(
    mapgrid_shape_mouseover = c(
      "122222030310",
      "0.0699345260623052",
      "qks",
      "3.85899759362869",
      "1.98335540317661"
    ),
    allow_no_input_binding_ = TRUE
  )
  app$set_inputs(
    mapgrid_shape_mouseout = c(
      "122222030310",
      "0.794855618843523",
      "qks",
      "3.8288621362097",
      "2.04791332440333"
    ),
    allow_no_input_binding_ = TRUE
  )
  app$expect_values()
  app$expect_screenshot()
})
