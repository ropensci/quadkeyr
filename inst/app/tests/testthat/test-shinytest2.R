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
  app$expect_values()
  app$expect_screenshot()
  app$set_window_size(width = 1333, height = 853)
  app$set_inputs(xmin = "1")
  app$set_inputs(ymin = "3")
  app$set_inputs(xmax = "2")
  app$set_inputs(ymax = "4")
  app$set_inputs(zoom = 12)
  app$set_inputs(zoom = 11)
  app$set_inputs(zoom = 10)
  app$click("grid")
  app$expect_values()
  app$expect_screenshot()
})
