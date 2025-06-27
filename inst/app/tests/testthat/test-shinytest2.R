library(shinytest2)

test_that("{shinytest2} recording: full_test", {
  app <- AppDriver$new(variant = platform_variant(), name = "full_test", height = 872,
      width = 1445)
  app$set_inputs(`plotly_afterplot-A` = "\"fishspeciesPlot\"", allow_no_input_binding_ = TRUE,
      priority_ = "event")
  app$set_inputs(`plotly_relayout-A` = "{\"width\":1119.1761474609375,\"height\":449.9999694824219}",
      allow_no_input_binding_ = TRUE, priority_ = "event")
  app$set_inputs(`plotly_afterplot-A` = "\"fishspeciesPlot\"", allow_no_input_binding_ = TRUE,
      priority_ = "event")
  app$set_inputs(`plotly_relayout-A` = "{\"width\":1119.1761474609375,\"height\":449.98577880859375}",
      allow_no_input_binding_ = TRUE, priority_ = "event")
  app$expect_screenshot()
  app$set_inputs(effort_Industrial = 0.5)
  app$set_inputs(`plotly_hover-A` = "[{\"curveNumber\":2,\"pointNumber\":1,\"x\":2,\"y\":0}]",
      allow_no_input_binding_ = TRUE, priority_ = "event")
  app$set_inputs(`plotly_hover-A` = character(0), allow_no_input_binding_ = TRUE,
      priority_ = "event")
  app$expect_screenshot()
  app$set_inputs(`plotly_hover-A` = "[{\"curveNumber\":5,\"pointNumber\":2,\"x\":10.3,\"y\":4.983494086885782}]",
      allow_no_input_binding_ = TRUE, priority_ = "event")
  app$set_inputs(`plotly_hover-A` = character(0), allow_no_input_binding_ = TRUE,
      priority_ = "event")
  app$set_inputs(`plotly_hover-A` = "[{\"curveNumber\":4,\"pointNumber\":6,\"x\":9.3,\"y\":0}]",
      allow_no_input_binding_ = TRUE, priority_ = "event")
  app$set_inputs(`plotly_hover-A` = character(0), allow_no_input_binding_ = TRUE,
      priority_ = "event")
  app$set_inputs(`plotly_hover-A` = "[{\"curveNumber\":2,\"pointNumber\":6,\"x\":9,\"y\":0}]",
      allow_no_input_binding_ = TRUE, priority_ = "event")
  app$set_inputs(`plotly_hover-A` = character(0), allow_no_input_binding_ = TRUE,
      priority_ = "event")
  app$set_inputs(`plotly_hover-A` = "[{\"curveNumber\":0,\"pointNumber\":1,\"x\":1.7,\"y\":0}]",
      allow_no_input_binding_ = TRUE, priority_ = "event")
  app$set_inputs(`plotly_hover-A` = character(0), allow_no_input_binding_ = TRUE,
      priority_ = "event")
  app$set_inputs(fishyear = 9)
  app$set_inputs(`plotly_afterplot-A` = "\"fishspeciesPlot\"", allow_no_input_binding_ = TRUE,
      priority_ = "event")
  app$set_inputs(`plotly_relayout-A` = "{\"width\":1119.1761474609375,\"height\":449.9999694824219}",
      allow_no_input_binding_ = TRUE, priority_ = "event")
  app$set_inputs(`plotly_afterplot-A` = "\"fishspeciesPlot\"", allow_no_input_binding_ = TRUE,
      priority_ = "event")
  app$set_inputs(`plotly_relayout-A` = "{\"width\":1119.1761474609375,\"height\":449.98577880859375}",
      allow_no_input_binding_ = TRUE, priority_ = "event")
  app$expect_screenshot()
  app$set_inputs(`plotly_hover-A` = "[{\"curveNumber\":5,\"pointNumber\":1,\"x\":5.3,\"y\":2.6799988414878477}]",
      allow_no_input_binding_ = TRUE, priority_ = "event")
  app$set_inputs(`plotly_hover-A` = character(0), allow_no_input_binding_ = TRUE,
      priority_ = "event")
  app$set_inputs(`plotly_hover-A` = "[{\"curveNumber\":2,\"pointNumber\":0,\"x\":1,\"y\":0}]",
      allow_no_input_binding_ = TRUE, priority_ = "event")
  app$set_inputs(`plotly_hover-A` = character(0), allow_no_input_binding_ = TRUE,
      priority_ = "event")
  app$click("decYear_fish")
  app$set_inputs(`plotly_afterplot-A` = "\"fishspeciesPlot\"", allow_no_input_binding_ = TRUE,
      priority_ = "event")
  app$set_inputs(`plotly_relayout-A` = "{\"width\":1119.1761474609375,\"height\":449.9999694824219}",
      allow_no_input_binding_ = TRUE, priority_ = "event")
  app$expect_screenshot()
  app$set_inputs(`plotly_hover-A` = "[{\"curveNumber\":4,\"pointNumber\":5,\"x\":11.3,\"y\":0}]",
      allow_no_input_binding_ = TRUE, priority_ = "event")
  app$set_inputs(`plotly_hover-A` = character(0), allow_no_input_binding_ = TRUE,
      priority_ = "event")
  app$set_inputs(triplotToggleFish = FALSE)
  app$expect_screenshot()
  app$click("customOrderInfo_fish")
  app$click("save_custom_order_fish")
  app$set_inputs(custom_order_rank_fish = c("Sprat", "Sandeel", "N.pout", "Herring",
      "Dab", "Whiting", "Sole", "Gurnard", "Plaice", "Haddock", "Cod", "Saithe"),
      allow_no_input_binding_ = TRUE)
  app$set_inputs(custom_order_rank_fish = c("N.pout", "Sprat", "Sandeel", "Herring",
      "Dab", "Whiting", "Sole", "Gurnard", "Plaice", "Haddock", "Cod", "Saithe"),
      allow_no_input_binding_ = TRUE)
  app$click("save_custom_order_fish")
  app$expect_screenshot()
  app$set_inputs(effort2_Pelagic = 1.55)
  app$set_inputs(effort2_Beam = 0.2)
  app$set_inputs(`plotly_hover-A` = "[{\"curveNumber\":4,\"pointNumber\":1,\"x\":12.6,\"y\":0}]",
      allow_no_input_binding_ = TRUE, priority_ = "event")
  app$set_inputs(`plotly_hover-A` = character(0), allow_no_input_binding_ = TRUE,
      priority_ = "event")
  app$expect_screenshot()
  app$set_inputs(`plotly_hover-A` = "[{\"curveNumber\":1,\"pointNumber\":0,\"x\":2,\"y\":0}]",
      allow_no_input_binding_ = TRUE, priority_ = "event")
  app$set_inputs(`plotly_hover-A` = character(0), allow_no_input_binding_ = TRUE,
      priority_ = "event")
  app$set_inputs(`plotly_hover-A` = "[{\"curveNumber\":0,\"pointNumber\":0,\"x\":2,\"y\":0}]",
      allow_no_input_binding_ = TRUE, priority_ = "event")
  app$set_inputs(`plotly_hover-A` = character(0), allow_no_input_binding_ = TRUE,
      priority_ = "event")
  app$set_inputs(fishy_plots = "Yield Composition")
  app$expect_screenshot()
  app$set_inputs(fishy_plots = "Size")
  app$expect_screenshot()
  app$set_inputs(fishy_plots = "Guild")
  app$set_inputs(`plotly_hover-A` = "[{\"curveNumber\":12,\"pointNumber\":0,\"x\":0.4,\"y\":0}]",
      allow_no_input_binding_ = TRUE, priority_ = "event")
  app$set_inputs(`plotly_hover-A` = character(0), allow_no_input_binding_ = TRUE,
      priority_ = "event")
  app$set_inputs(sim_choice = "sim2")
  app$set_inputs(`plotly_hover-A` = "[{\"curveNumber\":3,\"pointNumber\":0,\"x\":1,\"y\":11.838608649196516}]",
      allow_no_input_binding_ = TRUE, priority_ = "event")
  app$set_inputs(`plotly_hover-A` = character(0), allow_no_input_binding_ = TRUE,
      priority_ = "event")
  app$set_inputs(triguildToggleFish = FALSE)
  app$set_inputs(`plotly_hover-A` = "[{\"curveNumber\":1,\"pointNumber\":1,\"x\":4,\"y\":50.77598459023588}]",
      allow_no_input_binding_ = TRUE, priority_ = "event")
  app$set_inputs(`plotly_hover-A` = character(0), allow_no_input_binding_ = TRUE,
      priority_ = "event")
  app$expect_screenshot()
  app$set_inputs(fishy_plots = "Spectra")
  app$expect_screenshot()
  app$set_inputs(fishy_plots = "Diet")
  app$set_inputs(`plotly_hover-A` = "[{\"curveNumber\":1,\"pointNumber\":30,\"x\":0.201,\"y\":0.9999999999999937}]",
      allow_no_input_binding_ = TRUE, priority_ = "event")
  app$set_inputs(`plotly_hover-A` = character(0), allow_no_input_binding_ = TRUE,
      priority_ = "event")
  app$set_inputs(`plotly_hover-A` = "[{\"curveNumber\":0,\"pointNumber\":null,\"x\":0.342,\"y\":0}]",
      allow_no_input_binding_ = TRUE, priority_ = "event")
  app$set_inputs(`plotly_hover-A` = character(0), allow_no_input_binding_ = TRUE,
      priority_ = "event")
  app$set_inputs(fish_name_select = "Gurnard")
  app$set_inputs(`plotly_hover-A` = "[{\"curveNumber\":2,\"pointNumber\":26,\"x\":23.8,\"y\":0.054034781778468426}]",
      allow_no_input_binding_ = TRUE, priority_ = "event")
  app$set_inputs(`plotly_hover-A` = character(0), allow_no_input_binding_ = TRUE,
      priority_ = "event")
  app$set_inputs(`plotly_hover-A` = "[{\"curveNumber\":7,\"pointNumber\":37,\"x\":480,\"y\":0.040985316480608255}]",
      allow_no_input_binding_ = TRUE, priority_ = "event")
  app$set_inputs(`plotly_hover-A` = character(0), allow_no_input_binding_ = TRUE,
      priority_ = "event")
  app$expect_screenshot()
  app$set_inputs(`plotly_hover-A` = "[{\"curveNumber\":12,\"pointNumber\":72,\"x\":337,\"y\":0.3214819260618707}]",
      allow_no_input_binding_ = TRUE, priority_ = "event")
  app$set_inputs(`plotly_hover-A` = character(0), allow_no_input_binding_ = TRUE,
      priority_ = "event")
  app$set_inputs(`plotly_hover-A` = "[{\"curveNumber\":12,\"pointNumber\":60,\"x\":40.4,\"y\":0.6677217302215804}]",
      allow_no_input_binding_ = TRUE, priority_ = "event")
  app$set_inputs(`plotly_hover-A` = character(0), allow_no_input_binding_ = TRUE,
      priority_ = "event")
  app$set_inputs(`plotly_hover-A` = "[{\"curveNumber\":12,\"pointNumber\":57,\"x\":23.8,\"y\":0.7448749698430891}]",
      allow_no_input_binding_ = TRUE, priority_ = "event")
  app$set_inputs(`plotly_hover-A` = character(0), allow_no_input_binding_ = TRUE,
      priority_ = "event")
  app$set_inputs(fishy_plots = "Nutrition")
  app$set_inputs(`plotly_hover-A` = "[{\"curveNumber\":0,\"pointNumber\":1,\"x\":11,\"y\":0}]",
      allow_no_input_binding_ = TRUE, priority_ = "event")
  app$set_inputs(`plotly_hover-A` = character(0), allow_no_input_binding_ = TRUE,
      priority_ = "event")
  app$expect_screenshot()
  app$set_inputs(`plotly_hover-A` = "[{\"curveNumber\":1,\"pointNumber\":1,\"x\":11.6,\"y\":0}]",
      allow_no_input_binding_ = TRUE, priority_ = "event")
  app$set_inputs(`plotly_hover-A` = character(0), allow_no_input_binding_ = TRUE,
      priority_ = "event")
  app$set_inputs(`plotly_hover-A` = "[{\"curveNumber\":0,\"pointNumber\":0,\"x\":7,\"y\":0}]",
      allow_no_input_binding_ = TRUE, priority_ = "event")
  app$set_inputs(`plotly_hover-A` = character(0), allow_no_input_binding_ = TRUE,
      priority_ = "event")
  app$set_inputs(`plotly_hover-A` = "[{\"curveNumber\":0,\"pointNumber\":0,\"x\":7,\"y\":0}]",
      allow_no_input_binding_ = TRUE, priority_ = "event")
  app$set_inputs(`plotly_hover-A` = character(0), allow_no_input_binding_ = TRUE,
      priority_ = "event")
  app$click("start_tutorial")
  app$expect_screenshot()
  app$set_inputs(bigtabpanel = "Species Role")
  app$set_window_size(width = 1445, height = 872)
  app$set_inputs(`plotly_afterplot-A` = "\"speciesPlot\"", allow_no_input_binding_ = TRUE,
      priority_ = "event")
  app$set_inputs(`plotly_relayout-A` = "{\"width\":1119.1761474609375,\"height\":449.9999694824219}",
      allow_no_input_binding_ = TRUE, priority_ = "event")
  app$expect_screenshot()
  app$set_inputs(species_name_select = "N.pout")
  app$set_inputs(`plotly_afterplot-A` = "\"speciesPlot\"", allow_no_input_binding_ = TRUE,
      priority_ = "event")
  app$set_inputs(`plotly_relayout-A` = "{\"width\":1119.1761474609375,\"height\":449.98577880859375}",
      allow_no_input_binding_ = TRUE, priority_ = "event")
  app$set_inputs(`plotly_afterplot-A` = "\"speciesPlot\"", allow_no_input_binding_ = TRUE,
      priority_ = "event")
  app$set_inputs(`plotly_relayout-A` = "{\"width\":1119.1761474609375,\"height\":449.9999694824219}",
      allow_no_input_binding_ = TRUE, priority_ = "event")
  app$set_inputs(species = 53)
  app$set_inputs(`plotly_afterplot-A` = "\"speciesPlot\"", allow_no_input_binding_ = TRUE,
      priority_ = "event")
  app$set_inputs(`plotly_relayout-A` = "{\"width\":1119.1761474609375,\"height\":449.98577880859375}",
      allow_no_input_binding_ = TRUE, priority_ = "event")
  app$set_inputs(`plotly_afterplot-A` = "\"speciesPlot\"", allow_no_input_binding_ = TRUE,
      priority_ = "event")
  app$set_inputs(`plotly_relayout-A` = "{\"width\":1119.1761474609375,\"height\":449.9999694824219}",
      allow_no_input_binding_ = TRUE, priority_ = "event")
  app$set_inputs(mortspecies = -13)
  app$set_inputs(`plotly_afterplot-A` = "\"speciesPlot\"", allow_no_input_binding_ = TRUE,
      priority_ = "event")
  app$set_inputs(`plotly_relayout-A` = "{\"width\":1119.1761474609375,\"height\":449.98577880859375}",
      allow_no_input_binding_ = TRUE, priority_ = "event")
  app$set_inputs(year = 9)
  app$expect_screenshot()
  app$set_inputs(triplotToggle = FALSE)
  app$expect_screenshot()
  app$set_inputs(species_order_bio = "Size")
  app$expect_screenshot()
  app$set_inputs(plotTabs = "Size")
  app$set_inputs(logToggle = FALSE)
  app$expect_screenshot()
  app$set_inputs(plotTabs = "Guilds")
  app$expect_screenshot()
  app$set_inputs(plotTabs = "Diet")
  app$set_inputs(`plotly_hover-A` = "[{\"curveNumber\":1,\"pointNumber\":14,\"x\":0.0119,\"y\":1}]",
      allow_no_input_binding_ = TRUE, priority_ = "event")
  app$set_inputs(`plotly_hover-A` = character(0), allow_no_input_binding_ = TRUE,
      priority_ = "event")
  app$set_inputs(`plotly_hover-A` = "[{\"curveNumber\":3,\"pointNumber\":21,\"x\":0.0409,\"y\":0.9999999999999999}]",
      allow_no_input_binding_ = TRUE, priority_ = "event")
  app$set_inputs(`plotly_hover-A` = character(0), allow_no_input_binding_ = TRUE,
      priority_ = "event")
  app$set_inputs(`plotly_hover-A` = "[{\"curveNumber\":2,\"pointNumber\":null,\"x\":0.0696,\"y\":0}]",
      allow_no_input_binding_ = TRUE, priority_ = "event")
  app$set_inputs(`plotly_hover-A` = character(0), allow_no_input_binding_ = TRUE,
      priority_ = "event")
  app$set_inputs(diet_species_select = "Haddock")
  app$set_inputs(`plotly_hover-A` = "[{\"curveNumber\":19,\"pointNumber\":23,\"x\":11.7,\"y\":0.029473975734731265}]",
      allow_no_input_binding_ = TRUE, priority_ = "event")
  app$set_inputs(`plotly_hover-A` = character(0), allow_no_input_binding_ = TRUE,
      priority_ = "event")
  app$set_inputs(`plotly_hover-A` = "[{\"curveNumber\":22,\"pointNumber\":16,\"x\":198,\"y\":0.008173902266487783}]",
      allow_no_input_binding_ = TRUE, priority_ = "event")
  app$set_inputs(`plotly_hover-A` = character(0), allow_no_input_binding_ = TRUE,
      priority_ = "event")
  app$set_inputs(`plotly_hover-A` = "[{\"curveNumber\":23,\"pointNumber\":73,\"x\":402,\"y\":0.3348673291484208}]",
      allow_no_input_binding_ = TRUE, priority_ = "event")
  app$set_inputs(`plotly_hover-A` = character(0), allow_no_input_binding_ = TRUE,
      priority_ = "event")
  app$set_inputs(`plotly_hover-A` = "[{\"curveNumber\":23,\"pointNumber\":76,\"x\":683,\"y\":0.2577713931854299}]",
      allow_no_input_binding_ = TRUE, priority_ = "event")
  app$set_inputs(`plotly_hover-A` = character(0), allow_no_input_binding_ = TRUE,
      priority_ = "event")
  app$set_inputs(`plotly_hover-A` = "[{\"curveNumber\":0,\"pointNumber\":29,\"x\":1160,\"y\":0.020032775814215736}]",
      allow_no_input_binding_ = TRUE, priority_ = "event")
  app$set_inputs(`plotly_hover-A` = character(0), allow_no_input_binding_ = TRUE,
      priority_ = "event")
  app$set_inputs(`plotly_hover-A` = "[{\"curveNumber\":1,\"pointNumber\":38,\"x\":3350,\"y\":0.3000947230963866}]",
      allow_no_input_binding_ = TRUE, priority_ = "event")
  app$set_inputs(`plotly_hover-A` = character(0), allow_no_input_binding_ = TRUE,
      priority_ = "event")
  app$expect_screenshot()
  app$set_inputs(`plotly_hover-A` = "[{\"curveNumber\":1,\"pointNumber\":39,\"x\":4000,\"y\":0.30372279090299636}]",
      allow_no_input_binding_ = TRUE, priority_ = "event")
  app$set_inputs(`plotly_hover-A` = character(0), allow_no_input_binding_ = TRUE,
      priority_ = "event")
  app$set_inputs(`plotly_hover-A` = "[{\"curveNumber\":0,\"pointNumber\":32,\"x\":1970,\"y\":0.020424803240619745}]",
      allow_no_input_binding_ = TRUE, priority_ = "event")
  app$set_inputs(`plotly_hover-A` = character(0), allow_no_input_binding_ = TRUE,
      priority_ = "event")
  app$set_inputs(`plotly_hover-A` = "[{\"curveNumber\":23,\"pointNumber\":78,\"x\":973,\"y\":0.21427406784725675}]",
      allow_no_input_binding_ = TRUE, priority_ = "event")
  app$set_inputs(`plotly_hover-A` = character(0), allow_no_input_binding_ = TRUE,
      priority_ = "event")
  app$set_inputs(`plotly_hover-A` = "[{\"curveNumber\":22,\"pointNumber\":20,\"x\":402,\"y\":0.010550734361908648}]",
      allow_no_input_binding_ = TRUE, priority_ = "event")
  app$set_inputs(`plotly_hover-A` = character(0), allow_no_input_binding_ = TRUE,
      priority_ = "event")
  app$set_inputs(`plotly_hover-A` = "[{\"curveNumber\":12,\"pointNumber\":12,\"x\":57.5,\"y\":0.007113944329514793}]",
      allow_no_input_binding_ = TRUE, priority_ = "event")
  app$set_inputs(`plotly_hover-A` = character(0), allow_no_input_binding_ = TRUE,
      priority_ = "event")
  app$set_inputs(`plotly_hover-A` = "[{\"curveNumber\":12,\"pointNumber\":8,\"x\":28.4,\"y\":0.004280347344100352}]",
      allow_no_input_binding_ = TRUE, priority_ = "event")
  app$set_inputs(`plotly_hover-A` = character(0), allow_no_input_binding_ = TRUE,
      priority_ = "event")
  app$set_inputs(plotTabs = "Biomass")
  app$set_inputs(`plotly_afterplot-A` = "\"speciesPlot\"", allow_no_input_binding_ = TRUE,
      priority_ = "event")
  app$set_inputs(`plotly_afterplot-A` = "\"speciesPlot\"", allow_no_input_binding_ = TRUE,
      priority_ = "event")
  app$set_inputs(`plotly_relayout-A` = "{\"width\":1119.1761474609375,\"height\":449.9999694824219}",
      allow_no_input_binding_ = TRUE, priority_ = "event")
  app$set_inputs(`plotly_relayout-A` = "{\"width\":1119.1761474609375,\"height\":449.98577880859375}",
      allow_no_input_binding_ = TRUE, priority_ = "event")
  app$set_inputs(species_name_select = "Herring")
  app$set_inputs(`plotly_afterplot-A` = "\"speciesPlot\"", allow_no_input_binding_ = TRUE,
      priority_ = "event")
  app$set_inputs(`plotly_relayout-A` = "{\"width\":1119.1761474609375,\"height\":449.9999694824219}",
      allow_no_input_binding_ = TRUE, priority_ = "event")
  app$set_inputs(`plotly_afterplot-A` = "\"speciesPlot\"", allow_no_input_binding_ = TRUE,
      priority_ = "event")
  app$set_inputs(`plotly_relayout-A` = "{\"width\":1119.1761474609375,\"height\":449.98577880859375}",
      allow_no_input_binding_ = TRUE, priority_ = "event")
  app$set_inputs(`plotly_afterplot-A` = "\"speciesPlot\"", allow_no_input_binding_ = TRUE,
      priority_ = "event")
  app$set_inputs(`plotly_relayout-A` = "{\"width\":1119.1761474609375,\"height\":449.9999694824219}",
      allow_no_input_binding_ = TRUE, priority_ = "event")
  app$expect_screenshot()
  app$set_inputs(`plotly_afterplot-A` = "\"speciesPlot\"", allow_no_input_binding_ = TRUE,
      priority_ = "event")
  app$set_inputs(`plotly_relayout-A` = "{\"width\":1119.1761474609375,\"height\":449.98577880859375}",
      allow_no_input_binding_ = TRUE, priority_ = "event")
})
