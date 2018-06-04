# ### Hide and show output plots using the shinyjs package
#
#
# ###############################################################################
# # ### Hide all plot outputs when app is first started
# # shinyjs::hide("model_preview_plot", time = 0)
# shinyjs::hide("model_preview_interactive_plot", time = 0)
# # shinyjs::hide("overlay_preview_base", time = 0)
# # shinyjs::hide("overlay_preview_overlaid", time = 0)
# # shinyjs::hide("create_ens_weights_poly_preview_plot", time = 0)
# # shinyjs::hide("ens_preview_plot", time = 0)
# shinyjs::hide("pretty_plot_plot", time = 0)
#
#
# ###############################################################################
# # This is done because even if new predictions are loaded/created, the
# # reactive functions still store and plot/display old information. Thus,
# # this code hides that old information until new, updated info is generated.
#
#
# # The following must be done so that these plots render to NULL and thus
# # # shinycssloaders spinner isn't shown.
#
# observe({
#   input$tabs
#   # outputOptions(output, "model_preview_plot", suspendWhenHidden = FALSE)
#   outputOptions(output, "model_preview_interactive_plot",
#                 suspendWhenHidden = FALSE)
#   # outputOptions(output, "overlay_preview_base", suspendWhenHidden = FALSE)
#   # # ^ Ok to be hear because renderLeaflet() doesn't need height
#   # outputOptions(output, "overlay_preview_overlaid", suspendWhenHidden = FALSE)
#   # outputOptions(output, "create_ens_weights_poly_preview_plot",
#   #               suspendWhenHidden = FALSE)
#   # outputOptions(output, "ens_preview_plot", suspendWhenHidden = FALSE)
#   outputOptions(output, "pretty_plot_plot", suspendWhenHidden = FALSE)
# })
#
#
# ###############################################################################
# # Show elements when their respective 'execute' buttons are clicked
# # Also hide other elements as appropriate
#
# ### Show original model predictions preview
# observeEvent(input$model_preview_execute, {
#   shinyjs::show("model_preview_plot", time = 0)
#   shinyjs::hide("model_preview_interactive_plot", time = 0)
#
#   outputOptions(output, "model_preview_plot", suspendWhenHidden = FALSE)
# })
#
# # Show/hide is in model_preview_interactive_event() in server_plots.R
# #   Not sure why observeEvent() executes first for other buttons
# ### Show original model predictions interactive preview
# observeEvent(input$model_preview_interactive_execute, {
#   shinyjs::show("model_preview_interactive_plot", time = 0)
#   shinyjs::hide("model_preview_plot", time = 0)
# })
#
# # # Show/hide is in overlay_preview_base() in server_plots.R
# # ### Show base grid preview
# # observeEvent(input$overlay_preview_base_execute, {
# #   shinyjs::show("overlay_preview_base", time = 0)
# # })
#
# # ### Show overlaid model predictions preview
# # observeEvent(input$overlay_preview_overlaid_execute, {
# #   shinyjs::show("overlay_preview_overlaid", time = 0)
# # })
# #
# # # Show overlaid model + weight polygon preview
# # observeEvent(input$create_ens_weights_poly_preview_execute, {
# #   shinyjs::show("create_ens_weights_poly_preview_plot", time = 0)
# # })
# #
# #
# # ### Show 'ensemble created' message when ensemble is created
# # observeEvent(input$create_ens_create_action, {
# #   shinyjs::show("ens_create_ensemble_text", time = 0)
# # })
# #
# # ### Show ensemble model predictions preview
# # observeEvent(input$ens_preview_execute, {
# #   shinyjs::show("ens_pix_preview_plot", time = 0)
# # })
# #
# ### Show pretty plot
# observeEvent(input$pretty_plot_execute, {
#   shinyjs::show("pretty_plot_plot", time = 0)
# })
#
#
# ###############################################################################
# ### Hide elements when 'create overlaid models' button is clicked
# # observeEvent(input$overlay_create_overlaid_models, {
# #   shinyjs::hide("overlay_preview_overlaid", time = 0)
# #   shinyjs::hide("create_ens_weights_poly_preview_plot", time = 0)
# #   shinyjs::hide("ens_create_ensemble_text", time = 0)
# #   shinyjs::hide("ens_pix_preview_plot", time = 0)
# #   shinyjs::hide("pretty_plot_plot", time = 0)
# #   # Make ^ this more robust so it only happens when an overlaid or ensemble model is plotted
# # })
#
#
# ###############################################################################
# ### Hide elements when a saved environment is loaded
# observeEvent(input$load_app_envir_file, {
#   shinyjs::hide("model_preview_plot", time = 0)
#   shinyjs::hide("model_preview_interactive_plot", time = 0)
#   # shinyjs::hide("overlay_preview_base", time = 0)
#   # shinyjs::hide("overlay_preview_overlaid", time = 0)
#   # shinyjs::hide("create_ens_weights_poly_preview_plot", time = 0)
#   # shinyjs::hide("ens_create_ensemble_text", time = 0)
#   # shinyjs::hide("ens_pix_preview_plot", time = 0)
#   shinyjs::hide("pretty_plot_plot", time = 0)
# })
#
# ###############################################################################
