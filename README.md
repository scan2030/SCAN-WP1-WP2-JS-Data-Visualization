Color Scheme for Historical vs Forecasted Data
Key Visual Distinction
The visualization system uses a clear visual language to distinguish between historical and forecasted data:

Line Charts (Incidence and Prevalence):
Historical data is represented with solid lines
Forecasted data is represented with dashed lines (borderDash: [5, 5])
The same color is maintained for each category across historical and forecast data:
Total: #fb8500 (orange)
Male: #023047 (dark blue)
Female: #8ecae6 (light blue)
Forecast Prediction Intervals:
Forecasted data includes 95% prediction intervals shown as semi-transparent colored bands
These bands match the color of their respective lines but with reduced opacity (0.2)
Both the forecast lines and prediction intervals start hidden by default
They can be toggled by clicking on the forecast items in the legend
Bar Charts (Cost):
Historical costs use solid fill colors:
Inpatient: #0072B2 (blue)
Outpatient: #009E73 (green)
A&E: #f94144 (red)
Forecasted costs use the same base colors but with diagonal white stripes:
Inpatient forecast: #00b4d8 (blue) with diagonal pattern
Outpatient forecast: #38b000 (green) with diagonal pattern
A&E forecast: #FA7476 (red) with diagonal pattern
Custom Legend System:
The legend clearly differentiates between historical and forecasted data
Historical items use solid color blocks
Forecast items use color blocks with diagonal lines and "(Forecast)" in the label
Clicking any legend item toggles its visibility in the chart