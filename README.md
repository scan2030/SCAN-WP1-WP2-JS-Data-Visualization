# 📊 Color Scheme for Historical vs Forecasted Data

## 🎯 Key Visual Distinction

This visualization system uses a clear and consistent color scheme to differentiate between **historical** and **forecasted** data across different chart types.

---

## 📈 Line Charts (Incidence and Prevalence)

- **Historical Data**: Solid lines  
- **Forecasted Data**: Dashed lines (`borderDash: [5, 5]`)  
- **Color Coding**:
  - **Total**: `#fb8500` (orange)
  - **Male**: `#023047` (dark blue)
  - **Female**: `#8ecae6` (light blue)

### 🔮 Forecast Prediction Intervals
- Represented as semi-transparent bands with `opacity: 0.2`
- Match the color of the data line
- Hidden by default and toggled via the legend

---

## 📊 Bar Charts (Costs)

### 🔵 Historical Costs (Solid Fill)
- **Inpatient**: `#0072B2` (blue)
- **Outpatient**: `#009E73` (green)
- **A&E**: `#f94144` (red)

### 🔮 Forecasted Costs (Striped Pattern)
- **Inpatient Forecast**: `#00b4d8` (blue, diagonal pattern)
- **Outpatient Forecast**: `#38b000` (green, diagonal pattern)
- **A&E Forecast**: `#FA7476` (red, diagonal pattern)

---

## 🧭 Custom Legend System

- **Historical Items**: Solid color blocks
- **Forecasted Items**: Diagonal-striped blocks with `"(Forecast)"` label
- **Interactive**: Click any legend item to toggle its visibility

---

## 🎨 How to Modify Forecast Colors

### 1. Line Chart Colors (Incidence & Prevalence)

In `updateIncidenceCharts()` and `updatePrevalenceCharts()` (approx. lines 855–1130 and 1280–1560):

```javascript
// Total Cases - Forecast
{
  label: 'Total (Forecast)',
  borderColor: '#fb8500', // <-- Modify here
  pointBackgroundColor: '#fb8500', // <-- Modify here
  borderDash: [5, 5],
  hidden: true,
}

// Male Cases - Forecast
{
  label: 'Male (Forecast)',
  borderColor: '#023047',
  pointBackgroundColor: '#023047',
  borderDash: [5, 5],
  hidden: true,
}

// Female Cases - Forecast
{
  label: 'Female (Forecast)',
  borderColor: '#8ecae6',
  pointBackgroundColor: '#8ecae6',
  borderDash: [5, 5],
  hidden: true,
}
```

Update custom legend (around line 520):

```javascript
const forecastItems = [
  { label: 'Total (Forecast)', color: '#fb8500', dash: true, datasetIndex: 4 },
  { label: 'Male (Forecast)', color: '#023047', dash: true, datasetIndex: 7 },
  { label: 'Female (Forecast)', color: '#8ecae6', dash: true, datasetIndex: 10 }
];
```

---

### 2. Prediction Interval Colors

```javascript
{
  label: 'Total Lower PI',
  backgroundColor: 'rgba(255, 0, 0, 0.2)', // <-- Modify this
  borderColor: 'rgba(255, 0, 0, 0.2)',     // <-- Modify this
  fill: '+2',
  hidden: true
}
```

---

### 3. Cost Chart Colors

In `updateCostChart()` (around line 1800):

```javascript
const ipPattern = createDiagonalPattern('#00b4d8'); // Inpatient Forecast
const opPattern = createDiagonalPattern('#38b000'); // Outpatient Forecast
const aePattern = createDiagonalPattern('#FA7476'); // A&E Forecast
```

Update legend colors:

```javascript
const legendItems = [
  { label: 'Inpatient Cost', color: '#0072B2', datasetIndex: 0 },
  { label: 'Outpatient Cost', color: '#009E73', datasetIndex: 1 },
  { label: 'Accident & Emergency Cost', color: '#f94144', datasetIndex: 2 },
  { label: 'Inpatient Cost (Forecast)', pattern: ipPattern, color: '#00b4d8', datasetIndex: 0 },
  { label: 'Outpatient Cost (Forecast)', pattern: opPattern, color: '#38b000', datasetIndex: 1 },
  { label: 'Accident & Emergency Cost (Forecast)', pattern: aePattern, color: '#FA7476', datasetIndex: 2 }
];
```

---

## ✅ Best Practices for Color Selection

- **Line Charts**:
  - Use dashed lines for forecast
  - Use lighter/darker variants or distinct colors with contrast
- **Bar Charts**:
  - Maintain consistent pattern usage (diagonal)
  - Ensure contrast with historical bars
- **Prediction Intervals**:
  - Use semi-transparent versions of corresponding line color
  - Recommended opacity: `0.1–0.3`

---

By maintaining these visual conventions, you ensure that users can quickly and accurately distinguish between **historical** and **forecasted** data, enhancing the clarity and usability of your charts.