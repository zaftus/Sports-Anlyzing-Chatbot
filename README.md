# 🏀⚽⚾🏈 Sports Stat Analyzer

**Sports Stat Analyzer** is an interactive **R Shiny** web application for analyzing and modeling player statistics across **Basketball, Football, Baseball, and Soccer**. It enables users to explore data, visualize performance trends, build predictive models, and export results—all within a clean dashboard interface.

---

## 🚀 Features

* **Multi-Sport Analysis:** Choose from Basketball, Football, Baseball, or Soccer.
* **Data Upload or Sample Data:** Upload your own CSV or use built-in realistic sample datasets.
* **Exploratory Data Analysis:**

  * Interactive plots (scatter, histogram, boxplot)
  * Summary statistics for all numeric variables
* **Predictive Modeling:**

  * Linear Regression and Random Forest models
  * Visualize model predictions vs. actual values
* **Player Lookup:** Search by player name or ID.
* **Export Options:** Download current dataset (`.csv`) or trained model (`.rds`).

---

## 💻 Technologies Used

* **R Shiny** – Dashboard and interactivity
* **ggplot2** – Visualization
* **dplyr**, **tidyr** – Data manipulation
* **caret**, **ranger** – Modeling and prediction
* **shinydashboard**, **shinythemes** – UI design

---

## ⚙️ Installation & Setup

### 1️⃣ Clone the Repository

```bash
git clone https://github.com/yourusername/sports-stat-analyzer.git
cd sports-stat-analyzer
```

### 2️⃣ Install Dependencies

In R:

```r
packages <- c("shiny","shinydashboard","ggplot2","DT","dplyr","tidyr","ranger","caret","shinythemes")
install.packages(packages, repos = "https://cloud.r-project.org")
```

### 3️⃣ Run the App

```r
shiny::runApp()
```

Or open `app.R` in **RStudio** and click **Run App**.

---

## 📊 Expected CSV Formats

Each sport expects player-level rows with relevant performance columns.

| Sport          | Example Columns                                              |
| -------------- | ------------------------------------------------------------ |
| **Basketball** | player_id, player, games, minutes, points, rebounds, assists |
| **Football**   | player_id, player, games, attempts, yards, touchdowns        |
| **Baseball**   | player_id, player, games, at_bats, hits, home_runs, average  |
| **Soccer**     | player_id, player, matches, minutes, goals, assists          |

If no CSV is provided, you can use the **“Use sample dataset”** button to load a pre-generated dataset.

---

## 🌐 Deployment

Deploy your app easily to:

* [**shinyapps.io**](https://www.shinyapps.io)
* Your own Shiny Server instance

Example:

```r
rsconnect::deployApp('path/to/sports-stat-analyzer')
```

---

## 📁 Project Structure

```
📦 sports-stat-analyzer
 ┣ 📜 app.R                # Main Shiny app
 ┣ 📜 README.md            # Project documentation
 ┣ 📂 data/                # (Optional) Sample CSVs
 ┣ 📜 LICENSE              # Recommended MIT License
 ┗ 📜 .gitignore           # Ignore .Rhistory, .RData, etc.
```

---

## 📄 License

MIT License © 2025 [Your Name]

You are free to use, modify, and distribute this project with attribution.

---

## 💡 Acknowledgements

* Built with **R Shiny** and open-source packages from the R community.
* Designed for data scientists, students, and sports enthusiasts exploring performance analytics.
