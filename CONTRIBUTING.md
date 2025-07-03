
# Contributing to ZooDietConverter

Thank you for your interest in contributing to **ZooDietConverter**! We welcome contributions from the community to improve functionality, fix bugs, and enhance the user experience. This guide will walk you through the steps for contributing to the app's development.

---
## 📋 Code of Conduct

This project follows a [Code of Conduct](CODE_OF_CONDUCT.md). All contributors are expected to follow it in all project spaces.

---
## 🧰 Local Development Setup

1. **Fork this repository**  
   Go to [ZooDietConverter](https://github.com/KaraWatts/ZooDietConverter) and click **Fork** to create your own copy.

2. **Clone your fork**
   ```bash
   git clone https://github.com/YOUR_USERNAME/ZooDietConverter.git
   cd ZooDietConverter
    ````

3. **Open the app in RStudio**

4. **Install required packages**
   Run the following in your R console:

   ```r
   install.packages(c("shiny", "shinydashboard", "DT", "readxl", "dplyr", "writexl", "stringr"))
   ```

5. **Run the application**

   ```r
   shiny::runApp()
   ```

    Make sure your changes do not break existing functionality.


---
## 🛠 What You Can Contribute

* Bug fixes
* UI/UX improvements
* Additional functionality (e.g., more unit conversion options)
* Code refactoring or performance improvements
* Improved documentation or in-app instructions


---
## ✅ Making a Contribution

1. **Create a feature branch**

   ```bash
   git checkout -b feature/your-feature-name
   ```

2. **Make your changes**
   Ensure your code is clean, commented, and tested locally.

3. **Commit your work**

   ```bash
   git add .
   git commit -m "Add: short description of your changes"
   ```

4. **Push to your fork**

   ```bash
   git push origin feature/your-feature-name
   ```

5. **Create a Pull Request**

   * Go to the original repo on GitHub.
   * Click **New Pull Request** and select your branch.
   * Describe the changes clearly and reference related issues if applicable.

---

## 🔍 Reporting Bugs

If you're not ready to submit code, you can still help by [opening an issue](https://github.com/KaraWatts/ZooDietConverter/issues). Include:

* What happened and what you expected to happen
* Steps to reproduce the issue
* Screenshots or error messages if available

---

## 🤝 Code Review Expectations

* Keep pull requests focused and relevant.
* Be respectful and open to feedback.
* Update documentation if your code changes behavior.

---

## 🙏 Thank You

Your input helps make ZooDietConverter better for the entire community. We appreciate your contributions!


