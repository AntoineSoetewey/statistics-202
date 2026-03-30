# Contributing to statistics-202

Thank you very much for your interest in improving this project.
Any help is appreciated, whether it is reporting a typo, proposing an idea, improving code, or refining the learning experience for students.

This app was built to support introductory statistics teaching, so contributions that improve clarity, correctness, and usability are especially valuable.

## Ways to contribute

You are welcome to help by:

- Reporting bugs or unexpected behavior
- Suggesting improvements to the app UI or explanations
- Improving documentation (README, report wording, comments)
- Proposing or implementing code improvements
- Sharing teaching-oriented feedback from classroom use

## Before you start

Please open an issue first for large changes so we can align on scope and approach.
A short discussion helps avoid duplicate work and keeps contributions focused.

For small fixes (typos, minor wording, simple bug fixes), feel free to open a pull request directly.

## Development setup

1. Clone the repository.
2. Open the project in RStudio or your preferred R environment.
3. Install required packages:

```r
install.packages(c(
  "shiny",
  "shinythemes",
  "ggplot2",
  "plotly",
  "DT",
  "rmarkdown",
  "knitr",
  "pander",
  "performance",
  "see"
))
```

4. Run the app locally:

```r
shiny::runApp()
```

## Pull request guidelines

To help reviews go smoothly, please:

- Keep pull requests focused on one topic
- Write clear commit messages and PR descriptions
- Explain what changed and why
- Include screenshots for UI changes when relevant
- Update documentation when behavior changes

If your change affects calculations or interpretation text, please include a short reproducible example so the result can be verified quickly.

## Coding and content style

- Prefer readable, simple, and explicit code
- Keep explanations student-friendly and statistically accurate
- Avoid unnecessary complexity in UI or wording
- Preserve existing app behavior unless a change is intentional and documented

## Review process

All contributions are reviewed with care.
Feedback is meant to improve the project, never to discourage contributors.

Thank you again for your time, effort, and goodwill.
Your help genuinely makes this educational tool better for everyone.
