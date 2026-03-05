# NBA Shot Analysis Website

This Quarto website publishes league-level NBA shooting trends and an interactive shot explorer.

## Local Build

```bash
cd Scrollytell_NBA
quarto render
```

Output is generated in `Scrollytell_NBA/docs/`.

## GitHub Pages Setup

1. Push this repo to GitHub.
2. In **Settings -> Pages** set Source to `GitHub Actions`.
3. Push to `main` and the workflow in `.github/workflows/deploy-quarto.yml` will publish the site.

## Key Files

- `index.qmd`: primary immersive scrollytelling story page.
- `steph-vs-player.qmd`: Curry-vs-player hypothesis testing and shot-map comparison.
- `player-vs-player.qmd`: head-to-head player analytics.
- `player-dashboard.qmd`: single-player dashboard with trends and shot profile.
- `shot-explorer.qmd`: interactive filters and shot charts.
- `R/prepare_data.R`: shared cleaning and aggregation logic.
- `_quarto.yml`: site config and navigation.
