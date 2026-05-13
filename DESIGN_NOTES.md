# Design notes for kdhenderson.github.io

Layout, sizing, and naming conventions for content on this site. These are
forward-looking standards for new content. Existing files may not match yet;
update incrementally when touching them.

## Repository structure

The site uses a parallel pattern for projects and assets: an active directory
for content the live site uses, plus optional sibling `_*_archive/` and
`_*_template/` directories for content kept in the repo but not deployed.

| Purpose | Projects | Assets |
|---|---|---|
| Active (deployed) | `_projects/` | `assets/` |
| al-folio stock kept for reference | `_projects_template/` | `_assets_template/` |
| Own superseded content | `_projects_archive/` | `_assets_archive/` |

Jekyll auto-ignores `_*` directories that aren't configured as collections in
`_config.yml`, so files in template and archive directories don't deploy.

When in doubt, **delete unused content rather than archive it**. Git history
is the archive for one-off files. Reserve archive directories for content with
intentional reference value (e.g., a previous design iteration you might
revisit).

## Project cards (on /projects/)

### Title
- Target: 28 characters or fewer to fit on 2 lines
- Titles 32+ characters risk wrapping to 3 lines
- Verify on render when approaching the limit
- Use Title Case (e.g., `Emerging Brand Scoring`, not `emerging brand scoring`)
- Acronyms keep their natural capitalization (e.g., `RAG`, `ML`, `ARIMA`)

### Description
- Target: 100 characters or fewer to fit on 3 lines

### Thumbnail image
- Standard aspect ratio: 3:2 (e.g., 1500 by 1000 pixels)
- File size: keep under 500KB; under 200KB ideal

## Project page internal layout

Standard structure for individual project pages, beyond the YAML
frontmatter, the author/date header div, and the abstract body.

### Download links and status notes

For paper or slide-deck downloads plus any status note (e.g., "Forthcoming"):

- Each download link on its own paragraph (blank line between), wrapped in `**` for bold
- Status note as a single italicized paragraph at the end

Example:

    **[Read the paper](/assets/pdf/EmergingBrandScoring.pdf)**

    **[View the slide deck](/assets/pdf/EmergingBrandScoring.pptx)**

    *Forthcoming in the SMU Data Science Review.*

### Skills section

- Heading: `#### Skills` (one level smaller than `### Abstract`)
- Body: wrap in `<small>` for slightly smaller font; items separated by ` · ` (interpunct)
- Add `<br>` before the section for visual breathing room

Example:

    <br>

    #### Skills
    <small>R · Statistical modeling · Multi-criteria decision analysis</small>

## Asset naming

- Content-named, not name-named (e.g., `EmergingBrandScoring.pdf`, not `Kristin_Henderson_Capstone.pdf`)
- Resume is the exception, since it travels off-site
- PascalCase for project files and their matching assets
