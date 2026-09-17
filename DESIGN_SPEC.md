# MEA Explorer — Design Handbook

Generated from the MEA Explorer design system project. Sections: design guide, UI kit notes, component reference.

---

# MEA Explorer — Design System

The design system for **MEA Explorer**, an open-source R/Shiny application for automated analysis and
visualization of multielectrode array (MEA) experiments. v0.1 parses Axion/AxIS CSV exports, extracts
well-level **Mean Firing Rate (Hz)** and **Number of Bursts**, converts them to a standardized tidy
format, lets researchers assign experimental metadata on a plate map, and generates reproducible
scientific figures.

The product is one surface — a five-stage analysis workspace:

**Upload → Verify → Plate Map → Explore → Export**

It should read as a *scientific instrument*, not an analytics dashboard: hairline borders, graph-paper
grounds, monospace numerals, no gauges, no KPI walls, no gradients.

---

## Sources

| Source | URL / path | What we got |
| --- | --- | --- |
| MEA Explorer repository | https://github.com/jpflores-13/MEA-Explorer | **Empty at the time of writing** — GitHub returns an empty-tree response for `main` and `master`. No R/Shiny code, no CSS, no assets, no logo. |
| Product brief | Supplied in the design request | Product vision, data format, the five MVP stages, scientific principles, visual-identity direction. |
| Lucide icons | https://github.com/lucide-icons/lucide | 35 SVGs copied into `assets/icons/` (see Iconography). |

> **Read the repository yourself before extending this system.** Once `jpflores-13/MEA-Explorer`
> contains the Shiny app, its `bslib` theme, `ggplot2` scale functions and UI module structure are the
> ground truth for anything here — especially chart defaults and control chrome. `github.md` records
> the association for one-click sync.

Because the repository was empty, **everything in this system is an original design derived from the
written brief**, not a recreation. Nothing was reconstructed from memory of another product, and no
logo was invented: the brand appears as a type-only wordmark (see `guidelines/brand-wordmark.card.html`).

---

## Index

| Path | What it is |
| --- | --- |
| `styles.css` | Single entry point for consumers — `@import` list only. |
| `tokens/` | `fonts`, `colors`, `typography`, `spacing`, `borders`, `elevation`, `motion`, `dataviz`, `base`. |
| `components/` | React primitives in five groups: `core/`, `forms/`, `feedback/`, `navigation/`, `data/`. |
| `ui_kits/mea-explorer/` | Click-through recreation of all five stages. Start at `index.html`; see its `README.md`. |
| `guidelines/` | 21 foundation specimen cards (Colors, Type, Spacing, Brand). |
| `assets/icons/` | Lucide SVGs actually used by the product. |
| `thumbnail.html` | Homepage tile. |
| `SKILL.md` | Agent-Skills wrapper so this folder works inside Claude Code. |

### Components

**core/** — `Icon`, `Button`, `IconButton`, `Badge`, `Tag`, `Card`, `StatReadout`
**forms/** — `Input`, `Select`, `Checkbox`, `Switch`, `SegmentedControl`, `FileDrop`
**feedback/** — `Alert`, `StatusDot`, `ProgressBar`, `Dialog`, `Tooltip`
**navigation/** — `StageNav`, `Tabs`
**data/** — `PlateGrid`, `DataTable`, `MetricSelector`, `ChartFrame`, `Legend`, `StripPlot`, `ProvenanceNote`

Each component directory holds `<Name>.jsx`, `<Name>.d.ts` (props contract), `<Name>.prompt.md`
(when & how to use it) and one `@dsCard` specimen HTML.

#### Intentional additions

No source defined a component inventory, so the set above was authored from the brief. Three entries
exist specifically because MEA Explorer needs them and a generic kit would not have them:

- **`PlateGrid`** — the signature feature. Derives rows/columns from well IDs; three modes (identity,
  condition, values-as-viridis-heatmap); row/column headers are batch-select controls.
- **`ProvenanceNote`** — a standing, non-dismissible record of exclusions, filters and transforms.
  Scientific transparency outranks a clean panel.
- **`StripPlot` + `ChartFrame`** — a dot/strip plot that always draws every observation (excluded wells
  as open dashed circles) inside a frame that forces an N statement and a caption.

Deliberately **absent**: no Toast (nothing in this workflow succeeds silently enough to need one), no
Avatar (single-user desktop-style app), no Accordion, no Breadcrumb, no gauge/donut/KPI-card family.

---

## Data contract (verified against a real export)

Checked against `20260505 KOLF EGC BUMP d40(000).csv` — a Maestro Pro / AxIS 4.1.0.1 Statistics
Compiler export, CytoView MEA 12, 12 wells (A1–C4), 168 lines.

- **Parse the `Well Averages` block only.** Wells are columns; measurements are rows. v0.1 takes exactly
  two rows: `Mean Firing Rate (Hz)` and `Number of Bursts`.
- **Never read `Treatment Averages`.** That block is already plate-averaged (`Mean Firing Rate (Hz) - Avg`,
  `… - Std`). Using it would silently collapse 12 wells into one number.
- **Plate geometry comes from the header**, not from guesswork: `Plate Type,CytoView MEA 12` and the
  `Well Information` row. `PlateGrid` still derives rows/columns from well IDs as a fallback.
- **`Treatment/ID` is empty in practice.** In this file every well's Treatment, Concentration and
  Additional Information field is blank — which is precisely why the Plate Map stage exists.
- **Timepoint comes from `Recording Name`** (`20260505 KOLF EGC BUMP d40`), with the file name as fallback.
  Show the researcher which one was used and let them override it.
- **Values span four orders of magnitude** in one plate: MFR `2.6E-05` → `0.424880` Hz, bursts `0` → `457`.
  Two consequences for design: scientific notation must survive parsing as a number, and the log10
  y-axis switch on Explore is not a nicety — the linear view flattens rows A and B into the axis.
- **Zeros are real data**, not missing values. Wells with `0` bursts and a non-zero firing rate are
  plotted at zero, never dropped. Blank cells (e.g. `Burst Duration`) are missing and stay missing.
- **Context worth surfacing**, read from the settings header: `Minimum Spike Rate for Active Electrode,5
  spikes/minute`, ISI burst threshold `100 ms` / min 5 spikes, analysis duration `599.25 s`. These belong
  in the exported metadata JSON so a figure can be reproduced.
- **Electrode-level exports are rejected with an explanation**, not silently ignored.

---

## Content fundamentals

**Voice: a careful colleague reporting what the software found.** Declarative, specific, never
congratulatory.

- **Person.** Address the researcher as *you* ("Check the wells before assigning conditions"). The app
  refers to itself by name in third person when it is the actor: "MEA Explorer locates the Well
  Averages section", "MEA Explorer does not drop wells automatically." Never "we".
- **Casing.** Sentence case for headings, buttons, alerts and table cells. UPPERCASE only for
  micro-caps field/axis labels (`TREATMENT`, `WELLS DETECTED`). Never Title Case.
- **Domain terms keep their scientific capitalisation**: Mean Firing Rate, Number of Bursts, Well
  Averages, IFNγ, D40. Tidy column names stay snake_case and monospace: `mean_firing_rate`.
- **Numbers are always exact and always monospace.** `0.0017`, not "~0.002". Units follow the number in
  muted type: `0.041 Hz`, `71 bursts`.
- **State N everywhere a value is summarised.** "n = 11 wells · 2 technical replicates · 1 plate".
  A chart without an N note is considered a bug.
- **Buttons name their scope**, not their mechanism: "Apply to 4 wells", "Export 5 files",
  "Continue to Plate Map" — not "Submit", "OK", "Next".
- **Errors say what, where, and what happens next.** "plate3_raw.csv could not be parsed. No 'Well
  Averages' section was found. MEA Explorer 0.1 reads the well-average block of an AxIS export."
- **Never hide a caveat to look simple.** Exclusions, inferred timepoints and missing measurements get
  their own visible line: "Timepoints inferred from file names."
- **No emoji, anywhere.** Status is carried by a Lucide glyph plus a semantic colour. ✓/✗ appear only
  as `check` / `x` icons, never as text characters.
- **No marketing adjectives.** Not "powerful", "seamless", "beautiful insights". The closest the
  product comes to a flourish is "Ready to analyze".

---

## Visual foundations

**Colour.** A single interactive hue — *Signal* teal `--signal-600 #0B6E63` — carries every action,
selection and focus state. *Trace* amber `--trace-600 #B4530A` is its secondary and doubles as the
Number-of-Bursts metric colour. Text is five cool greys (`--ink-1…5`) on warm off-white paper
(`--paper-1 #F8F8F5`), which keeps long analysis sessions calmer than pure white. At most two
background values on a screen: `--paper-1` page, `--paper-0` cards.

Three colour systems never mix roles: **metric identity** (teal MFR / amber bursts), **categorical
conditions** (Okabe–Ito, colour-vision-deficiency safe), **sequential values** (viridis, so heatmaps
match `ggplot2` output the researcher will produce downstream).

**Type.** IBM Plex Sans for interface, IBM Plex Mono for every number, well ID, file name and tidy
column name. Plex is an engineering typeface with genuine instrument character and excellent small-size
legibility; the sans/mono pair shares skeletons, so a mono value inside a sans sentence does not fight.
Display sizes get `-0.02em` tracking; micro-caps labels get `+0.08em` and uppercase. Tabular figures are
mandatory (`font-variant-numeric: tabular-nums`) so columns of values align.

**Backgrounds.** No photography, no illustration, no gradients. One texture exists: an 8px
**graph-paper grid** at 4.5% ink (`--bg-graph` / `.mea-graph`), applied only to surfaces where data
lives — plot bodies, the plate map, the upload drop zone. Tables, forms and prose sit on flat
`--paper-0`. The grid is the brand's only decorative move and it earns its place by reading as bench
paper.

**Layout.** 52px top bar, 44px stage bar, both full-bleed and fixed; content in a centred column
(1080–1320px) with 32px page padding. The workspace pattern is content + 296px inspector rail
(`--inspector-width`); the inspector holds selection state, view switches and the provenance note.
Density is allowed — this is an instrument — but gaps never drop below 8px between semantic groups.

**Borders, radii, shadows.** 1px hairlines (`--line-1` inside cards, `--line-2` on controls) do the
structural work; shadows are almost absent in-page (`--shadow-2` = `0 1px 3px rgba(15,20,24,.07)`).
Radii are deliberately small and hardware-like: 3px controls, 5px inputs/buttons, 8px cards, 3px plate
wells. Depth is reserved for overlays: `--shadow-3` popovers, `--shadow-4` dialogs.

**Transparency & blur.** Used in exactly one place — the dialog scrim (`--scrim` 42% ink +
`--blur-overlay`). Never behind text, never on cards, no frosted panels.

**States.** Hover = background step (`--paper-2` on light, `--signal-700` on primary); no colour
inversion, no lift. Press = 0.5px downward nudge, no scale. Selected = 1.5px inset teal ring
(`--ring-selected`) — the plate map depends on it. Focus = 2px white gap + 4px teal halo
(`--ring-focus`), always visible for keyboard users. Disabled = 45% opacity, cursor not-allowed.

**Motion.** 80–280ms on `cubic-bezier(.2,0,.2,1)`. Transitions carry state change only: control
colours, progress widths, dialog fade. **Data never animates in** — a plot that grows from zero
misrepresents itself mid-animation. `prefers-reduced-motion` zeroes every duration.

**Charts.** Axes `--chart-axis`, gridlines `--chart-gridline`, points 0.72 alpha at r≈3.2px, median
1.2px solid ink, box fill 4% ink. Excluded observations are drawn as open dashed circles, never removed.
Per-group n is printed under every column.

---

## Iconography

- **Set:** [Lucide](https://github.com/lucide-icons/lucide), 24×24 viewBox, 2px stroke, round caps.
  35 SVGs used by the product are copied into `assets/icons/`. **Substitution flagged:** the source
  repository contained no icons, so Lucide was chosen for its neutral, scientific line quality; swap it
  if the Shiny app standardises on Bootstrap Icons (the `bslib` default) or Font Awesome.
- **How to render:** always the `Icon` component (`<Icon name="circle-check" size="sm" />`). It
  mask-tints the glyph with `currentColor` from the pinned `lucide-static@0.544.0` CDN, so icons inherit
  text colour. In static HTML, `<img src="assets/icons/<name>.svg">` is acceptable.
- **Sizes:** 13–15px inline with text, 18px in toolbars, 24px in the drop zone. Nothing larger; there
  are no hero icons or icon illustrations in this product.
- **Colour:** `--ink-3` at rest, `--signal-600` when active, semantic colour inside alerts and file
  rows. Icons are never the only carrier of meaning — every status glyph sits beside a text label.
- **No emoji. No unicode glyph icons** (no ✓, →, ●, ▲ in copy). No icon font. No custom or
  hand-drawn SVG: if a glyph is missing, pick the nearest Lucide slug and add it to `assets/icons/`.
- **Domain glyphs in use:** `flask-conical` (session), `microscope` / `activity` / `zap` (spike
  activity), `grid-3x3` (plate), `table` (tidy data), `funnel` (filters), `eye-off` (exclusions).

---

## Using this system

```html
<link rel="stylesheet" href="styles.css">
<script src="_ds_bundle.js"></script>
<script>
  const { Button, PlateGrid, ChartFrame, StripPlot } = window.MEAExplorerDesignSystem_727df7;
</script>
```

Rules that matter more than the tokens:

1. One primary button per view, and it names the next stage.
2. Every number in mono with tabular figures; every summary with its N.
3. Show the observations. Bars and donuts are not in this system on purpose.
4. Exclusions, filters and transforms stay visible — use `ProvenanceNote`.
5. Well identity and experimental condition are different facts; never merge them into one label.

---

## Known gaps

- **No R/Shiny source** was available, so no `bslib` theme values, `ggplot2` themes or module names are
  reflected here.
- **No brand assets** (logo, illustration, imagery) existed to copy; the wordmark is type-only.
- **Fonts are loaded from Google Fonts**, not local binaries — no font files were supplied.
- Statistical testing, plate-layout saving, per-electrode data and multi-plate sessions are out of v0.1
  scope and intentionally undesigned.



---

# UI kit

# MEA Explorer — application UI kit

A click-through recreation of the MEA Explorer v0.1 workflow, composed entirely from this design
system's components. Open `index.html` and use the stage bar (Upload → Verify → Plate Map → Explore
→ Export) or the primary button on each screen to move forward.

## Files

| File | What it is |
| --- | --- |
| `index.html` | App entry. Loads `styles.css`, the compiled component bundle, `data.js`, then each screen. |
| `data.js` | Fake-but-plausible experiment: one 12-well plate, 3 timepoints (D40/D47/D54), 3 treatments, well B2 excluded. Exposes `window.MEA`. |
| `AppShell.jsx` | Top bar (wordmark, session, status), `StageNav`, `Page` / `PageHead` layout helpers. |
| `UploadScreen.jsx` | Drop zone, per-file parse results, failed-file alert, detected-experiment summary. |
| `VerifyScreen.jsx` | Per-file detection cards, flagged well, tidy / wide / warnings tabs over the extracted data. |
| `PlateMapScreen.jsx` | Interactive plate with identity / condition / values modes, batch metadata assignment, apply dialog. |
| `ExploreScreen.jsx` | Metric selector, four plot views (by condition, time course, plate pattern, by replicate), group summary, provenance panel. |
| `ExportScreen.jsx` | Export contents picker, figure format options, exact file manifest. |
| `TimeCourse.jsx` | Kit-local time-course chart (group means + individual wells). Not a shared component. |

## Interactions that actually work

- Stage navigation across all five screens.
- Upload: remove a file; simulated parse progress on drop.
- Verify: tab switching between tidy preview, as-read wide view and warnings.
- Plate Map: click wells, click row/column headers to batch-select, switch plate mode, open the apply dialog, change timepoint.
- Explore: switch metric, switch plot type, change timepoint, toggle points and box overlay, hover points for a readout.
- Export: toggle contents and figure format — the manifest recomputes.

## Screen notes

Nothing here invents product behaviour beyond the v0.1 brief: descriptive statistics only, no modelling,
no automatic exclusion, and every summary states its N. The provenance panel is deliberately always
visible rather than tucked into a menu.



---

# Component reference


## core


### Badge

Small status or metric marker. Uppercase label by default, monospace when it carries data.

```jsx
<Badge tone="ok">Parsed</Badge>
<Badge tone="mfr" mono>12 wells</Badge>
```

Tones mfr/bursts carry the metric identity colours and should only label that metric.


### Button

Primary action control. One primary button per view; everything else is secondary or ghost.

```jsx
<Button variant="primary" iconLeft={<Icon name="arrow-right" size="sm" />}>Continue to Verify</Button>
```

Variants: primary (teal, the single forward action), secondary (bordered, on white), ghost (toolbars), danger (destructive only). `loading` shows an inline spinner.


### Card

The standard panel: white, 1px hairline border, 8px radius, near-flat shadow.

```jsx
<Card title="Detected measurements" subtitle="Experiment_D40.csv" actions={<IconButton icon="eye" label="Preview" />}>
  ...
</Card>
```

Use `graph` for plot and plate surfaces so the grid motif shows through. Never stack shadows deeper than --shadow-2 in-page.


### Icon

Renders one Lucide glyph, mask-tinted so it inherits text colour — MEA Explorer's only icon source.

```jsx
<Icon name="circle-check" size="sm" color="var(--ok-600)" title="Parsed" />
```

Sizes: sm 14 / md 16 / lg 20 / xl 24, or pass a number. Never hand-roll an SVG; if a glyph is missing, pick the nearest Lucide slug.


### IconButton

Square, label-less control for toolbars, card headers and table rows.

```jsx
<IconButton icon="download" label="Export figure" variant="outline" />
```

Use `active` for toggles (tints teal). Always pass `label` — these are unlabelled on screen.


### StatReadout

A single measured number with its unit — monospace, tabular figures. Not a KPI card.

```jsx
<StatReadout label="Median MFR" value="0.041" unit="Hz" tone="mfr" note="12 wells · 3 replicates" />
```

Always give `note` when the number summarises multiple observations; a bare summary hides its N.


### Tag

Removable chip for an experimental condition (treatment, timepoint, replicate).

```jsx
<Tag color="var(--cat-2)" onRemove={() => drop("IFNy")}>IFNy</Tag>
```

The swatch must match the colour that condition uses in every chart and on the plate map.


## forms


### Checkbox

Multi-select control — export contents, well exclusion lists, column pickers.

```jsx
<Checkbox checked={tidy} onChange={...} label="Tidy well-level data" hint="one row per well per metric · CSV" />
```

Use `indeterminate` for a parent covering a partial selection.


### FileDrop

The entry point of the whole app: drag-and-drop for Axion/AxIS CSV exports.

```jsx
<FileDrop onFiles={queue} hint="Axion / AxIS CSV export · Well Averages section required" />
```

Graph-paper ground at rest, teal wash while dragging. Use `compact` once the file list is populated.


### Input

Labelled text field. Labels are uppercase micro-caps; hints sit below in muted grey.

```jsx
<Input label="Treatment" placeholder="e.g. Cytokine cocktail" hint="Applied to 4 selected wells" />
```

Set `mono` for anything the researcher will compare character by character. `suffix` carries units ("Hz", "µM").


### SegmentedControl

Two to four mutually exclusive view modes, side by side in a sunken track.

```jsx
<SegmentedControl value={plot} onChange={setPlot} options={["Strip","Box","Heatmap","Time course"]} />
```

For the metric switch use MetricSelector instead — the metric deserves more weight than a segment.


### Select

Native select with MEA Explorer chrome — used for timepoint, replicate, grouping variable.

```jsx
<Select label="Group by" value={by} onChange={e => setBy(e.target.value)} options={["Treatment","Timepoint","Replicate"]} />
```


### Switch

Binary view setting that takes effect immediately (show points, log scale, free y-axis).

```jsx
<Switch checked={logY} onChange={setLogY} label="Log10 y-axis" description="Zeros shown as open points at the axis" />
```

If a toggle changes what the data means, say so in `description` — never silently.


## feedback


### Alert

Inline parsing or data-integrity message. Say what was found, in which file, and what will happen.

```jsx
<Alert tone="warn" title="Number of Bursts missing in 1 file">
  Experiment_D47.csv has no Number of Bursts row. Mean Firing Rate is still available.
</Alert>
```

Never use an alert to congratulate the user; use it to state facts they must act on.


### Dialog

Modal for a bounded decision: batch metadata assignment, export options, raw-row inspection.

```jsx
<Dialog open={open} title="Assign metadata" subtitle="4 wells selected" onClose={close}
  footer={<><Button variant="ghost">Cancel</Button><Button>Apply to 4 wells</Button></>}>...</Dialog>
```

Positioned absolutely inside its app frame, over a blurred scrim. Footer action names the scope ("Apply to 4 wells").


### ProgressBar

Determinate progress for parsing and export. 4px track, no stripes or gradients.

```jsx
<ProgressBar value={72} label="Parsing Experiment_D40.csv" />
```


### StatusDot

Seven-pixel state marker for file rows, session state and the top bar.

```jsx
<StatusDot status="busy" pulse label="parsing" />
```


### Tooltip

Hover readout on dark ink — the standard way to show a well's exact value or a term's definition.

```jsx
<Tooltip mono content={"A3\n0.0500 Hz\nControl · D40"}><span>A3</span></Tooltip>
```

Newlines are preserved, so use it for compact key/value stacks on charts and plate wells.


## navigation


### StageNav

The app's spine: Upload -> Verify -> Plate Map -> Explore -> Export, always visible under the top bar.

```jsx
<StageNav current="plate" onSelect={go} stages={[
  {id:"upload",label:"Upload",complete:true},{id:"verify",label:"Verify",complete:true},
  {id:"plate",label:"Plate Map"},{id:"explore",label:"Explore",enabled:false},{id:"export",label:"Export",enabled:false}]} />
```

Completed stages keep a teal check and stay clickable — researchers must be able to go back and re-verify.


### Tabs

Within-stage view switch (Files / Tidy preview / Warnings). Counts sit after the label in mono.

```jsx
<Tabs value={tab} onChange={setTab} tabs={[{id:"files",label:"Files",count:3},{id:"tidy",label:"Tidy preview",count:36}]} />
```


## data


### ChartFrame

Figure wrapper: axis titles, N note, legend and caption around any plot body.

```jsx
<ChartFrame title="Mean Firing Rate by treatment" yLabel="Mean firing rate (Hz)" xLabel="Treatment"
  nNote="n = 12 wells · 3 biological replicates" legend={<Legend items={groups} />}
  caption="Points are well averages; boxes show median and IQR. Wells are technical units within a replicate.">
  <StripPlot data={rows} />
</ChartFrame>
```

Every chart in the product ships with `nNote` and `caption` — a plot with no stated N is not publishable.


### DataTable

Tidy-data preview and summary tables. Sticky uppercase header, zebra rows, monospace numbers.

```jsx
<DataTable maxHeight={260} caption="36 rows · 3 files" rows={tidy}
  columns={[{key:"well",label:"Well",mono:true},{key:"metric",label:"Metric"},{key:"value",label:"Value",mono:true,align:"right"}]} />
```

Always caption with the true row count so a truncated preview can never be mistaken for the whole dataset.


### Legend

Discrete condition keys (with per-group N) or a continuous viridis scale bar.

```jsx
<Legend items={[{label:"Control",color:"var(--cat-1)",n:4},{label:"IFNy",color:"var(--cat-2)",n:4}]} />
<Legend ramp rampMin="0.00" rampMax="0.14" />
```


### MetricSelector

The prominent metric switch at the top of Explore. Each metric owns a colour used by every chart downstream.

```jsx
<MetricSelector value={metric} onChange={setMetric} />
```

mfr = Mean Firing Rate (Hz, teal) · bursts = Number of Bursts (count, amber). Do not demote this to a dropdown.


### PlateGrid

MEA Explorer's signature component: the plate itself. Rows and columns are derived from the well IDs, so a 12- or 48-well plate needs no configuration.

```jsx
<PlateGrid mode="heatmap" unit="Hz" wells={wells} selected={sel}
  onSelectWell={toggle} onSelectRow={selectRow} onSelectColumn={selectCol} />
```

Row and column headers are batch-select buttons (select A1-A4, assign Control). Heatmap fill is viridis, matching ggplot2 output. Keep well identity (mono ID, top-left) visually separate from condition (corner flag / tint) — they are different kinds of fact.


### ProvenanceNote

Standing record of every filter, exclusion and transform affecting the current view. Scientific transparency beats a clean-looking panel.

```jsx
<ProvenanceNote items={[
  {label:"Excluded wells", detail:"B2, C4", icon:"eye-off"},
  {label:"Files in view", detail:"3 of 3", icon:"file-text"}]} />
```

Show it wherever data is summarised (Explore sidebar, Export sheet). Never hide exclusions behind a menu.


### StripPlot

Dot/strip plot with an optional box overlay — the default comparison chart. Every observation is drawn, including excluded wells (open dashed circles).

```jsx
<StripPlot data={rows} colors={{Control:"var(--cat-1)","IFNy":"var(--cat-2)"}} showBox onPointHover={setHover} />
```

Per-group n is printed under each column. Do not replace with bars: a bar hides the observations it averages.



---

# Skill wrapper (SKILL.md)

```
---
name: mea-explorer-design
description: Use this skill to generate well-branded interfaces and assets for MEA Explorer, either for production or throwaway prototypes/mocks/etc. Contains essential design guidelines, colors, type, fonts, assets, and UI kit components for protoyping.
user-invocable: true
---

Read the README.md file within this skill, and explore the other available files.
If creating visual artifacts (slides, mocks, throwaway prototypes, etc), copy assets out and create static HTML files for the user to view. If working on production code, you can copy assets and read the rules here to become an expert in designing with this brand.
If the user invokes this skill without any other guidance, ask them what they want to build or design, ask some questions, and act as an expert designer who outputs HTML artifacts _or_ production code, depending on the need.
```
