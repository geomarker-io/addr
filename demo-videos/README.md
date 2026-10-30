# addr console demos

Two short, silent R sessions show how to read a CSV, match or geocode its
addresses, inspect the results, and write a CSV. Comments explain each step
while automated typing drives a real R process. The recordings retain the
actual typing, output, execution time, and reading pauses.

| Demo | Length | Hosted video |
| --- | --- | --- |
| Address matching | 1:03 | [Watch](https://github.com/user-attachments/assets/bd1e8318-7547-4a31-ab75-3dcdae2f7d7f) |
| Street-range geocoding | 1:02 | [Watch](https://github.com/user-attachments/assets/fb8e0416-0926-434e-bbb8-a3997ab8d3be) |

The source scripts and two example CSVs are tracked here. MP4s, terminal
recordings, output CSVs, virtual environments, and build/QA files are ignored.
The entire `demo-videos` directory is excluded from R package builds.

## Rebuild the videos

Use Python 3.10 or later and R with `addr` and `dplyr` installed. Recording
requires a Unix pseudo-terminal (macOS or Linux). The published videos were
recorded with addr 2.0.0 and cached 2025 TIGER Address Features.

Before recording geocoding, install the needed reference data using the
[package README's geocoding instructions](../README.md#geocoding). The example
calls `geocode()` with its defaults; running it once before recording warms the
cache, so the on-screen comment about cached data is accurate.

From the repository root:

```sh
python3 -m venv demo-videos/.venv
demo-videos/.venv/bin/python -m pip install -r demo-videos/requirements.txt
demo-videos/.venv/bin/python demo-videos/console/source/record.py
demo-videos/.venv/bin/python demo-videos/console/source/render-console.py
demo-videos/.venv/bin/python demo-videos/console/source/verify.py
```

The recorder finds `R` on `PATH`; use `--r /path/to/R` to select another
installation. The renderer uses Menlo on macOS or DejaVu/Liberation Mono when
available; select a monospaced font explicitly with `--font /path/to/font.ttf`.
Exports are 1920 by 1080, 24 fps, H.264 MP4, without audio or subtitles.

`record.py --episode address-matching` records only one example. To keep a
separate run, use `record.py --output-dir /path/to/run`, then pass that directory
to the renderer and verifier with `--directory /path/to/run`.

The `.R` files in `console/source/` are also ordinary runnable examples. Run
them from a folder containing the CSVs in `console/data/`, or substitute your
own files. Each nonblank script line is a complete R expression so it can be
typed directly at the console prompt.

The reference CSV uses invented `property_id` values (`site_001`, `site_002`).
The four input rows include two formatting variants, a street-name typo, and
an unmatched street. Three rows receive a reference ID; three range geocodes
represent two distinct locations, and one row has no coordinate. These small
examples are not accuracy estimates. Range geocodes estimate locations along
street address ranges, rather than verifying building or parcel locations.

`verify.py` checks the output rows, parses the terminal recordings, fully
decodes the MP4s, checks their silent playback format and timing, extracts
review frames, and confirms generated media are ignored and untracked when
stored in this checkout. If `asciinema` is installed, it also checks conversion
with that player. Review the extracted frames in `console/qa/` before upload.

## Upload to GitHub

The current videos are attachments on [PR #108](https://github.com/geomarker-io/addr/pull/108).
For future versions, attach the MP4s to the PR introducing the updated demos.
Only their URLs go into Markdown; the binaries stay outside Git history.
Attachments to this public repository are publicly accessible.

With a GitHub CLI version that supports `--attach` (tested with 2.103.3):

```sh
gh pr edit PR_NUMBER \
  --attach demo-videos/console/address-matching.mp4 \
  --attach demo-videos/console/range-geocoding.mp4
gh pr view PR_NUMBER --json body --jq .body
```

This appends players to the PR description. Copy the resulting
`https://github.com/user-attachments/assets/...` URL for each video. In the web
interface, dragging each MP4 into the PR description editor performs the same
upload. See [GitHub's attachment instructions](https://docs.github.com/en/github-cli/github-cli/attaching-files-with-github-cli).

Each replacement upload produces a new URL. Update the table above and the
README embeds together when publishing a new recording. Keep a normal link to
each video below its player as a fallback for other Markdown renderers. Put
that link in a sentence: a standalone attachment link can create a second
video player on GitHub.

## Insert the players into the package README

The root README includes both players. To recreate or update the embeds, use
the following markup under the corresponding section headings.

Under `### Address Matching` in the root `README.md`:

```html
<video controls preload="metadata" width="100%"
  src="https://github.com/user-attachments/assets/bd1e8318-7547-4a31-ab75-3dcdae2f7d7f"></video>

Video link: [Watch the address-matching demo](https://github.com/user-attachments/assets/bd1e8318-7547-4a31-ab75-3dcdae2f7d7f).
```

Under `### Geocoding`:

```html
<video controls preload="metadata" width="100%"
  src="https://github.com/user-attachments/assets/fb8e0416-0926-434e-bbb8-a3997ab8d3be"></video>

Video link: [Watch the street-range geocoding demo](https://github.com/user-attachments/assets/fb8e0416-0926-434e-bbb8-a3997ab8d3be).
```

Use the uploaded attachment URLs as `src`, and preview the README on GitHub.
Build the pkgdown homepage locally and verify that both players load and seek:

```r
pkgdown::build_home()
```

The pkgdown homepage uses the root README, and its Markdown renderer preserves
the HTML video elements. The site's current GitHub Actions workflow builds on
pushes and PRs but deploys only when a release is published. Merging a README
change therefore updates GitHub immediately; the live pkgdown site updates
with the next release. See the [deployment workflow](../.github/workflows/pkgdown.yaml).
