# Video Script - GitTruck@Duck Demo (~4 min)

**Scenario:** onboarding onto an unfamiliar open-source project — using GitTruck to quickly understand the structure, identify hotspots, and see how the project evolved over time

We'll use Express.js — a popular Node.js web framework with 300+ contributors.

---

## 1. Intro (~30s)

"Hi, we're group 933 and we're demoing GitTruck, a Git repository visualization tool. The paper we chose is GitTruck@Duck from ICSME 2024, which adds time range selection to the visualization. Our scenario is: imagine you're a new developer joining an open-source project and you want to quickly understand the codebase — what's big, what's active, who works on what."

**Action:** show the terminal, run `npx -y git-truck` in the repo

---

## 2. Repository overview (~45s)

"This is the main view — a bubble chart where each bubble is a file. Bigger bubbles mean bigger files, and the nesting shows the folder structure. Colored by file type, we can immediately get a feel for the project — where the code lives, what languages are used, and which files are the largest."

**Action:** hover over bubbles to show file names and sizes, point out the main folders and largest files

---

## 3. Contributor view (~45s)

"Now switching to top contributor — each color is a different developer. This is where it gets interesting for onboarding: you can see who owns which parts of the codebase. If you had a question about a specific module, you'd know exactly who to ask. You can also see if certain areas are single-person dependencies, which is a risk."

**Action:** change color to "top contributor", click into folders, show the author breakdown panel

---

## 4. Last changed view (~30s)

"Switching to last changed — green means recently modified, blue means older. As a new developer, this tells you where active development is happening and which parts are stable or potentially abandoned. The blue areas might have tech debt or need a fresh look."

**Action:** switch color to "last changed", point out contrast between active and stale areas

---

## 5. Time range selection - the Duck feature (~1 min)

"This is the key feature from the paper — the timeline at the bottom shows commits per week. By dragging the slider, we can focus on a specific time period. For example, if I narrow it to the last month, I can see exactly which files were touched recently. Or I can look at an older period to understand what the project looked like six months ago. This is super useful for understanding how the project evolved and what changed between releases."

**Action:** use the timeline slider to narrow the range, try a recent window and an older window, show how the visualization changes

---

## 6. Wrap-up (~15s)

"So that's GitTruck with the Duck extension — a quick way to visually understand a project's structure, ownership, and evolution over time, all running locally with zero setup. Thanks for watching."

**Action:** show the full overview one last time
