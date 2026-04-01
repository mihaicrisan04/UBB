# GitTruck@Duck - Interactive Time Range Selection in Hierarchy-Oriented Polymetric Visualization of Git Repository Evolution

**Paper:** GitTruck@Duck - Interactive Time Range Selection in Hierarchy-Oriented Polymetric Visualization of Git Repository Evolution\
**DOI:** 10.1109/ICSME58944.2024.00090\
**Conference:** ICSME 2024 - Tool Demo Track\
**Authors:** Adrian Hoff, Thomas Hoffmann Kilbak, Leonel Merino, Mircea Lungu\
**Team members:** Mihai Crisan, Catalin Giga, Ovidiu Daescu, Alex Danciu, Hunor Feketics\
**Group:** 933

## Approach and motivation

GitTruck is a web-based software visualization tool designed to help developers and educators understand the structure and evolution of Git repositories. The motivation behind the tool stems from the challenge of comprehending large codebases — understanding which files are most active, who contributes to what, and how the project has evolved over time is difficult to grasp from raw Git history alone. GitTruck addresses this by aggregating a repository's Git history using an in-memory relational database and rendering it as an interactive 2D bubble chart or treemap, where file size and color encode configurable metrics such as file size, number of commits, top contributor, or last change date. The tool runs entirely locally, requires no cloud services, and is privacy-first by design.

## Aim and novelty

The core contribution of this ICSME 2024 paper is the "Duck" extension, which introduces interactive time range selection to the existing GitTruck visualization. Previous versions of GitTruck could only display cumulative, all-time metrics for a repository. The Duck feature adds a commits-per-week timeline with an adjustable slider, allowing users to restrict the visualization to a specific time window. This enables fine-grained temporal analysis — for example, seeing which parts of a codebase were most active during a sprint, identifying stale files that haven't been touched in months, or tracking how contributor focus shifted over time. The tool also includes an algorithm for detecting file renames across the selected time range, ensuring that file identity is preserved even as paths change.

## Validation

GitTruck has been validated through multiple peer-reviewed publications and real-world use cases. The original tool was presented at VISSOFT 2022, accompanied by a study demonstrating how Git repository visualization supports educators in assessing group projects. The ICSME 2024 paper builds on this by presenting the time range selection feature and demonstrating its utility through usage scenarios on open-source projects. A further validation study was published at SIGCSETS 2025, exploring the adaptability and usefulness of GitTruck for assessing software capstone project development. Across these studies, the tool has been shown to be effective for both developers seeking codebase understanding and educators evaluating student collaboration patterns.
