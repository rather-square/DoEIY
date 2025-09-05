# DoEIY
An Open-Access Web Platform for Democratizing Experimental Design in Chemical and Materials Research

# Introduction

Welcome to DoEIY! This tool helps plan, conduct, and analyse Design of Experiments (DoE) in a structured, accessible, and user-friendly way. It guides the user from creating a design through model fitting and analysis, and on to visualising results and optimising responses. No software installation is needed, only an internet connection. This guide provides clear explanations of key DoE concepts along with step-by-step instructions.

# DoEIY Overview

![Overview of the DoEIY software workflow](figures/DoEIY_Overview.png)  
*Overview of the DoEIY software workflow. 1. Make a Design: users select a design type, define factors, and generate the experimental design. 2. Enter/Edit Results: experimental responses are entered directly into the editable design table, with options to save or load designs. 3. Analyze the Design: users select or create custom models; the model summary and statistics as well as design diagnostics are presented. 4. Explore the Design: interactive visualisation of factor–response relationships and optimisation and prediction tools allow users to identify factor settings that maximise, minimise, or achieve a target response.*

The DoEIY software comprises 5 modules or tabs; these can be found in the navigation bar on the left:

- **Make a Design**
- **Enter/Edit Results**
- **Analyze the Design**
- **Model Explorer**
- **Resources & References**

This layout mirrors the natural progression of a DoE; first you create an experimental design based on your needs, then you carry out the experiments and record your results, next you fit and assess a model, and finally, you use the model as needed (e.g. to maximise a response, understand how different experimental conditions impact the response). When you launch the webapp, you will land on the **Make a Design** tab. You can click on any of the other tabs in the navigation bar on the left to navigate to one of the other sections.

**Note: The software will time-out after 30 minutes of inactivity. DoEIY doesn't store any information between sessions so be sure to save your design to continue working on it later.**

# Make a Design

The first step in the DoE process is to Make a Design. You begin by selecting a design from the dropdown menu:

![Make a Design](figures/Make_a_Design__Choose_Design.png)  
*Make a Design. The page in DoEIY where you select a design type from the dropdown menu and enter the factor details to create a design.*

...

# Screening Designs

![Plackett-Burman Design](figures/Plackett_Burman.png)  
*Make a Plackett-Burman Design. a. Factor details window for entering names, types, and two level values in a Plackett–Burman design. b. Design details window showing a summary of factors and runs, with the option to randomise the run order.*

...

# Factorial Designs

![Full Factorial Design](figures/Full_Factorial.png)  
*Make a Full Factorial Design. a. Factor details window for specifying names, types, number of levels, and level values in a full factorial design. b. Design details window summarising the factors and the total number of runs, with the option to randomise the run order.*

![Fractional Factorial Design](figures/Fractional_Factorial.png)  
*Make a Fractional Factorial Design. a. Factor details window for entering names, types, and two level values. b. Design diagnostics window with dropdown menus to select the total number of runs and the number of blocks, plus the option to randomise.*

...

# Response Surface Designs

![Box-Behnken Design](figures/Box_Behnken.png)  
*Make a Box-Behnken Design. a. Factor details window for entering names and three equally spaced levels for each continuous factor. b. Design diagnostics window where blocking can be selected, along with the option to randomise.*

![Central Composite Design](figures/Central_Composite.png)  
*Make a Central-Composite Design. a. Factor details window for specifying names and upper/lower level values for each continuous factor. b. Design details window where the design type (circumscribed, inscribed, or face-centred) is selected, with the option to randomise.*

...

# Space Filling Designs

![Latin Hypercube Design](figures/Latin_Hypercube.png)  
*Make a Latin Hypercube Design. a. Factor details window for entering names, types, and level values. b. Design details window for selecting the number of runs, with the option to randomise.*

...

# Custom Designs

![D-Optimal Design](figures/D_Optimal.png)  
*Make a D-Optimal Design. a. Factor details window for entering names, types, and level values. b. Design details window for selecting model components and specifying the number of runs.*

# Entering and Editing Results

![Enter/Edit Results](figures/Enter_Edit_Results.png)  
*Enter/Edit Results. The tab where you can enter responses, save or load designs, and edit rows or columns in the design matrix.*

...

# Analyze the Design

![Analyze the Design](figures/Analyze_Design.png)  
*Analyze the Design. The tab containing Create Model, ANOVA, and Design Diagnostics subtabs.*

![Model Specification](figures/Model_Specification.png)  
*Model Specification. The Create Model subtab, showing the default-model dropdown, model term lists, and controls for adding, removing, or crossing terms.*

![ANOVA Results](figures/ANOVA_Results.png)  
*ANOVA Results. The ANOVA subtab, showing the parity plot, model fit metrics, and excerpts from the ANOVA and Estimates tables.*

![Diagnostics](figures/Diagnostics.png)  
*Diagnostics. The Design Diagnostics subtab, showing an alias report and a correlation heatmap.*

# Model Explorer

![Factor-Response Visualizer](figures/Visualizer.png)  
*Factor-Response Visualizer. The Visualizer subtab, showing factor-response plots with sliders and a predicted response read-out.*

![Interact Optimisation](figures/Interact.png)  
*Interact Optimisation. The Interact subtab, showing goal selection, optional starting values, and the returned solution.*

# DoE Glossary

Before using DoEIY, it is helpful to review some core DoE terminology:

- **Factors** are the variables you choose to study and deliberately control, such as temperature, solvent choice, or concentration.
- **Levels** are the values for each factor that are investigated.
- The **Design Space** is the multi-dimensional region defined by the factors.
- A **Response** is the measured output(s) of interest.
- **Blocking** is a way to group experimental runs to account for known sources of variability.
- An **Interaction** is when the effect of one factor on the response changes depending on the level of another factor.
- A **Run** is a single execution of the experiment at a specific combination of factor levels.
- **Aliasing** occurs when the effects of two or more factors cannot be distinguished due to the design.
- **Correlation** describes how strongly two model terms vary together.
- **ANOVA** is a statistical method used to separate total variability into explained and unexplained parts.
- **Rotatability** ensures prediction precision depends only on distance from the design centre.
- **Orthogonality** ensures different model terms are uncorrelated.

[^1]: Corresponding author: <s.guldin@ucl.ac.uk>
