# Executable Environment for OSF Project [74qnu](https://osf.io/74qnu/)

This repository was automatically generated as part of a project to test the reproducibility of open science projects hosted on the Open Science Framework (OSF).

**Project Title:** Predicting Actual Social Skill Expression From Personality and Skill Self-Concepts

**Project Description:**
> Social skills are of key importance in everyday and work life. However, the way in which they are typically assessed via self-report questionnaires has one potential downside; self-reports assess individuals’ global self-concepts, which do not necessarily reflect individuals’ actual social behaviors. In this research, we aimed to investigate how self-concepts assessed via questionnaires relate to skill expression assessed via behavioral observations after short interpersonal simulations. For this, we used an alternative behavior-based skill assessment approach designed to capture expressions of predefined social skills. Self- and observer ratings were collected to assess three different social skills: agency (i.e., getting ahead in social situations), communion (i.e., getting along in social situations), and interpersonal resilience (i.e., staying calm in social situations). We explored how these skills were related to self-concepts by differentiating between a classic personality measure (i.e., Big Five Inventory 2; BFI-2) and a novel skill questionnaire (i.e., Behavioral, Emotional, and Social Skills Inventory; BESSI). The results (N = 137) showed that both personality and skill self-concepts predicted self-rated skill expression, with the BESSI showing incremental validity. For both personality and skills self-concepts, the relationships with observer-rated skill expression were significant for agency but not for communion or interpersonal resilience. We discuss these results and highlight the theoretical and practical importance of differentiating between skill self-concepts and actual skill expression.

**Original OSF Page:** [https://osf.io/74qnu/](https://osf.io/74qnu/)

---

**Important Note:** The contents of the `74qnu_src` folder were cloned from the OSF project on **12-03-2025**. Any changes made to the original OSF project after this date will not be reflected in this repository.

The `DESCRIPTION` file was automatically added to make this project Binder-ready. For more information on how R-based OSF projects are containerized, please refer to the `osf-to-binder` GitHub repository: [https://github.com/Code-Inspect/osf-to-binder](https://github.com/Code-Inspect/osf-to-binder)

## flowR Integration

This version of the repository has the **[flowR Addin](https://github.com/flowr-analysis/rstudio-addin-flowr)** preinstalled. flowR allows visual design and execution of data analysis workflows within RStudio, supporting better reproducibility and modular analysis pipelines.

To use flowR, open the project in RStudio and go to `Addins` > `flowR`.

## How to Launch:

**Launch in your Browser:**

🚀 **MyBinder:** [![Binder](https://mybinder.org/badge_logo.svg)](https://mybinder.org/v2/gh/code-inspect-binder/osf_74qnu-f/HEAD?urlpath=rstudio)

   * This will launch the project in an interactive RStudio environment in your web browser.
   * Please note that Binder may take a few minutes to build the environment.

🚀 **NFDI JupyterHub:** [![NFDI](https://nfdi-jupyter.de/images/nfdi_badge.svg)](https://hub.nfdi-jupyter.de/r2d/gh/code-inspect-binder/osf_74qnu-f/HEAD?urlpath=rstudio)

   * This will launch the project in an interactive RStudio environment on the NFDI JupyterHub platform.

**Access Downloaded Data:**
The downloaded data from the OSF project is located in the `74qnu_src` folder.

## Run via Docker for Long-Term Reproducibility

In addition to launching this project using Binder or NFDI JupyterHub, you can reproduce the environment locally using Docker. This is especially useful for long-term access, offline use, or high-performance computing environments.

### Pull the Docker Image

```bash
docker pull meet261/repo2docker-74qnu-f:latest
```

### Launch RStudio Server

Run the container (with a name, e.g. `rstudio-dev`):
```bash
docker run -it --name rstudio-dev --platform linux/amd64 -p 8888:8787 --user root meet261/repo2docker-74qnu-f bash
```

Inside the container, start RStudio Server with no authentication:
```bash
/usr/lib/rstudio-server/bin/rserver --www-port 8787 --auth-none=1
```

Then, open your browser and go to: [http://localhost:8888](http://localhost:8888)

> **Note:** If you're running the container on a remote server (e.g., via SSH), replace `localhost` with your server's IP address.
> For example: `http://<your-server-ip>:8888`

## Looking for the Base Version?

For the original Binder-ready repository **without flowR**, visit:
[osf_74qnu](https://github.com/code-inspect-binder/osf_74qnu)

