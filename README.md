# Supervision guide build files

This repo is a storage location for the build files of the supervision guide available here:
https://robabsmith.github.io/supervision_gitpage/

The GitPage is a "work-in progress" guide for project writing for AAU Economics students. The website (HTML) files are built to the second branch, called gh-pages, using RStudio.

There are additional files in this repo to facilitate a Travis-CI build, but they have been disabled, because I haven't been able to get the build to automate.

To replicate this Repo on a local machine, you will need to have the bookdown package installed, and to follow instructions on building a GitBook on Yihui Xie's pages:

You should be able to just run it if you have the packages installed, if you open `Supervision_GitPage.Rproj`, and then open the `Build` tab in the environment window of Rstudio and click "Build" and choose the `gitbook` option.

**Bookdown Demo**
https://github.com/rstudio/bookdown-demo

**Minimal book example**
https://bookdown.org/flavioadsm/bookdown_one_file_html_1/

**Publishing to a gitbook**
https://bookdown.org/yihui/bookdown/github.html

# Gitbook hosting
You will also need to create a second branch on whatever repo you want to host, called "gh_pages" - but the instructions for doing this are in the third link above.

The gitbook is hosted by simply renaming the folder called "\_book" to "docs", in the gh_pages branch.

Then, from the settings tab in GitHub repo that you have created, open the "pages" tab, and mark that the repo should be presented as a webpage. Choose the `gh-pages` branch, and the `/docs` location rather than `root` option.
