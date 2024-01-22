# Econ 512: Coding for Economics PhD Students

Professor: Bradley Setzler, Penn State

Econ 512 is the required course on Empirical Methods for second-year PhD students at Penn State. Most of my lectures are theory, but we will also have lectures on coding.

During my coding lectures, we will improvise: based in part on class suggestions, I will write code live on the projector while you write similar code in class from your laptop. We will try to follow best-practices, e.g., we will write tests for the code and documentation live. The improvised code will then be pushed to this github repository.



# Getting Started

To prepare for the first live-coding class, complete the following steps:

### Step 1. Install a Language.

Download and install your preferred language. I strongly suggest one of the following:

- **R**: [link](https://mirror.las.iastate.edu/CRAN/). I will primarily use R in class.

- **Julia**: [link](https://julialang.org/downloads/). I will occasionally use Julia in class.

- **Python**: You can install the latest Python release [here](https://www.python.org/downloads/). Unfortunately, to install the packages that we actually use, you will need to separately install third-party package managers. The most popular is [pip](https://pypi.org/project/pip/), but you will likely also need `setuptools` to finish some installations, while alternatives such as `npm` and `brew` are required to install certain packages. You could also install [Anaconda](https://www.anaconda.com) which comes with some packages already installed, but then you're dependent on `conda` commands and Anaconda's sometimes problematic choices of paths and associated permission issues. Most of the package management in Python will occur at the command line, often requiring you to use tools like `vim` to fix issues with defaults, so you'll probably need to learn some command line programming along the way to successfully use Python. No matter which route you take, setting up Python is, and always has been, a disastrous mix of disorganization and arbitrary complexity. If you do not have prior experience with Python, I **strongly recommend** R or Julia. In R and Julia, you just click install and you're ready to perform economic analyses, which is the goal.



### Step 2. Install an IDE and link it to your language.

I recommend using VS Code as your environment for writing and executing code.

Download and install VS Code [here](https://code.visualstudio.com/download).

Once VS Code is installed, it needs to be linked to your language using an extension:

- **R**: First, launch R in the terminal and run the command `install.packages("languageserver")`. Then, install the VS Code extension for R [here](https://marketplace.visualstudio.com/items?itemName=REditorSupport.r). As an alternative to VS Code, you could use RStudio [here](https://posit.co/download/rstudio-desktop/).

- **Julia**: Install the VS Code extension for Julia [here](https://marketplace.visualstudio.com/items?itemName=julialang.language-julia).

- **Python**: Install the VS Code extension for Python [here](https://marketplace.visualstudio.com/items?itemName=ms-python.python). As an alternative to VS Code, you could use Jupyter [here](https://jupyter.org/install).


## Step 3. Configure your local environment.

Here, I will list the steps I took to configure VS Code for use primarily with R:

- **VS Code settings**: In VS Code, you can launch the `Settings` tab and scroll through all of the options to adjust the colors and fonts to help with eye strain, place the side bar and activity bar in your preferred layout, etc. My VS Code settings file `settings.json` is available for reference [here](https://www.github.com/setzler/Econ512/tree/main/Utils/settings.json).

- **Key packages**: In R, use the `install.packages(...)` command to install `data.table`, `ggplot2`, and `fixest`. We will install other packages during class as needed.

- **Linter**: A linter is a program that checks your code in real-time for possible errors. VS Code comes with `lintr` pre-installed. However, it is overly aggressive by default: it considers using `=` instead of `<-` to be a mistake, considers any line with more than 80 characters to be a mistake, considers capitalized variable names (like "TFP") to be a mistake, etc. To make it less aggressive, you can simply copy my `.lintr` file [here](https://www.github.com/setzler/Econ512/tree/main/Utils/.lintr) into your default home directory or your project directory.

- **Spell Checker**: If you are going to use VS Code to write documentation in the `Markdown` language (you are currently reading a markdown `.md` file), it is helpful to install a VS Code extension to check for spelling errors. I use the spell check extension available [here](https://marketplace.visualstudio.com/items?itemName=streetsidesoftware.code-spell-checker). Unfortunately, it spell checks your variable names in R code by default, which doesn't make sense. You can copy the cSpell components from my settings file [here](https://www.github.com/setzler/Econ512/tree/main/Utils/settings.json) to change it to only check spelling of comments within R code.


## Step 4. Configure Github.

We will always work within a github repository.

- **Github:** Create a github account [here](https://github.com/signup?user_email=&source=form-home-signup).

- **Private git repo:** Once you have a Github account, click the  `New` button under "Repositories," give this repository a name, click the "Private" option, then click "Create". You now have a secure private location to store files online. 

- **VS Code git management**: The next step is to clone the repo you made online to your laptop. In VS Code, open the `Source Control` panel and click "Clone Repository." It will ask for the URL of your repo, which you can obtain on Github by clicking the Copy URL button. Once it has been cloned to your laptop, the `Source Control` panel will also visualize the changes you have made in the repo and give you the option to commit and push your changes back to Github.

- **Github Copilot**: For our lecture on using Artificial Intelligence (AI) to assist you in your research, we will rely on the VS Code integration with Github Copilot. First, sign up for Github Copilot [here](https://docs.github.com/en/billing/managing-billing-for-github-copilot/about-billing-for-github-copilot). Note that Copilot requires a credit card to sign up, although it is currently free for the first 60 days. Once you have successfully signed up for Copilot, install the VS Code extension for Copilot [here](https://marketplace.visualstudio.com/items?itemName=GitHub.copilot).
