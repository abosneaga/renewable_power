# Electricity Demand Forecasting on the CAISO grid
This repository contains the code, models, and data utilized in the Electricity Demand Forecasting capstone project as presented by [Anton Bosneaga](https://www.linkedin.com/in/antonbosneaga/), [Arsh Hothi](https://www.linkedin.com/in/arshhothi/), [Alan Jian](https://alanjjian.github.io/), [Naman Patel](https://www.linkedin.com/in/naman-patel-190694156/), and [Aska Zaman](https://www.linkedin.com/in/aska-zaman/). For a summary of the project, take a look at the [project website](https://www.ischool.berkeley.edu/projects/2023/smarter-grids-greener-planet-energy-demand-forecasting) and interact with the [demo website](https://abosneaga.pythonanywhere.com/).

## Project Directory Organization
- **[`EDA`](https://github.com/abosneaga/renewable_power/tree/main/EDA)**: Contains ipython notebooks pertaining to exploratory data analysis
- **[`data-collection`](https://github.com/abosneaga/renewable_power/tree/main/data-collection)**: Contains ipython notebooks that make API calls to download data necessary for the project
- **[`data-files`](https://github.com/abosneaga/renewable_power/tree/main/data-files)**: Contain downloaded data files in `.csv` format
- **[`deploy`](https://github.com/abosneaga/renewable_power/tree/main/deploy)**: Contain files that aid in the deployment of the demo website
- **[`model_notebooks`](https://github.com/abosneaga/renewable_power/tree/main/model_notebooks)**: Contain ipython notebooks and R notebooks that help train and tune models used in the project
- **[`models`](https://github.com/abosneaga/renewable_power/tree/main/models)**: Contain pickled models ready for deployment
- **[`predictions`](https://github.com/abosneaga/renewable_power/tree/main/predictions)**: Contain `.csv` files with predicted electricity demand corresponding to each particular time period

## Project Dependencies
You can access this project's specific dependencies in the [`deploy/requirements.txt`](https://github.com/abosneaga/renewable_power/tree/main/deploy/requirements.txt) file. To download the required dependencies, we recommend setting up a new conda environment, and running `pip install -r deploy/requirements.txt`.

