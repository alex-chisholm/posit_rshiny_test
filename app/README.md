# Getting Started

Here's how to run this app locally:

## Running the app for the first time

1. **Extract the git repository to your local machine:**
```
git clone git@github.com:thefreshwatertrust/standard_web_app.git
```

2. **Open the project on RStudio:** 
In the directory `app/`, open file `app.Rproj`

3. **Create a DB connection:**
Create a `.Renviron` file with the following code and save it in the `app` directory.

```  
dbname = 'dbname'
host = 'host'
user = 'user'
password = 'pass'
```

**Do not forget to replace the credentials with their correct values.**

4. **Configure app packages**
```  
# Install renv package
# install.packages("renv")
library(renv)

# Restore the project's package environment
renv::restore()
```

## Important

- If you modify the list of packages used, update the R environment:
```
renv::snapshot()
```

- Do not forget to push your changes so that others can access the updated app.

## Deployment
The deployment of our Shiny Apps is done using the platform https://www.shinyapps.io/.
Contact the team for further instructions
