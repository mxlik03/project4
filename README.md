Visualization of Differentiability Using R Shiny

About This Project
This project is an R Shiny app I made to make differentiability easier to understand visually.
Differentiability can feel a little abstract when it is only shown through formulas, so I wanted to build something that lets you actually see what is happening. The app shows how secant lines approach tangent lines and lets you compare left-hand and right-hand derivative estimates at a selected point.
The goal was to connect the math with something more interactive instead of just solving everything on paper.

What the App Does
- Graphs mathematical functions
- Shows secant and tangent lines
- Estimates left-hand derivatives
- Estimates right-hand derivatives
- Helps show whether a function is differentiable at a point
- Includes a theory section explaining the math behind it
- 
Math Behind It
The project is based on the definition of the derivative:
f'(x) = lim(h -> 0) [f(x + h) - f(x)] / h
The app uses small values of h to estimate the derivative numerically.
If the left-hand and right-hand derivative estimates approach the same value, then the function is differentiable at that point. If they do not match, then the function is not differentiable there.

Tools I Used
- R
- Shiny
- Calculus
- Numerical approximation
- Data visualization
Project Preview

<img width="3840" height="1918" alt="image" src="https://github.com/user-attachments/assets/bb48ee55-44dc-45cf-b423-c2302b77d7e6" />

<img width="3836" height="1914" alt="E7C784DB-AFD0-4044-BCC7-6C339EBA6137" src="https://github.com/user-attachments/assets/8f9d3fff-258d-408d-a37d-14151ee88145" />


What I Learned
This project helped me understand differentiability better because I had to do more than just solve the math. I had to figure out how to turn the math into code and then display it in a way that actually made sense visually.
I also learned that making something work and making something easy for someone else to understand are two different things. Revising this project for a portfolio made me pay more attention to how I explained the math, how the app looked, and how someone else would use it.

Links
- Live Shiny App: https://malik-harrison.shinyapps.io/project4/
- Full Report:** [View Report](Project4_Report.pdf)
- Source Code:** [View app.R](app.R)
  
Running the App
If you want to run the app locally, open the project in RStudio and run:
shiny::runApp()
You may need to install the required R packages first.
