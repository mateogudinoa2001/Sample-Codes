
"This file defines an economy that we will use for the Spline and Linear interpolation example"

#####################################################################################
# DEFINE THE STRUCTURE FOR THE ECONOMY
#####################################################################################
mutable struct Economy_PolFxIt <: Economy
    #Parameters
    β::Float64     # Discount factor
    γ::Float64     # Risk aversion
    R::Float64     # Interest rate 
    ϕ::Float64     # Max level of borrowing
    ρ::Float64     # TFP Autocorrelation 
    σ::Float64     # TFP Volatility
    #Set grid points
    a_points::Int64  
    y_points::Int64
    ϵ_points::Int64 
    

    #Set grid bounds
    a_l::Float64 
    a_u::Float64
    y_l::Float64
    y_u::Float64
    #Create grids
    a_grid::Array{Float64,1}
    y_grid::Array{Float64,1}
    #Grids (only for the Spline interpolation); requires an StepRangeLen equally spaced object
    a_grid_bs::StepRangeLen{Float64, Base.TwicePrecision{Float64}, Base.TwicePrecision{Float64}, Int64}
    y_grid_bs::StepRangeLen{Float64, Base.TwicePrecision{Float64}, Base.TwicePrecision{Float64}, Int64}
    #Grid for the ϵ shock 
    ϵ_grid::Array{Float64,1}
    #Probability for each discretize iid shock
    Pi_ϵ::Array{Float64,1}

    #Create matrices to store results
    ap_mat::Array{Float64,2}        #Policy function
    c_mat::Array{Float64,2}         #Policy function
    error_mat::Array{Float64,2}     #Auxiliary matrix to store some eror terms

    #Options 
    Interpolation_Type::String   #Choose between Linear interpolation or Spline interpolation (linear and cubic)
    StationaryDistr_Type::String #Choose between "Young" and "MC" (Monte Carlo simulation) 

    N_agents::Int64 # Number of agents for Monte Carlo simulation
    T_sim::Int64 # Number of periods for Monte Carlo simulation

end
####################################################################################


####################################################################################
#Set values for the parameters and compute grids and matrices 
####################################################################################
function ce_economy_polfxit(;β= 0.98, R=1.0, γ=2.0, ϕ=0.0, ρ=0.9, σ=0.1, a_points=50,  a_u=100.0, y_points=5, ϵ_points=5, Interpolation_Type="Linear", StationaryDistr_Type = "Young", N_agents = 1000, T_sim = 2000) 
     
     #Grid for assets 
     a_l       = -ϕ;
     a_grid_bs = range(  a_l,  a_u, a_points)
     a_grid    = collect(a_grid_bs);

     #Grid for income (in logs)
     y_l       = -3. * sqrt(σ^2 / (1-ρ^2));
     y_u       =  3. * sqrt(σ^2 / (1-ρ^2));
     y_grid_bs = range( y_l,  y_u, y_points)
     y_grid    = collect(y_grid_bs);

     #Grid for iid shocks
     ϵ_grid, Pi_ϵ    =  qnwnorm(ϵ_points, 0, 1)
    
     ap_mat     = repeat(a_grid, 1, y_points)  #Initial guess: a'=a
     c_mat      = ones(a_points,  y_points)    #Initial value is irrelevant
     error_mat  = zeros(a_points, y_points)

     return Economy_PolFxIt(    β,γ, R, ϕ, ρ,σ,
                        a_points,y_points, ϵ_points, a_l, a_u, y_l,y_u,
                        a_grid, y_grid, a_grid_bs, y_grid_bs,  
                        ϵ_grid,Pi_ϵ,
                        ap_mat,c_mat, error_mat,
                        Interpolation_Type,
                        StationaryDistr_Type,
                        N_agents, T_sim)
end
####################################################################################


"My example is flexible enough to allow for different types of interpolation (linear or spline).
The function below selects and performs the interpolation, based on the user choice"

####################################################################################
#Create interpolation objects
####################################################################################
function Interpolation_Object(ce::Economy, W)

    if ce.Interpolation_Type =="Linear"
    knots     = (ce.a_grid, ce.y_grid);
    itp_W     = interpolate(knots, W, Gridded(Linear()));

    elseif ce.Interpolation_Type =="Spline"
    itp_W     = scale(interpolate(W, BSpline(Linear())), ce.a_grid_bs, ce.y_grid_bs)

    elseif ce.Interpolation_Type =="Spline_Cubic"
    itp_W = scale(interpolate(W, BSpline(Cubic(Line(OnGrid())))), ce.a_grid_bs, ce.y_grid_bs)

    else
    error("Choose a correct interpolation object: Linear, Spline, or Spline_Cubic") 
    end

return itp_W
end
####################################################################################


