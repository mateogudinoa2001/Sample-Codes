#=
 Mateo Gudino Alvarado
 Macroeconomics II - Matias matiasmoretti
 Problem Set 4
=#
       

        
 #Set path 
#--------------------------------------------------------------------------------------------------------
cd("C:/Users/mateo/Dropbox/Tareas Rochester/Matías/Tareas/PS4")  
  #Include .jl files with the functions
#--------------------------------------------------------------------------------------------------------
  include("Example1_PolFxIteration/00_GeneralFunctions.jl");    #Load some general functions that we will use in all of our examples
  include("Example1_PolFxIteration/Example1_01_Structure.jl");     #Load the structure of the model for the Spline interpolation case
  include("Example1_PolFxIteration/10_YoungMethod.jl");
  include("Example1_PolFxIteration/Example1_02_Solvers.jl");       #Load the main solver


### Question 1

# Solve the problem using PFI and approximate the invariant distribution with Young's Method

# Increase borrowing constraint to ϕ=1 because, if not, no one borrows/lends
youngEQ = General_Equilibrium_StationaryD(β = 0.975, γ = 2.0, ϕ = 1.0, ρ = 0.90, σ = 0.06, a_points = 1000, a_u = 10.0, y_points = 15, ϵ_points = 5, Interpolation_Type = "Spline", lower_bound = 0.8, upper_bound = 1.025, N_agents = 1000, T_sim = 2000, StationaryDistr_Type = "Young")



  ##################################################################################################################################
  #Select the type of interpolation: "Linear"; "Spline"; "Spline_Cubic" and solve the model
  #--------------------------------------------------------------------------------------------------------
  ce_y = ce_economy_polfxit(; Interpolation_Type ="Spline",  β= 0.975, R=youngEQ,  ϕ=1.0,  γ=2.0, ρ=0.90, σ=0.06, a_points=1000, a_u=10.0, y_points=15, ϵ_points=5);
  @time PolFxIteration(ce_y::Economy, iter_max=1_000, dist_max=1e-5)
  #Plot policy function
  plot(ce_y.a_grid[10:end],  ce_y.ap_mat[10:end, 4],   lw=1.5, label="Low income",  color=:red)
  plot!(ce_y.a_grid[10:end], ce_y.ap_mat[10:end, 12], lw=1.5, label="High income", color=:blue)
  ##################################################################################################################################
savefig("Assets_Young.png")



  #Chech error terms of solver
  maximum(abs,ce_y.error_mat)

  #Plot Stationary Distribution
  Density_t, p1, p1_log, p2  = StationaryDistribution_fx(ce_y::Economy);
  p1
  savefig("AssetsDistr_Young.png")

  p2
  savefig("IncomeDistr_Young.png")




# Solve the problem using PFI and approximate the invariant distribution with Monte Carlo Simulations

mcEQ = General_Equilibrium_StationaryD(β = 0.975, γ = 2.0, ϕ = 1.0, ρ = 0.90, σ = 0.06, a_points = 1000, a_u = 10.0, y_points = 15, ϵ_points = 5, Interpolation_Type = "Spline", lower_bound = 0.9, upper_bound = 1.025, N_agents = 1000, T_sim = 2000, StationaryDistr_Type = "MC")

  #Select the type of interpolation: "Linear"; "Spline"; "Spline_Cubic" and solve the model
  #--------------------------------------------------------------------------------------------------------
  ce_mc = ce_economy_polfxit(; Interpolation_Type ="Spline",  β= 0.975, R=mcEQ,  ϕ=1.0,  γ=2.0, ρ=0.90, σ=0.06, a_points=1000, a_u=10.0, y_points=15, ϵ_points=5);
  @time PolFxIteration(ce_mc::Economy, iter_max=1_000, dist_max=1e-5)
  #Plot policy function
  plot(ce_mc.a_grid[10:end],  ce_mc.ap_mat[10:end, 4],   lw=1.5, label="Low income",  color=:red)
  plot!(ce_mc.a_grid[10:end], ce_mc.ap_mat[10:end, 12], lw=1.5, label="High income", color=:blue)

  savefig("Assets_MC.png")
  ##################################################################################################################################

  #Chech error terms of solver
  maximum(abs,ce_mc.error_mat)

### We perform several different approximation with different number of agents and periods for comparison

# 1000 agents, 2000 periods
a_sim, y_sim = StationaryDistribution_MonteCarlo(ce_mc::Economy, N_agents = 1000, T_sim = 2000)
y_sim = log.(y_sim .+ 1)
  #Plot Stationary Distribution
histogram(a_sim, bins=1000, normalize=:probability, 
          title="Assets (Monte Carlo)", 
          label="", color=:blue, alpha=0.7)
savefig("AssetsDistr1000_MC.png")
histogram(y_sim, bins=100, normalize=:probability, 
          title="Log Income (Monte Carlo)", 
          label="", color=:red, alpha=0.7)
savefig("IncomeDistr1000_MC.png")

# 3000 agents, 2000 periods

a_sim, y_sim = StationaryDistribution_MonteCarlo(ce_mc::Economy, N_agents = 3000, T_sim = 2000)
y_sim = log.(y_sim .+ 1)
histogram(a_sim, bins=1000, normalize=:probability, 
          title="Assets (Monte Carlo)", 
          label="", color=:blue, alpha=0.7)
savefig("AssetsDistr3000_MC.png")
histogram(y_sim, bins=100, normalize=:probability, 
          title="Log Income (Monte Carlo)", 
          label="", color=:red, alpha=0.7)
savefig("IncomeDistr3000_MC.png")


# 5000 agents, 2000 periods


a_sim, y_sim = StationaryDistribution_MonteCarlo(ce_mc::Economy, N_agents = 5000, T_sim = 2000)
y_sim = log.(y_sim .+ 1)
histogram(a_sim, bins=1000, normalize=:probability, 
          title="Assets (Monte Carlo)", 
          label="", color=:blue, alpha=0.7)
savefig("AssetsDistr5000_MC.png")
histogram(y_sim, bins=100, normalize=:probability, 
          title="Log Income (Monte Carlo)", 
          label="", color=:red, alpha=0.7)
savefig("IncomeDistr5000_MC.png")

# 10000 agents, 2000 periods


a_sim, y_sim = StationaryDistribution_MonteCarlo(ce_mc::Economy, N_agents = 10000, T_sim = 2000)
y_sim = log.(y_sim .+ 1)
histogram(a_sim, bins=1000, normalize=:probability, 
          title="Assets (Monte Carlo)", 
          label="", color=:blue, alpha=0.7)
savefig("AssetsDistr10000_MC.png")
histogram(y_sim, bins=100, normalize=:probability, 
          title="Log Income (Monte Carlo)", 
          label="", color=:red, alpha=0.7)
savefig("IncomeDistr10000_MC.png")




### ======================================================================================================================================================

### Question 2

#=
Same economy as in Question 1. At time t=0, steady state; at t=1, deterministic shock ν=0.01 to the aggregate income Y
Income follows: Y_0 = 1; Y_1 = 1+ν; Y_t+1 = (1-ρ)+ρY_t
Solve for the entire path of R such that the bond market is in equilibrium using a backward-forward algorithm
=#


R_path, Y_path, ap_path, dist_path = Solve_Transition_Dynamics(ce_y, youngEQ, 0.01, 100; iter_max = 1000, tol=1e-4)



### Transition Plots


T = length(R_path)
c_path = [zeros(ce_y.a_points, ce_y.y_points) for _ in 1:T]

for t in 1:T
    # 1. Shift the log-income grid for period t
    y_grid_t = ce_y.y_grid .+ log(Y_path[t])
    
    # 2. Vectorized Budget Constraint
    # R_path[t] .* ce_y.a_grid scales the asset grid
    # exp.(y_grid_t)' transposes the income grid to a row vector for matrix broadcasting
    # ap_path[t] subtracts the 2D matrix of chosen assets
    c_path[t] = R_path[t] .* ce_y.a_grid .+ exp.(y_grid_t)' .- ap_path[t]
end
i_y = 8 


grid_range = 1:ce_y.a_points

# ==========================================================
# PLOT 1: ASSET POLICY FUNCTION TRANSITION

p_assets = plot(title="Asset Policy a'(a, y)",
                xlabel="Current Assets (a)", 
                ylabel="Next Period Assets (a')",
                legend=:bottomright)

# 45-degree line for reference (where a' = a)
plot!(p_assets, ce_y.a_grid[grid_range], ce_y.a_grid[grid_range], 
      label="45-degree line", color=:black, linestyle=:dot, lw=2)

# Plot policy functions at different stages of the transition
plot!(p_assets, ce_y.a_grid[grid_range], ap_path[1][grid_range, i_y],  label="t = 1 (Shock hits)", lw=2, color=:red)
#plot!(p_assets, ce_y.a_grid[grid_range], ap_path[10][grid_range, i_y], label="t = 10", lw=2, color=:orange)
#plot!(p_assets, ce_y.a_grid[grid_range], ap_path[50][grid_range, i_y], label="t = 50", lw=2, color=:green)
plot!(p_assets, ce_y.a_grid[grid_range], ap_path[T][grid_range, i_y],  label="t = T (Steady State)", linestyle=:dash, color=:black, lw=2)


# ==========================================================
# Consumption policy

p_cons = plot(title="Consumption Policy c(a, y)",
              xlabel="Current Assets (a)", 
              ylabel="Consumption (c)",
              legend=:bottomright)
              
plot!(p_cons, ce_y.a_grid[grid_range], c_path[1][grid_range, i_y],  label="t = 1 (Shock hits)", lw=2, color=:red)
#plot!(p_cons, ce_y.a_grid[grid_range], c_path[10][grid_range, i_y], label="t = 10", lw=2, color=:orange)
#plot!(p_cons, ce_y.a_grid[grid_range], c_path[50][grid_range, i_y], label="t = 50", lw=2, color=:green)
plot!(p_cons, ce_y.a_grid[grid_range], c_path[T][grid_range, i_y],  label="t = T (Steady State)", linestyle=:dash, color=:black, lw=2)


# ==========================================================
# Interest Rate Path

p_rate = plot(1:T, R_path, 
              title="Equilibrium Interest Rate Path (R)",
              xlabel="Time Period (t)",
              ylabel="Market Clearing Interest Rate",
              label="R_t", lw=2.5, color=:blue)

plot!(p_rate, [1, T], [R_path[T], R_path[T]], 
      label="Steady State R", linestyle=:dash, color=:black, lw=1.5)


# ==========================================================

# Calculate Aggregate Paths
agg_c_path = zeros(T)
agg_a_path = zeros(T)
# Aggregate assets are in 0-net supply, but it is still useful to plot them to see the error w.r.t. the guessed interest rate.

for t in 1:T
    agg_c_path[t] = sum(c_path[t] .* dist_path[t])
    agg_a_path[t] = sum(ap_path[t] .* dist_path[t])
end

# Aggregate consumption
t_cons = plot(1:T, agg_c_path, 
              title="Aggregate Consumption Path",
              xlabel="Time Period (t)",
              ylabel="Aggregate Consumption (C_t)",
              label="C_t", lw=2.5, color=:blue)

plot!(t_cons, [1, T], [agg_c_path[T], agg_c_path[T]], 
      label="Steady State C", linestyle=:dash, color=:black, lw=1.5)


      
display(p_assets)
display(p_cons)
display(p_rate)
display(t_cons)

savefig(p_assets, "Transition_Assets.png")
savefig(p_cons, "Transition_Consumption.png")
savefig(p_rate, "Transition_InterestRate.png")
savefig(t_cons, "Aggregate_Consumption_Time.png")




