

####################################################################################
#Euler Equation. Main function for the policy function iteration algorithm. Not used in the EGM
####################################################################################
function Euler_fun(ce::Economy, a::Float64, y::Float64, ap::Float64, uc_expected::Float64)
    c    = c_fun(ce, a,y,ap)
    diff = u_fun_derivative(ce, c)  - ce.β * ce.R * uc_expected   #This has to be zero (or >0 if it a' binds)
    if  c <1e-5
    diff = 1000.0    # This is just a trick so that the solver never picks c<0
    end
return diff
end
####################################################################################


####################################################################################
function solve_main(ce::Economy, uc_expected::Array{Float64,2})
#-------------------------------------------------------------------------------
#Create matrices to fill-in
ap_mat       = zeros(ce.a_points,ce.y_points);
c_mat        = zeros(ce.a_points,ce.y_points);
error_mat    = zeros(ce.a_points,ce.y_points);
    
    #Create interpolation objects
    itp_uce    = Interpolation_Object(ce, uc_expected)
    
    #Solve for optimal a
    @sync @threads for i_y in 1:length(ce.y_grid)   #Allow for multiple threads
    y   = ce.y_grid[i_y]
    for (i_a, a) in enumerate(ce.a_grid)

    #Check if a'=ϕ binds 
    error_Euler = Euler_fun(ce, a, y, ce.a_grid[1], itp_uce(ce.a_grid[1],y))

    if error_Euler > 0.0
        ap_v       = ce.a_grid[1]
        error_term = 0.0
    else 
    
    #[1] Use non-linear solver to find a' such that Euler Equation holds
    #Issue is that algorithm may not find a solution... and find_zero crashes...
    #fun_ap_fx = (ap::Float64) ->   Euler_fun(ce, a, y, ap, itp_uce[ap,y])
    #ap_v      = find_zero(fun_ap_fx , (ce.a_grid[1],ce.a_grid[end]), Secant())  
    #error_term =0.0

    #[2] Alternative (minimize square error)
    upper_bound               =   min(ce.R*a+exp.(y)-1e-7, ce.a_grid[end])  #Max a' so that c>0. Do not let solver to enter a region with c<0
    fun_ap_fx = (ap::Float64) ->  (Euler_fun(ce, a, y, ap, itp_uce(ap,y)).^2)
    res_ValFx                 =   optimize(fun_ap_fx, ce.a_grid[1], upper_bound,   abs_tol=1e-6, iterations=400, Brent())  #Alternative: GoldenSection()
    ap_v                      =   Optim.minimizer(res_ValFx)
    error_term                =   Optim.minimum(res_ValFx)
    end

    #Store results
    c_val                  =  c_fun(ce, a, y, ap_v);
    ap_mat[    i_a, i_y]   =  ap_v;
    c_mat[     i_a, i_y]   =  c_val;
    error_mat[ i_a, i_y]   =  error_term;
    end 
    end

    #Compute distances across each iteration
    dist_abs    = maximum(abs,ce.ap_mat -  ap_mat);
    dist_norm   = norm(vec(   ce.ap_mat -  ap_mat));

    #Store in ce structure the updated guess for the value function W
    damp       = 0.0   #We can set damp>0 if we want to update W gradually
    ce.ap_mat  = damp * ce.ap_mat + (1-damp)*ap_mat
    
    #Store back in the ce structure
    copy!(ce.c_mat,   c_mat);
    copy!(ce.error_mat, error_mat);

return dist_abs, dist_norm
end
####################################################################################


####################################################################################
# Compute Expectations for value function
# -------------------------------------------------------------------------------
function compute_expected_uc(ce::Economy)
    #Matrix to store results
    uce_mat   = Array{Float64}(undef,size(ce.ap_mat));
    #Create interpolation object 
    itp_ap    = Interpolation_Object(ce, ce.ap_mat);
 
    @sync @threads for i_y in 1:length(ce.y_grid)   #Allow for multiple threads
        y   = ce.y_grid[i_y]
    for (i_ap, ap)  in enumerate(ce.a_grid)  #End-of-period choice of capital
        uce_val = 0.0
        #Next-period shock
        for (i_ϵp, ϵp)  in enumerate(ce.ϵ_grid)
        yp      = ce.ρ*y + ce.σ * ϵp;           #Next-period TFP
        yp      = min(max(yp,ce.y_l), ce.y_u);  #Avoid extrapolating
        app     = itp_ap(ap,yp)
        cp      = c_fun(ce, ap,yp,app)
            uce_val  = uce_val + ce.Pi_ϵ[i_ϵp] * u_fun_derivative(ce, cp)
        end
    uce_mat[i_ap, i_y] = uce_val
    end
    end
    return uce_mat
end
####################################################################################  
  


####################################################################################
# Main value function iteration
####################################################################################
function PolFxIteration(ce::Economy; iter_max=1_000, dist_max=1e-7)
    i = 1;
    dist_norm  = 100;
    dist_abs   = 100;

    while i<iter_max && dist_norm > dist_max
    #Update the expected derivative of utility fx wrt c
    uc_expected          = compute_expected_uc(ce);
    #Solve for policies via Euler Eq
    dist_abs, dist_norm  = solve_main(ce::Economy, uc_expected);
    if i%200 ==0
    println([i, dist_abs,dist_norm])
    end
    i +=1
    end
    println([i, dist_abs,dist_norm])
    return Nothing   #The output overwrites the "ce" structure
end
####################################################################################
  




function General_Equilibrium_StationaryD(; 
    β = 0.975, γ = 2.0, ϕ = 0.0, ρ = 0.90, σ = 0.06, 
    a_points = 1000, a_u = 10.0, y_points = 15, ϵ_points = 5, 
    Interpolation_Type = "Spline", 
    lower_bound = 0.8, upper_bound = 1.025,
    N_agents = 1000, T_sim = 2000,
    StationaryDistr_Type = "Young")

    # Define the objective function INSIDE the wrapper.
    # It takes ONLY the guessed R, satisfying the find_zero requirement, 
    # but it "captures" all the parameters from the outer function.
    function Excess_Asset_Demand(R_guess::Float64)
        
        # Initialize the economy using the parameters captured from above
        ce_GE = ce_economy_polfxit(
            Interpolation_Type = Interpolation_Type,
            StationaryDistr_Type = StationaryDistr_Type,  
            β = β, 
            R = R_guess,    # The solver feeds its guess in right here
            ϕ = ϕ,  
            γ = γ, 
            ρ = ρ, 
            σ = σ, 
            a_points = a_points, 
            a_u = a_u, 
            y_points = y_points, 
            ϵ_points = ϵ_points,
            N_agents = N_agents,
            T_sim = T_sim
        )
        
        # Solve the Partial Equilibrium
        PolFxIteration(ce_GE, iter_max=1_000, dist_max=1e-5)
        
        # Compute the Stationary Distribution

        if StationaryDistr_Type == "Young"
            Density_t = StationaryDistribution_quiet(ce_GE)
        
            # Calculate Aggregate Asset Demand
            agg_assets = sum(ce_GE.ap_mat .* Density_t)
        
            println("Tested R = ", round(R_guess, digits=6), " | Aggregate Assets = ", round(agg_assets, digits=6))
        else
            a_sim, _ = StationaryDistribution_MonteCarlo(ce_GE, N_agents = N_agents, T_sim = T_sim)

            agg_assets = sum(a_sim) / N_agents

            println("Tested R = ", round(R_guess, digits=6), " | Aggregate Assets = ", round(agg_assets, digits=6))

        end
        return agg_assets
    end

# Execute the Solver with Explicit Tolerances
    @time R_star = find_zero(
        Excess_Asset_Demand, 
        (lower_bound, upper_bound), 
        Roots.Bisection(); 
        maxiters = 50,       # Stop after 50 attempts
        atol = 1e-3,         # Stop if aggregate assets are within 0.001 of exactly zero
        xatol = 1e-4         # Stop if it starts adjusting R by less than 0.0001
    )

    println("Equilibrium Market Clearing Interest Rate (R*): ", R_star)

    return R_star
end     





### Function to solve for the transition dynamics given a calculated steady state with any of the previous methods

function Solve_Transition_Dynamics(ce_ss::Economy, R_ss::Float64, ν::Float64, T::Int; iter_max=100, tol=1e-4)

    # 1. Generate deterministic path for aggregate income Y_t
    # Income follows: Y_0 = 1; Y_1 = 1+ν; Y_t+1 = (1-ρ)+ρY_t
    # The path is deterministic because there is no aggregate risk; if we had aggregate risk we would need to compute this incorporating the aggregate shocks
    # Individuals still face individual shocks, but the aggregate distribution/path is known, so they do not have to recalculate their policy functions.
    Y_path = ones(T)
    Y_path[1] = 1.0       # t=0
    Y_path[2] = 1.0 + ν   # t=1: shock
    for t in 3:T    # No more shocks after t=1
        Y_path[t] = (1 - ce_ss.ρ) + ce_ss.ρ * Y_path[t-1]
    end

    # 2. Initialize R path guess
    R_path = fill(R_ss, T)

    # Pre-allocate paths for policies and distributions
    ap_path = [zeros(ce_ss.a_points, ce_ss.y_points) for _ in 1:T]
    dist_path = [zeros(ce_ss.a_points, ce_ss.y_points) for _ in 1:T]
    agg_assets = zeros(T)

    # Get initial SS distribution (t=0) as this is both the initial and terminal condition
    Density_ss = StationaryDistribution_quiet(ce_ss)

    for iter in 1:iter_max
        # ---------------------------------------------------------
        # Backward step: solve for the T-1 problem first given the steady state 
        # ---------------------------------------------------------
        # At T, continuation values match the steady state 
        ap_path[T] = ce_ss.ap_mat
        expected_uc_next = compute_expected_uc(ce_ss) # we can calculate the expected utility/value function for the next period instead of guessing it because we have the policy functions

        for t in (T-1):-1:1
            ce_t = deepcopy(ce_ss) # copy the structure to modify the R in each loop-time (Gemini's suggestion, I don't know this command)
            ce_t.R = R_path[t]
            
            # Incorporate aggregate shock by shifting the log-income grid 
            ce_t.y_grid = ce_ss.y_grid .+ log(Y_path[t])
            ce_t.y_grid_bs = range(ce_t.y_grid[1], ce_t.y_grid[end], length=ce_t.y_points)
            ce_t.y_l = ce_t.y_grid[1]    # avoid extrapolation again
            ce_t.y_u = ce_t.y_grid[end]  # avoid extrapolation again
            # Solve static problem given next period's expected marginal utility
            dist_abs, dist_norm = solve_main(ce_t, expected_uc_next)
            ap_path[t] = ce_t.ap_mat
            
            # Update expected_uc_next to be used for period t-1
            expected_uc_next = compute_expected_uc(ce_t) # pre-compute for all grid points the expected utility
        end

        # ---------------------------------------------------------
        # 4. Forward step: Using the guessed R path and variables, calculate distributions
        # ---------------------------------------------------------
        dist_path[1] = Density_ss

        for t in 1:(T-1)
            ce_t = deepcopy(ce_ss)
            ce_t.ap_mat = ap_path[t]
            
            # Shift income grid so the Update_Distribution interpolation matches t
            ce_t.y_grid = ce_ss.y_grid .+ log(Y_path[t])
            ce_t.y_l = ce_t.y_grid[1]    # avoid extrapolating again
            ce_t.y_u = ce_t.y_grid[end]  # avoid extrapolating again

            dist_path[t+1] = Update_Distribution(ce_t, dist_path[t])
        end

        # ---------------------------------------------------------
        # 5. Calculate aggregate assets by the asset levels times their ad-hoc distribution (after guessing)
        # ---------------------------------------------------------
for t in 1:T
            agg_assets[t] = sum(ap_path[t] .* dist_path[t])
        end

        # We only check errors for the transition periods (t=2 to T-1)
        transition_assets = agg_assets[2:T-1]
        
        # Find the index of the element with the largest absolute magnitude
        max_idx = argmax(abs.(transition_assets))
        
        # Extract the actual value using that index (this preserves the sign)
        signed_max_error = transition_assets[max_idx]
        
        # The absolute error is still needed for the while-loop/convergence tolerance
        error_assets = abs(signed_max_error)
        
        println("Iteration $iter | Max Excess Demand (with sign): $(round(signed_max_error, digits=8))")
        println("Iteration $iter | Maximum Absolute Error: $(round(error_assets, digits=8))")

        if error_assets < tol
            println("Transition dynamics converged")
            break
        end

        # Update Guess: Dampening for updating the R_guess (Gemini calls it simple relaxation method); smooths guess-jumps
        # This dampening was suggested by Gemini because excess demand was jumping from 10 to -1 very aggressively
        update_speed = 0.005 
        R_path[2:T-1] = R_path[2:T-1] .- update_speed .* agg_assets[2:T-1]
    end

    return R_path, Y_path, ap_path, dist_path
end