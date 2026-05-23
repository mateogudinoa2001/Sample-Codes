

"This file has the relevant functions to use in Young (2010) algorithm"

#----------------------------------------------------------------------------------------------------
#Search index (lower and uper bound) that best approximate x value
#----------------------------------------------------------------------------------------------------
function Search_x_index(x_grid::Vector{Float64},x::Float64) 
# Dice que seguro hay un paquete que hace esto por ti, pero no importa: esto sólo identifica el aj, aj+1 tal que ap está dentro.
      j   = Int64(0)
      dif = -10.0
      
      while dif<0.0
       tmp_ind = j+1
       dif = x_grid[tmp_ind]-x
       j = j+1
       if j >=length(x_grid)
       dif = 10.0
       end
       end
      j_l     = Int(max( 1,j-1)  )
      if j_l  <=1
      j    = min(length(x_grid), 2)
      end
      
      return  j_l, j   #Lower_bound, upper_bound, and x_grid(lower_bound)
      end
#----------------------------------------------------------------------------------------------------

#---------------------------------------------------------------------------------------------------------------------------------
function Update_Distribution(ce::Economy, Density_t::Array{Float64,2})

      #Create object to store next-period density
      Density_next     = zeros(size(Density_t)); # Esto conviene que esté afuera de la función; son ceros, el lado izquierdo de la ecuación matricial de la slide 18

            for i_y       = 1:ce.y_points 
            y = ce.y_grid[i_y]
            for (i_a, a) in enumerate(ce.a_grid)

                  ap          =  ce.ap_mat[i_a,i_y] # Esta es la policy function de cuando ya resolvimos el modelo con VFI
                  ap_l, ap_u  =  Search_x_index(ce.a_grid,ap); # En las slides, estos son aj y aj+1 para ver que la ap esté en medio
                  weight_a_l  =  (ce.a_grid[ap_u]-ap)/(ce.a_grid[ap_u]-ce.a_grid[ap_l]) #weight for lower bound
                  weight_a_u  =  1-weight_a_l; # Estos son los weights para cada bound, proporcionales a la distancia entre ap y el punto en el grid

                  for (i_ϵp, ϵp)  in enumerate(ce.ϵ_grid) #Esto viene de usar Gaussian Quadrature, entonces necesitamos la siguiente interpolación.
                  wp          =  ce.Pi_ϵ[i_ϵp] #Estos son los pesos de los nodos que sacamos del Tauchen.
                  yp          =  ce.ρ * y  + ce.σ * ϵp
                  yp_l, yp_u  =  Search_x_index(ce.y_grid,yp); # Esta es la parte que toma más tiempo del loop
                  weight_y_l  =  (ce.y_grid[yp_u]-yp)/(ce.y_grid[yp_u]-ce.y_grid[yp_l]) ;  #weight for lower bound
                  weight_y_u  =  1-weight_y_l;
                  # Estos son los 4 casos de la slide 18, y tenemos que agregar (+=) por todas las posibles realizaciones de los shocks.
                  Density_next[ap_l,yp_l]  +=    wp * weight_a_l * weight_y_l  *  Density_t[i_a,i_y];
                  Density_next[ap_u,yp_l]  +=    wp * weight_a_u * weight_y_l  *  Density_t[i_a,i_y];
                  Density_next[ap_l,yp_u]  +=    wp * weight_a_l * weight_y_u  *  Density_t[i_a,i_y];
                  Density_next[ap_u,yp_u]  +=    wp * weight_a_u * weight_y_u  *  Density_t[i_a,i_y];
                  end # Aquí no estamos guardando la Q (como en las slides) porque es muy grande y no la necesitamos para más.
                  
            end
            end

      #Cross check 
      if sum(Density_next)<0.99 || sum(Density_next)>1.01
      throw(error("Density does not add up to 1"))
      end

      return Density_next
      end
#---------------------------------------------------------------------------------------------------------------------------------



#----------------------------------------------------------------------------
# Simulate the Economy until convergence (No Plots)
#----------------------------------------------------------------------------
function StationaryDistribution_quiet(ce::Economy)
      Density_t = ones(ce.a_points, ce.y_points) ./ (ce.a_points * ce.y_points)
      dist = 100
      it   = 0
      while it < 2_000 && dist > 1e-13
            Density_next  = Update_Distribution(ce, Density_t)
            it = it + 1
            dist = maximum(abs, Density_next - Density_t)
            copy!(Density_t, Density_next)
      end
      return Density_t
end
#-------------------------------------------------------------------------------









#----------------------------------------------------------------------------
# Simulate the Economy until convergence of the distribution
#----------------------------------------------------------------------------
function StationaryDistribution_fx(ce::Economy)
# Esta primera loop empieza con una distribución de unos porque no tenemos mejores priors e iteramos para puntos finos
      Density_t = ones(ce.a_points,ce.y_points) ./ (ce.a_points*ce.y_points)
      dist = 100
      it   = 0
      while it<2_000 && dist>1e-13
            # Update density matrix: next-period initial density
            Density_next  = Update_Distribution(ce, Density_t); # Esto es el loop importante de aquí dice
            it = it+1
            dist = maximum(abs, Density_next-Density_t )
            copy!(Density_t,Density_next)
      end
      println(["Distribution Converged at" dist])

      #Plot a histogram for the distribution of assets
      #-------------------------------------------------------------------------------
      a_hist   = zeros(length(ce.a_grid))
      for i_a=1:length(ce.a_grid)
            a_hist_tmp   = 0.
            for i_y=1:length(ce.y_grid)
            a_hist_tmp  = a_hist_tmp  + Density_t[i_a,i_y];
            end
      a_hist[i_a] = a_hist_tmp;
      end
      p1    = Plots.bar(ce.a_grid,       a_hist,  title="Assets (Young)", label="")
      p1_log= Plots.bar(log.(ce.a_grid .+1.0000000001), a_hist,  title="log Assets", label="")
      # Adjusted the plot for log assets (as we have negative assets now)

      #Plot a histogram for the distribution of income (exp)
      #-------------------------------------------------------------------------------
      y_hist   = zeros(length(ce.y_grid))
      for i_y=1:length(ce.y_grid)
            y_hist_tmp   = 0.
            for i_a=1:length(ce.a_grid)
            y_hist_tmp  = y_hist_tmp  + Density_t[i_a,i_y];
            end
      y_hist[i_y] = y_hist_tmp;
      end
      p2= Plots.bar(ce.y_grid,y_hist,  title="log Income (Young)", label="")

      return Density_t, p1, p1_log, p2
      end
#-------------------------------------------------------------------------------


"The following are the relevant functions to compute a Monte-Carlo Simulation of the economy for the stationary distribution"
# Approximation function with Monte Carlo simulation

function StationaryDistribution_MonteCarlo(ce::Economy; N_agents = 1000, T_sim = 2000)
      #Initial values for agents assets and shock
      a_sim = zeros(N_agents)
      y_sim = zeros(N_agents)

      itp_ap = Interpolation_Object(ce, ce.ap_mat)

      #Transitions over T periods
      for t in 1:T_sim
            # shocks for all agents
            ϵ_draws = randn(N_agents)

            #Update each agent with new shocks
            Threads.@threads for i in 1:N_agents
                  # Avoid extrapolating assets and shocks
                  a_curr = min(max(a_sim[i], ce.a_l), ce.a_u)
                  y_curr = min(max(y_sim[i], ce.y_l), ce.y_u)

                  # Asset transition
                  a_sim[i] = itp_ap(a_curr, y_curr)
                  # Shock/income transition
                  y_next = ce.ρ*y_curr + ce.σ*ϵ_draws[i]
                  y_sim[i] = min(max(y_next, ce.y_l), ce.y_u)

            end
      end

      return a_sim, y_sim
end






#Function to compute the transition matrix Q that we can then use to compute the invariant 
# distribution, using the eigenvector decomposition
#---------------------------------------------------------------------------------------------------------------------------------
function Transition_Matrix(ce::Economy)
      # En la slide 15, esto nos va a dar una Q con las mismas dimensiones que en la slide 15, aunque sea sparse.
  #We first need to stack our grids and reshape our policies accordingly
  X_grid = gridmake(ce.a_grid,ce.y_grid)
  ap_vec = reshape(ce.ap_mat, ce.a_points*ce.y_points,1 )
  
  #Create object to store next-period density
  Q_mat     = zeros(ce.a_points*ce.y_points, ce.a_points*ce.y_points);
      
  @sync @threads for i_X = 1:size(X_grid,1)
      y = X_grid[i_X,2]
      a = X_grid[i_X,1]  
            ap          =  ap_vec[i_X]
            ap_l, ap_u  =  Search_x_index(ce.a_grid,ap);
            weight_a_l  =  (ce.a_grid[ap_u]-ap)/(ce.a_grid[ap_u]-ce.a_grid[ap_l]) #weight for lower bound 
            weight_a_u  =  1-weight_a_l;
      
                  for (i_ϵp, ϵp)  in enumerate(ce.ϵ_grid)
                  wp          =  ce.Pi_ϵ[i_ϵp]
                  yp          =  ce.ρ * y  + ce.σ * ϵp
                  yp_l, yp_u  =  Search_x_index(ce.y_grid,yp);
                  weight_y_l  =  (ce.y_grid[yp_u]-yp)/(ce.y_grid[yp_u]-ce.y_grid[yp_l]) ;  #weight for lower bound
                  weight_y_u  =  1-weight_y_l;
                  
                  #Search {ap_l,yp_l} in the X_grid and store results...
                  i_X_ll = matching_indices = findall(i -> X_grid[i, 1] == ce.a_grid[ap_l] && X_grid[i, 2] == ce.y_grid[yp_l], 1:size(X_grid, 1))[1]
                  Q_mat[i_X,  i_X_ll]      +=    wp * weight_a_l * weight_y_l;
                  
                  i_X_ul = matching_indices = findall(i -> X_grid[i, 1] == ce.a_grid[ap_u] && X_grid[i, 2] == ce.y_grid[yp_l], 1:size(X_grid, 1))[1]
                  Q_mat[i_X,  i_X_ul ]  +=    wp * weight_a_u * weight_y_l;
                  
                  i_X_lu = matching_indices = findall(i -> X_grid[i, 1] == ce.a_grid[ap_l] && X_grid[i, 2] == ce.y_grid[yp_u], 1:size(X_grid, 1))[1]
                  Q_mat[i_X,  i_X_lu ]  +=    wp * weight_a_l * weight_y_u;
                  
                  i_X_uu = matching_indices = findall(i -> X_grid[i, 1] == ce.a_grid[ap_u] && X_grid[i, 2] == ce.y_grid[yp_u], 1:size(X_grid, 1))[1]
                  Q_mat[i_X,  i_X_uu]   +=    wp * weight_a_u * weight_y_u;
                  end
                  
      end

      #Normalize elements so that sum of elements in each row = 1
      Q_mat      = Q_mat ./ sum(Q_mat, dims=2);
  
      return Q_mat
      end
#---------------------------------------------------------------------------------------------------------------------------------
  

