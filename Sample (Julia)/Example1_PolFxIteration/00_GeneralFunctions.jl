

"Some general functions that we use across all our examples"


####################################################################################
#Load packages that we will use
####################################################################################
linspace(a,b,n) = range(a; stop=b, length=n)
using QuantEcon 
using Interpolations
using Optim 
using LinearAlgebra
using StaticArrays
using BasisMatrices 
using BenchmarkTools
using Roots
using Plots
using Random

#Allow for multi-threading -- parallelize the code
#--------------------------------------------------------------------------------------------------------
using Base.Threads 
Threads.nthreads()  #check number of threads loaded 
####################################################################################


####################################################################################
#Define the abstract type Economy; will have different subgroups, one for each type of interpolation
####################################################################################
abstract type Economy end


####################################################################################
# Function for consumption; resource constraint of the household
####################################################################################
function c_fun(ce::Economy, a,y,ap)
  c = ce.R * a + exp.(y) - ap    # grid for y is in logs
return c 
end

####################################################################################
#Function to get current a as a function of {c,a',y} [used in the EGM]
####################################################################################
function a_fun(ce::Economy, c,y,ap)
  a = (c .+ ap .- exp.(y)) ./ ce.R
return a
end

####################################################################################
# Utility Function
####################################################################################
function u_fun(ce::Economy, c::Float64)
  utility = (c.^(1.0-ce.γ))/(1.0-ce.γ)
  if ce.γ==1
  utility = log.(c)
  end
  return utility
end

####################################################################################
# Utility Function; first derivative
####################################################################################
function u_fun_derivative(ce::Economy, c::Float64)
  utility_derivative = c.^(-ce.γ)
  if ce.γ==1
  utility_derivative = 1.0./c
  end
  return utility_derivative
end
####################################################################################


####################################################################################
# Value fx: Used only for the VFI algorithm (Example 0)
####################################################################################
function Val_fun(ce::Economy, a::Float64, y::Float64, ap::Float64, V_Expected::Float64)
  c   = c_fun(ce, a,y,ap)
  V   = u_fun(ce, max(c,1e-14 )) + ce.β*V_Expected 
  if  c < 0.0
  V   = -Inf   # This is just a trick so that the solver never picks c<0
  end
return V
end
####################################################################################
