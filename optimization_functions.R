# Functions to formulate and solve an energy storage dispatch optimization
require(CVXR)
require(magrittr)

sum_maxes <- function(dispatch_vector, rates_matrix) {
    # Calculates the demand charges of a tariff given its rates and a dispatch.
    # Returns a scalar demand charge ($)
    
    # Parameters:
    # dispatch_vector: A vector of kW consumed at a site (neg is power sold)
    # rates_matrix: A matrix with the same number of rows as dispatch_vector
    #   and a column for each peak to be added to the total demand charge ($/kW)
    
    # ----------
    
    
    # Multiply the dispatch vector element-wise by each column of rates matrix
    # then return the max of each column
    column_max_list <- rates_matrix %>%
        apply(MARGIN = 2, FUN = function(x)
            multiply(lh_exp = x, rh_exp = dispatch_vector)) %>%
        lapply(max)
    
    # Sum the max of each column into one scalar value
    scalar_sum <- Reduce(f = '+', x = column_max_list)
    
    return(scalar_sum)
    
}


optimize_dispatch <- function(storage_power, storage_energy, poi_limit,
                              charge_efficiency, discharge_efficiency,
                              soc_start, fees_kWh, tax_kWh,
                              charge_solar_only,
                              price_kWh, solar = NULL, load = NULL,
                              peakcost_monthly1 = NULL,
                              peakcost_monthly2 = NULL,
                              peakcost_daily1 = NULL,
                              peakcost_daily2 = NULL) {
    # Optimize the dispatch of an energy storage system by maximizing net
    # revenue (or minimizing net costs) of solar + storage system and tariff
    # structure defined in parameters.
    # Returns a vector of storage dispatch (charge = pos, discharge = neg)(kWac)
    
    # Scalar Parameters:
    # storage_power: The AC Capacity of storage inverter (kW)
    # storage_energy: The Nominal (DC) Capacity of energy storage (kWh)
    # poi_limit: The max amount of power that can be exported to grid (kWac)
    # charge_efficiency: Percent of power converted from AC power to DC charge
    # discharge_efficiency: Percent of power converted from DC to AC
    # soc_start: The state of charge of storage system at start of period (kWh)
    # fees_kWh: The cost of electricity delivery fees on utility bill ($/kWh)
    # tax_kWh: The electricity delivery tax added to utility bill (%)

    # Boolean Parameters:
    # charge_solar_only: TRUE means storage system can only charge when
    #   offsetting solar production.  FALSE allows to charge from grid.
    
    # Vector Parameters:
    # price_kWh: Timeseries electricity prices ($/kWh)
    # solar: Timeseries solar production (kWhac)
    # load: Timeseries load consumption (same meter as solar and storage) (kWac)
    
    
    # Matrix Parameters:
    # peakcost_monthly1: The hourly load pricing for monthly peaks of
    #   optimization period.
    #   Rows = hours during period; Columns = each billable peak;
    #   Should include all applicable taxes and fees ($/kW-month)
    # peakcost_monthly2: Same as peakcost_monthly1 but with different onpeak
    #   hours and prices. ($/kW-month)
    # peakcost_daily1: The hourly load pricing for daily peaks of optimization
    #   period.  Rows = hours during period;  Columns = each billable day;
    #   Should include all applicable taxes and fees ($/kW-day)
    # peakcost_daily2: Same as peakcost_daily1 but with different onpeak hours
    #   and prices. ($/kW-day)
  
    # ----------
    
    
    # Check Inputs ----
    
    # All vectors and matrix columns must be the same length for calculations
    dispatch_length <- length(price_kWh)
    
    # Will call this list multiple times so save it
    peakcost_list <- list(peakcost_monthly1, peakcost_monthly2, peakcost_daily1,
                          peakcost_daily2)
    
    # All vectors are included in Objective Function, 
    # so need to make placeholders for the NULL inputs
    if (is.null(solar)) {
        solar <- rep(0, dispatch_length)
    }
    
    if (is.null(load)) {
        load <- rep(0, dispatch_length)
    }
    
    if (length(solar) != dispatch_length | length(load) != dispatch_length) {
        
        stop(paste('Vector inputs have different lengths: price_kWh',
                   dispatch_length, 'solar', length(solar),
                   'load', length(load)))
    
    }
    
    for (i in 1:length(peakcost_list)) {
      
      if (!is.null(peakcost_list[[i]])) {
        
        if (nrow(peakcost_list[[i]]) != dispatch_length) {
          
          stop(paste('Input matrix', i, 'unequal to vector lengths:',
                     nrow(peakcost_list[[i]]), dispatch_length))
        }
      }
    }
    
    # Define Optimization Variables ----
    
    # Amount of kW charge/discharged during each time_step of dispatch
    # AC-side of converter
    # Note:  Need two variables to apply different efficiency losses
    discharge <- Variable(dispatch_length)
    charge <- Variable(dispatch_length)
    
    # Net Load variable needed because charge/discharge don't always mean
    # net import/export from grid and this affects the value/cost of electricity
    # Notation of net_load assumes a consumption perspective (not "net_export")
    net_load <- load - solar + charge - discharge
    
    # Define Objective Function ----
    
    # Energy Market Value = Energy Price * (sold - purchased) -
    # purchased * (fees + taxes)
    # Messy equation due to taxing the purchased (pos) part of net_load while
    # maintaining Disciplined Convex Programming
    # price * -(bought - sold) - bought * ((price + fees) * (1 + tax) - price)
    # = price * sold  - (price + fees) * (1 + tax) * bought
    
    # The complexity of this equation also slows down generalized solvers,
    # so use the simple formulation when taxes and fees aren't applied
    if (fees_kWh == 0 & tax_kWh == 0) {
      
      energy_arbitrage <- sum_entries(price_kWh * -net_load)
    
    } else {
      
      energy_arbitrage <- sum_entries(price_kWh * -net_load -
          pos(net_load) * ((price_kWh + fees_kWh) * (1 + tax_kWh) - price_kWh))
    
    }
    
    # Demand Charges found by summing up the peak demand * price for each 
    # billable period provided in "peakcost" parameters.
    demandcharge_list <- lapply(peakcost_list, function(x) if (is.null(x)) 0 
                                         else sum_maxes(net_load, as.matrix(x)))
    
    # Maximize Net Revenue (or Minimize Net Costs)
    objective <- Maximize(energy_arbitrage -
                              Reduce(f = '+', x = demandcharge_list))
    
    # Define Constraints ----
    
    constraints <- list(
        # Physics-Based #
        # Note: Charge/Discharge are AC but SOC is DC (nominal)
        charge >= 0,
        discharge >= 0,
        # Can charge inverter amount + efficiency loss
        charge <= storage_power / charge_efficiency,
        # Can only discharge the inverter amount
        discharge <= storage_power,
        # SOC is done on DC side, so must apply efficiency losses
        cumsum_axis(charge * charge_efficiency - discharge /
                        discharge_efficiency) + soc_start <= storage_energy,
        cumsum_axis(charge * charge_efficiency - discharge /
                        discharge_efficiency) + soc_start >= 0,
        # Note:  Don't need a binary constraint forcing either charge and
        # discharge to not be positive at same time because the rest of the 
        # constraints make that a non-optimal dispatch strategy

        # Site Specific #
        # Limit on the amount of power that can be sent to the grid at once
        solar + discharge <= poi_limit,
        # Allowed to charge from grid?
        # charge_solar_only treated as a 1 if TRUE and 0 if FALSE
        charge * as.numeric(charge_solar_only) <= solar
        
        # Operation Limits #
        # Can add degradation/throughput limits here
        # Can add forced discharge and charge events here
    )
    
    # Solve the Problem ----
    
    problem <- Problem(objective, constraints)
    
    # GLPK solves problem formulation most reliably of free CVXR plug-ins
    solution <- solve(problem, solver = 'GLPK')
    
    # CVXR integration with solvers is still premature and solvers sometimes
    # fail on valid problem formulations. If problem fits DCP requirement and 
    # one solver fails, try another one.
    if (solution$status == 'solver_error') {
        
        for (solver in c('OSQP', 'ECOS_BB')) {
            
            solution <- solve(problem, solver = solver)
            
            if (solution$status == 'optimal') {
                break
            }
            
        }
        
    }
    
    # Solution was found!
    # Return the dispatch
    storage <- solution$getValue(charge) - solution$getValue(discharge)
    
    return(storage)
    
}

