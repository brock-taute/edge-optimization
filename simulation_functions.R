# Functions to setup and run simulations with the optimization engine

get_forecast_sequences <- function(first_forecast_point = 1,
                                   first_forecast_length = 24,
                                   first_forecast_gap = 11,
                                   recurring_forecast_length = 37,
                                   recurring_forecast_gap = 24,
                                   trial_length = 8760) {
    # Prepares a sequence modeling the unveiling of forecasted information
    # over time.  This can be used to provide inputs to the optimize_dispatch
    # function when simulating an operating solar + storage system.
    
    # Parameters:
    # first_forecast_point: Data point of first optimization
    # first_forecast_length: Hours of data in first optimization
    # first_forecast_gap: Hours until second optimization
    # recurring_forecast_length: Hours of data in recurring optimizations
    # recurring_forecast_gap: Hours between recurring optimizations
    # trial_length: Number of hours in analysis
    
    # Default Parameters correspond to the NYSERDA Day-Ahead Pricing Procedure
    # with simulation lasting one year
    
    # Returns:
    # forecast_sequence: A vector of ints indicating the indices where new
    #   forecast info is released
    # forecast_lengths: A vector of ints indicating the length of each
    #   corresponding forecast
    
    # Sequence Algorithm #
    
    # forecast_sequence:
    # first_forecast_point, first_forecast_point + first_forecast_gap,
    # sequence from second point to last point offset by recurring_forecast_gap
    # Last point will always be trial_length - forecast_length
    # Note: (+1 because of R indexing)
    
    # forecast_lengths:
    # first_forecast_length, recurring_forecast_length, 
    # ...., recurring_forecast_length - 1
    
    ## Example Calculation ##
    
    # NYSERDA Full-Year Example: Day Ahead pricing comes in at 11AM.
    # So, 11AM, we know pricing for next day (midnight to midnight)
    
    # 1. First optimization is at start of data (midnight but index of 1 implied
    # Hour Ending) and has a forecast_length until next midnight (24 hours)
    
    # 2. Second optimization is when DA pricing comes in (11am, or index 12
    # (Hour Ending)). Forecast_length is now rest of day + midnight to midnight
    # (13 + 24 = 37)
    
    # 3. The next very many optimizations are spaced 24 hours apart, every day 
    # at 11AM. With 24 hours of new information each, each optimization is for 
    # the next 37 hours.
    
    # 4. On the very last day, we won't get any new pricing at 11am because we
    # don't have that data. So the optimization from the day before is actually
    # the very last dispatch. However it is only for 36 hours (37 - 1) because
    # of the way SOC is calculated. 
    # (This introduces a degree of healthy conservatism as well.)
    
    # Example Results:
    # forecast_sequence = 1, 12, 36, ..., 8700, 8724
    # forecast_lengths = 24, 37, 37, ..., 37, 36
    # Note that because of R indexing,
    # an optimization of length 37 at point 8724 ends in a vector
    # of length 8760 (8724 gets included in the interval)
    
    # ----------
    
    # The last optimization point will always be the recurring_forecast_length
    # away from the final available forecast data point, as no new information
    # will arive.
    # Note: The +1 is because of R base-1 indexing
    last_forecast_point <- trial_length - recurring_forecast_length + 1
    
    # Put together the sequence of optimization points:
    # First, second, recurring...
    forecast_sequence <- c(first_forecast_point,
                           seq(from = first_forecast_point + first_forecast_gap,
                               to = last_forecast_point,
                               by = recurring_forecast_gap))
    
    # If the last datapoint didn't get included (because of an odd timestep
    # interval), add it.
    if (forecast_sequence[length(forecast_sequence)] != last_forecast_point) {
        
        forecast_sequence <- c(forecast_sequence, last_forecast_point)
    
    }
    
    # Put together the vector of forecast_lengths
    # The last 1 is 1 short so SOC can be calculated on that last dispatch hour
    forecast_lengths <- rep(recurring_forecast_length,
                            length(forecast_sequence))
    
    forecast_lengths[1] <- first_forecast_length
    
    forecast_lengths[length(forecast_lengths)] <- recurring_forecast_length - 1
    
    return(list(forecast_sequence = forecast_sequence,
                forecast_lengths = forecast_lengths))
    
}


simulate_remote_net_meter <- function(forecast_sequence, forecast_lengths, 
                                      price_kWh, solar,
                                      storage_power, storage_energy, poi_limit,
                                      charge_efficiency, discharge_efficiency,
                                      soc_start) {
    # Model the optimal dispatch of a solar + storage system participating in
    # Remote Net Metering.
    # This system can only charge when offsetting solar production and has no
    # load connected to the same meter.  So there are no utility costs.
    # An optimal dispatch will shift solar production to take full advantage of
    # the variations in the Day Ahead electricity pricing market.
    # Optimal dispatch will be determined with each release of data, and storage
    # will follow this dispatch until it gets overwritten.
    
    # Vector Parameters:
    # forecast_sequence: sequence of indicies where each optimization occurs
    # forecast_lengths: sequence of the amount of data points in each
    #   optimization
    # price_kWh: Timeseries electricity price for full simulation period ($/kWh)
    # solar: Timeseries solar production for full simulation period (kWhac)
    
    # Scalar Parameters:
    # storage_power: The AC Capacity of storage inverter (kW)
    # storage_energy: The Nominal (DC) Capacity of energy storage (kWh)
    # poi_limit: The max amount of power that can be exported to grid (kWac)
    # charge_efficiency: Percent of power converted from AC power to DC charge
    # discharge_efficiency: Percent of power converted from DC to AC
    # soc_start: The state of charge of storage system at simulation start (kWh)
    
    # Returns:
    # storage: timeseries vector of storage dispatch for each interval of the 
    #   simulation; Charge is positive; Discharge is negative (kWhac)
    # soc: timeseries vector of the state of charge of the storage system for
    #   each interval of the simulation;  State of charge is interval_beginning
    #   (nominal kWhdc)
    
    # ----------
    
    # Set Timer and Check Inputs ----
    
    start_time <- Sys.time()
    
    trial_length <- forecast_sequence[length(forecast_sequence)] +
        forecast_lengths[length(forecast_lengths)]
    
    if (trial_length < length(solar) | trial_length < length(price_kWh)) {
        stop('The analysis period is longer than the available data.')
    }
    
    # Loop through the sequence ----
    
    storage <- rep(0, length(price_kWh))
    soc <- rep(0, length(price_kWh))
    
    # soc always needs to be one ahead of storage since it's hour beginning
    soc[forecast_sequence[1]] <- soc_start
    
    for (n in 1:length(forecast_sequence)) {
        
        i <- forecast_sequence[n]
        forecast_length <- forecast_lengths[n]
        
        # Keep track of progress
        print(paste0(n / length(forecast_sequence) * 100, '% Complete!'))
        
        # Set up optimization parameters #
        
        # Note: -1 because of base-1 indexing
        forecast_range <- i:(i + forecast_length - 1) 
        
        forecast_solar <- solar[forecast_range]
        forecast_price <- price_kWh[forecast_range]
        soc_i <- soc[i]
        
        # Get optimal storage dispatch given current information
        optimal_storage <- optimize_dispatch(storage_power, storage_energy,
                                             poi_limit, charge_efficiency,
                                             discharge_efficiency, soc_i,
                                             fees_kWh = 0, tax_kWh = 0,
                                             charge_solar_only = TRUE,
                                             forecast_price,
                                             forecast_solar)
        
        storage[forecast_range] <- optimal_storage
        
        # To place SOC results, I need to first convert storage back to DC
        dc_storage <- optimal_storage
        
        dc_storage[dc_storage > 0] <- dc_storage[dc_storage > 0] *
            charge_efficiency
        
        dc_storage[dc_storage < 0] <- dc_storage[dc_storage < 0] /
            discharge_efficiency
        
        # And save SOC results where SOC is start of the hour
        # (and thus forward looking soc[i+1] = soc[i] + dc_storage[i])
        soc[forecast_range + 1] <- soc[i] + cumsum(dc_storage)
        
    }
    
    print(Sys.time() - start_time)
    
    return(list(storage = storage, soc = soc))
    
}