price asset volatility rate strike expiry nosteps = 
    let timestep = expiry / nosteps
        discountFactor = exp (-rate * (fromIntegral timestep))
        temp1 = exp ((rate * volatility^2) * timestep)
        temp2 = 0.5 * (discountFactor * temp1)
        u = temp2 + sqrt (temp2^2 - 1)
        d = 1 / u
        p = ((exp (rate * (fromIntegral timestep)) - d)) / (u - d)
        zip [nosteps, nosteps - 1, 0] [0, 1, nosteps]
    in 

        0
