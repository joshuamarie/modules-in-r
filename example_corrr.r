box::use(
    ./module/statistics/corrr
)

set.seed(123)
xx = rnorm(10000) 
yy = rnorm(10000) 
zz = rnorm(10000)

corrr$corr(xx, yy, zz)
# corrr$corr(speed, dist, .data = cars) <- This doesn't work...for now

# Pipe is provided
cars |> 
    corrr$cor_pipe()
cars |> 
    corrr$cor_pipe(speed, dist)

# When you want the applied changes
box::reload(corrr)
# Remove the module cache
box::unload(corrr)
