# Wealth
Implementations of Model Wealth Prediction Markets in various languages as shown in https://arxiv.org/pdf/1201.6655.pdf

## Examples
### Julia
```julia
w = Wealth(["a", "b", "c"])

md = calculate_market_details([
  SourceProbabilityEstimate("a", 0.6),
  SourceProbabilityEstimate("b", 0.5),
  SourceProbabilityEstimate("c", 0.8)
], w)

update_wealth!(md, true, w)
println(w)
#> Wealth(["a", "b", "c"], [0.3157894736842105, 0.2631578947368421, 0.42105263157894735])
```
### R
w <- create_new_wealth_record(c("a", "b", "c"))
md <- calculate_market_details(tibble(source = c("a", "b", "c"), probability = c(0.6, 0.5, 0.8)), w)
w <- update_wealth(md, T, w)
print(w)
#>   source wealth
#>   <chr>   <dbl>
#> 1 a       0.316
#> 2 b       0.263
#> 3 c       0.421

## Lanuages supported
- [x] Julia
- [x] R
- [ ] TypeScript
