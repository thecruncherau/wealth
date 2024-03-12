# Wealth
Implementations of Model Wealth Prediction Markets in various languages as shown in https://arxiv.org/pdf/1201.6655.pdf

## An example
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

## Lanuages supported
- [x] Julia
- [ ] R
- [ ] TypeScript
