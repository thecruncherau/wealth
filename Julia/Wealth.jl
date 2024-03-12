function vector_contains_duplicates(vec::Vector{T}) where {T<:Any}
    return Set(vec) != Set(vec |> unique)
end

mutable struct Wealth
    sources::Vector{String}
    wealths::Vector{Float64}

    Wealth(sources::Vector{String}) = begin
        if vector_contains_duplicates(sources)
            error("parameter `sources` must contain unique values")
        end

        new(sources, fill(1 / length(sources), length(sources)))
    end

    Wealth(sources::Vector{String}, wealths::Vector{Float64}) = begin
        if vector_contains_duplicates(sources)
            error("parameter `sources` must contain unique values")
        end

        new(sources, wealths)
    end
end

struct SourceProbabilityEstimate
    source::String
    probability_estimate::T where {T<:Real}
    kelly_fraction::S where {S<:Real}

    # when kelly_fraction isn't provided, use 1 by default
    SourceProbabilityEstimate(source::String, probability_estimate::T) where {T<:Real} = begin
        new(source, probability_estimate, 1)
    end

    SourceProbabilityEstimate(source::String, probability_estimate::T, kelly_fraction::S) where {T<:Real,S<:Real} = begin
        new(source, probability_estimate, kelly_fraction)
    end
end

struct Wager
    source::String
    wealth::T where {T<:Real}
    portion_of_wealth_to_stake::S where {S<:Real}
    kelly_fraction::R where {R<:Real}
    wagering_for_event::Bool # is this source wagering for or against the event occurring?
end

struct MarketDetails
    market_probability::T where {T<:Real}
    wagers::Vector{Wager}
end

function calculate_market_details(source_probability_estimates::Vector{SourceProbabilityEstimate}, wealth_instance::Wealth)
    source_indices = [
        findfirst(isequal(source_probability_estimate.source), wealth_instance.sources)
        for source_probability_estimate in source_probability_estimates
    ]

    source_wealths = wealth_instance.wealths[source_indices]
    source_estimated_probabilities = map(x -> x.probability_estimate, source_probability_estimates)
    source_kelly_fractions = map(x -> x.kelly_fraction, source_probability_estimates)

    # could be easily made more efficient by saving the unsummed denominator
    market_probability = sum(source_kelly_fractions .* source_wealths .* source_estimated_probabilities) /
                         sum(source_kelly_fractions .* source_wealths)

    return MarketDetails(
        market_probability,
        eachindex(source_indices) .|> (ix -> begin
            kelly_fraction = source_kelly_fractions[ix]
            kellyised_source_probability = kelly_fraction * source_estimated_probabilities[ix] +
                                           (1 - kelly_fraction) * market_probability

            if kellyised_source_probability > market_probability
                portion_of_wealth_to_stake = (kellyised_source_probability - market_probability) / (1 - market_probability)
                wagering_for_event = true
            else 
                portion_of_wealth_to_stake = (market_probability - kellyised_source_probability) / market_probability
                wagering_for_event = false
            end

            return Wager(
                source_probability_estimates[ix].source,
                source_wealths[ix],
                portion_of_wealth_to_stake,
                kelly_fraction,
                wagering_for_event
            )
        end)
    )
end

function update_source_wealth!(wager::Wager, market_probability::Float64, event_occurred::Bool, wealth_instance::Wealth)
    source_index = findfirst(isequal(wager.source), wealth_instance.sources)
    
    if event_occurred == wager.wagering_for_event
        wealth_instance.wealths[source_index] += (wager.portion_of_wealth_to_stake * wager.wealth) * (1 / market_probability - 1)
    else
        wealth_instance.wealths[source_index] -= wager.portion_of_wealth_to_stake * wager.wealth
    end
end

function update_wealth!(market_details::MarketDetails, event_occurred::Bool, wealth_instance::Wealth)
    for wager in market_details.wagers
        update_source_wealth!(wager, market_details.market_probability, event_occurred, wealth_instance)
    end
end
