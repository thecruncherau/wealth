library(tibble)

create_new_wealth_record <- function(sources, wealths = rep(1 / length(sources), length(sources))) {
    if (length(sources) != length(unique(sources))) {
        stop("parameter `sources` must contain unique values")
    }

    if (length(sources) != length(wealths)) {
        stop("parameters `sources` and `wealths` must be the same length")
    }

    return(tibble(source = sources, wealth = wealths))
}

check_for_valid_wealth_record <- function(wealth_record) {
    if (!is_tibble(wealth_record)) {
        stop("parameter `wealth_record` must be a tibble")
    }

    if (!all(c("source", "wealth") %in% names(wealth_record))) {
        stop("parameter `wealth_record` must have names \"source\" and \"wealth\"")
    }

    if (!is.numeric(wealth_record$wealth)) {
        stop("column `$wealth` of parameter `wealth_record` must be numeric")
    }

    if (any(is.na(wealth_record$wealth))) {
        stop("column $wealth of parameter `wealth_record` contains NA values")
    }

    if (any(is.na(wealth_record$source))) {
        stop("column $source of parameter `wealth_record` contains NA values")
    }
}

check_for_valid_source_predictions_df <- function(source_predictions_df) {
    if (!is_tibble(source_predictions_df)) {
        stop("parameter `source_predictions_df` must be a tibble")
    }

    if (!all(c("source", "probability") %in% names(source_predictions_df))) {
        stop("parameter `source_predictions_df` must have names \"source\" and \"probability\"")
    }

    if (length(source_predictions_df$source) != length(unique(source_predictions_df$source))) {
        stop("column $source of parameter `source_predictions_df` must contain only unique values")
    }

    if (!is.numeric(source_predictions_df$probability)) {
        stop("column $probability of parameter `source_predictions_df` must be numeric")
    }

    if (any(is.na(source_predictions_df$source))) {
        stop("column $source of parameter `source_predictions_df` contains NA values")
    }

    if ("kelly_fraction" %in% names(source_predictions_df)) {
        if (!is.numeric(source_predictions_df$kelly_fraction)) {
            stop("column $kelly_fraction of parameter `source_predictions_df` must be numeric")
        }

        if (any(is.na(source_predictions_df$kelly_fraction))) {
            stop("column $kelly_fraction of parameter `source_predictions_df` contains NA values")
        }
    }
}

# source_predictions_df is a tibble with columns `source`, `probability` and (optionally) `kelly_fraction`
calculate_market_details <- function(source_predictions_df, wealth_record) {
    check_for_valid_wealth_record(wealth_record)
    check_for_valid_source_predictions_df(source_predictions_df)

    if (!("kelly_fraction" %in% names(source_predictions_df))) {
        kelly_fractions <- rep(1, nrow(source_predictions_df))
    } else {
        kelly_fractions <- source_predictions_df$kelly_fraction
    }

    if (!all(source_predictions_df$source %in% wealth_record$source)) {
        stop("sources included in `source_predictions_df` must be present in `wealth_record`")
    }

    source_wealths <- wealth_record$wealth[source_predictions_df$source |> sapply(\(x) match(x, wealth_record$source))]

    market_probability <- sum(kelly_fractions * source_wealths * source_predictions_df$probability) /
        sum(kelly_fractions * source_wealths)

    kellyised_source_probabilities <- (kelly_fractions * source_predictions_df$probability) +
        ((1 - kelly_fractions) * market_probability)

    return(
        tibble(
            source = source_predictions_df$source,
            wealth = source_wealths,
            portion_of_wealth_to_stake = ifelse(
                kellyised_source_probabilities > market_probability,
                (kellyised_source_probabilities - market_probability) / (1 - market_probability),
                (market_probability - kellyised_source_probabilities) / market_probability
            ),
            kelly_fraction = kelly_fractions,

            # is the source wagering for or against the event occurring?
            wagering_for_event = kellyised_source_probabilities > market_probability,
            market_probability = market_probability
        )
    )
}

check_for_valid_market_details_df <- function(market_details_df) {
    if (!is_tibble(market_details_df)) {
        stop("parameter `market_details_df` must be a tibble")
    }

    needed_names <- c(
        "source", "wealth", "portion_of_wealth_to_stake", "kelly_fraction",
        "wagering_for_event", "market_probability"
    )

    if (!all(needed_names %in% names(market_details_df))) {
        stop(paste("parameter `market_details_df` must have columns:", paste0(" $", needed_names)))
    }

    if (any(is.na(market_details_df |> unlist()))) {
        stop("there is at least one NA value in parameter `market_details_df")
    }

    if (!is.numeric(market_details_df$wealth)) {
        stop("column $wealth of parameter `market_details_df` must be numeric")
    }

    if (!is.numeric(market_details_df$portion_of_wealth_to_stake)) {
        stop("column $portion_of_wealth_to_stake of parameter `market_details_df` must be numeric")
    }

    if (!is.numeric(market_details_df$kelly_fraction)) {
        stop("column $kelly_fraction of parameter `market_details_df` must be numeric")
    }

    if (!is.logical(market_details_df$wagering_for_event)) {
        stop("column $wagering_for_event of parameter `market_details_df` must be numeric")
    }

    if (!is.numeric(market_details_df$market_probability)) {
        stop("column $market_probability of parameter `market_details_df` must be numeric")
    }
}


update_wealth <- function(market_details, event_occurred, wealth_record) {
    # gotta type check
    market_probability <- market_details$market_probability[1]

    for (ix in seq_len(nrow(market_details))) {
        wealth_record_index <- match(market_details$source[ix], wealth_record$source)
        portion_staked <- market_details$portion_of_wealth_to_stake[ix]
        wealth <- market_details$wealth[ix]

        if (market_details$wagering_for_event[ix] == event_occurred) {
            wealth_record$wealth[wealth_record_index] <- wealth_record$wealth[wealth_record_index] + 
                (portion_staked * wealth) * (1 / (ifelse(event_occurred, market_probability, 1 - market_probability)) - 1)
        } else {
            wealth_record$wealth[wealth_record_index] <- wealth_record$wealth[wealth_record_index] - 
                (portion_staked * wealth)
        }
    }

    return(wealth_record)
}
